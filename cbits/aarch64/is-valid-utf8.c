/*
Copyright (c) Koz Ross 2021

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of the author nor the names of his contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.
*/
#pragma GCC push_options
#pragma GCC optimize("-O2")
#include <arm_neon.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// Fallback (for tails).
static inline int is_valid_utf8_fallback(uint8_t const *const src,
                                         size_t const len) {
  uint8_t const *ptr = (uint8_t const *)src;
  // This is 'one past the end' to make loop termination and bounds checks
  // easier.
  uint8_t const *const end = ptr + len;
  while (ptr < end) {
    uint8_t const byte = *ptr;
    // Check if the byte is ASCII.
    if (byte <= 0x7F) {
      ptr++;
    }
    // Check for a valid 2-byte sequence.
    //
    // We use a signed comparison to avoid an extra comparison with 0x80, since
    // _signed_ 0x80 is -128.
    else if (ptr + 1 < end && byte >= 0xC2 && byte <= 0xDF &&
             ((int8_t) * (ptr + 1)) <= (int8_t)0xBF) {
      ptr += 2;
    }
    // Check for a valid 3-byte sequence.
    else if (ptr + 2 < end && byte >= 0xE0 && byte <= 0xEF) {
      uint8_t const byte2 = *(ptr + 1);
      bool byte2_valid = (int8_t)byte2 <= (int8_t)0xBF;
      bool byte3_valid = ((int8_t) * (ptr + 2)) <= (int8_t)0xBF;
      if (byte2_valid && byte3_valid &&
          // E0, A0..BF, 80..BF
          ((byte == 0xE0 && byte2 >= 0xA0) ||
           // E1..EC, 80..BF, 80..BF
           (byte >= 0xE1 && byte <= 0xEC) ||
           // ED, 80..9F, 80..BF
           (byte == 0xED && byte2 <= 0x9F) ||
           // EE..EF, 80..BF, 80..BF
           (byte >= 0xEE && byte <= 0xEF))) {
        ptr += 3;
      } else {
        return 0;
      }
    }
    // Check for a valid 4-byte sequence.
    else if (ptr + 3 < end) {
      uint8_t const byte2 = *(ptr + 1);
      bool byte2_valid = (int8_t)byte2 <= (int8_t)0xBF;
      bool byte3_valid = ((int8_t) * (ptr + 2)) <= (int8_t)0xBF;
      bool byte4_valid = ((int8_t) * (ptr + 3)) <= (int8_t)0xBF;
      if (byte2_valid && byte3_valid && byte4_valid &&
          // F0, 90..BF, 80..BF, 80..BF
          ((byte == 0xF0 && byte2 >= 0x90) ||
           // F1..F3, 80..BF, 80..BF, 80..BF
           (byte >= 0xF1 && byte <= 0xF3) ||
           // F4, 80..8F, 80..BF, 80..BF
           (byte == 0xF4 && byte2 <= 0x8F))) {
        ptr += 4;
      } else {
        return 0;
      }
    }
    // Otherwise, invalid.
    else {
      return 0;
    }
  }
  // If we got this far, we're valid.
  return 1;
}

static uint8_t const first_len_lookup[16] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3,
};

static uint8_t const first_range_lookup[16] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8,
};

static uint8_t const range_min_lookup[16] = {
    0x00, 0x80, 0x80, 0x80, 0xA0, 0x80, 0x90, 0x80,
    0xC2, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
};

static uint8_t const range_max_lookup[16] = {
    0x7F, 0xBF, 0xBF, 0xBF, 0xBF, 0x9F, 0xBF, 0x8F,
    0xF4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
};

static uint8_t const range_adjust_lookup[32] = {
    2, 3, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0,
};

static bool is_ascii(uint8x16_t const *const inputs,
                     uint8x16_t const prev_first_len) {
  // Check if we have ASCII, and also that we don't have to treat the prior
  // block as special.
  // First, verify that we didn't see any non-ASCII bytes in the first half of
  // the stride.
  uint8x16_t const first_half_clean = vorrq_u8(inputs[0], inputs[1]);
  // Then we do the same for the second half of the stride.
  uint8x16_t const second_half_clean = vorrq_u8(inputs[2], inputs[3]);
  // Check cleanliness of the entire stride.
  uint8x16_t const stride_clean = vorrq_u8(first_half_clean, second_half_clean);
  // Leave only the high-order set bits.
  uint8x16_t const masked = vandq_u8(stride_clean, vdupq_n_u8(0x80));
  // Finally, check that we didn't have any leftover marker bytes in the
  // previous block: these are indicated by non-zeroes in prev_first_len. In
  // order to trigger a failure, we have to have non-zeroes set in the high bit
  // of the lane: we do this by doing a greater-than comparison with a block of
  // zeroes.
  uint8x16_t const no_prior_dirt = vcgtq_u8(prev_first_len, vdupq_n_u8(0x00));
  // Check for all-zero.
  uint64x2_t const result =
      vreinterpretq_u64_u8(vorrq_u8(masked, no_prior_dirt));
  return !(vgetq_lane_u64(result, 0) || vgetq_lane_u64(result, 1));
}

static void
check_block_neon(uint8x16_t const prev_input, uint8x16_t const prev_first_len,
                 uint8x16_t *errors, uint8x16_t const first_range_tbl,
                 uint8x16_t const range_min_tbl, uint8x16_t const range_max_tbl,
                 uint8x16x2_t const range_adjust_tbl, uint8x16_t const all_ones,
                 uint8x16_t const all_twos, uint8x16_t const all_e0s,
                 uint8x16_t const input, uint8x16_t const first_len) {
  // Get the high 4-bits of the input.
  uint8x16_t const high_nibbles = vshrq_n_u8(input, 4);
  // Set range index to 8 for bytes in [C0, FF] by lookup (first byte).
  uint8x16_t range = vqtbl1q_u8(first_range_tbl, high_nibbles);
  // Reduce the range index based on first_len (second byte).
  // This is 0 for [00, 7F], 1 for [C0, DF], 2 for [E0, EF], 3 for [F0, FF].
  range = vorrq_u8(range, vextq_u8(prev_first_len, first_len, 15));
  uint8x16_t tmp[2];
  // Set range index to the saturation of (first_len - 1) (third byte).
  // This is 0 for [00, 7F], 0 for [C0, DF], 1 for [E0, EF], 2 for [F0, FF].
  tmp[0] = vextq_u8(prev_first_len, first_len, 14);
  tmp[0] = vqsubq_u8(tmp[0], all_ones);
  range = vorrq_u8(range, tmp[0]);
  // Set range index to the saturation of (first_len - 2) (fourth byte).
  // This is 0 for [00, 7F], 0 for [C0, DF], 0 for [E0, EF] and 1 for [F0, FF].
  // This is 'split apart' for speed, as we're not as register-starved as on
  // x86.
  tmp[1] = vextq_u8(prev_first_len, first_len, 13);
  tmp[1] = vqsubq_u8(tmp[1], all_twos);
  range = vorrq_u8(range, tmp[1]);
  // At this stage, we have calculated range indices correctly, except for
  // special cases for first bytes (E0, ED, F0, F4). We repair this to avoid
  // missing in the range table.
  uint8x16_t const shift1 = vextq_u8(prev_input, input, 15);
  uint8x16_t const pos = vsubq_u8(shift1, all_e0s);
  range = vaddq_u8(range, vqtbl2q_u8(range_adjust_tbl, pos));
  // We can now load minimum and maximum values from our tables based on the
  // calculated indices.
  uint8x16_t const minv = vqtbl1q_u8(range_min_tbl, range);
  uint8x16_t const maxv = vqtbl1q_u8(range_max_tbl, range);
  // Accumulate errors, if any.
  errors[0] = vorrq_u8(errors[0], vcltq_u8(input, minv));
  errors[1] = vorrq_u8(errors[1], vcgtq_u8(input, maxv));
}

int bytestring_is_valid_utf8(uint8_t const *const src, size_t const len) {
  if (len == 0) {
    return 1;
  }
  // We step 64 bytes at a time.
  size_t const big_strides = len / 64;
  size_t const remaining = len % 64;
  uint8_t const *ptr = (uint8_t const *)src;
  // Tracking state
  uint8x16_t prev_input = vdupq_n_u8(0);
  uint8x16_t prev_first_len = vdupq_n_u8(0);
  uint8x16_t errors[2] = {
      vdupq_n_u8(0),
      vdupq_n_u8(0),
  };
  // Load our lookup tables.
  uint8x16_t const first_len_tbl = vld1q_u8(first_len_lookup);
  uint8x16_t const first_range_tbl = vld1q_u8(first_range_lookup);
  uint8x16_t const range_min_tbl = vld1q_u8(range_min_lookup);
  uint8x16_t const range_max_tbl = vld1q_u8(range_max_lookup);
  uint8x16x2_t const range_adjust_tbl = vld2q_u8(range_adjust_lookup);
  // Useful constants.
  uint8x16_t const all_ones = vdupq_n_u8(1);
  uint8x16_t const all_twos = vdupq_n_u8(2);
  uint8x16_t const all_e0s = vdupq_n_u8(0xE0);
  for (size_t i = 0; i < big_strides; i++) {
    // Load 64 bytes
    uint8x16_t const inputs[4] = {vld1q_u8(ptr), vld1q_u8(ptr + 16),
                                  vld1q_u8(ptr + 32), vld1q_u8(ptr + 48)};
    // Check if we have ASCII
    if (is_ascii(inputs, prev_first_len)) {
      // Prev_first_len cheaply.
      prev_first_len = vqtbl1q_u8(first_len_tbl, vshrq_n_u8(inputs[3], 4));
    } else {
      uint8x16_t first_len =
          vqtbl1q_u8(first_len_tbl, vshrq_n_u8(inputs[0], 4));
      check_block_neon(prev_input, prev_first_len, errors, first_range_tbl,
                       range_min_tbl, range_max_tbl, range_adjust_tbl, all_ones,
                       all_twos, all_e0s, inputs[0], first_len);
      prev_first_len = first_len;
      first_len = vqtbl1q_u8(first_len_tbl, vshrq_n_u8(inputs[1], 4));
      check_block_neon(inputs[0], prev_first_len, errors, first_range_tbl,
                       range_min_tbl, range_max_tbl, range_adjust_tbl, all_ones,
                       all_twos, all_e0s, inputs[1], first_len);
      prev_first_len = first_len;
      first_len = vqtbl1q_u8(first_len_tbl, vshrq_n_u8(inputs[2], 4));
      check_block_neon(inputs[1], prev_first_len, errors, first_range_tbl,
                       range_min_tbl, range_max_tbl, range_adjust_tbl, all_ones,
                       all_twos, all_e0s, inputs[2], first_len);
      prev_first_len = first_len;
      first_len = vqtbl1q_u8(first_len_tbl, vshrq_n_u8(inputs[3], 4));
      check_block_neon(inputs[2], prev_first_len, errors, first_range_tbl,
                       range_min_tbl, range_max_tbl, range_adjust_tbl, all_ones,
                       all_twos, all_e0s, inputs[3], first_len);
      prev_first_len = first_len;
    }
    // Set prev_input based on last block.
    prev_input = inputs[3];
    // Advance.
    ptr += 64;
  }
  // Combine error carriers with a manually-unrolled loop, then check if
  // anything went awry.
  if (vmaxvq_u8(vorrq_u8(errors[0], errors[1])) != 0) {
    return 0;
  }
  //'Roll back' our pointer a little to prepare for a slow search of the rest.
  uint32_t token;
  vst1q_lane_u32(&token, vreinterpretq_u32_u8(prev_input), 3);
  uint8_t const *token_ptr = (uint8_t const *)&token;
  ptrdiff_t rollback = 0;
  // We must not roll back if no big blocks were processed, as then
  // the fallback function would examine out-of-bounds data (#620).
  // In that case, prev_input contains only nulls and we skip the if body.
  if (token_ptr[3] >= 0x80u) {
    // Look for an incomplete multi-byte code point
    if (token_ptr[3] >= 0xC0u) {
      rollback = 1;
    } else if (token_ptr[2] >= 0xE0u) {
      rollback = 2;
    } else if (token_ptr[1] >= 0xF0u) {
      rollback = 3;
    }
  }
  // Finish the job.
  uint8_t const *const small_ptr = ptr - rollback;
  size_t const small_len = remaining + rollback;
  return is_valid_utf8_fallback(small_ptr, small_len);
}

#pragma GCC pop_options
