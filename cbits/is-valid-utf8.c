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
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __x86_64__
#include <emmintrin.h>
#include <immintrin.h>
#include <tmmintrin.h>
#include <cpuid.h>
#endif

#ifndef __STDC_NO_ATOMICS__
#include <stdatomic.h>
#endif

#include <MachDeps.h>

#ifdef WORDS_BIGENDIAN
#define to_little_endian(x) __builtin_bswap64(x)
#else
#define to_little_endian(x) (x)
#endif

// 0x80 in every 'lane'.
static uint64_t const high_bits_mask = 0x8080808080808080ULL;

static inline int is_valid_utf8_fallback(uint8_t const *const src, size_t const len) {
  uint8_t const *ptr = (uint8_t const *)src;
  // This is 'one past the end' to make loop termination and bounds checks
  // easier.
  uint8_t const *const end = ptr + len;
  while (ptr < end) {
    uint8_t const byte = *ptr;
    // Check if the byte is ASCII.
    if (byte <= 0x7F) {
      ptr++;
      // If we saw one ASCII byte, as long as it's not whitespace, it's quite
      // likely we'll see more.
      bool is_not_whitespace = byte > 32;
      // If possible, do a block-check ahead.
      if ((ptr + 32 < end) && is_not_whitespace) {
        uint64_t const *big_ptr = (uint64_t const *)ptr;
        // Non-ASCII bytes have a set MSB. Thus, if we AND with 0x80 in every
        // 'lane', we will get 0 if everything is ASCII, and something else
        // otherwise.
        uint64_t results[4] = {to_little_endian(*big_ptr) & high_bits_mask,
                               to_little_endian(*(big_ptr + 1)) & high_bits_mask,
                               to_little_endian(*(big_ptr + 2)) & high_bits_mask,
                               to_little_endian(*(big_ptr + 3)) & high_bits_mask};
        if (results[0] == 0) {
          ptr += 8;
          if (results[1] == 0) {
            ptr += 8;
            if (results[2] == 0) {
              ptr += 8;
              if (results[3] == 0) {
                ptr += 8;
              } else {
                ptr += (__builtin_ctzl(results[3]) / 8);
              }
            } else {
              ptr += (__builtin_ctzl(results[2]) / 8);
            }
          } else {
            ptr += (__builtin_ctzl(results[1]) / 8);
          }
        } else {
          ptr += (__builtin_ctzl(results[0]) / 8);
        }
      }
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

#ifdef __x86_64__

// SSE2

static inline int is_valid_utf8_sse2(uint8_t const *const src,
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
      // If we saw one ASCII byte, as long as it's not whitespace, it's quite
      // likely we'll see more.
      bool is_not_whitespace = byte > 32;
      // If possible, do a block-check ahead.
      if ((ptr + 64 < end) && is_not_whitespace) {
        __m128i const *big_ptr = (__m128i const *)ptr;
        // Non-ASCII bytes have a set MSB. Thus, if we evacuate the MSBs, we
        // will get a set bit somewhere if there's a non-ASCII byte in that
        // block.
        uint16_t result = _mm_movemask_epi8(_mm_loadu_si128(big_ptr));
        if (result == 0) {
          ptr += 16;
          // Try one more.
          result = _mm_movemask_epi8(_mm_loadu_si128(big_ptr + 1));
          if (result == 0) {
            ptr += 16;
            // And one more.
            result = _mm_movemask_epi8(_mm_loadu_si128(big_ptr + 2));
            if (result == 0) {
              ptr += 16;
              // Last one.
              result = _mm_movemask_epi8(_mm_loadu_si128(big_ptr + 3));
              if (result == 0) {
                ptr += 16;
              } else {
                ptr += __builtin_ctz(result);
              }
            } else {
              ptr += __builtin_ctz(result);
            }
          } else {
            ptr += __builtin_ctz(result);
          }
        } else {
          ptr += __builtin_ctz(result);
        }
      }
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
    else if (ptr + 2 < end) {
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

// SSSE3

// Lookup tables

// Map high nibble the first byte to legal character length minus 1
// [0x00, 0xBF] --> 0
// [0xC0, 0xDF] --> 1
// [0xE0, 0xEF] --> 2
// [0xF0, 0xFF] --> 3
static int8_t const first_len_lookup[16] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3,
};

// Map first byte to 8th item of range table if it's in [0xC2, 0xF4]
static int8_t const first_range_lookup[16] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8,
};

// Range tables, mapping range index to min and max values
// Index 0    : 00 ~ 7F (First Byte, ascii)
// Index 1,2,3: 80 ~ BF (Second, Third, Fourth Byte)
// Index 4    : A0 ~ BF (Second Byte after E0)
// Index 5    : 80 ~ 9F (Second Byte after ED)
// Index 6    : 90 ~ BF (Second Byte after F0)
// Index 7    : 80 ~ 8F (Second Byte after F4)
// Index 8    : C2 ~ F4 (First Byte, non ascii)
// Index 9~15 : illegal: i >= 127 && i <= -128
static int8_t const range_min_lookup[16] = {
    0x00, 0x80, 0x80, 0x80, 0xA0, 0x80, 0x90, 0x80,
    0xC2, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F,
};

static int8_t const range_max_lookup[16] = {
    0x7F, 0xBF, 0xBF, 0xBF, 0xBF, 0x9F, 0xBF, 0x8F,
    0xF4, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
};

// Tables for fast handling of four special First Bytes(E0,ED,F0,F4), after
// which the Second Byte are not 80~BF. It contains "range index adjustment".
// +------------+---------------+------------------+----------------+
// | First Byte | original range| range adjustment | adjusted range |
// +------------+---------------+------------------+----------------+
// | E0         | 2             | 2                | 4              |
// +------------+---------------+------------------+----------------+
// | ED         | 2             | 3                | 5              |
// +------------+---------------+------------------+----------------+
// | F0         | 3             | 3                | 6              |
// +------------+---------------+------------------+----------------+
// | F4         | 4             | 4                | 8              |
// +------------+---------------+------------------+----------------+
// index1 -> E0, index14 -> ED
static int8_t const df_ee_lookup[16] = {
    0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
};

// index1 -> F0, index5 -> F4
static int8_t const ef_fe_lookup[16] = {
    0, 3, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
};

__attribute__((target("ssse3"))) static inline bool
is_ascii_sse2(__m128i const *src) {
  // OR together everything, then check for a high bit anywhere.
  __m128i const ored =
      _mm_or_si128(_mm_or_si128(src[0], src[1]), _mm_or_si128(src[2], src[3]));
  return (_mm_movemask_epi8(ored) == 0);
}

__attribute__((target("ssse3"))) static inline __m128i
high_nibbles_of(__m128i const src) {
  return _mm_and_si128(_mm_srli_epi16(src, 4), _mm_set1_epi8(0x0F));
}

__attribute__((target("ssse3"))) static inline __m128i
check_block_sse3(__m128i prev_input, __m128i prev_first_len,
                 __m128i const errors, __m128i const first_range_tbl,
                 __m128i const range_min_tbl, __m128i const range_max_tbl,
                 __m128i const df_ee_tbl, __m128i const ef_fe_tbl,
                 __m128i const input, __m128i const first_len) {
  // Get the high 4-bits of the input.
  __m128i const high_nibbles =
      _mm_and_si128(_mm_srli_epi16(input, 4), _mm_set1_epi8(0x0F));
  // Set range index to 8 for bytes in [C0, FF] by lookup (first byte).
  __m128i range = _mm_shuffle_epi8(first_range_tbl, high_nibbles);
  // Reduce the range index based on first_len (second byte)
  // This is 0 for [00, 7F], 1 for [C0, DF], 2 for [E0, EF], 3 for [F0, FF].
  range = _mm_or_si128(range, _mm_alignr_epi8(first_len, prev_first_len, 15));
  // Set range index to the saturation of (first_len - 1) (third byte).
  // This is 0 for [00, 7F], 0 for [C0, DF], 1 for [E0, EF], 2 for [F0, FF].
  __m128i tmp = _mm_alignr_epi8(first_len, prev_first_len, 14);
  tmp = _mm_subs_epu8(tmp, _mm_set1_epi8(1));
  range = _mm_or_si128(range, tmp);
  // Set range index to the saturation of (first_len - 2) (fourth byte).
  // This is 0 for [00, 7F], 0 for [C0, DF], 0 for [E0, EF] and 1 for [F0, FF].
  tmp = _mm_alignr_epi8(first_len, prev_first_len, 13);
  tmp = _mm_subs_epu8(tmp, _mm_set1_epi8(2));
  range = _mm_or_si128(range, tmp);
  // At this stage, we have calculated range indices correctly, except for
  // special cases for first bytes (E0, ED, F0, F4). We repair this to avoid
  // missing in the range table.
  __m128i const shift1 = _mm_alignr_epi8(input, prev_input, 15);
  __m128i const pos = _mm_sub_epi8(shift1, _mm_set1_epi8(0xEF));
  tmp = _mm_subs_epu8(pos, _mm_set1_epi8(0xF0));
  __m128i range2 = _mm_shuffle_epi8(df_ee_tbl, tmp);
  tmp = _mm_adds_epu8(pos, _mm_set1_epi8(0x70));
  range2 = _mm_add_epi8(range2, _mm_shuffle_epi8(ef_fe_tbl, tmp));
  range = _mm_add_epi8(range, range2);
  // We can now load minimum and maximum values from our tables based on the
  // calculated indices.
  __m128i const minv = _mm_shuffle_epi8(range_min_tbl, range);
  __m128i const maxv = _mm_shuffle_epi8(range_max_tbl, range);
  // Calculate the error (if any).
  tmp = _mm_or_si128(_mm_cmplt_epi8(input, minv), _mm_cmpgt_epi8(input, maxv));
  // Accumulate error.
  return _mm_or_si128(errors, tmp);
}

__attribute__((target("ssse3"))) static inline int
is_valid_utf8_ssse3(uint8_t const *const src, size_t const len) {
  // We stride 64 bytes at a time.
  size_t const big_strides = len / 64;
  size_t const remaining = len % 64;
  uint8_t const *ptr = (uint8_t const *)src;
  // Tracking state.
  __m128i prev_input = _mm_setzero_si128();
  __m128i prev_first_len = _mm_setzero_si128();
  __m128i errors = _mm_setzero_si128();
  for (size_t i = 0; i < big_strides; i++) {
    // Pre-load tables.
    __m128i const first_len_tbl =
        _mm_loadu_si128((__m128i const *)first_len_lookup);
    __m128i const first_range_tbl =
        _mm_loadu_si128((__m128i const *)first_range_lookup);
    __m128i const range_min_tbl =
        _mm_loadu_si128((__m128i const *)range_min_lookup);
    __m128i const range_max_tbl =
        _mm_loadu_si128((__m128i const *)range_max_lookup);
    __m128i const df_ee_tbl = _mm_loadu_si128((__m128i const *)df_ee_lookup);
    __m128i const ef_fe_tbl = _mm_loadu_si128((__m128i const *)ef_fe_lookup);
    // Load 64 bytes.
    __m128i const *big_ptr = (__m128i const *)ptr;
    __m128i const inputs[4] = {
        _mm_loadu_si128(big_ptr), _mm_loadu_si128(big_ptr + 1),
        _mm_loadu_si128(big_ptr + 2), _mm_loadu_si128(big_ptr + 3)};
    // Check if we have ASCII.
    if (is_ascii_sse2(inputs)) {
      // Prev_first_len cheaply.
      prev_first_len =
          _mm_shuffle_epi8(first_len_tbl, high_nibbles_of(inputs[3]));
    } else {
      __m128i first_len =
          _mm_shuffle_epi8(first_len_tbl, high_nibbles_of(inputs[0]));
      errors = check_block_sse3(prev_input, prev_first_len, errors,
                                first_range_tbl, range_min_tbl, range_max_tbl,
                                df_ee_tbl, ef_fe_tbl, inputs[0], first_len);
      prev_first_len = first_len;
      first_len = _mm_shuffle_epi8(first_len_tbl, high_nibbles_of(inputs[1]));
      errors = check_block_sse3(inputs[0], prev_first_len, errors,
                                first_range_tbl, range_min_tbl, range_max_tbl,
                                df_ee_tbl, ef_fe_tbl, inputs[1], first_len);
      prev_first_len = first_len;
      first_len = _mm_shuffle_epi8(first_len_tbl, high_nibbles_of(inputs[2]));
      errors = check_block_sse3(inputs[1], prev_first_len, errors,
                                first_range_tbl, range_min_tbl, range_max_tbl,
                                df_ee_tbl, ef_fe_tbl, inputs[2], first_len);
      prev_first_len = first_len;
      first_len = _mm_shuffle_epi8(first_len_tbl, high_nibbles_of(inputs[3]));
      errors = check_block_sse3(inputs[2], prev_first_len, errors,
                                first_range_tbl, range_min_tbl, range_max_tbl,
                                df_ee_tbl, ef_fe_tbl, inputs[3], first_len);
      prev_first_len = first_len;
    }
    // Set prev_input based on last block.
    prev_input = inputs[3];
    // Advance.
    ptr += 64;
  }
  // Write out the error, check if it's OK.
  uint64_t results[2];
  _mm_storeu_si128((__m128i *)results, errors);
  if (results[0] != 0 || results[1] != 0) {
    return 0;
  }
  // 'Roll back' our pointer a little to prepare for a slow search of the rest.
  int16_t tokens[2];
  tokens[0] = _mm_extract_epi16(prev_input, 6);
  tokens[1] = _mm_extract_epi16(prev_input, 7);
  int8_t const *token_ptr = (int8_t const *)tokens;
  ptrdiff_t lookahead = 0;
  if (token_ptr[3] > (int8_t)0xBF) {
    lookahead = 1;
  } else if (token_ptr[2] > (int8_t)0xBF) {
    lookahead = 2;
  } else if (token_ptr[1] > (int8_t)0xBF) {
    lookahead = 3;
  }
  uint8_t const *const small_ptr = ptr - lookahead;
  size_t const small_len = remaining + lookahead;
  return is_valid_utf8_fallback(small_ptr, small_len);
}

// AVX2
//
// These work similarly to the SSSE3 version, but with registers twice the
// width.

static int8_t const first_len_lookup2[32] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3,
};

static int8_t const first_range_lookup2[32] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8,
};

static int8_t const range_min_lookup2[32] = {
    0x00, 0x80, 0x80, 0x80, 0xA0, 0x80, 0x90, 0x80, 0xC2, 0x7F, 0x7F,
    0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x00, 0x80, 0x80, 0x80, 0xA0, 0x80,
    0x90, 0x80, 0xC2, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F,
};

static int8_t const range_max_lookup2[32] = {
    0x7F, 0xBF, 0xBF, 0xBF, 0xBF, 0x9F, 0xBF, 0x8F, 0xF4, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x7F, 0xBF, 0xBF, 0xBF, 0xBF, 0x9F,
    0xBF, 0x8F, 0xF4, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
};

static int8_t const df_ee_lookup2[32] = {
    0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
    0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
};

static int8_t const ef_fe_lookup2[32] = {
    0, 3, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 3, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
};

__attribute__((target("avx,avx2"))) static inline __m256i
high_nibbles_of_avx2(__m256i const src) {
  return _mm256_and_si256(_mm256_srli_epi16(src, 4), _mm256_set1_epi8(0x0F));
}

__attribute__((target("avx,avx2"))) static inline __m256i
push_last_byte_of_a_to_b(__m256i const a, __m256i const b) {
  return _mm256_alignr_epi8(b, _mm256_permute2x128_si256(a, b, 0x21), 15);
}

__attribute__((target("avx,avx2"))) static inline __m256i
push_last_2bytes_of_a_to_b(__m256i const a, __m256i const b) {
  return _mm256_alignr_epi8(b, _mm256_permute2x128_si256(a, b, 0x21), 14);
}

__attribute__((target("avx,avx2"))) static inline __m256i
push_last_3bytes_of_a_to_b(__m256i const a, __m256i const b) {
  return _mm256_alignr_epi8(b, _mm256_permute2x128_si256(a, b, 0x21), 13);
}

__attribute__((target("avx,avx2"))) static inline void
check_block_avx2(__m256i const prev_input, __m256i const prev_first_len,
                 __m256i *errors, __m256i const first_range_tbl,
                 __m256i const range_min_tbl, __m256i const range_max_tbl,
                 __m256i const df_ee_tbl, __m256i const ef_fe_tbl,
                 __m256i const input, __m256i const first_len) {
  // Set range index to 8 for bytes in [C0, FF] by lookup (first byte).
  __m256i range =
      _mm256_shuffle_epi8(first_range_tbl, high_nibbles_of_avx2(input));
  // Reduce the range index based on first_len (second byte)
  // This is 0 for [00, 7F], 1 for [C0, DF], 2 for [E0, EF], 3 for [F0, FF].
  range = _mm256_or_si256(range,
                          push_last_byte_of_a_to_b(prev_first_len, first_len));
  // Set range index to the saturation of (first_len - 1) (third byte).
  // This is 0 for [00, 7F], 0 for [C0, DF], 1 for [E0, EF], 2 for [F0, FF].
  __m256i tmp1 = push_last_2bytes_of_a_to_b(prev_first_len, first_len);
  __m256i tmp2 = _mm256_subs_epu8(tmp1, _mm256_set1_epi8(0x01));
  range = _mm256_or_si256(range, tmp2);
  // Set range index to the saturation of (first_len - 2) (fourth byte).
  tmp1 = push_last_3bytes_of_a_to_b(prev_first_len, first_len);
  tmp2 = _mm256_subs_epu8(tmp1, _mm256_set1_epi8(0x02));
  range = _mm256_or_si256(range, tmp2);
  // At this stage, we have calculated range indices correctly, except for
  // special cases for first bytes (E0, ED, F0, F4). We repair this to avoid
  // missing in the range table.
  __m256i const shift1 = push_last_byte_of_a_to_b(prev_input, input);
  __m256i pos = _mm256_sub_epi8(shift1, _mm256_set1_epi8(0xEF));
  tmp1 = _mm256_subs_epu8(pos, _mm256_set1_epi8(0xF0));
  __m256i range2 = _mm256_shuffle_epi8(df_ee_tbl, tmp1);
  tmp2 = _mm256_adds_epu8(pos, _mm256_set1_epi8(0x70));
  range2 = _mm256_add_epi8(range2, _mm256_shuffle_epi8(ef_fe_tbl, tmp2));
  range = _mm256_add_epi8(range, range2);
  // We can now load minimum and maximum values from our tables based on the
  // calculated indices.
  __m256i const minv = _mm256_shuffle_epi8(range_min_tbl, range);
  __m256i const maxv = _mm256_shuffle_epi8(range_max_tbl, range);
  // Calculate the error, if any.
  errors[0] = _mm256_or_si256(errors[0], _mm256_cmpgt_epi8(minv, input));
  errors[1] = _mm256_or_si256(errors[1], _mm256_cmpgt_epi8(input, maxv));
}

__attribute__((target("avx,avx2"))) static inline int
is_valid_utf8_avx2(uint8_t const *const src, size_t const len) {
  // We stride 128 bytes at a time.
  size_t const big_strides = len / 128;
  size_t const remaining = len % 128;
  uint8_t const *ptr = (uint8_t const *)src;
  // Tracking state.
  __m256i prev_input = _mm256_setzero_si256();
  __m256i prev_first_len = _mm256_setzero_si256();
  __m256i errors[2] = {_mm256_setzero_si256(), _mm256_setzero_si256()};
  for (size_t i = 0; i < big_strides; i++) {
    // Pre-load tables.
    __m256i const first_len_tbl =
        _mm256_loadu_si256((__m256i const *)first_len_lookup2);
    __m256i const first_range_tbl =
        _mm256_loadu_si256((__m256i const *)first_range_lookup2);
    __m256i const range_min_tbl =
        _mm256_loadu_si256((__m256i const *)range_min_lookup2);
    __m256i const range_max_tbl =
        _mm256_loadu_si256((__m256i const *)range_max_lookup2);
    __m256i const df_ee_tbl =
        _mm256_loadu_si256((__m256i const *)df_ee_lookup2);
    __m256i const ef_fe_tbl =
        _mm256_loadu_si256((__m256i const *)ef_fe_lookup2);
    // Load 128 bytes.
    __m256i const *big_ptr = (__m256i const *)ptr;
    __m256i const inputs[4] = {
        _mm256_loadu_si256(big_ptr), _mm256_loadu_si256(big_ptr + 1),
        _mm256_loadu_si256(big_ptr + 2), _mm256_loadu_si256(big_ptr + 3)};
    // Check if we have ASCII.
    bool is_ascii = _mm256_movemask_epi8(_mm256_or_si256(
                        _mm256_or_si256(inputs[0], inputs[1]),
                        _mm256_or_si256(inputs[2], inputs[3]))) == 0;
    if (is_ascii) {
      // Prev_first_len cheaply
      prev_first_len =
          _mm256_shuffle_epi8(first_len_tbl, high_nibbles_of_avx2(inputs[3]));
    } else {
      __m256i first_len =
          _mm256_shuffle_epi8(first_len_tbl, high_nibbles_of_avx2(inputs[0]));
      check_block_avx2(prev_input, prev_first_len, errors, first_range_tbl,
                       range_min_tbl, range_max_tbl, df_ee_tbl, ef_fe_tbl,
                       inputs[0], first_len);
      prev_first_len = first_len;
      first_len =
          _mm256_shuffle_epi8(first_len_tbl, high_nibbles_of_avx2(inputs[1]));
      check_block_avx2(inputs[0], prev_first_len, errors, first_range_tbl,
                       range_min_tbl, range_max_tbl, df_ee_tbl, ef_fe_tbl,
                       inputs[1], first_len);
      prev_first_len = first_len;
      first_len =
          _mm256_shuffle_epi8(first_len_tbl, high_nibbles_of_avx2(inputs[2]));
      check_block_avx2(inputs[1], prev_first_len, errors, first_range_tbl,
                       range_min_tbl, range_max_tbl, df_ee_tbl, ef_fe_tbl,
                       inputs[2], first_len);
      prev_first_len = first_len;
      first_len =
          _mm256_shuffle_epi8(first_len_tbl, high_nibbles_of_avx2(inputs[3]));
      check_block_avx2(inputs[2], prev_first_len, errors, first_range_tbl,
                       range_min_tbl, range_max_tbl, df_ee_tbl, ef_fe_tbl,
                       inputs[3], first_len);
      prev_first_len = first_len;
    }
    // Set prev_input based on last block.
    prev_input = inputs[3];
    // Advance.
    ptr += 128;
  }
  // Write out the error, check if it's OK.
  __m256i const combined_errors = _mm256_or_si256(errors[0], errors[1]);
  if (_mm256_testz_si256(combined_errors, combined_errors) != 1) {
    return 0;
  }
  // 'Roll back' our pointer a little to prepare for a slow search of the rest.
  uint32_t tokens_blob = _mm256_extract_epi32(prev_input, 7);
  int8_t const *tokens = (int8_t const *)&tokens_blob;
  ptrdiff_t lookahead = 0;
  if (tokens[3] > (int8_t)0xBF) {
    lookahead = 1;
  } else if (tokens[2] > (int8_t)0xBF) {
    lookahead = 2;
  } else if (tokens[1] > (int8_t)0xBF) {
    lookahead = 3;
  }
  uint8_t const *const small_ptr = ptr - lookahead;
  size_t const small_len = remaining + lookahead;
  return is_valid_utf8_fallback(small_ptr, small_len);
}

#endif

#ifdef __x86_64__
bool has_sse2() {
  uint32_t eax = 0, ebx = 0, ecx = 0, edx = 0;
  __get_cpuid_count(1, 0, &eax, &ebx, &ecx, &edx);
  // https://en.wikipedia.org/wiki/CPUID#EAX=1:_Processor_Info_and_Feature_Bits
  return edx & (1 << 26);
}

bool has_ssse3() {
  uint32_t eax = 0, ebx = 0, ecx = 0, edx = 0;
  __get_cpuid_count(1, 0, &eax, &ebx, &ecx, &edx);
  // https://en.wikipedia.org/wiki/CPUID#EAX=1:_Processor_Info_and_Feature_Bits
  return ecx & (1 << 9);
}

bool has_avx2() {
  uint32_t eax = 0, ebx = 0, ecx = 0, edx = 0;
  __get_cpuid_count(7, 0, &eax, &ebx, &ecx, &edx);
  // https://en.wikipedia.org/wiki/CPUID#EAX=7,_ECX=0:_Extended_Features
  return ebx & (1 << 5);
}
#endif

typedef int (*is_valid_utf8_t) (uint8_t const *const, size_t const);

int bytestring_is_valid_utf8(uint8_t const *const src, size_t const len) {
  if (len == 0) {
    return 1;
  }
#ifdef __x86_64__
  static _Atomic is_valid_utf8_t s_impl = (is_valid_utf8_t)NULL;
  is_valid_utf8_t impl = atomic_load_explicit(&s_impl, memory_order_relaxed);
  if (!impl) {
    impl = has_avx2() ? is_valid_utf8_avx2 : (has_ssse3() ? is_valid_utf8_ssse3 : (has_sse2() ? is_valid_utf8_sse2 : is_valid_utf8_fallback));
    atomic_store_explicit(&s_impl, impl, memory_order_relaxed);
  }
  return (*impl)(src, len);
#else
  return is_valid_utf8_fallback(src, len);
#endif
}

#pragma GCC pop_options
