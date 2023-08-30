/*
 * Copyright (c) 2003 David Roundy
 * Copyright (c) 2005-6 Don Stewart
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the authors or the names of any contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include "fpstring.h"
#if defined(__x86_64__)
#include <x86intrin.h>
#include <cpuid.h>
#endif

#include <stdint.h>
#include <stdbool.h>

#if defined(__x86_64__) && (__GNUC__ >= 7 || __GNUC__ == 6 && __GNUC_MINOR__ >= 3 || defined(__clang_major__)) && !defined(__STDC_NO_ATOMICS__)
#include <stdatomic.h>
#define USE_SIMD_COUNT
#endif

#include "bytestring-cpp-macros.h"

/* copy a string in reverse */
void fps_reverse(unsigned char *q, unsigned char *p, size_t n) {
    p += n-1;
    while (n-- != 0)
        *q++ = *p--;
}

/* duplicate a string, interspersing the character through the elements
   of the duplicated string */
void fps_intersperse(unsigned char *q,
                     unsigned char *p,
                     size_t n,
                     unsigned char c) {
#if defined(__x86_64__)
  {
    const __m128i separator = _mm_set1_epi8(c);
    const unsigned char *const p_begin = p;
    const unsigned char *const p_end = p_begin + n - 9;
    while (p < p_end) {
      const __m128i eight_src_bytes = _mm_loadl_epi64((__m128i *)p);
      const __m128i sixteen_dst_bytes = _mm_unpacklo_epi8(eight_src_bytes, separator);
      _mm_storeu_si128((__m128i *)q, sixteen_dst_bytes);
      p += 8;
      q += 16;
    }
    n -= p - p_begin;
  }
#endif
    while (n > 1) {
        *q++ = *p++;
        *q++ = c;
        n--;
    }
    if (n == 1)
        *q = *p;
}

/* find maximum char in a packed string */
unsigned char fps_maximum(unsigned char *p, size_t len) {
    unsigned char *q, c = *p;
    for (q = p; q < p + len; q++)
        if (*q > c)
            c = *q;
    return c;
}

/* find minimum char in a packed string */
unsigned char fps_minimum(unsigned char *p, size_t len) {
    unsigned char *q, c = *p;
    for (q = p; q < p + len; q++)
        if (*q < c)
            c = *q;
    return c;
}

int fps_compare(const void *a, const void *b) {
    return (int)*(unsigned char*)a - (int)*(unsigned char*)b;
}

void fps_sort(unsigned char *p, size_t len) {
    return qsort(p, len, 1, fps_compare);
}

#if !HS_UNALIGNED_POKES_OK
  void fps_unaligned_write_u16(uint16_t x, uint8_t *p) {
    memcpy(p, &x, 2);
    return;
  }

  void fps_unaligned_write_u32(uint32_t x, uint8_t *p) {
    memcpy(p, &x, 4);
    return;
  }

  void fps_unaligned_write_u64(uint64_t x, uint8_t *p) {
    memcpy(p, &x, 8);
    return;
  }
#endif

/* count the number of occurrences of a char in a string */
size_t fps_count_naive(unsigned char *str, size_t len, unsigned char w) {
    size_t c;
    for (c = 0; len-- != 0; ++str)
        if (*str == w)
            ++c;
    return c;
}


#ifdef USE_SIMD_COUNT
__attribute__((target("sse4.2")))
size_t fps_count_cmpestrm(unsigned char *str, size_t len, unsigned char w) {
    const __m128i pat = _mm_set1_epi8(w);

    size_t res = 0;

    size_t i = 0;

    for (; i < len && (intptr_t)(str + i) % 64; ++i) {
        res += str[i] == w;
    }

    for (size_t end = len - 128; i < end; i += 128) {
        __m128i p0 = _mm_load_si128((const __m128i*)(str + i + 16 * 0));
        __m128i p1 = _mm_load_si128((const __m128i*)(str + i + 16 * 1));
        __m128i p2 = _mm_load_si128((const __m128i*)(str + i + 16 * 2));
        __m128i p3 = _mm_load_si128((const __m128i*)(str + i + 16 * 3));
        __m128i p4 = _mm_load_si128((const __m128i*)(str + i + 16 * 4));
        __m128i p5 = _mm_load_si128((const __m128i*)(str + i + 16 * 5));
        __m128i p6 = _mm_load_si128((const __m128i*)(str + i + 16 * 6));
        __m128i p7 = _mm_load_si128((const __m128i*)(str + i + 16 * 7));
        // Here, cmpestrm compares two strings in the following mode:
        // * _SIDD_SBYTE_OPS: interprets the strings as consisting of 8-bit chars,
        // * _SIDD_CMP_EQUAL_EACH: computes the number of `i`s
        //    for which `p[i]`, a part of `str`, is equal to `pat[i]`
        //    (the latter being always equal to `w`).
        //
        // q.v. https://software.intel.com/sites/landingpage/IntrinsicsGuide/#text=_mm_cmpestrm&expand=835
#define MODE _SIDD_SBYTE_OPS | _SIDD_CMP_EQUAL_EACH
        __m128i r0 = _mm_cmpestrm(p0, 16, pat, 16, MODE);
        __m128i r1 = _mm_cmpestrm(p1, 16, pat, 16, MODE);
        __m128i r2 = _mm_cmpestrm(p2, 16, pat, 16, MODE);
        __m128i r3 = _mm_cmpestrm(p3, 16, pat, 16, MODE);
        __m128i r4 = _mm_cmpestrm(p4, 16, pat, 16, MODE);
        __m128i r5 = _mm_cmpestrm(p5, 16, pat, 16, MODE);
        __m128i r6 = _mm_cmpestrm(p6, 16, pat, 16, MODE);
        __m128i r7 = _mm_cmpestrm(p7, 16, pat, 16, MODE);
#undef MODE
        res += _popcnt64(_mm_extract_epi64(r0, 0));
        res += _popcnt64(_mm_extract_epi64(r1, 0));
        res += _popcnt64(_mm_extract_epi64(r2, 0));
        res += _popcnt64(_mm_extract_epi64(r3, 0));
        res += _popcnt64(_mm_extract_epi64(r4, 0));
        res += _popcnt64(_mm_extract_epi64(r5, 0));
        res += _popcnt64(_mm_extract_epi64(r6, 0));
        res += _popcnt64(_mm_extract_epi64(r7, 0));
    }

    for (; i < len; ++i) {
        res += str[i] == w;
    }

    return res;
}

__attribute__((target("avx2")))
size_t fps_count_avx2(unsigned char *str, size_t len, unsigned char w) {
    __m256i pat = _mm256_set1_epi8(w);

    size_t prefix = 0, res = 0;

    size_t i = 0;

    for (; i < len && (intptr_t)(str + i) % 64; ++i) {
        prefix += str[i] == w;
    }

    for (size_t end = len - 128; i < end; i += 128) {
        __m256i p0 = _mm256_load_si256((const __m256i*)(str + i + 32 * 0));
        __m256i p1 = _mm256_load_si256((const __m256i*)(str + i + 32 * 1));
        __m256i p2 = _mm256_load_si256((const __m256i*)(str + i + 32 * 2));
        __m256i p3 = _mm256_load_si256((const __m256i*)(str + i + 32 * 3));
        __m256i r0 = _mm256_cmpeq_epi8(p0, pat);
        __m256i r1 = _mm256_cmpeq_epi8(p1, pat);
        __m256i r2 = _mm256_cmpeq_epi8(p2, pat);
        __m256i r3 = _mm256_cmpeq_epi8(p3, pat);
        res += _popcnt64(_mm256_extract_epi64(r0, 0));
        res += _popcnt64(_mm256_extract_epi64(r0, 1));
        res += _popcnt64(_mm256_extract_epi64(r0, 2));
        res += _popcnt64(_mm256_extract_epi64(r0, 3));
        res += _popcnt64(_mm256_extract_epi64(r1, 0));
        res += _popcnt64(_mm256_extract_epi64(r1, 1));
        res += _popcnt64(_mm256_extract_epi64(r1, 2));
        res += _popcnt64(_mm256_extract_epi64(r1, 3));
        res += _popcnt64(_mm256_extract_epi64(r2, 0));
        res += _popcnt64(_mm256_extract_epi64(r2, 1));
        res += _popcnt64(_mm256_extract_epi64(r2, 2));
        res += _popcnt64(_mm256_extract_epi64(r2, 3));
        res += _popcnt64(_mm256_extract_epi64(r3, 0));
        res += _popcnt64(_mm256_extract_epi64(r3, 1));
        res += _popcnt64(_mm256_extract_epi64(r3, 2));
        res += _popcnt64(_mm256_extract_epi64(r3, 3));
    }

    // _mm256_cmpeq_epi8(p, pat) returns a SIMD vector
    // with `i`th byte consisting of eight `1`s if `p[i] == pat[i]`,
    // and of eight `0`s otherwise,
    // hence each matching byte is counted 8 times by popcnt.
    // Dividing by 8 corrects for that.
    res /= 8;

    res += prefix;

    for (; i < len; ++i) {
        res += str[i] == w;
    }

    return res;
}

typedef size_t (*fps_impl_t) (unsigned char*, size_t, unsigned char);

fps_impl_t select_fps_simd_impl() {
    uint32_t eax = 0, ebx = 0, ecx = 0, edx = 0;

    uint32_t ecx1 = 0;
    if (__get_cpuid(1, &eax, &ebx, &ecx, &edx)) {
        ecx1 = ecx;
    }

    const bool has_xsave = ecx1 & (1 << 26);
    const bool has_popcnt = ecx1 & (1 << 23);

    if (__get_cpuid_count(7, 0, &eax, &ebx, &ecx, &edx)) {
        const bool has_avx2 = has_xsave && (ebx & (1 << 5));
        if (has_avx2 && has_popcnt) {
            return &fps_count_avx2;
        }
    }

    const bool has_sse42 = ecx1 & (1 << 19);
    if (has_sse42 && has_popcnt) {
        return &fps_count_cmpestrm;
    }

    return &fps_count_naive;
}
#endif



size_t fps_count(unsigned char *str, size_t len, unsigned char w) {
#ifndef USE_SIMD_COUNT
    return fps_count_naive(str, len, w);
#else
    // 1024 is a rough guesstimate of the string length
    // for which the extra performance of the main SIMD loop
    // starts to compensate the extra work and extra branching outside the SIMD loop.
    // The real optimal number depends on the specific μarch
    // and isn't worth optimizing for in this context,
    // since counting characters in shorter strings is unlikely to be a hot spot.
    if (len <= 1024) {
        return fps_count_naive(str, len, w);
    }

    static _Atomic fps_impl_t s_impl = (fps_impl_t)NULL;
    fps_impl_t impl = atomic_load_explicit(&s_impl, memory_order_relaxed);
    if (!impl) {
      impl = select_fps_simd_impl();
      atomic_store_explicit(&s_impl, impl, memory_order_relaxed);
    }

    return (*impl)(str, len, w);
#endif
}
