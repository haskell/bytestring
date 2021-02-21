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
#include <emmintrin.h>
#include <xmmintrin.h>
#endif

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

/* count the number of occurences of a char in a string */
size_t fps_count(unsigned char *p, size_t len, unsigned char w) {
    size_t c;
    for (c = 0; len-- != 0; ++p)
        if (*p == w)
            ++c;
    return c;
}

int fps_compare(const void *a, const void *b) {
    return (int)*(unsigned char*)a - (int)*(unsigned char*)b;
}

void fps_sort(unsigned char *p, size_t len) {
    return qsort(p, len, 1, fps_compare);
}
