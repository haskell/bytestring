/*
 * Copyright (C) 2003 David Roundy
 * Copyright (C) 2005 Don Stewart
 * Most of the UTF code is Copyright (C) 1999-2001 Free Software Foundation, Inc.
 * This file is part of darcs.
 *
 * Darcs is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the GNU LIBICONV Library; see the file COPYING.LIB.
 * If not, write to the Free Software Foundation, Inc., 59 Temple Place -
 * Suite 330, Boston, MA 02111-1307, USA.
 *
 */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/mman.h>
#endif

#include "fpstring.h"

/* A locale-independent isspace(3) so patches are interpreted the same
 * everywhere. */
#define ISSPACE(c) \
    ((c) == ' ' || (c) == '\t' || (c) == '\n' || (c) == '\r')

int firstspace(const char *s, int len)
{
    const char *start;
    const char *end;

    for (start = s, end = s + len; s < end && !ISSPACE(*s); s++);

    return s - start;
}

/* return index of first non-space character */
int firstnonspace(const char *s, int len)
{
    const char *start;
    const char *end;

    for (start = s, end = s + len; s < end && ISSPACE(*s); s++);

    return s - start;
}

// mmapping...

#ifdef _WIN32

/* I have no idea if this works or not, and it is very tied to the usage
 * of mmap in FastPackedString. Most arguments are ignored...
 */

char *my_mmap(int length, int fd)
{
    HANDLE file = (HANDLE)_get_osfhandle(fd);
    HANDLE hnd = CreateFileMapping(file, 0, PAGE_READONLY, 0, 0, 0);
    DWORD offhi = 0, offlo = 0;
    SIZE_T size = 0;
    char *p = MapViewOfFile(hnd, FILE_MAP_READ, offhi, offlo, size);
    return p;
}

int munmap(void *start, size_t length)
{
    UnmapViewOfFile(start);
}

#else

char *my_mmap(int len, int fd) {
  void *maybeok = mmap(NULL, len, PROT_READ, MAP_SHARED, fd, 0);
  if (maybeok == MAP_FAILED) return NULL;
  else return (char *)maybeok;
}

#endif

/* Specification: RFC 2279 */

int utf8_to_ints(HsInt *pwc, const unsigned char *s, int n) {
  /* returns number of unicode chars in the output.  The output array is
     assumed to have the same number of elements as the input array, which
     is n. */

  HsInt *pwc_original = pwc;
  while (n > 0) {
    unsigned char c = s[0];

    if (c < 0x80) {
      *pwc++ = c;
      n--;
      s++;
    } else if (c < 0xc2) {
      return -1;
    } else if (c < 0xe0) {
      if (n < 2) return -1;
      if (!((s[1] ^ 0x80) < 0x40)) return -1;
      *pwc++ = ((unsigned) (c & 0x1f) << 6)
        | (unsigned) (s[1] ^ 0x80);
      n -= 2;
      s += 2;
    } else if (c < 0xf0) {
      if (n < 3) return -1;
      if (!((s[1] ^ 0x80) < 0x40 && (s[2] ^ 0x80) < 0x40
            && (c >= 0xe1 || s[1] >= 0xa0)))
        return -1;
      *pwc++ = ((unsigned) (c & 0x0f) << 12)
        | ((unsigned) (s[1] ^ 0x80) << 6)
        | (unsigned) (s[2] ^ 0x80);
      n -= 3;
      s += 3;
    } else if (c < 0xf8 && sizeof(unsigned)*8 >= 32) {
      if (n < 4) return -1;
      if (!((s[1] ^ 0x80) < 0x40 && (s[2] ^ 0x80) < 0x40
            && (s[3] ^ 0x80) < 0x40
            && (c >= 0xf1 || s[1] >= 0x90)))
        return -1;
      *pwc++ = ((unsigned) (c & 0x07) << 18)
        | ((unsigned) (s[1] ^ 0x80) << 12)
        | ((unsigned) (s[2] ^ 0x80) << 6)
        | (unsigned) (s[3] ^ 0x80);
      n -= 4;
      s += 4;
    } else if (c < 0xfc && sizeof(unsigned)*8 >= 32) {
      if (n < 5) return -1;
      if (!((s[1] ^ 0x80) < 0x40 && (s[2] ^ 0x80) < 0x40
            && (s[3] ^ 0x80) < 0x40 && (s[4] ^ 0x80) < 0x40
            && (c >= 0xf9 || s[1] >= 0x88)))
        return -1;
      *pwc++ = ((unsigned) (c & 0x03) << 24)
        | ((unsigned) (s[1] ^ 0x80) << 18)
        | ((unsigned) (s[2] ^ 0x80) << 12)
        | ((unsigned) (s[3] ^ 0x80) << 6)
        | (unsigned) (s[4] ^ 0x80);
      n -= 5;
      s += 5;
    } else if (c < 0xfe && sizeof(unsigned)*8 >= 32) {
      if (n < 6) return -1;
      if (!((s[1] ^ 0x80) < 0x40 && (s[2] ^ 0x80) < 0x40
            && (s[3] ^ 0x80) < 0x40 && (s[4] ^ 0x80) < 0x40
            && (s[5] ^ 0x80) < 0x40
            && (c >= 0xfd || s[1] >= 0x84)))
        return -1;
      *pwc++ = ((unsigned) (c & 0x01) << 30)
        | ((unsigned) (s[1] ^ 0x80) << 24)
        | ((unsigned) (s[2] ^ 0x80) << 18)
        | ((unsigned) (s[3] ^ 0x80) << 12)
        | ((unsigned) (s[4] ^ 0x80) << 6)
        | (unsigned) (s[5] ^ 0x80);
      n -= 6;
      s += 6;
    } else
      return -1;
  }
  return pwc - pwc_original;
}

/* copy a string in reverse */
void reverse(unsigned char *dest, unsigned char *from, int len)
{
    unsigned char *p, *q;
    p = from + len - 1;
    q = dest;

    while (p >= from)
        *q++ = *p--;
}

/* compare bytes ascii-wise */
static int cmp(const void *p, const void *q) {
    return (*(unsigned char *)p - *(unsigned char *)q);
}

/* quicksort wrapper */
void my_qsort(unsigned char *base, size_t size)
{
    qsort(base, size, sizeof(char), cmp);
}

/* duplicate a string, interspersing the character through the elements
   of the duplicated string */
void intersperse(unsigned char *dest, unsigned char *from, int len, char c)
{
    unsigned char *p, *q;
    p = from;
    q = dest;
    while (p < from + len - 1) {
        *q++ = *p++; 
        *q++ = c;
    }
    *q = *p;
}

/* find maximum char in a packed string */
unsigned char maximum(unsigned char *p, int len)
{
    unsigned char *q, c = *p;
    for (q = p; q < p + len; q++)
        if (*q > c)
            c = *q;
    return c;
}

/* find minimum char in a packed string */
unsigned char minimum(unsigned char *p, int len)
{
    unsigned char *q, c = *p;
    for (q = p; q < p + len; q++)
        if (*q < c)
            c = *q;
    return c;
}
