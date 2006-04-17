/*
 * Copyright (C) 2003 David Roundy
 * Copyright (C) 2005-6 Don Stewart
 *
 * Most of the UTF code is Copyright (C) 1999-2001 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>

#include <sys/mman.h>

#include "fpstring.h"

/* ------------------------------------------------------------- */

/* could be replaced with some .hsc magic */

char *my_mmap(int len, int fd) {
  void *maybeok = mmap(NULL, len, PROT_READ, MAP_SHARED, fd, 0);
  if (maybeok == MAP_FAILED) return NULL;
  else return (char *)maybeok;
}

/* ------------------------------------------------------------- */

/* A locale-independent isspace(3) */
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

/* return index of last non-space character */
int lastnonspace(const char *s, int len)
{
    const char *p;
    const char *end;

    for (p = s + len - 1, end = s; p >= end && ISSPACE(*p); p--);

    return p - s;
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
