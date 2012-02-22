
#include <string.h>

void fps_reverse(unsigned char *dest, unsigned char *from, unsigned long  len);
void fps_intersperse(unsigned char *dest, unsigned char *from, unsigned long  len, unsigned char c);
unsigned char fps_maximum(unsigned char *p, unsigned long  len);
unsigned char fps_minimum(unsigned char *p, unsigned long  len);
unsigned long fps_count(unsigned char *p, unsigned long  len, unsigned char w);

#ifndef INLINE
# if defined(_MSC_VER)
#  define INLINE extern __inline
# else
#  define INLINE static inline
# endif
#endif
INLINE void *
__hscore_memcpy_src_off( char *dst, char *src, int src_off, size_t sz )
{ return memcpy(dst, src+src_off, sz); }


