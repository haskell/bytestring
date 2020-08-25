#include <string.h>
#include <stdlib.h>

void fps_reverse(unsigned char *dest, unsigned char *from, size_t len);
void fps_intersperse(unsigned char *dest, unsigned char *from, size_t len, unsigned char c);
unsigned char fps_maximum(unsigned char *p, size_t len);
unsigned char fps_minimum(unsigned char *p, size_t len);
unsigned long fps_count(unsigned char *p, size_t len, unsigned char w);
void fps_sort(unsigned char *p, size_t len);
