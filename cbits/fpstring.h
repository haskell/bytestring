#include <HsFFI.h>

#if defined(USE_MMAP)
char *my_mmap(int len, int fd);
#endif

void reverse(unsigned char *dest, unsigned char *from, int len);
void my_qsort(unsigned char *base, size_t size);
void intersperse(unsigned char *dest, unsigned char *from, int len, char c);
