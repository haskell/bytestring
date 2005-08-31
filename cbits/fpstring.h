#include <HsFFI.h>

int wfindps_helper(char c, const char *s, int len);

int firstspace(const char *s, int len);
int firstnonspace(const char *s, int len);

char *my_mmap(int len, int fd);

int utf8_to_ints(HsInt *pwc, const unsigned char *s, int n);

void reverse(unsigned char *dest, unsigned char *from, int len);
void my_qsort(unsigned char *base, size_t size);
void intersperse(unsigned char *dest, unsigned char *from, int len, char c);
unsigned char maximum(unsigned char *p, int len);
unsigned char minimum(unsigned char *p, int len);
