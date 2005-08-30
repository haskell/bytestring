#include <HsFFI.h>

int wfindps_helper(char c, const char *s, int len);
void debug_free(void *p);
void debug_alloc(void *p, const char *name);

int first_white(const char *s, int len);
int first_nonwhite(const char *s, int len);

char *my_mmap(int len, int fd);

int utf8_to_ints(HsInt *pwc, const unsigned char *s, int n);

void conv_to_hex(unsigned char *dest, unsigned char *from, int num_chars);
void conv_from_hex(unsigned char *dest, unsigned char *from, int num_chars);

void reverse(unsigned char *dest, unsigned char *from, int len);
void my_qsort(unsigned char *base, size_t size);
void intersperse(unsigned char *dest, unsigned char *from, int len, char c);
