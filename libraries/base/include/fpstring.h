#include <HsFFI.h>

char *my_mmap(int len, int fd);

void reverse(unsigned char *dest, unsigned char *from, int len);
void my_qsort(unsigned char *base, size_t size);
void intersperse(unsigned char *dest, unsigned char *from, int len, char c);
unsigned char maximum(unsigned char *p, int len);
unsigned char minimum(unsigned char *p, int len);
