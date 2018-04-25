/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 *  contributed by Mr Ledrug
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

typedef struct {
   float p;
   char c;
} amino;

amino iub[] = {
   { 0.27, 'a' }, { 0.12, 'c' }, { 0.12, 'g' },
   { 0.27, 't' }, { 0.02, 'B' }, { 0.02, 'D' },
   { 0.02, 'H' }, { 0.02, 'K' }, { 0.02, 'M' },
   { 0.02, 'N' }, { 0.02, 'R' }, { 0.02, 'S' },
   { 0.02, 'V' }, { 0.02, 'W' }, { 0.02, 'Y' },
   { 0, 0 }
};

amino homosapiens[] = {
   {0.3029549426680, 'a'},
   {0.1979883004921, 'c'},
   {0.1975473066391, 'g'},
   {0.3015094502008, 't'},
   {0, 0}
};

#define RMAX 139968U
#define RA 3877U
#define RC 29573U
#define WIDTH 60
#define LENGTH(a) (sizeof(a)/sizeof(a[0]))

static inline void str_write(char *s) {
   write(fileno(stdout), s, strlen(s));
}

void str_repeat(char *s, int outlen) {
   int len = strlen(s) * (1 + WIDTH);
   outlen += outlen / WIDTH;

   char *ss = s;
   char *buf = malloc(len);
   int pos = 0;

   while (pos < len) {
      if (!*ss) ss = s;
      buf[pos++] = *ss++;
      if (pos >= len) break;
      if (pos % (WIDTH + 1) == WIDTH)
         buf[pos++] = '\n';
   }

   int fd = fileno(stdout);
   int l = 0;
   while (outlen > 0) {
      l = outlen > len ? len : outlen;
      write(fd, buf, l);
      outlen -= len;
   }
   if (buf[l-1] != '\n') str_write("\n");

   free(buf);
}

static char *alu =
   "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
   "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
   "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
   "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
   "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
   "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
   "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";

inline unsigned int rnd(void) {
   static unsigned rseed = 42;
   return rseed = (rseed * RA + RC) % RMAX;
}

char lookup[RMAX];
void rand_fasta(amino *s, size_t outlen) {
   int fd = fileno(stdout);
   char buf[WIDTH+1];

   int i, j, k;
   float sum = 0;
   for (i = j = k = 0; s[i].p && k < RMAX; i++) {
      if (s[i].p) {
         sum += s[i].p;
         k = RMAX * sum + 1;
      }
      else
         k = RMAX;
      if (k > RMAX) k = RMAX;
      memset(lookup + j, s[i].c, k - j);
      j = k;
   }

   i = 0;
   buf[WIDTH] = '\n';
   while (outlen--) {
      buf[i++] = lookup[rnd()];
      if (i == WIDTH) {
         write(fd, buf, WIDTH + 1);
         i = 0;
      }
   }
   if (i) {
      buf[i] = '\n';
      write(fd, buf, i + 1);
   }
}

int main(int argc, char **argv) {
   int n;
   if (argc < 2 || (n = atoi(argv[1])) <= 0) {
      printf("usage: %s length\n", argv[0]);
      return 0;
   }

   str_write(">ONE Homo sapiens alu\n");
   str_repeat(alu, n * 2);

   str_write(">TWO IUB ambiguity codes\n");
   rand_fasta(iub, n * 3);

   str_write(">THREE Homo sapiens frequency\n");
   rand_fasta(homosapiens, n * 5);

   return 0;
}
