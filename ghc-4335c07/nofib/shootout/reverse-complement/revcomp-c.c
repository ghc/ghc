/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/

   contributed by Mr Ledrug
*/

#define _GNU_SOURCE
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>

char *pairs = "ATCGGCTAUAMKRYWWSSYRKMVBHDDHBVNN\n\n";
char tbl[128];

typedef struct work_s work_t;
struct work_s {
   pthread_t id;
   work_t *next;
   char *begin, *end;
};

void *process(void *ww) {
   work_t *w = ww;
   char *from = w->begin, *to = w->end;
   while (*from++ != '\n');

   size_t len = to - from;
   size_t off = 60 - (len % 61);

   if (off) {
      char *m;
      for (m = from + 60 - off; m < to; m += 61) {
         memmove(m + 1, m, off);
         *m = '\n';
      }
   }

   char c;
   for (to--; from <= to; from++, to--)
      c = tbl[(int)*from], *from = tbl[(int)*to], *to = c;

   return 0;
}

int main() {
   char *s;
   for (s = pairs; *s; s += 2) {
      tbl[toupper(s[0])] = s[1];
      tbl[tolower(s[0])] = s[1];
   }


   size_t buflen = 1024, len, end = 0;
   char *buf = malloc(1024);

   int in = fileno(stdin);
   while ((len = read(in, buf + end, buflen - 256 - end))) {
      end += len;
      if (end < buflen - 256) break;
      buf = realloc(buf, buflen *= 2);
   }
   buf[end] = '>';

   work_t *work = 0;
   char *from, *to = buf + end - 1;
   while (1) {
      for (from = to; *from != '>'; from--);

      work_t *w = malloc(sizeof(work_t));
      w->begin = from;
      w->end = to;
      w->next = work;
      work = w;

      pthread_create(&w->id, 0, process, w);

      to = from - 1;
      if (to < buf) break;
   }

   while (work) {
      work_t *w = work;
      work = work->next;
      pthread_join(w->id, 0);
      free(w);
   }

   write(fileno(stdout), buf, end);
   free(buf);

   return 0;
}
