#include <Rts.h>
#include <stdio.h>

void flush_stdout(void)
{
    fflush(stdout);
}

void print_it(
    StgWord r1,
    StgWord r2,
    StgWord r3,
    StgWord r4,
    StgWord r5,
    StgWord r6,
    StgWord r7,
    StgWord r8,
    StgWord r9,
    StgWord r10
    )
{
  printf("%"  FMT_Word
         " %" FMT_Word
         " %" FMT_Word
         " %" FMT_Word
         " %" FMT_Word
         " %" FMT_Word
         " %" FMT_Word
         " %" FMT_Word
         " %" FMT_Word
         " %" FMT_Word "\n",
         r1, r2, r3, r4, r5,
         r6, r7, r8, r9, r10);
}
