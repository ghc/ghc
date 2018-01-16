#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <time.h>

#include <HsFFI.h>
#include "Timing.h"

int rows;
int cols;

typedef struct {
  HsInt  size;
  void *data;
} Array;

Array vector;
Array lengths;
Array indices;
Array values;
Array result;

#define DATA(arr,i,t) (((t *)(arr).data)[i])

void new( HsInt size, Array * arr, int el_size )
{
  arr->size = size;
  arr->data = malloc( el_size * size );
}

void load( int file, Array * arr, int el_size )
{
  read( file, &(arr->size), sizeof(HsInt) );
  arr->data = malloc( el_size * arr->size );
  read( file, arr->data, arr->size*el_size );
}

void compute()
{
  HsInt row, el, idx;
  HsDouble sum;

  el = 0;
  idx = 0;
  for( row = 0; row < lengths.size; ++row ) {
    sum = 0;
    for( el = 0; el < DATA(lengths,row,HsInt); ++el ) {
       sum += DATA(values, idx, HsDouble)
            * DATA(vector, DATA(indices, idx, HsInt), HsDouble);
       ++idx;
    }
    DATA(result, row, HsDouble) = sum;
  }
}

HsDouble checksum( Array * arr )
{
  HsDouble sum = 0;
  int i;

  for( i = 0; i < arr->size; ++i )
     sum += DATA((*arr), i, HsDouble);
  return sum;
}

int usage()
{
  printf("usage: smvm <reps> <file>\n");
  exit(0);
}

                       
int main( int argc, char * argv[] )
{
  int file;
  int i;

  if (argc != 3) usage();
  // Repetitions for benchmarking.
  int reps      = atoi(argv[1]);

  // Load in file.
  char* fileName = argv[2];
  file  = open( fileName, O_RDONLY );
  if(file == -1) {
    printf ("can't open file '%s'\n", fileName);
    exit(1);
  }

  // Check for magic numbers at start of file.
  HsInt magic1, magic2;
  read( file, &magic1,  sizeof(HsInt) );
  read( file, &magic2,  sizeof(HsInt) );
  if (! (magic1 == 0xc0ffee00 && magic2 == 0x12345678)) {
    printf ("bad magic in %s\n",  fileName);
    printf ("got = %0lx, %0lx\n", magic1, magic2);
    exit(1);
  }

  // Array of how many non-zero elemens there are in each row.
  load( file, &lengths, sizeof(HsInt) );

  // Indices of the elements in each row.
  load( file, &indices, sizeof(HsInt) );

  // All non-zero values in the matrix.
  load( file, &values,  sizeof(HsDouble) );

  // The dense vector.
  load( file, &vector,  sizeof(HsDouble) );
  close(file);

  // Do the deed
  new( lengths.size, &result, sizeof(HsDouble) );
  struct benchtime *bt	= bench_begin();

  for (i = 0; i < reps; i++)
        compute(reps);

  bench_done(bt);

  // Print checksum of resulting vector.
  printf( "sum of result   = %Lf\n", (long double)(checksum(&result)));

  // Print some details about the matrix.
  printf( "matrix rows     = %ld\n", (long)lengths.size);
  printf( "matrix columns  = %ld\n", (long)vector.size);
  printf( "non-zero values = %ld\n", (long)values.size);

  return 0;
}

