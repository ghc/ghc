#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>

#include "Timing.h"

double compute(double n)
{
  double sum = 0;

  while ( n >= 1 ) {
    sum += n * n;
    n--;
  }
  return sum;
}

int main( int argc, char * argv[] )
{
  if (argc != 3) {
	printf("usage: sumsq COUNT REPEATS\n");
	exit(1);
  }	

  int count    = atoi( argv[1] );
  assert (count >= 1);
  
  int repeats = atoi( argv[2] );
  assert (repeats >= 1);

  struct benchtime *bt	= bench_begin();
  double result		= 0;
  for (int i = 0; i < repeats; i++) {
  	result = compute( count );
  }
  bench_done(bt);

  printf( "result = %g\n", result);

  return 0;
}

