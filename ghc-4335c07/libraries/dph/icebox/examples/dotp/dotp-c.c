#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <pthread.h>
#include <string.h>
#include <sys/resource.h>

#define CHECK( e )      \
  do {                  \
    int r = (e);        \
    if( r ) { printf( "%s: %s\n", strerror(r), #e ); exit( 1 ); } \
  } while( 0 )

/* Arrays */

typedef struct {
  int      start;
  int      length;
  double * data;
} Arr;

#define indexA( arr, i )  ((arr)->data[(arr)->start + (i)])

void newA( Arr * arr, int len )
{
  arr->start  = 0;
  arr->length = len;
  arr->data   = calloc( len, sizeof(double) );
}

void sliceA( Arr *arr, const Arr *xs, int i, int n )
{
  arr->start  = xs->start + i;
  arr->length = n;
  arr->data   = xs->data;
}

void replicateA( Arr *arr, int n, double d )
{
  int i;

  newA( arr, n );

  for ( i = 0; i < n; ++i )
    indexA( arr, i ) = d;
}

double dotpA( const Arr *xs, const Arr *ys )
{
  int i;
  double r;
  int j;

  r = 0;

  for( i = 0; i < xs->length; ++i )
  {
    /* for( j = 0; j < 100; ++j ); */
    r += indexA( xs, i ) * indexA( ys, i );
  }

  return r;
}

/* Parallel arrays */

void splitLen( int *lens, int threads, int n )
{
  int i, l, m;

  l = n / threads;
  m = n % threads;

  for( i = 0; i < m; ++i )
    lens[i] = l+1;

  for( i = m; i < threads; ++i )
    lens[i] = l;
}

void plusScan( int *is, const int *js, int n )
{
  int i;
  int k;

  k = 0;

  for( i = 0; i < n; ++i ) {
    is[i] = k;
    k += js[i];
  }
}
    

void splitA( Arr **arrs, int threads, const Arr *arr )
{
   int *lens;
   int *idxs;
   int  i;

   lens = (int *) calloc( threads, sizeof(int) );
   idxs = (int *) calloc( threads, sizeof(int) );

   splitLen( lens, threads, arr->length );
   plusScan( idxs, lens, threads );

   *arrs = (Arr *) calloc( threads, sizeof(Arr) );
   
   for( i = 0; i < threads; ++i )
     sliceA( &(*arrs)[i], arr, idxs[i], lens[i] );
}

typedef struct {
  Arr             xs;
  Arr             ys;
  double          result;
} Req;

void * worker( void *arg )
{
  Req * req = (Req *) arg;

  req->result = dotpA( &req->xs, &req->ys );
  return NULL;
}

void dotp( Arr *xs, Arr *ys, int threads )
{
  int i;
  pthread_t * ts;
  Req       * reqs;

  ts   = (pthread_t *) calloc( threads, sizeof( pthread_t ) );
  reqs = (Req *)       calloc( threads, sizeof( Req ) );

  for( i = 0; i < threads; ++i ) {
    reqs[i].xs = xs[i];
    reqs[i].ys = ys[i];
    CHECK( pthread_create( &ts[i], NULL, worker, &reqs[i] ) );
  }

  for( i = 0; i < threads; ++i ) {
    CHECK( pthread_join( ts[i], NULL ) );
  }

  free( ts );
  free( reqs );
}

void
add_timeval( struct timeval *x, const struct timeval *y )
{
  x->tv_sec += y->tv_sec;
  x->tv_usec += y->tv_usec;
  if( x->tv_usec > 1000000 ) {
    ++x->tv_sec;
    x->tv_usec -= 1000000;
  }
}

void
sub_timeval( struct timeval *x, const struct timeval *y )
{
  x->tv_sec -= y->tv_sec;
  if( x->tv_usec < y->tv_usec ) {
    --x->tv_sec;
    x->tv_usec = x->tv_usec + (1000000 - y->tv_usec);
  } else
    x->tv_usec -= y->tv_usec;
}

void
print_timeval( const struct timeval *t )
{
  printf( "%ld", (long int) t->tv_sec * 1000 + (long int) t->tv_usec / 1000 );
}

int main( int argc, char *argv[] )
{
  int elems, threads, runs;
  struct timeval start, finish;
  struct rusage start_ru, finish_ru;

  Arr xs;
  Arr ys;
  Arr *xss;
  Arr *yss;

  runs    = atoi( argv[1] );	/* FIXME: runs currently ignored */
  threads = atoi( argv[2] );
  elems   = atoi( argv[3] );
  printf ("N = %d; P = %d, R = %d: ", elems, threads, runs);

  replicateA( &xs, elems, 5 );
  replicateA( &ys, elems, 6 );
  splitA( &xss, threads, &xs );
  splitA( &yss, threads, &ys );

  gettimeofday( &start, NULL );
  getrusage( RUSAGE_SELF, &start_ru );
  dotp( xss, yss, threads );
  gettimeofday( &finish, NULL );
  getrusage( RUSAGE_SELF, &finish_ru );

  sub_timeval( &finish, &start );
  sub_timeval( &finish_ru.ru_utime, &start_ru.ru_utime );
  sub_timeval( &finish_ru.ru_stime, &start_ru.ru_stime );
  add_timeval( &finish_ru.ru_utime, &finish_ru.ru_stime );

  print_timeval( &finish ); putchar( '/' ); print_timeval( &finish_ru.ru_utime);
  putchar( '\n' );

  return 0;
}

