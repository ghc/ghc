#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>

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

