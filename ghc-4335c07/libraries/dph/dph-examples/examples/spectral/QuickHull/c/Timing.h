#ifndef _Timing_h_
#define _Timing_h_

#include <sys/time.h>
#include <sys/resource.h>

void
add_timeval( struct timeval *x, const struct timeval *y );

void
sub_timeval( struct timeval *x, const struct timeval *y );

void
print_timeval( const struct timeval *t );



#endif

