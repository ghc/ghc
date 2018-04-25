#ifndef _Timing_h_
#define _Timing_h_

#include <sys/time.h>
#include <sys/resource.h>

// -- timeval -----------------------------------------------------------------
void	timeval_add( struct timeval *x, const struct timeval *y);
void	timeval_sub( struct timeval *x, const struct timeval *y);
void	timeval_print( const struct timeval *t );


// -- benchtime ---------------------------------------------------------------
struct benchtime;

struct benchtime*	bench_begin();
void	bench_end	(struct benchtime *bt);
void	bench_print	(struct benchtime *bt);
void	bench_done	(struct benchtime *bt);


#endif

