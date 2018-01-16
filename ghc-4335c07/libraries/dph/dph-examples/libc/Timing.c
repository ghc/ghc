#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
#include <stdlib.h>

// -- timeval -----------------------------------------------------------------
void timeval_add( struct timeval *x, const struct timeval *y )
{
  x->tv_sec += y->tv_sec;
  x->tv_usec += y->tv_usec;
  if( x->tv_usec > 1000000 ) {
    ++x->tv_sec;
    x->tv_usec -= 1000000;
  }
}


void timeval_sub( struct timeval *x, const struct timeval *y )
{
  x->tv_sec -= y->tv_sec;
  if( x->tv_usec < y->tv_usec ) {
    --x->tv_sec;
    x->tv_usec = x->tv_usec + (1000000 - y->tv_usec);
  } else
    x->tv_usec -= y->tv_usec;
}


void timeval_print( const struct timeval *t )
{
	printf( "%ld", (long int) t->tv_sec * 1000 + (long int) t->tv_usec / 1000 );
}


// -- benchtime ---------------------------------------------------------------
struct benchtime {
	struct timeval	start_tv;
	struct rusage	start_ru;
		
	struct timeval  elapsed_tv;
	struct rusage   elapsed_ru;
};


// Called at the start of a benchmark.
// Returns a benchtime which records the starting time.
struct benchtime *bench_begin()
{
	struct benchtime *bt 
		= malloc(sizeof(struct benchtime));

	gettimeofday(&bt->start_tv,        NULL);
	getrusage   (RUSAGE_SELF, &bt->start_ru);

	return bt;
}


// Called at the end of a benchmark.
// Sets the elapsed time in a benchtime.
void bench_end(struct benchtime *bt)
{
	// Print how long it took.
	gettimeofday(&bt->elapsed_tv, NULL);
	getrusage   (RUSAGE_SELF, &bt->elapsed_ru);


	timeval_sub (&bt->elapsed_tv, &bt->start_tv);
	timeval_sub (&bt->elapsed_ru.ru_utime, &bt->start_ru.ru_utime);
	timeval_sub (&bt->elapsed_ru.ru_stime, &bt->start_ru.ru_stime);

	// Add system time to user time for printing.
	timeval_add (&bt->elapsed_ru.ru_utime, &bt->start_ru.ru_stime);
}


// Print the elapsed times of a benchtime.
void bench_print(struct benchtime *bt)
{
	printf("elapsedTimeMS   = ");
	timeval_print (&bt->elapsed_tv);
	putchar( '\n' );

	printf("cpuTimeMS       = ");
	timeval_print (&bt->elapsed_ru.ru_utime);
	putchar( '\n' );
}

// Called at the end of a benchmark.
// Sets the elapsed time in a benchtime, prints the times, and frees the benchtime
void bench_done(struct benchtime *bt)
{
	bench_end(bt);
	bench_print(bt);
	free(bt);
}



