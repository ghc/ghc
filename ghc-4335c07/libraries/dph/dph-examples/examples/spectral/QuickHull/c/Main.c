
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "Vector.h"
#include "Timing.h"

// FFS people.
#ifndef M_PI
#define M_PI 3.1415926535
#endif


extern int  quickHull	(Vector* points, Vector* hull);
extern void dumpSVG	(FILE* file, Vector* points, Vector* hull);


int main(int argc, char** argv)
{
	// Parse cmd line args.
	int	pointCount	= 0;
	char*	outSVG		= 0;

	if(argc == 2) {
		pointCount	= atoi(argv[1]);
		outSVG		= 0;
	}
	else if (argc == 3) {
		pointCount	= atoi(argv[1]);
		outSVG		= argv[2];
	}
	else {
		printf("usage: quickhull <points> [out.svg]\n");
		exit(1);
	}

	// Initialise the vector to hold the hull.
	Vector* hull		= vector_new(pointCount);
		
	// Use random points for test data.
	Vector* points		= vector_new(pointCount);

	double	originX		= 300;
	double	originY		= 300;
	long	maxDist		= 250;
	
	srandom(170);
	for (int i = 0; i < pointCount; i++) {
		double r	= (random() % (maxDist * 2)) - maxDist;
		double a	= M_PI * (random() % 360) / 360;

		vector_append
			( points
			, originX + r * cos (a)
			, originY + r * sin (a));
	}

	// Timing setup
        struct timeval start, finish;
        struct rusage start_ru, finish_ru;

        gettimeofday( &start, NULL );
        getrusage( RUSAGE_SELF, &start_ru );

	// Do the deed.
	int depth = quickHull (points, hull);

	// Print how long it took.
        gettimeofday( &finish, NULL );
        getrusage( RUSAGE_SELF, &finish_ru );

//	printf("depth          = %d\n", depth);
//	printf("points on hull = %d\n", hull->length);

        sub_timeval( &finish, &start );
        sub_timeval( &finish_ru.ru_utime, &start_ru.ru_utime );
        sub_timeval( &finish_ru.ru_stime, &start_ru.ru_stime );
        add_timeval( &finish_ru.ru_utime, &finish_ru.ru_stime );

	printf("elapsedTimeMS   = ");
        print_timeval( &finish ); putchar( '\n' );

 	printf("cpuTimeMS       = ");
        print_timeval( &finish_ru.ru_utime); putchar( '\n' );

	// Write output to file if requested.
	if(outSVG != 0) {
		FILE* file = fopen(outSVG, "w");
		dumpSVG	(file, points, hull);	
		fclose	(file);
	}
}
