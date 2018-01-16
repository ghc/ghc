
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "Timing.h"
#include <HsFFI.h>

int main(int argc, char** argv)
{
	if (argc != 2) {
		printf("usage: evens LENGTH\n");
		exit(1);
	}
	int length	= atoi(argv[1]);
	assert (length > 0);
	
	// Allocate buffers
	HsInt *src 	= malloc(sizeof(HsInt) * length);
	HsInt *dst	= malloc(sizeof(HsInt) * length);
	
	// Fill the source with test data
	for(int i = 0; i < length; i++)
		src[i] = i;
		
	// Copy out just the even elements to the destination
	// This is the only part we're measuring the runtime of.
	struct benchtime *bt	= bench_begin();
	int j = 0;
	for(int i = 0; i < length; i++) 
		if (src[i] % 2 == 0)
		 	dst[j++] = src[i];
	
	bench_done(bt);
	
	// Cleanup
	free(src);
	free(dst);
}