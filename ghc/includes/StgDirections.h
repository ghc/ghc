#ifndef STGDIRECTION_H
#define STGDIRECTION_H

/* Here's where we hide things about heap and stack directions.

   NB: The call to "setNewHeapUsage 0 words_required" in CgClosure in
   the code generator is also direction-sensitive.
*/

/* for now --
    heap:	grows upwards
    A stack:	grows downwards
    B stack:	grows upwards
*/

/*	ALL THE ARITHMETIC IN HERE IS IN UNITS OF WORDS		*/


/****************************************************************
*								*
*		Heapery						*
* 								*
* ***************************************************************/

/*	HEAP_FRAME_BASE( low-addr, size ) gives the address of the
	first word to be allocated in the heap space 
	starting at address low-addr.  size is in words.

	HEAP_FRAME_LIMIT( low-addr, size ) gives the address of the
	last word to be allocated.
*/
#define HEAP_FRAME_BASE(space,size) 	(space)
#define HEAP_FRAME_LIMIT(space,size) 	(((P_) (space)) + (size) - 1)


/*	Hp + HREL(n) addresses the n'th word into already-allocated space
	from Hp.  n=0 addresses the ``most recently allocated'' word.

OBSOLETE BECAUSE WE'VE FIXED THE DIRECTION OF HEAP GROWTH (upwards)

#define HREL(offset)	(-(offset))
*/



/*	HEAP_OVERFLOW_OP( heap-ptr, heap-limit ) is true if the heap has
	overflowed.
*/
#define HEAP_OVERFLOW_OP(a,hplim) ((a) > (hplim))


/****************************************************************
*								*
*		Stackery					*
* 								*
* ***************************************************************/

/*	STK_A_FRAME_BASE( low-addr, size ) gives the address of the bottom-most
	word of A stack, given that A and B stack are to be allocated 
	from a block of store starting at low-addr.  size is in words

	STK_B_FRAME_BASE( low-addr, size) does the same for B stack
*/
#define STK_A_FRAME_BASE(space,size)	(((PP_) (space)) + (size) - 1)
#define STK_B_FRAME_BASE(space,size)	(space)


/* 	SpA + AREL(n) addresses the n'th word from the top of A stack
			(0'th is top one)
	Similarly BREL 
*/
#define AREL(offset)	(offset)
#define BREL(offset)	(-(offset))


/*	STKS_OVERFLOW_OP( a-stack-space, b-stack-space ) is true if SpA and SpB
	have collided.

	We cast SpA to StgPtr, because it is normally an StgPtrPtr.
*/
#define STKS_OVERFLOW_OP(a,b) ((P_)(SpA) - AREL((a) + (b)) <= SpB)

/*	And a version that generates slightly-worse
	code, but which does not need to know about
	SpA and SpB (used in RTS)
*/
#define UNREG_STKS_OVERFLOW_OP(a,b) ((P_)(a) <= (b))

#endif /* ! STGDIRECTION_H */
