/* -*- mode: hugs-c; -*- */
/* -----------------------------------------------------------------------------
 * $Id: QueueTemplate.h,v 1.3 1999/02/05 16:02:48 simonm Exp $
 *
 * (c) The GHC Team, 1998
 *
 * Template for generating queues of various types
 *
 * #define Queue##ChunkSize, Queue and Type before #including this file
 * to define the following:
 *
 *   typedef { ...; nat len } Queue;
 *   static void insertQueue( Queue* q, Type i );
 *   static void initQueue  ( Queue* q );
 *   static void setQueue   ( Queue* q, nat i, Type x );
 *
 * Copyright (c) 1994-1998.
 *
 * $RCSfile: QueueTemplate.h,v $
 * $Revision: 1.3 $
 * $Date: 1999/02/05 16:02:48 $
 *
 * ------------------------------------------------------------------------*/

/* These macros are rather delicate - read a good ANSI C book carefully
 * before meddling.
 */
#define mystr(x)      #x
#define mycat(x,y)    x##y
#define mycat2(x,y)   mycat(x,y)
#define mycat3(x,y,z) mycat2(x,mycat2(y,z))

typedef struct mycat3(_,Queue,Chunk) {
    struct mycat3(_,Queue,Chunk)* next;
    Type                    xs[mycat2(Queue,ChunkSize)];
} mycat2(Queue,Chunk);

static mycat2(Queue,Chunk)* mycat3(alloc,Queue,Chunk)( void )
{
    mycat2(Queue,Chunk)* new = malloc(sizeof(mycat2(Queue,Chunk)));
    if (new == NULL) {
        barf("Can't allomycate " mystr(Queue) "Chunk");
    }
    new->next = NULL;
    return new;
}

typedef struct {
    mycat2(Queue,Chunk)*  head;
    mycat2(Queue,Chunk)*  tail;
    nat    len;          /* position of next free instruction */
} Queue;

static void mycat2(insert,Queue)( Queue* q, Type i )
{
    if (q->len == 0) {
        mycat2(Queue,Chunk)* new = mycat3(alloc,Queue,Chunk)();
        new->next = NULL;
        q->head = new;
	q->tail = new;
    } else if (q->len % mycat2(Queue,ChunkSize) == 0) {
        mycat2(Queue,Chunk)* new = mycat3(alloc,Queue,Chunk)();
        new->next = NULL;
        q->tail->next = new;
	q->tail = new;
    }
    q->tail->xs[q->len % mycat2(Queue,ChunkSize)] = i;
    q->len++;
}

static inline void mycat2(init,Queue)( Queue* q )
{
   q->head = q->tail = NULL;
   q->len = 0;
}
 
static void mycat2(set,Queue)( Queue* q, nat i, Type x )
{
    mycat2(Queue,Chunk)* chunk = q->head;
    ASSERT(i <= q->len);
    /* ToDo: optimise case where i is in the last chunk in the list */
    for(; i >= mycat2(Queue,ChunkSize); i -= mycat2(Queue,ChunkSize)) {
        ASSERT(chunk);
        chunk = chunk->next;
    }
    ASSERT(chunk);
    chunk->xs[i] = x;
}

/* evaluate a statement s once for every element in a queue q.
 * i and x are usually free in s
 * queueTy and eltTy are the types of the container and element respectively
 */
#define mapQueue(queueTy,eltTy,q,s)                         \
do {                                                        \
    mycat2(queueTy,Chunk)* chunk = (q).head;		    \
    nat i = 0;					            \
    eltTy x;                                                \
    while( i < (q).len ) {			            \
        ASSERT(chunk);                                      \
	x = chunk->xs[i % mycat2(queueTy,ChunkSize)];       \
	s;					      	    \
	++i;					      	    \
	if (i % mycat2(queueTy,ChunkSize) == 0) {	    \
	    chunk = chunk->next;		      	    \
	}                                             	    \
    }                                                       \
} while (0)

/* --------------------------------------------------------------------------
 * End of Queue template
 * ------------------------------------------------------------------------*/
