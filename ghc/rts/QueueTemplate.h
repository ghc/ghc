
/* -----------------------------------------------------------------------------
 * $Id: QueueTemplate.h,v 1.6 2000/04/14 15:08:14 sewardj Exp $
 *
 * (c) The GHC Team, 1998
 *
 * Template for generating queues of various types
 *
 * #define Queue and Type before #including this file
 * to define the following:
 *
 *   typedef { Type* elems; nat used; nat size } Queue;
 *   static void insertQueue( Queue* q, Type i );
 *   static void initQueue  ( Queue* q );
 *   static void setQueue   ( Queue* q, nat i, Type x );
 *   static void freeQueue  ( Queue* q );
 *
 * $RCSfile: QueueTemplate.h,v $
 * $Revision: 1.6 $
 * $Date: 2000/04/14 15:08:14 $
 *
 * ------------------------------------------------------------------------*/

/* These macros are rather delicate - read a good ANSI C book carefully
 * before meddling.
 */
#define mystr(x)      #x
#define mycat(x,y)    x##y
#define mycat2(x,y)   mycat(x,y)
#define mycat3(x,y,z) mycat2(x,mycat2(y,z))


typedef struct {
    Type*  elems;
    nat    len;   /* always <= size */
    nat    size;
} Queue;


#if MAKE_findIn
static int mycat2(findIn,Queue)( Queue* q, Type x )
{
   nat i;
   for (i = 0; i < q->len; i++)
      if (q->elems[i] == x) return i;
   return -1;
}
#endif

static void mycat2(init,Queue)( Queue* q )
{
   q->len   = 0;
   q->size  = 8;
   q->elems = malloc(q->size * sizeof(Type));
   if (q->elems == NULL) {
      barf("Out of memory: can't allocate initial " mystr(Queue) " space");
   }
}
 

static void mycat2(free,Queue)( Queue* q )
{
   free(q->elems);
   q->elems = NULL;
}


static void mycat2(insert,Queue)( Queue* q, Type x )
{
   nat i;
   if (q->len == q->size) {
      Type* elems2 = malloc(2 * q->size * sizeof(Type));
      if (elems2 == NULL) {
         barf("Out of memory: can't resize " mystr(Queue) " space");
      }
      for (i = 0; i < q->len; i++)
         elems2[i] = q->elems[i];
      free(q->elems);
      q->elems = elems2;
      q->size *= 2;
   }
   q->elems[q->len] = x;
   q->len++;
}


static void mycat2(set,Queue)( Queue* q, nat i, Type x )
{
    ASSERT(i < q->len);
    q->elems[i] = x;
}



/* evaluate a statement s once for every element in a queue q.
 * i and x are usually free in s
 * queueTy and eltTy are the types of the container and element respectively
 */
#define mapQueue(queueTy,eltTy,q,s)     \
do {                                    \
    nat i = 0;                          \
    eltTy x;                            \
    while( i < (q).len ) {              \
        x = q.elems[i];                 \
        s;                              \
        ++i;                            \
    }                                   \
} while (0)

/* --------------------------------------------------------------------------
 * End of Queue template
 * ------------------------------------------------------------------------*/
