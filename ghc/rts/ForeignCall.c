
/* -----------------------------------------------------------------------------
 * $Id: ForeignCall.c,v 1.8 1999/10/22 09:59:34 sewardj Exp $
 *
 * (c) The GHC Team 1994-1999.
 *
 * Foreign Function calls
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

#ifdef INTERPRETER

#include "Assembler.h" /* for CFun stuff */
#include "Evaluator.h"
#include "ForeignCall.h"


/* --------------------------------------------------------------------------
 * Calling out to C: a simple, universal calling API
 * ------------------------------------------------------------------------*/

/* The universal call-C API supplies a single function:

      void universal_call_c ( int   n_args,
                              void* args, 
                              char* argstr, 
                              void* fun )

   PRECONDITIONS

   args points to the start of a block of memory containing the
   arguments.  This block is an array of 8-byte entities,
   containing (n_args+1) slots.   The zeroth slot is where the 
   return result goes. Slots [1 .. n_args] contain the arguments,
   presented left-to-right.

   Arguments are stored in the host's byte ordering inside
   the slots.  Only 4 or 8 byte entities are allowed.
   4-byte entities are stored in the half-slot with lower
   addresses.

   For example, a 32-bit value 0xAABBCCDD would be stored, on
   a little-endian, as

      DD CC BB AA  0  0  0  0

   whereas on a big-endian would expect

      AA BB CC DD  0  0  0  0

   Clients do not need to fill in the zero bytes; they are there
   only for illustration.

   argstr is a simplified argument descriptor string.  argstr
   has one character for each (notional) argument slot of
   args.  That means the first byte of argstr describes the
   return type.  args should be allocated by the caller to hold 
   as many slots as implied by argstr.  

   argstr always specifies a return type.  If the function to
   be called returns no result, you must specify a bogus
   return type in argstr[0]; a 32-bit int seems like a good bet.

   Characters in argstr specify the result and argument types:

      i    32-bit integral
      I    64-bit integral
      f    32-bit floating
      F    64-bit floating

   Pointers should travel as integral entities.  At the moment
   there are no descriptors for entities smaller than 32 bits
   since AFAIK all calling conventions expand smaller entities
   to 32 bits anyway.  Users of this routine need to handle
   packing/unpacking of 16 and 8 bit quantities themselves.

   If the preconditions are not met, behaviour of
   universal_call_c is entirely undefined.


   POSTCONDITION

   The function specified by fun is called with arguments
   in args as specified by argstr.  The result of the call
   is placed in the first 8 bytes of args, again as specified
   by the first byte of argstr.  Calling and returning is to
   be done using the correct calling convention for the
   architecture.

   It's clear that implementations of universal_call_c will
   have to be handwritten assembly.  The above design is intended
   to make that assembly as simple as possible, at the expense
   of a small amount of complication for the API's user.
*/

/* ToDo: move these to the Right Place */
extern StgInt          PopTaggedInt        ( void ) ;
extern StgWord         PopTaggedWord       ( void ) ;
extern StgAddr         PopTaggedAddr       ( void ) ;
extern StgStablePtr    PopTaggedStablePtr  ( void ) ;
extern StgChar         PopTaggedChar       ( void ) ;
extern StgFloat        PopTaggedFloat      ( void ) ;
extern StgDouble       PopTaggedDouble     ( void ) ;

extern void   PushTaggedInt        ( StgInt       );
extern void   PushTaggedWord       ( StgWord      );
extern void   PushTaggedAddr       ( StgAddr      );
extern void   PushTaggedStablePtr  ( StgStablePtr );
extern void   PushTaggedChar       ( StgChar      );
extern void   PushTaggedFloat      ( StgFloat     );
extern void   PushTaggedDouble     ( StgDouble    );

extern void   PushPtr        ( StgPtr );
extern StgPtr PopPtr         ( void );


extern void universal_call_c_x86_linux  ( int, void*, char*, void* );
       void universal_call_c_generic    ( int, void*, char*, void* );

/* --------------------------------------------------------------------------
 * This is a generic version of universal call that
 * only works for specific argument patterns.
 * 
 * It allows ports to work on the Hugs Prelude immeduately,
 * even if univeral_call_c_<os/specific> is not ported.
 * ------------------------------------------------------------------------*/

void universal_call_c_generic
( int   n_args,
  void* args, 
  char* argstr, 
  void* fun )
{
  unsigned int *p = (unsigned int*) args;

#define ARG(n)  (p[n*2])
#define CMP(str) ((n_args + 1 == strlen(str)) && \
		  (!strncmp(str,argstr,n_args + 1)))

#define CALL(retType,callTypes,callVals) \
	((retType(*)callTypes)(fun))callVals

  if (CMP("i")) {
    int res = CALL(int,(void),());
    ARG(0) = res;
  } else if (CMP("ii")) {
    int arg1 = (int) ARG(1);
    int res = CALL(int,(int),(arg1));
    ARG(0) = res;
  } else if (CMP("iii")) {
    int arg1 = (int) ARG(1);
    int arg2 = (int) ARG(2);
    int res = CALL(int,(int,int),(arg1,arg2));
    ARG(0) = res;
  } else {
    /* Do not have the generic call for this argument list. */
    int i;
    printf("Can not call external function at address %d\n",(int)fun);
    printf("Argument string = '");
    for(i=0;i<n_args;i++) {
      printf("%c",(char)argstr[i]);
    }
    printf("' [%d arg(s)]\n",n_args);
    assert(0);
  }
#undef CMP
}

/* --------------------------------------------------------------------------
 * Move args/results between STG stack and the above API's arg block
 * Returns 0 on success
 *         1 if too many args/results or non-handled type
 *         2 if config error on this platform
 * Tries to automatically handle 32-vs-64 bit differences.
 * Assumes an LP64 programming model for 64 bit: 
 *    sizeof(long)==sizeof(void*)==64  on a 64 bit platform
 *    sizeof(int)==32                  on a 64 bit platform
 * ------------------------------------------------------------------------*/

int ccall ( CFunDescriptor* d, void (*fun)(void), StgBCO** bco )
{
   double         arg_vec [31];
   char           argd_vec[31];
   unsigned int*  p;
   int            i;
   unsigned long  ul;

   if (sizeof(int) != 4 || sizeof(double) != 8 || sizeof(float) != 4
       || (sizeof(void*) != 4 && sizeof(void*) != 8)
       || (sizeof(unsigned long) != sizeof(void*)))
      return 2;

   if (d->num_args > 30 || d->num_results > 1)
      return 1; /* unlikely, but ... */

   //fprintf ( stderr, "ccall: `%s' %d -> `%s' %d\n",
   //         d-> arg_tys, d->num_args, d->result_tys, d->num_results );

   p = (unsigned int*) &arg_vec[1];
   for (i = 0; i < d->num_args; i++) {
      switch (d->arg_tys[i]) {

         case INT_REP:
            ul = (unsigned long)PopTaggedInt();
            goto common_int32_or_64;
         case WORD_REP:
            ul = (unsigned long)PopTaggedWord();
            goto common_int32_or_64;
         case ADDR_REP:
            ul = (unsigned long)(PopTaggedAddr());
            goto common_int32_or_64;
         case STABLE_REP:
            ul = (unsigned long)PopTaggedStablePtr();
            common_int32_or_64:
            if (sizeof(void*) == 4) {
               *(unsigned long *)p = ul; p++; *p++ = 0;
               argd_vec[i+1] = 'i';
            } else {
               *(unsigned long *)p = ul;
               p += 2;
               argd_vec[i+1] = 'I';
            }
            break;

         case CHAR_REP: {
            int j = (int)PopTaggedChar();
            *p++ = j; *p++ = 0;
            argd_vec[i+1] = 'i';
            break;
         }
         case FLOAT_REP: {
            float f = PopTaggedFloat();
            *(float*)p = f; p++; *p++ = 0;
            argd_vec[i+1] = 'f';
            break;
         }
         case DOUBLE_REP: {
            double d = PopTaggedDouble();
            *(double*)p = d; p+=2;
            argd_vec[i+1] = 'F';
            break;
         }
         default:
            return 1;
      }
   }

   if (d->num_results == 0) {
      argd_vec[0] = 'i'; 
   } else {
      switch (d->result_tys[0]) {
         case INT_REP: case WORD_REP: case ADDR_REP: case STABLE_REP:
            argd_vec[0] = (sizeof(void*)==4) ? 'i' : 'I'; break;
         case CHAR_REP:
            argd_vec[0] = 'i'; break;
         case FLOAT_REP:
            argd_vec[0] = 'f'; break;
         case DOUBLE_REP:
            argd_vec[0] = 'F'; break;
         default:
            return 1;
      }
   }
 
   PushPtr((StgPtr)(*bco));
   SaveThreadState();

   //fprintf(stderr, " argc=%d  arg_vec=%p  argd_vec=%p `%s' fun=%p\n", 
   //          d->num_args, arg_vec, argd_vec, argd_vec, fun );

#if 1
   universal_call_c_x86_linux ( 
      d->num_args, (void*)arg_vec, argd_vec, fun );
#else
   universal_call_c_generic ( 
      d->num_args, (void*)arg_vec, argd_vec, fun );
#endif
   LoadThreadState();
   *bco=(StgBCO*)PopPtr();

   /* INT, WORD, ADDR, STABLE don't need to do a word-size check
      since the result is in the bytes starting at p regardless. */

   if (d->num_results > 0) {
      p = (unsigned int*) &arg_vec[0];
      switch (d->result_tys[0]) {

         case INT_REP:
            PushTaggedInt ( ((StgInt*)p) [0] );
            break;
         case WORD_REP:
            PushTaggedWord ( ((StgWord*)p) [0] );
            break;
         case ADDR_REP:
            PushTaggedAddr ( ((StgAddr*)p) [0] );
            break;
         case STABLE_REP:
            PushTaggedStablePtr ( ((StgStablePtr*)p) [0] );
            break;

         case CHAR_REP:
            PushTaggedChar ( (StgChar) p[0]);
            break;
         case FLOAT_REP:
            PushTaggedFloat ( ((StgFloat*)p) [0] );
            break;
         case DOUBLE_REP:
            PushTaggedDouble ( ((StgDouble*)p) [0] );
            break;

         default:
            return 1;
      }
   }

   return 0;
}



CFunDescriptor* mkDescriptor( char* as, char* rs ) 
{ 
    /* ToDo: don't use malloc */
    CFunDescriptor *d  = malloc(sizeof(CFunDescriptor));
    if (d == NULL) return d;
    d->arg_tys     = as;
    d->result_tys  = rs;
    d->num_args    = strlen(as);
    d->num_results = strlen(rs);
    return d;
}


#endif /* INTERPRETER */
