
/* -----------------------------------------------------------------------------
 * $Id: ForeignCall.c,v 1.13 2000/03/02 10:32:17 sewardj Exp $
 *
 * (c) The GHC Team 1994-1999.
 *
 * Implementation of foreign import and foreign export.
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

#ifdef INTERPRETER

#include "RtsUtils.h"    /* barf :-) */
#include "Assembler.h"   /* for CFun stuff */
#include "Schedule.h"
#include "Evaluator.h"
#include "ForeignCall.h"

/* Exports of this file:
      mkDescriptor
      ccall
      createAdjThunk
   Everything else is local, I think.
*/

/* ----------------------------------------------------------------------
 * Some misc-ery to begin with.
 * --------------------------------------------------------------------*/

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


/* ----------------------------------------------------------------------
 * Part the first: CALLING OUT -- foreign import
 * --------------------------------------------------------------------*/

/* SOME NOTES ABOUT PARAMETERISATION.

   These pertain equally to foreign import and foreign export.
  
   Implementations for calling in and out are very architecture
   dependent.  After some consideration, it appears that the two
   important factors are the instruction set, and the calling
   convention used.  Factors like the OS and compiler are not
   directly relevant.

   So: routines which are architecture dependent are have
       _instructionsetname_callingconventionname attached to the
       the base name.  For example, code specific to the ccall
       convention on x86 would be suffixed _x86_ccall.

   A third possible dimension of parameterisation relates to the
   split between callee and caller saves registers.  For example,
   x86_ccall code needs to assume a split, and different splits
   using ccall on x86 need different code.  However, that does not
   yet seem an issue, so it is ignored here.
*/


/* ------------------------------------------------------------------
 * Calling out to C: a simple, universal calling API
 * ----------------------------------------------------------------*/

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

   These architecture-dependent assembly routines are in
   rts/universal_call_c.S.
*/


/* ----------------------------------------------------------------*
 * External  refs for the assembly routines.
 * ----------------------------------------------------------------*/

#if i386_TARGET_ARCH
extern void universal_call_c_x86_stdcall  ( int, void*, char*, void* );
extern void universal_call_c_x86_ccall    ( int, void*, char*, void* );
#else
static void universal_call_c_generic      ( int, void*, char*, void* );
#endif

/* ----------------------------------------------------------------*
 * This is a generic version of universal call that
 * only works for specific argument patterns.
 * 
 * It allows ports to work on the Hugs Prelude immediately,
 * even if universal_call_c_arch_callingconvention is not available.
 * ----------------------------------------------------------------*/

static void universal_call_c_generic
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
#undef CALL
#undef CMP
#undef ARG
}


/* ----------------------------------------------------------------*
 * Move args/results between STG stack and the above API's arg block
 * Returns 0 on success
 *         1 if too many args/results or non-handled type
 *         2 if config error on this platform
 * Tries to automatically handle 32-vs-64 bit differences.
 * Assumes an LP64 programming model for 64 bit: 
 *    sizeof(long)==sizeof(void*)==64  on a 64 bit platform
 *    sizeof(int)==32                  on a 64 bit platform
 * This code attempts to be architecture neutral (viz, generic).
 * ----------------------------------------------------------------*/

int ccall ( CFunDescriptor*  d, 
            void             (*fun)(void), 
            StgBCO**         bco,
            char             cc,
            Capability*      cap
          )
{
   double         arg_vec [31];
   char           argd_vec[31];
   unsigned int*  p;
   int            i;
   unsigned long  ul;
   unsigned int   token;

   if (sizeof(int) != 4 || sizeof(double) != 8 || sizeof(float) != 4
       || (sizeof(void*) != 4 && sizeof(void*) != 8)
       || (sizeof(unsigned long) != sizeof(void*)))
      return 2;

   if (d->num_args > 30 || d->num_results > 1)
      return 1; /* unlikely, but ... */

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
   cap->rCurrentTSO->sp    = MainRegTable.rSp;
   cap->rCurrentTSO->su    = MainRegTable.rSu;
   cap->rCurrentTSO->splim = MainRegTable.rSpLim;
   token = suspendThread(cap);

#if i386_TARGET_ARCH
   if (cc == 'c')
      universal_call_c_x86_ccall ( 
         d->num_args, (void*)arg_vec, argd_vec, fun );
   else if (cc == 's')
      universal_call_c_x86_stdcall ( 
         d->num_args, (void*)arg_vec, argd_vec, fun );
   else barf ( "ccall(i386): unknown calling convention" );
#else
   universal_call_c_generic ( 
      d->num_args, (void*)arg_vec, argd_vec, fun );
#endif

   cap = resumeThread(token);
   MainRegTable.rSp    = cap->rCurrentTSO->sp;
   MainRegTable.rSu    = cap->rCurrentTSO->su;
   MainRegTable.rSpLim = cap->rCurrentTSO->splim;
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



/* ----------------------------------------------------------------------
 * Part the second: CALLING IN -- foreign export {dynamic}
 * --------------------------------------------------------------------*/

/* Make it possible for the evaluator to get hold of bytecode
   for a given function by name.  Useful but a hack.  Sigh.
 */
extern void* getHugs_AsmObject_for ( char* s );
extern int /*Bool*/ combined;

/* ----------------------------------------------------------------*
 * The implementation for x86_ccall and x86_stdcall.
 * ----------------------------------------------------------------*/

static 
HaskellObj
unpackArgsAndCallHaskell_x86_nocallconv_wrk ( StgStablePtr stableptr, 
                                              char* tydesc, char* args)
{
   /* Copy args out of the C stack frame in an architecture
      dependent fashion, under the direction of the type description
      string tydesc.  Dereference the stable pointer, giving the
      Haskell function to call.  Build an application of this to
      the arguments, and finally wrap primRunST round the whole
      thing, since we know it returns an IO type.  Then evaluate
      the whole, which leaves nodeOut as the evaluated 'a', where
      the type of the function called is .... -> IO a.

      We can't immediately unpack the results and return, since
      int results need to return in a different register (%eax and
      possibly %edx) from float things (%st(0)).  So return nodeOut
      to the relevant wrapper function, which knows enough about
      the return type to do the Right Thing.

      There's no getting round it: this is most heinous hack.
   */

   HaskellObj      node;
   HaskellObj      nodeOut;
   SchedulerStatus sstat;

   char* resp = tydesc;
   char* argp = tydesc;

   node = (HaskellObj)deRefStablePtr(stableptr);

   if (*argp != ':') argp++;
   ASSERT( *argp == ':' );
   argp++;
   while (*argp) {
      switch (*argp) {
         case CHAR_REP:
            node = rts_apply ( node, rts_mkChar ( *(char*)args ) );
            args += 4;
            break;
         case INT_REP:
            node = rts_apply ( node, rts_mkInt ( *(int*)args ) );
            args += 4;
            break;
         case WORD_REP:
            node = rts_apply ( node, rts_mkWord ( *(unsigned int*)args ) );
            args += 4;
            break;
         case ADDR_REP:
            node = rts_apply ( node, rts_mkAddr ( *(void**)args ) );
            args += 4;
            break;
         case STABLE_REP:
            node = rts_apply ( node, rts_mkStablePtr ( *(int*)args ) );
            args += 4;
            break;
         case FLOAT_REP:
            node = rts_apply ( node, rts_mkFloat ( *(float*)args ) );
            args += 4;
            break;
         case DOUBLE_REP:
            node = rts_apply ( node, rts_mkDouble ( *(double*)args ) );
            args += 8;
            break;
         default:
            barf(
               "unpackArgsAndCallHaskell_x86_nocallconv: "
               "unexpected arg type rep");
      }
      argp++;
   }

   if (combined) {
      sstat = rts_evalIO ( node, &nodeOut );
   } else {
      node = rts_apply ( 
                asmClosureOfObject(getHugs_AsmObject_for("primRunST")), 
                node );
      sstat = rts_eval ( node, &nodeOut );
   }

   if (sstat != Success)
      barf ("unpackArgsAndCallHaskell_x86_nocallconv: eval failed");

   return nodeOut;
}


static 
double
unpackArgsAndCallHaskell_x86_nocallconv_DOUBLE ( 
      StgStablePtr stableptr, char* tydesc, char* args
   )
{
   HaskellObj nodeOut
      = unpackArgsAndCallHaskell_x86_nocallconv_wrk ( 
           stableptr, tydesc, args 
        );
   /* Return a double.  This return will go into %st(0), which 
      is unmodified by the adjustor thunk.
   */
   ASSERT(tydesc[0] == DOUBLE_REP);
   return rts_getDouble(nodeOut);
}


static 
float
unpackArgsAndCallHaskell_x86_nocallconv_FLOAT ( 
      StgStablePtr stableptr, char* tydesc, char* args
   )
{
   HaskellObj nodeOut
      = unpackArgsAndCallHaskell_x86_nocallconv_wrk ( 
           stableptr, tydesc, args 
        );
   /* Probably could be merged with the double case, since %st(0) is
      still the return register.
   */
   ASSERT(tydesc[0] == FLOAT_REP);
   return rts_getFloat(nodeOut);
}


static 
unsigned long
unpackArgsAndCallHaskell_x86_nocallconv_INTISH ( 
      StgStablePtr stableptr, char* tydesc, char* args
   )
{
   HaskellObj nodeOut;
   nodeOut = unpackArgsAndCallHaskell_x86_nocallconv_wrk ( 
                stableptr, tydesc, args 
             );
   /* A complete hack.  We know that all these returns will be
      put into %eax (and %edx, if it is a 64-bit return), and
      the adjustor thunk will then itself return to the original
      (C-world) caller without modifying %eax or %edx, so the
      original caller will be a Happy Bunny.
   */
   switch (*tydesc) {
      case ':':        return 0;
      case CHAR_REP:   return (unsigned long)rts_getChar(nodeOut);
      case INT_REP:    return (unsigned long)rts_getInt(nodeOut);
      case WORD_REP:   return (unsigned long)rts_getWord(nodeOut);
      case ADDR_REP:   return (unsigned long)rts_getAddr(nodeOut);
      case STABLE_REP: return (unsigned long)rts_getStablePtr(nodeOut);
      default:
         barf(
            "unpackArgsAndCallHaskell_x86_nocallconv: "
            "unexpected res type rep");
   }
}


/* This is a bit subtle, since it can deal with both stdcall
   and ccall.  There are two call transitions to consider:

   1.  The call to "here".  If it's a ccall, we can return
       using 'ret 0' and let the caller remove the args.
       If stdcall, we have to return with 'ret N', where
       N is the size of the args passed.  N has to be 
       determined by inspecting the type descriptor string
       typestr.

   2.  The call to unpackArgsAndCallHaskell_x86_anycallconv_*.
       Whether these are done with stdcall or ccall depends on
       the conventions applied by the compiler that translated
       those procedures.  Fortunately, we can sidestep what it
       did by saving esp (in ebx), pushing the three args,
       calling unpack..., and restoring esp from ebx.  This
       trick assumes that ebx is a callee-saves register, so
       its value will be preserved across the unpack... call.
*/
static
StgAddr createAdjThunk_x86 ( StgStablePtr stableptr,
                             StgAddr      typestr,
                             char         callconv )
{
   unsigned char* codeblock;
   unsigned char* cp;
   unsigned int   ch;
   unsigned int   nwords;

   unsigned char* argp = (unsigned char*)typestr;
   unsigned int   ts   = (unsigned int)typestr;
   unsigned int   sp   = (unsigned int)stableptr;

   if (((char*)typestr)[0] == DOUBLE_REP)
      ch = (unsigned int)
              &unpackArgsAndCallHaskell_x86_nocallconv_DOUBLE;
   else if (((char*)typestr)[0] == FLOAT_REP)
      ch = (unsigned int)
              &unpackArgsAndCallHaskell_x86_nocallconv_FLOAT;
   else
      ch = (unsigned int)
              &unpackArgsAndCallHaskell_x86_nocallconv_INTISH;

   codeblock = malloc ( 0x26 );
   if (!codeblock)
      barf ( "createAdjThunk_x86: can't malloc memory\n");

   if (callconv == 's') {
      nwords = 0;
      if (*argp != ':') argp++;
      ASSERT( *argp == ':' );
      argp++;
      while (*argp) {
         switch (*argp) {
            case CHAR_REP: case INT_REP: case WORD_REP: 
            case ADDR_REP: case STABLE_REP: case FLOAT_REP:
               nwords += 4; break;
            case DOUBLE_REP:
               nwords += 8; break;
            default:
               barf("createAdjThunk_x86: unexpected type descriptor");
         }
         argp++;
      }
   } else
   if (callconv == 'c') {
      nwords = 0;
   } else {
      barf ( "createAdjThunk_x86: unknown calling convention\n");
   }

   cp = codeblock;
   /*
      0000 53           pushl %ebx        # save caller's registers
      0001 51           pushl %ecx
      0002 56           pushl %esi
      0003 57           pushl %edi
      0004 55           pushl %ebp
      0005 89E0         movl %esp,%eax    # sp -> eax
      0007 83C018       addl $24,%eax     # move eax back over 5 saved regs + retaddr
      000a 89E3         movl %esp,%ebx    # remember sp before pushing args
      000c 50           pushl %eax        # push arg-block addr
      000d 6844332211   pushl $0x11223344 # push addr of type descr string
      0012 6877665544   pushl $0x44556677 # push stableptr to closure
      0017 E8BBAA9988   call 0x8899aabb   # SEE COMMENT BELOW
                                          # return value is in %eax, or %eax:%edx, 
                                          # or %st(0), so don't trash these regs 
                                          # between here and 'ret'
      001c 89DC         movl %ebx,%esp    # restore sp from remembered value
      001e 5D           popl %ebp         # restore caller's registers
      001f 5F           popl %edi
      0020 5E           popl %esi
      0021 59           popl %ecx
      0022 5B           popl %ebx
      0023 C27766       ret  $0x6677      # return, clearing args if stdcall
   */
   *cp++ = 0x53;
   *cp++ = 0x51;
   *cp++ = 0x56;
   *cp++ = 0x57;
   *cp++ = 0x55;
   *cp++ = 0x89; *cp++ = 0xE0;
   *cp++ = 0x83; *cp++ = 0xC0; *cp++ = 0x18;
   *cp++ = 0x89; *cp++ = 0xE3;
   *cp++ = 0x50;
   *cp++ = 0x68; *cp++=ts;ts>>=8; *cp++=ts;ts>>=8; *cp++=ts;ts>>=8; *cp++=ts;
   *cp++ = 0x68; *cp++=sp;sp>>=8; *cp++=sp;sp>>=8; *cp++=sp;sp>>=8; *cp++=sp;

   /* call address needs to be: displacement relative to next insn */
   ch = ch - ( ((unsigned int)cp) + 5);
   *cp++ = 0xE8; *cp++=ch;ch>>=8; *cp++=ch;ch>>=8; *cp++=ch;ch>>=8; *cp++=ch;

   *cp++ = 0x89; *cp++ = 0xDC;
   *cp++ = 0x5D;
   *cp++ = 0x5F;
   *cp++ = 0x5E;
   *cp++ = 0x59;
   *cp++ = 0x5B;
   *cp++ = 0xC2; *cp++=nwords;nwords>>=8; *cp++=nwords;

   return codeblock;
}


/* ----------------------------------------------------------------*
 * The only function involved in foreign-export that needs to be
 * visible outside this file.
 * ----------------------------------------------------------------*/

StgAddr createAdjThunk ( StgStablePtr stableptr,
                         StgAddr      typestr,
                         StgChar      callconv )
{
   return 
#if i386_TARGET_ARCH
      createAdjThunk_x86 ( stableptr, typestr, callconv );
#else
      0;
      #warn foreign export not implemented on this architecture
#endif
}


#endif /* INTERPRETER */

