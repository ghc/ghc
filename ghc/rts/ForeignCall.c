/* -*- mode: hugs-c; -*- */
/* -----------------------------------------------------------------------------
 * $Id: ForeignCall.c,v 1.3 1999/02/05 16:02:40 simonm Exp $
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

/* the assymetry here seem to come from the caller-allocates 
 * calling convention.  But does the caller really allocate 
 * result??
 */

void hcall( HFunDescriptor* d, StablePtr fun, void* as, void* rs)
{
#if 0
    /* out of date - ADR */
    marshall(d->arg_tys,as);
    prim_hcall(fun);
    unmarshall(d->result_tys,rs);
#else
    assert(0);
#endif
}

/* By experiment on an x86 box, we found that gcc's
 * __builtin_apply(fun,as,size) expects *as to look like this:
 *   as[0] = &first arg = &as[1]
 *   as[1] = arg1
 *   as[2] = arg2
 *   ...
 *
 * on an x86, it returns a pointer to a struct containing an
 * int/int64/ptr in its first 4-8 bytes and a float/double in the next
 * 8 bytes.
 *
 * On a sparc:
 *   as[0] = &first arg = &as[2]
 *   as[1] = where structures should be returned
 *   as[2] = arg1
 *   as[3] = arg2
 *   ...
 *
 * This is something of a hack - but seems to be more portable than
 * hacking it up in assembly language which is how I did it before - ADR
 */
void ccall( CFunDescriptor* d, void (*fun)(void) )
{
    void *rs;
    char* tys = d->arg_tys;
    /* ToDo: the use of ARG_SIZE is based on the assumption that Hugs
     * obeys the same alignment restrictions as C.
     * But this is almost certainly wrong!
     * We could use gcc's __va_rounded_size macro (see varargs.h) to do a
     * better job.
     */
#if i386_TARGET_ARCH
    void *as=alloca(4 + d->arg_size);
    StgWord* args = (StgWord*) as;
    *(void**)(args++) = 4 + (char*)as; /* incoming args ptr */
    for(; *tys; ++tys) {
      args += unmarshall(*tys,args);
    }
    rs = __builtin_apply(fun,as,(char*)args-(char*)as-4);
#elif sparc_TARGET_ARCH
    void *as=alloca(8 + d->arg_size);
    StgWord* args = (StgWord*) as;
    int argcount;
    *(void**)(args++) = (char*)as; /* incoming args ptr */
    *(void**)(args++) = 0;  /* structure value address - I think this is the address of a block of memory where structures are returned - in which case we should initialise with rs or something like that*/
    for(; *tys; ++tys) {
      args += unmarshall(*tys,args);
    }
    argcount = ((void*)args - as);
    ASSERT(8 + d->arg_size == argcount);
    if (argcount <= 8) {
      argcount = 0;
    } else {
      argcount -= 4;
    }
    rs = __builtin_apply(fun,as,argcount);
#else
#error Cant do ccall for this architecture
#endif

    /* ToDo: can't handle multiple return values at the moment
     * - it's hard enough to get single return values working
     */
    if (*(d->result_tys)) {
        char ty = *(d->result_tys);
        ASSERT(d->result_tys[1] == '\0');
        switch (ty) {
        case 'F':
        case 'D': 
                /* ToDo: is this right? */
                marshall(ty,(char*)rs+8);
                return;
        default:
                marshall(ty,rs);
                return;
        }
    }
}

CFunDescriptor* mkDescriptor( char* as, char* rs ) 
{ 
    /* ToDo: don't use malloc */
    CFunDescriptor *d = malloc(sizeof(CFunDescriptor));
    assert(d);
    d->arg_tys = as;
    d->arg_size = argSize(as);
    d->result_tys = rs;
    d->result_size = argSize(rs);
    return d;
}

#endif /* INTERPRETER */
