
/* -----------------------------------------------------------------------------
 * $Id: ForeignCall.c,v 1.5 1999/10/15 11:03:06 sewardj Exp $
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

#if 0
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
#endif




#if 1
/* HACK alert (red alert) */
extern StgInt          PopTaggedInt       ( void ) ;
extern StgDouble       PopTaggedDouble    ( void ) ;
extern StgFloat        PopTaggedFloat     ( void ) ;
extern StgChar         PopTaggedChar      ( void ) ;
extern StgAddr         PopTaggedAddr      ( void ) ;

extern void   PushTaggedInt  ( StgInt );
extern void   PushTaggedAddr ( StgAddr );
extern void   PushPtr        ( StgPtr );
extern StgPtr PopPtr         ( void );


int seqNr = 0;
#define IF(sss) if (strcmp(sss,cdesc)==0)
#define STS      PushPtr((StgPtr)(*bco));SaveThreadState()
#define LTS      LoadThreadState();*bco=(StgBCO*)PopPtr();
#define LTS_RET  LoadThreadState();*bco=(StgBCO*)PopPtr(); return
#define RET      return
void ccall( CFunDescriptor* d, void (*fun)(void), StgBCO** bco )
{
   int i;
   char cdesc[100];
   strcpy(cdesc, d->result_tys);
   strcat(cdesc, ":");
   strcat(cdesc, d->arg_tys);
   for (i = 0; cdesc[i] != 0; i++) {
      switch (cdesc[i]) {
         case 'x': cdesc[i] = 'A'; break;
         default:  break;
      }
   }

   //fprintf(stderr, "ccall: %d cdesc = `%s'\n", seqNr++, cdesc);

   IF(":") { STS; ((void(*)(void))(fun))(); LTS_RET; };

   IF(":I") { int a1=PopTaggedInt(); 
              STS; ((void(*)(int))(fun))(a1); LTS_RET; };
   IF(":A") { void* a1=PopTaggedAddr(); 
              STS; ((void(*)(void*))(fun))(a1); LTS_RET; };

   IF("I:") { int r; 
              STS; r= ((int(*)(void))(fun))(); LTS;
              PushTaggedInt(r); RET ;};

   IF(":II") { int a1=PopTaggedInt(); int a2=PopTaggedInt();
               STS; ((void(*)(int,int))(fun))(a1,a2); LTS_RET; };
   IF(":AI") { void* a1=PopTaggedAddr(); int a2=PopTaggedInt();
               STS; ((void(*)(void*,int))(fun))(a1,a2); LTS_RET; };

   IF("I:I") { int a1=PopTaggedInt(); int r;
               STS; r=((int(*)(int))(fun))(a1); LTS;
               PushTaggedInt(r); RET; };
   IF("A:I") { int a1=PopTaggedInt(); void* r;
               STS; r=((void*(*)(int))(fun))(a1); LTS;
               PushTaggedAddr(r); RET; };
   IF("A:A") { void* a1=PopTaggedAddr(); void* r;
               STS; r=((void*(*)(void*))(fun))(a1); LTS;
               PushTaggedAddr(r); RET; };

   IF("I:II") { int a1=PopTaggedInt(); int a2=PopTaggedInt(); int r;
                STS; r=((int(*)(int,int))(fun))(a1,a2); LTS;
                PushTaggedInt(r); RET; };
   IF("I:AI") { void* a1=PopTaggedAddr(); int a2=PopTaggedInt(); int r;
                STS; r=((int(*)(void*,int))(fun))(a1,a2); LTS;
                PushTaggedInt(r); RET; };
   IF("A:AI") { void* a1=PopTaggedAddr(); int a2=PopTaggedInt(); void* r;
                STS; r=((void*(*)(void*,int))(fun))(a1,a2); LTS;
                PushTaggedAddr(r); RET; };

   IF("I:III") { int a1=PopTaggedInt(); int a2=PopTaggedInt(); 
                 int a3=PopTaggedInt(); int r;
                 STS; r=((int(*)(int,int,int))(fun))(a1,a2,a3); LTS;
                 PushTaggedInt(r); RET; };

   IF(":AIDCF") { void*  a1 = PopTaggedAddr(); 
                  int    a2 = PopTaggedInt();
                  double a3 = PopTaggedDouble();
                  char   a4 = PopTaggedChar();
                  float  a5 = PopTaggedFloat();
                  STS;
                  ((void(*)(void*,int,double,char,float))(fun))(a1,a2,a3,a4,a5); 
                  LTS_RET; };


fprintf(stderr,"panic: ccall cdesc `%s' not implemented\n", cdesc );
   exit(1);


fprintf(stderr, 
        "ccall: arg_tys %s arg_size %d result_tys %s result_size %d\n",
        d->arg_tys, d->arg_size, d->result_tys, d->result_size );
}

#undef IF
#undef STS
#undef LTS
#undef LTS_RET
#undef RET

#endif






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
