
/* --------------------------------------------------------------------------
 * Optimiser
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: optimise.c,v $
 * $Revision: 1.5 $
 * $Date: 1999/04/27 10:06:57 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "backend.h"
#include "connect.h"
#include "errors.h"
#include "link.h"
#include "Assembler.h"

/* #define DEBUG_OPTIMISE */

/* --------------------------------------------------------------------------
 * Local functions
 * ------------------------------------------------------------------------*/

Int nLoopBreakersInlined;
Int nLetvarsInlined;
Int nTopvarsInlined;
Int nCaseOfLet;
Int nCaseOfCase;
Int nCaseOfPrimCase;
Int nCaseOfCon;
Int nCaseOfOuter;
Int nLetBindsDropped;
Int nLetrecGroupsDropped;
Int nLambdasMerged;
Int nCaseDefaultsDropped;
Int nAppsMerged;
Int nLetsFloatedOutOfFn;
Int nLetsFloatedIntoCase;
Int nCasesFloatedOutOfFn;
Int nBetaReductions;

Int nTotSizeIn;
Int nTotSizeOut;

Int  rDepth;
Bool copyInTopvar;
Bool inDBuilder;

static void local optimiseTopBind( StgVar v );

typedef
   enum {
      CTX_SCRUT,
      CTX_OTHER
   }
   InlineCtx;

/* Exactly like whatIs except it avoids a fn call for STG tags */
#define whatIsStg(xx) ((isPair(xx) ? (isTag(fst(xx)) ? fst(xx) : AP) : whatIs(xx)))


/* --------------------------------------------------------------------------
 * Transformation stats
 * ------------------------------------------------------------------------*/

void initOptStats ( void )
{
   nLoopBreakersInlined  = 0;
   nLetvarsInlined       = 0;
   nTopvarsInlined       = 0;
   nCaseOfLet            = 0;
   nCaseOfCase           = 0;
   nCaseOfPrimCase       = 0;
   nCaseOfCon            = 0;
   nCaseOfOuter          = 0;
   nLetBindsDropped      = 0;
   nLetrecGroupsDropped  = 0;
   nLambdasMerged        = 0;
   nCaseDefaultsDropped  = 0;
   nAppsMerged           = 0;
   nLetsFloatedOutOfFn   = 0;
   nLetsFloatedIntoCase  = 0;
   nCasesFloatedOutOfFn  = 0;
   nBetaReductions       = 0;
   nTotSizeIn            = 0;
   nTotSizeOut           = 0;
}

void printOptStats ( FILE* f )
{
   fflush(stdout); fflush(stderr); fflush(f);
   fprintf(f, "\n\n" );
   fprintf(f, "Inlining:     topvar %-5d        letvar %-5d"
              "      loopbrkr %-5d      betaredn %-5d\n",
              nTopvarsInlined, nLetvarsInlined, nLoopBreakersInlined, 
              nBetaReductions );
   fprintf(f, "Case-of-:        let %-5d          case %-5d"
              "           con %-5d         case# %-5d\n",
              nCaseOfLet, nCaseOfCase, nCaseOfCon, nCaseOfPrimCase );
   fprintf(f, "Dropped:     letbind %-5d      letgroup %-5d"
              "       default %-5d\n",
              nLetBindsDropped, nLetrecGroupsDropped, nCaseDefaultsDropped );
   fprintf(f, "Merges:       lambda %-5d           app %-5d\n",
              nLambdasMerged, nAppsMerged  );
   fprintf(f, "Fn-float:        let %-5d          case %-5d\n",
              nLetsFloatedOutOfFn, nCasesFloatedOutOfFn );
   fprintf(f, "Misc:     case-outer %-5d let-into-case %-5d\n",
              nCaseOfOuter, nLetsFloatedIntoCase );
   fprintf(f, "total size:       in %-5d           out %-5d\n",
              nTotSizeIn, nTotSizeOut );
   fprintf(f, "\n" );
}


/* --------------------------------------------------------------------------
 * How big is this STG tree (viz (primarily), do I want to inline it?)
 * ------------------------------------------------------------------------*/

Int stgSize_list ( List es )
{
   Int n = 0;
   for (; nonNull(es); es=tl(es)) n += stgSize(hd(es));
   return n;
}

Int stgSize ( StgExpr e )
{
   List xs;
   Int n = 1;

   if (isNull(e)) return 0;

   switch(whatIsStg(e)) {
      case STGVAR:
         break;
      case LETREC:
         for (xs = stgLetBinds(e); nonNull(xs);xs=tl(xs)) 
            n += stgSize(stgVarBody(hd(xs)));
         n += stgSize(stgLetBody(e));
         break;
      case LAMBDA:
         n += stgSize(stgLambdaBody(e));
         break;
      case CASE:
         n += stgSize_list(stgCaseAlts(e));
         n += stgSize(stgCaseScrut(e));
         break;
      case PRIMCASE:
         n += stgSize_list(stgPrimCaseAlts(e));
         n += stgSize(stgPrimCaseScrut(e));
         break;
      case STGAPP:
         n += stgSize_list(stgAppArgs(e));
         n += stgSize(stgAppFun(e));
         break;
      case STGPRIM:
         n += stgSize_list(stgPrimArgs(e));
         n += stgSize(stgPrimOp(e));
         break;
      case STGCON:
         n += stgSize_list(stgConArgs(e));
         n += stgSize(stgConCon(e));
         break;
      case DEEFALT:
         n  = stgSize(stgDefaultBody(e));
         break;
      case CASEALT:
         n  = stgSize(stgCaseAltBody(e));
         break;
      case PRIMALT:
         n  = stgSize(stgPrimAltBody(e));
         break;
      case INTCELL:
      case STRCELL:
      case PTRCELL:
      case CHARCELL:
      case FLOATCELL:
      case BIGCELL:
      case NAME:
      case TUPLE:
         break;
      default:
         fprintf(stderr, "sizeStg: unknown stuff %d\n",whatIsStg(e));
         assert(0);
   }
   return n;
}


/* --------------------------------------------------------------------------
 * Stacks of pairs of collectable things.  Used to implement associations.
 * cloneStg() uses its stack to map old var names to new ones.
 * ------------------------------------------------------------------------*/

#define M_PAIRS 400
#define SP_NOT_IN_USE (-123456789)

typedef
   struct { Cell pfst; Cell psnd; } 
   StgPair;

static Int     spClone;
static StgPair pairClone[M_PAIRS];

void markPairs ( void )
{
   Int i;
   if (spClone != SP_NOT_IN_USE) {
      for (i = 0; i <= spClone; i++) {
         mark(pairClone[i].pfst);
         mark(pairClone[i].psnd);
      }
   }
}

void pushClone ( Cell a, Cell b )
{
   spClone++;
   if (spClone >= M_PAIRS) internal("pushClone -- M_PAIRS too small");
   pairClone[spClone].pfst = a;
   pairClone[spClone].psnd = b;
}

void dropClone ( void )
{
   if (spClone < 0) internal("dropClone");
   spClone--;
}

Cell findClone ( Cell x )
{
   Int i;
   for (i = spClone; i >= 0; i--)
      if (pairClone[i].pfst == x)
         return pairClone[i].psnd;
   return NIL;
}


/* --------------------------------------------------------------------------
 * Cloning of STG trees
 * ------------------------------------------------------------------------*/

/* Clone v to create a new var.  Works for both StgVar and StgPrimVar. */
StgVar cloneStgVar ( StgVar v )
{
  return ap(STGVAR,triple(stgVarBody(v),stgVarRep(v),NIL));
}


/* For each StgVar in origVars, make a new one with cloneStgVar,
   and push the (old,new) pair on the clone pair stack.  Returns
   the list of new vars.
*/
List cloneStg_addVars ( List origVars )
{
   List newVars = NIL;
   while (nonNull(origVars)) {
      StgVar newv = cloneStgVar(hd(origVars));
      pushClone ( hd(origVars), newv );
      newVars    = cons(newv,newVars);
      origVars   = tl(origVars);
   }
   newVars = rev(newVars);
   return newVars;
}


void cloneStg_dropVars ( List vs )
{
   for (; nonNull(vs); vs=tl(vs)) 
      dropClone();
}


/* Print the clone pair stack.  Just for debugging purposes. */
void ppCloneEnv ( char* s )
{
   Int i;
   fflush(stdout);fflush(stderr);
   printf ( "\nenv-%s\n", s );
   for (i = 0; i <= spClone; i++) {
      printf ( "\t" ); 
      ppStgExpr(pairClone[i].pfst);
      ppStgExpr(pairClone[i].psnd);
      printf ( "\n" );
   };
   printf ( "vne-%s\n", s );
}


StgExpr cloneStg ( StgExpr e )
{
   List xs, newvs;
   StgVar newv;
   StgExpr t;

   switch(whatIsStg(e)) {
      case STGVAR:
         newv = findClone(e);
         if (nonNull(newv)) return newv; else return e;
      case LETREC:
         newvs = cloneStg_addVars ( stgLetBinds(e) );
         for (xs = newvs; nonNull(xs);xs=tl(xs)) 
            stgVarBody(hd(xs)) = cloneStg(stgVarBody(hd(xs)));
         t = mkStgLet(newvs,cloneStg(stgLetBody(e)));
         cloneStg_dropVars ( stgLetBinds(e) );
         return t;
      case LAMBDA:
         newvs = cloneStg_addVars ( stgLambdaArgs(e) );
         t = mkStgLambda(newvs, cloneStg(stgLambdaBody(e)));
         cloneStg_dropVars ( stgLambdaArgs(e) );
         return t;
      case CASE:
         xs = dupList(stgCaseAlts(e)); 
         mapOver(cloneStg,xs);
         return mkStgCase(cloneStg(stgCaseScrut(e)),xs);
      case PRIMCASE:
         xs = dupList(stgPrimCaseAlts(e));
         mapOver(cloneStg,xs);
         return mkStgPrimCase(cloneStg(stgPrimCaseScrut(e)),xs);
      case STGAPP:
         xs = dupList(stgAppArgs(e));
         mapOver(cloneStg,xs);
         return mkStgApp(cloneStg(stgAppFun(e)),xs);
      case STGPRIM:
         xs = dupList(stgPrimArgs(e));
         mapOver(cloneStg,xs);
         return mkStgPrim(cloneStg(stgPrimOp(e)),xs);
      case STGCON:
         xs = dupList(stgConArgs(e));
         mapOver(cloneStg,xs);
         return mkStgCon(cloneStg(stgConCon(e)),xs);
      case DEEFALT:
         newv = cloneStgVar(stgDefaultVar(e));
         pushClone ( stgDefaultVar(e), newv );
         t = mkStgDefault(newv,cloneStg(stgDefaultBody(e)));
         dropClone();
         return t;
      case CASEALT:
         newvs = cloneStg_addVars ( stgCaseAltVars(e) );
         t = mkStgCaseAlt(stgCaseAltCon(e),newvs,
                          cloneStg(stgCaseAltBody(e)));
         cloneStg_dropVars ( stgCaseAltVars(e) );
         return t;
      case PRIMALT:
         newvs = cloneStg_addVars ( stgPrimAltVars(e) );
         t = mkStgPrimAlt(newvs, cloneStg(stgPrimAltBody(e)));
         cloneStg_dropVars ( stgPrimAltVars(e) );
         return t;
      case INTCELL:
      case STRCELL:
      case PTRCELL:
      case BIGCELL:
      case CHARCELL:
      case FLOATCELL:
      case NAME:
      case TUPLE:
         return e;
      default:
         fprintf(stderr, "cloneStg: unknown stuff %d\n",whatIsStg(e));
         assert(0);
   }
}


/* Main entry point.  Checks against re-entrant use. */
StgExpr cloneStgTop ( StgExpr e )
{
   StgExpr res;
   if (spClone != SP_NOT_IN_USE) 
      internal("cloneStgTop");
   spClone = -1;
   res = cloneStg ( e );
   assert(spClone == -1);
   spClone = SP_NOT_IN_USE;
   return res;
}



/* --------------------------------------------------------------------------
 * Sets of StgVars, used by the strongly-connected-components machinery.  
 * Represented as an array of variables.  The vars
 * must be in strictly nondecreasing order.  Each value may appear
 * more than once, so as to make deletion relatively cheap.

 * After a garbage collection happens, the values may have changed,
 * so the array will need to be sorted.

 * Using a binary search, membership costs O(log N).  Union and
 * intersection cost O(N + M).  Deletion of a single element costs
 * O(N) in the worst case, although if it happens infrequently
 * compared to the other ops, it should asymptotically approach O(1).
 * ------------------------------------------------------------------------*/

#define M_VAR_SETS 4000
#define MIN_VAR_SET_SIZE 4
#define M_UNION_TMP 20000

typedef
   struct {
      Int   nextfree;
      Bool  inUse;
      Int   size;
      Int   used;
      Cell* vs;
   }
   StgVarSetRec;

typedef Int StgVarSet;

StgVarSetRec varSet[M_VAR_SETS];
Int varSet_nfree;
Int varSet_nextfree;
Cell union_tmp[M_UNION_TMP];

#if 0 /* unused since unnecessary */
/* Shellsort set elems to restore representation invariants */
static Int shellCells_incs[10] 
   = { 1, 4, 13, 40, 121, 364, 1093, 3280, 9841, 29524 };
static void shellCells ( Cell* a, Int lo, Int hi )
{
   Int i, j, h, N, hp;
   Cell v;

   N = hi - lo + 1; if (N < 2) return;
   hp = 0; 
   while (hp < 10 && shellCells_incs[hp] < N) hp++; hp--;

   for (; hp >= 0; hp--) {
      h = shellCells_incs[hp];
      i = lo + h;
      while (1) {
         if (i > hi) break;
         v = a[i];
         j = i;
         while (a[j-h] > v) {
            a[j] = a[j-h]; j = j - h;
            if (j <= (lo + h - 1)) break;
         }
         a[j] = v; i++;
      }
   }
}
#endif

/* check that representation invariant still holds */
static void checkCells ( Cell* a, Int lo, Int hi )
{
   Int i;
   for (i = lo; i < hi; i++)
      if (a[i] > a[i+1])
         internal("checkCells");
}


/* Mark set contents for GC */
void markStgVarSets ( void )
{
   Int i, j;
   for (i = 0; i < M_VAR_SETS; i++)
      if (varSet[i].inUse)
         for (j = 0; j < varSet[i].used; j++)
            mark(varSet[i].vs[j]);
}


/* Check representation invariants after GC */
void checkStgVarSets ( void )
{
   Int i;
   for (i = 0; i < M_VAR_SETS; i++)
      if (varSet[i].inUse)
         checkCells ( varSet[i].vs, 0, varSet[i].used-1 );
}


/* Allocate a set of a given size */
StgVarSet allocStgVarSet ( Int size )
{
   Int i, j;
   if (varSet_nextfree == -1)
      internal("allocStgVarSet -- run out of var sets");
   i = varSet_nextfree;
   varSet_nextfree = varSet[i].nextfree;
   varSet[i].inUse = TRUE;
   j = MIN_VAR_SET_SIZE;
   while (j <= size) j *= 2;
   varSet[i].used = 0;
   varSet[i].size = j;
   varSet[i].vs = malloc(j * sizeof(StgVar) );
   if (!varSet[i].vs) 
      internal("allocStgVarSet -- can't malloc memory");
   varSet_nfree--;
   return i;
}


/* resize (upwards) */
void resizeStgVarSet ( StgVarSet s, Int size )
{
   Cell* tmp;
   Cell* tmp2;
   Int i;
   Int j = MIN_VAR_SET_SIZE;
   while (j <= size) j *= 2;
   if (j < varSet[s].size) return;
   tmp = varSet[s].vs;
   tmp2 = malloc( j * sizeof(StgVar) );
   if (!tmp2) internal("resizeStgVarSet -- can't malloc memory");
   varSet[s].vs = tmp2;
   for (i = 0; i < varSet[s].used; i++)
      tmp2[i] = tmp[i];
   free(tmp);
}


/* Deallocation ... */
void freeStgVarSet ( StgVarSet s )
{
   if (s < 0 || s >= M_VAR_SETS || 
       !varSet[s].inUse || !varSet[s].vs)
      internal("freeStgVarSet");
   free(varSet[s].vs);
   varSet[s].inUse = FALSE;
   varSet[s].vs = NULL;
   varSet[s].nextfree = varSet_nextfree;
   varSet_nextfree = s;
   varSet_nfree++;
}


/* Initialisation */
void initStgVarSets ( void )
{
   Int i;
   for (i = M_VAR_SETS-1; i >= 0; i--) {
      varSet[i].inUse = FALSE;
      varSet[i].vs = NULL;
      varSet[i].nextfree = i+1;
   }
   varSet[M_VAR_SETS-1].nextfree = -1;
   varSet_nextfree = 0;
   varSet_nfree = M_VAR_SETS;
}


/* Find a var using binary search */
Int findInStgVarSet ( StgVarSet s, StgVar v )
{
   Int lo, mid, hi;
   lo = 0;
   hi = varSet[s].used-1;
   while (1) {
      if (lo > hi) return -1;
      mid = (hi+lo)/2;
      if (varSet[s].vs[mid] == v) return mid;
      if (varSet[s].vs[mid] < v) lo = mid+1; else hi = mid-1;
   }
}


Bool elemStgVarSet ( StgVarSet s, StgVar v )
{
   return findInStgVarSet(s,v) != -1;
}

void ppSet ( StgVarSet s )
{
   Int i;
   fprintf(stderr, "{ ");
   for (i = 0; i < varSet[s].used; i++)
      fprintf(stderr, "%d ", varSet[s].vs[i] );
   fprintf(stderr, "}\n" );
}


void deleteFromStgVarSet ( StgVarSet s, StgVar v )
{
   Int i, j;
   i = findInStgVarSet(s,v);
   if (i == -1) return;
   j = varSet[s].used-1;
   for (; i < j; i++) varSet[s].vs[i] = varSet[s].vs[i+1];
   varSet[s].used--;
}


void singletonStgVarSet ( StgVarSet s, StgVar v )
{
   varSet[s].used  = 1;
   varSet[s].vs[0] = v;
}


void emptyStgVarSet ( StgVarSet s )
{
   varSet[s].used = 0;
}


void copyStgVarSets ( StgVarSet dst, StgVarSet src )
{
   Int i;
   varSet[dst].used = varSet[src].used;
   for (i = 0; i < varSet[dst].used; i++)
      varSet[dst].vs[i] = varSet[src].vs[i];
}


Int sizeofVarSet ( StgVarSet s )
{
   return varSet[s].used;
}


void unionStgVarSets ( StgVarSet dst, StgVarSet src )
{
   StgVar v1;
   Int pd, ps, i, res_used, tmp_used, dst_used, src_used;
   StgVar* dst_vs;
   StgVar* src_vs;
   StgVar* tmp_vs;

   dst_vs = varSet[dst].vs;

   /* fast track a common (~ 50%) case */
   if (varSet[src].used == 1) {
      v1 = varSet[src].vs[0];
      pd = findInStgVarSet(dst,v1);
      if (pd != -1) return;
      if (varSet[dst].used < varSet[dst].size) {
         i = varSet[dst].used;
         while (i > 0 && dst_vs[i-1] > v1) {
            dst_vs[i] = dst_vs[i-1];
            i--;
         }
         dst_vs[i] = v1;
         varSet[dst].used++;
         return;
      }
   }

   res_used = varSet[dst].used + varSet[src].used;
   if (res_used > M_UNION_TMP) 
      internal("unionStgVarSets -- M_UNION_TMP too small");

   resizeStgVarSet(dst,res_used);
   dst_vs = varSet[dst].vs;
   src_vs = varSet[src].vs;
   tmp_vs = union_tmp;
   tmp_used = 0;
   dst_used = varSet[dst].used;
   src_used = varSet[src].used;

   /* merge the two sets into tmp */
   pd = ps = 0;
   while (pd < dst_used || ps < src_used) {
      if (pd == dst_used)
         tmp_vs[tmp_used++] = src_vs[ps++];
      else
      if (ps == src_used)
         tmp_vs[tmp_used++] = dst_vs[pd++];
      else {
         StgVar vald = dst_vs[pd];
         StgVar vals = src_vs[ps];
         if (vald < vals)
            tmp_vs[tmp_used++] = vald, pd++;
         else
         if (vald > vals)
            tmp_vs[tmp_used++] = vals, ps++;
         else
            tmp_vs[tmp_used++] = vals, ps++, pd++;
      }
   }

   /* copy setTmp back to dst */
   varSet[dst].used = tmp_used;
   for (i = 0; i < tmp_used; i++) {
      dst_vs[i] = tmp_vs[i];
   }
}



/* --------------------------------------------------------------------------
 * Strongly-connected-components machinery for STG let bindings.
 * Arranges let bindings in minimal mutually recursive groups, and
 * then throws away any groups not referred to in the body of the let.
 *
 * How it works: does a bottom-up sweep of the tree.  Each call returns
 * the set of variables free in the tree.  All nodes except LETREC are
 * boring.  
 * 
 * When 'let v1=e1 .. vn=en in e' is encountered:
 * -- recursively make a call on e.  This returns fvs(e) and scc-ifies
 *    inside e as well.
 * -- do recursive calls for e1 .. en too, giving fvs(e1) ... fvs(en).
 *
 * Then, using fvs(e1) ... fvs(en), the dependancy graph for v1 ... vn
 * can be cheaply computed.  Using that, compute the strong components
 * and rearrange the let binding accordingly.
 * Finally, for each of the strong components, we can use fvs(en) to 
 * cheaply determine if the component is used in the body of the let,
 * and if not, it can be omitted.
 *
 * oaScc destructively modifies the tree -- when it gets to a let --
 * we need to pass the address of the expression to scc, not the
 * (more usual) heap index of it.
 *
 * The main requirement of this algorithm is an efficient implementation
 * of sets of variables.  Because there is no name shadowing in these
 * trees, either mentioned-sets or free-sets would be ok, although 
 * free sets are presumably smaller.
 * ------------------------------------------------------------------------*/


#define  SCC             stgScc          /* make scc algorithm for StgVars */
#define  LOWLINK         stgLowlink
#define  DEPENDS(t)      thd3(t)
#define  SETDEPENDS(c,v) thd3(c)=v
#include "scc.c"
#undef   SETDEPENDS
#undef   DEPENDS
#undef   LOWLINK
#undef   SCC


StgVarSet oaScc ( StgExpr* e_orig )
{
   Bool grpUsed;
   StgExpr e;
   StgVarSet e_fvs, s1, s2;
   List bs, bs2, bs3, bsFinal, augs, augsL;

   bs=bs2=bs3=bsFinal=augs=augsL=e_fvs=s1=s2=e=NIL;
   grpUsed=FALSE;

   e = *e_orig;

   //fprintf(stderr,"\n==================\n");
   //ppStgExpr(*e_orig);
   //fprintf(stderr,"\n\n");fflush(stderr);fflush(stdout);


   switch(whatIsStg(e)) {
      case LETREC:
         /* first, recurse into the let body */
         e_fvs = oaScc(&stgLetBody(*e_orig));

         /* Make bs :: [StgVar] and e :: Stgexpr. */
         bs = stgLetBinds(e);
         e  = stgLetBody(e);

         /* make augs :: [(StgVar,fvs(bindee),NIL)] */
         augs = NIL;
         for (; nonNull(bs); bs=tl(bs)) {
            StgVarSet fvs_bindee = oaScc(&stgVarBody(hd(bs)));
            augs = cons( triple(hd(bs),mkInt(fvs_bindee),NIL), augs );
         }

	 bs2=bs3=bsFinal=augsL=s1=s2=NIL;

         /* In each of the triples in aug, replace the NIL field with 
            a list of the let-bound vars appearing in the bindee.
            ie, construct the adjacency list for the graph. 
            giving 
            augs :: [(StgVar,fvs(bindee),[pointers-back-to-this-list-of-pairs])]
         */
         for (bs=augs;nonNull(bs);bs=tl(bs)) {
            augsL = NIL;
            for (bs2=augs;nonNull(bs2);bs2=tl(bs2))
               if (elemStgVarSet( intOf(snd3(hd(bs))), fst3(hd(bs2)) ))
                  augsL = cons(hd(bs2),augsL);
            thd3(hd(bs)) = augsL;
         }

	 bs2=bs3=bsFinal=augsL=s1=s2=NIL;

         /* Do the Biz.  
            augs becomes :: [[(StgVar,fvs(bindee),aux_info_field)]] */
         augs = stgScc(augs);

         /* work backwards through augs, reconstructing the expression,
            dumping any unused groups as you go.
	 */
         bsFinal = NIL;
         for (augs=rev(augs); nonNull(augs); augs=tl(augs)) {
            bs2 = NIL;
            for (augsL=hd(augs);nonNull(augsL); augsL=tl(augsL))
               bs2 = cons(fst3(hd(augsL)),bs2);
            grpUsed = FALSE;
            for (bs3=bs2;nonNull(bs3);bs3=tl(bs3))
               if (elemStgVarSet(e_fvs,hd(bs3))) { grpUsed=TRUE; break; }
            if (grpUsed) {
               //e = mkStgLet(bs2,e);
               bsFinal = dupOnto(bs2,bsFinal);
               for (augsL=hd(augs);nonNull(augsL);augsL=tl(augsL)) {
                  unionStgVarSets(e_fvs, intOf(snd3(hd(augsL))) );
                  freeStgVarSet(intOf(snd3(hd(augsL))));
               }
            } else {
               nLetrecGroupsDropped++;
               for (augsL=hd(augs);nonNull(augsL);augsL=tl(augsL)) {
                  freeStgVarSet(intOf(snd3(hd(augsL))));
               }
            }
         }
         //*e_orig = e;
         *e_orig = mkStgLet(bsFinal,e);
         return e_fvs;

      case LAMBDA:
         s1 = oaScc(&stgLambdaBody(e));
         for (bs=stgLambdaArgs(e);nonNull(bs);bs=tl(bs))
            deleteFromStgVarSet(s1,hd(bs));
         return s1;
      case CASE:
         s1 = oaScc(&stgCaseScrut(e));
         for (bs=stgCaseAlts(e);nonNull(bs);bs=tl(bs)) {
            s2 = oaScc(&hd(bs));
            unionStgVarSets(s1,s2);
            freeStgVarSet(s2);
         }
         return s1;
      case PRIMCASE:
         s1 = oaScc(&stgPrimCaseScrut(e));
         for (bs=stgPrimCaseAlts(e);nonNull(bs);bs=tl(bs)) {
            s2 = oaScc(&hd(bs));
            unionStgVarSets(s1,s2);
            freeStgVarSet(s2);
         }
         return s1;
      case STGAPP:
         s1 = oaScc(&stgAppFun(e));
         for (bs=stgAppArgs(e);nonNull(bs);bs=tl(bs)) {
            s2 = oaScc(&hd(bs));
            unionStgVarSets(s1,s2);
            freeStgVarSet(s2);
         }
         return s1;
      case STGPRIM:
         s1 = oaScc(&stgPrimOp(e));
         for (bs=stgPrimArgs(e);nonNull(bs);bs=tl(bs)) {
            s2 = oaScc(&hd(bs));
            unionStgVarSets(s1,s2);
            freeStgVarSet(s2);
         }
         return s1;
      case STGCON:
         s1 = allocStgVarSet(0);
         for (bs=stgPrimArgs(e);nonNull(bs);bs=tl(bs)) {
            s2 = oaScc(&hd(bs));
            unionStgVarSets(s1,s2);
            freeStgVarSet(s2);
         }
         return s1;
      case CASEALT:
         s1 = oaScc(&stgCaseAltBody(e));
         for (bs=stgCaseAltVars(e);nonNull(bs);bs=tl(bs))
            deleteFromStgVarSet(s1,hd(bs));
         return s1;
      case DEEFALT:
         s1 = oaScc(&stgDefaultBody(e));
         deleteFromStgVarSet(s1,stgDefaultVar(e));
         return s1;
      case PRIMALT:
         s1 = oaScc(&stgPrimAltBody(e));
         for (bs=stgPrimAltVars(e);nonNull(bs);bs=tl(bs))
            deleteFromStgVarSet(s1,hd(bs));
         return s1;
      case STGVAR:
         s1 = allocStgVarSet(1);
         singletonStgVarSet(s1,e);
         return s1;
      case NAME:
      case INTCELL:
      case STRCELL:
      case PTRCELL:
      case BIGCELL:
      case CHARCELL:
      case FLOATCELL:
         return allocStgVarSet(0);
         break;
      default:
         fprintf(stderr, "oaScc: unknown stuff %d\n",whatIsStg(e));
         assert(0);
   }
}



/* --------------------------------------------------------------------------
 * Occurrence analyser.  Marks each let-bound var with the number of times
 * it is used, or some number >= OCC_IN_LAMBDA if it is used inside a lambda.
 *
 * Firstly, oaPre traverses the tree, attaching a mutable INT cell to each
 * let bound var, and NIL-ing the counts on all other vars.
 *
 * Then oaCount traveses the tree.  Because variables are represented by
 * pointers in the heap, we can just increment the count field of each
 * variable we see.  However, to deal with lambdas, the Hugs stack holds
 * all let-bound variables currently in scope, and the uppermost portion
 * of the stack, stack(spBase .. sp) inclusive, denotes the variables
 * introduced into scope since the nearest enclosing lambda.  When a 
 * let-bound var is seen, we search stack(spBase .. sp).  If it appears
 * there, no lambda exists between the binding site and this usage of the
 * var, so we can safely increment its use.  Otherwise, we must set it to
 * OCC_IN_LAMBDA.
 *
 * When passing a lambda, spBase is set to sp+1, so as to effectively
 * empty the set of vars-bound-since-the-latest-lambda.
 * 
 * Because oaPre pre-annotates the tree with mutable INT cells, oaCount
 * doesn't allocate any heap at all.
 * ------------------------------------------------------------------------*/

static int spBase;


#define OCC_IN_LAMBDA 50  /* any number > 1 will do */
#define nullCount(vv) stgVarInfo(vv)=NIL
#define nullCounts(vvs) { List tt=(vvs);for(;nonNull(tt);tt=tl(tt)) nullCount(hd(tt));}



void oaPre ( StgExpr e )
{
   List bs;
   switch(whatIsStg(e)) {
      case LETREC:
         for (bs = stgLetBinds(e);nonNull(bs);bs=tl(bs))
            stgVarInfo(hd(bs)) = mkInt(0);
         for (bs = stgLetBinds(e);nonNull(bs);bs=tl(bs))
            oaPre(stgVarBody(hd(bs)));
         oaPre(stgLetBody(e));
         break;
      case LAMBDA:
         nullCounts(stgLambdaArgs(e));
         oaPre(stgLambdaBody(e));
         break;
      case CASE:
         oaPre(stgCaseScrut(e));
         mapProc(oaPre,stgCaseAlts(e));
         break;
      case PRIMCASE:
         oaPre(stgPrimCaseScrut(e));
         mapProc(oaPre,stgPrimCaseAlts(e));
         break;
      case STGAPP:
         oaPre(stgAppFun(e));
         mapProc(oaPre,stgAppArgs(e));
         break;
      case STGPRIM:
         mapProc(oaPre,stgPrimArgs(e));
         break;
      case STGCON:
         mapProc(oaPre,stgConArgs(e));
         break;
      case CASEALT:
         nullCounts(stgCaseAltVars(e));
         oaPre(stgCaseAltBody(e));
         break;
      case DEEFALT:
         nullCount(stgDefaultVar(e));
         oaPre(stgDefaultBody(e));
         break;
      case PRIMALT:
         nullCounts(stgPrimAltVars(e));
         oaPre(stgPrimAltBody(e));
         break;
      case STGVAR:
      case NAME:
      case INTCELL:
      case STRCELL:
      case PTRCELL:
      case BIGCELL:
      case CHARCELL:
      case FLOATCELL:
         break;
      default:
         fprintf(stderr, "oaPre: unknown stuff %d\n",whatIsStg(e));
         assert(0);
   }
}


/* In oaCount:
   -- the stack is always the set of let-bound vars currently
      in scope.  viz, stack(0 .. sp) inclusive.
   -- spBase is always >= 0 and <= sp.  
      stack(spBase .. sp) inclusive will be the let vars bound
      since the nearest enclosing lambda.  When entering a lambda,
      we set spBase=sp+1 so as record this fact, and restore spBase
      afterwards.
*/
void oaCount ( StgExpr e )
{
   List bs;
   Int  spBase_saved;

   switch(whatIsStg(e)) {
      case LETREC:
         for (bs = stgLetBinds(e);nonNull(bs);bs=tl(bs))
            push(hd(bs));
         for (bs = stgLetBinds(e);nonNull(bs);bs=tl(bs))
            oaCount(stgVarBody(hd(bs)));
         oaCount(stgLetBody(e));
         for (bs = stgLetBinds(e);nonNull(bs);bs=tl(bs))
            drop();
         break;
      case LAMBDA:
         spBase_saved = spBase;
         spBase = sp+1;
         oaCount(stgLambdaBody(e));
         spBase = spBase_saved;
         break;
      case CASE:
         oaCount(stgCaseScrut(e));
         mapProc(oaCount,stgCaseAlts(e));
         break;
      case PRIMCASE:
         oaCount(stgPrimCaseScrut(e));
         mapProc(oaCount,stgPrimCaseAlts(e));
         break;
      case STGAPP:
         oaCount(stgAppFun(e));
         mapProc(oaCount,stgAppArgs(e));
         break;
      case STGPRIM:
         mapProc(oaCount,stgPrimArgs(e));
         break;
      case STGCON:
         mapProc(oaCount,stgConArgs(e));
         break;
      case CASEALT:
         nullCounts(stgCaseAltVars(e));
         oaCount(stgCaseAltBody(e));
         break;
      case DEEFALT:
         nullCount(stgDefaultVar(e));
         oaCount(stgDefaultBody(e));
         break;
      case PRIMALT:
         nullCounts(stgPrimAltVars(e));
         oaCount(stgPrimAltBody(e));
         break;
      case STGVAR:
         if (isInt(stgVarInfo(e))) {
            Int i, j;
            j = -1;
            for (i = sp; i >= spBase; i--)
               if (stack(i) == e) { j = i; break; };
            if (j == -1)
               stgVarInfo(e) = mkInt(OCC_IN_LAMBDA); else
               stgVarInfo(e) = mkInt(1 + intOf(stgVarInfo(e)));
         }
         break;
      case NAME:
      case INTCELL:
      case STRCELL:
      case PTRCELL:
      case BIGCELL:
      case CHARCELL:
      case FLOATCELL:
         break;
      default:
         fprintf(stderr, "oaCount: unknown stuff %d\n",whatIsStg(e));
         assert(0);
   }
}

void stgTopSanity ( char*, StgVar );

/* Top level entry point for the occurrence analyser. */
void oaTop ( StgVar v )
{
   assert (varSet_nfree == M_VAR_SETS);
   freeStgVarSet(oaScc(&stgVarBody(v)));
   assert (varSet_nfree == M_VAR_SETS);
   oaPre(stgVarBody(v));
   clearStack(); spBase = 0;
   oaCount(stgVarBody(v));
   assert(stackEmpty());
   stgTopSanity("oaTop",stgVarBody(v));
}


/* --------------------------------------------------------------------------
 * Transformation machinery proper
 * ------------------------------------------------------------------------*/

#define streq(aa,bb) (strcmp((aa),(bb))==0)
/* Return TRUE if the non-default alts in the given list are exhaustive.
   If in doubt, return FALSE.
*/
Bool stgAltsExhaustive ( List alts )
{
   Int   nDefnCons;
   Name  con;
   Tycon t;
   List  cs;
   char* s;
   List  alts0 = alts;
   while (nonNull(alts) && isDefaultAlt(hd(alts))) alts=tl(alts);
   if (isNull(alts)) {
      return FALSE;
   } else {
      con = stgCaseAltCon(hd(alts));
      /* special case: dictionary constructor */
      if (strncmp("Make.",textToStr(name(con).text),5)==0)
         return TRUE;
      /* special case: constructor boxing an unboxed value. */
      if (isBoxingCon(con))
         return TRUE;
      /* some other special cases which are not boxingCons */
      s = textToStr(name(con).text);
      if (streq(s,"Integer#")
          || streq(s,"Ref#")
          || streq(s,"PrimMutableArray#")
          || streq(s,"PrimMutableByteArray#")
          || streq(s,"PrimByteArray#")
          || streq(s,"PrimArray#")
         )
         return TRUE;
      if (strcmp("Ref#",textToStr(name(con).text))==0)
         return TRUE;
      /* special case: Tuples */
      if (isTuple(con) || (isName(con) && con==nameUnit))
         return TRUE;
      if (isNull(name(con).parent)) internal("stgAltsExhaustive(1)");
      t = name(con).parent;
      cs = tycon(t).defn;
      if (tycon(t).what != DATATYPE) internal("stgAltsExhaustive(2)");
      nDefnCons = length(cs);
      for (; nonNull(alts0);alts0=tl(alts0)) {
         if (isDefaultAlt(hd(alts0))) continue;
         nDefnCons--;
      }
   }
   return nDefnCons == 0;
}
#undef streq


/* If in doubt, return FALSE. 
*/
Bool isManifestCon ( StgExpr e )
{
   StgExpr altB;
   switch (whatIsStg(e)) {
      case STGCON: return TRUE;
      case LETREC: return isManifestCon(stgLetBody(e));
      case CASE:   if (length(stgCaseAlts(e))==1) {                      
                      if (isDefaultAlt(hd(stgCaseAlts(e))))
                         altB = stgDefaultBody(hd(stgCaseAlts(e))); else
                         altB = stgCaseAltBody(hd(stgCaseAlts(e)));
                         return isManifestCon(altB);
                   } else {
                      return FALSE;
                   }
      default:     return FALSE;
   }
}


/* Like isManifestCon, but doesn't give up at non-singular cases */
Bool constructsCon ( StgExpr e )
{
   List    as;
   switch (whatIsStg(e)) {
      case STGCON:   return TRUE;
      case LETREC:   return constructsCon(stgLetBody(e));
      case CASE:     for (as = stgCaseAlts(e); nonNull(as); as=tl(as))
                        if (!constructsCon(hd(as))) return FALSE;
                     return TRUE;
      case PRIMCASE: for (as = stgPrimCaseAlts(e); nonNull(as); as=tl(as))
                        if (!constructsCon(hd(as))) return FALSE;
                     return TRUE;
      case CASEALT:  return constructsCon(stgCaseAltBody(e));
      case DEEFALT:  return constructsCon(stgDefaultBody(e));
      case PRIMALT:  return constructsCon(stgPrimAltBody(e));
      default:       return FALSE;
   }
}


/* Inline v in the special case where expr is
   case v of C a1 ... an -> E
   and v's bindee returns a product constructed with C.
   and v does not appear in E
   and v does not appear in letDefs (ie, this expr isn't
       part of the definition of v.
*/
void tryLoopbreakerHack ( List letDefs, StgExpr expr )
{
   List       alts;
   StgExpr    scrut, ee, v_bindee;
   StgCaseAlt alt;
  
   assert (whatIsStg(expr)==CASE);
   alts      = stgCaseAlts(expr);
   scrut     = stgCaseScrut(expr);
   if (whatIsStg(scrut) != STGVAR || isNull(stgVarBody(scrut))) return;
   if (length(alts) != 1 || isDefaultAlt(hd(alts))) return;
   if (!stgAltsExhaustive(alts)) return;
   alt       = hd(alts);
   ee        = stgCaseAltBody(alt);
   if (nonNull(cellIsMember(scrut,letDefs))) return;

   v_bindee  = stgVarBody(scrut);
   if (!isManifestCon(v_bindee)) return;

   stgCaseScrut(expr) = cloneStgTop(v_bindee);
   nLoopBreakersInlined++;
}


/* Traverse a tree.  Replace let-bound vars marked as used-once
   by their definitions.  Replace references to top-level
   values marked inlineMe with their bodies.  Carry around a list
   of let-bound variables whose definitions we are currently in
   so as to know not to inline let-bound vars in their own
   definitions.
*/
StgExpr copyIn ( List letDefs, InlineCtx ctx, StgExpr e )
{
   List bs;

   switch(whatIsStg(e)) {
      // these are the only two interesting cases
      case STGVAR:
         assert(isPtr(stgVarInfo(e)) || isNull(stgVarInfo(e)) || 
                isInt(stgVarInfo(e)));
	 if (isInt(stgVarInfo(e)) && intOf(stgVarInfo(e))==1) {
            nLetvarsInlined++;
            return cloneStgTop(stgVarBody(e)); 
         } else
            return e;
      case NAME:
         // if we're not inlining top vars on this round, do nothing
         if (!copyInTopvar) return e;
         // if it doesn't want to be inlined, do nothing
         if (!name(e).inlineMe) return e;
         // we decline to inline dictionary builders inside other builders
         if (inDBuilder && name(e).isDBuilder) {
	   //fprintf(stderr, "decline to inline dbuilder %s\n", textToStr(name(e).text));
            return e;
         }
         // in fact, only inline dict builders into a case scrutinee
         if (name(e).isDBuilder && ctx != CTX_SCRUT)
            return e;

#if DEBUG_OPTIMISE
assert( stgSize(stgVarBody(name(e).stgVar)) == name(e).stgSize );
#endif

         // only inline large dict builders if it returns a manifest con
         if (name(e).isDBuilder &&
             name(e).stgSize > 180 && 
             !isManifestCon(stgVarBody(name(e).stgVar)))
            return e;
#if 0
         // if it's huge, don't inline into a boring place
         if (ctx != CTX_SCRUT &&
             name(e).stgSize > 270)
            return e;
#endif

         nTopvarsInlined++;
         return cloneStgTop(stgVarBody(name(e).stgVar));

      // the rest are a boring recursive traversal of the tree      
      case LETREC:
         stgLetBody(e) = copyIn(letDefs,CTX_OTHER,stgLetBody(e));
         letDefs = dupOnto(stgLetBinds(e),letDefs);
         for (bs=stgLetBinds(e);nonNull(bs);bs=tl(bs))
            stgVarBody(hd(bs)) = copyIn(letDefs,CTX_OTHER,stgVarBody(hd(bs)));
         break;
      case LAMBDA:
         stgLambdaBody(e) = copyIn(letDefs,CTX_OTHER,stgLambdaBody(e));
         break;
      case CASE:
         stgCaseScrut(e) = copyIn(letDefs,CTX_SCRUT,stgCaseScrut(e));
         map2Over(copyIn,letDefs,CTX_OTHER,stgCaseAlts(e));
         if (copyInTopvar) tryLoopbreakerHack(letDefs,e);
         break;
      case PRIMCASE:
         stgPrimCaseScrut(e) = copyIn(letDefs,CTX_OTHER,stgPrimCaseScrut(e));
         map2Over(copyIn,letDefs,CTX_OTHER,stgPrimCaseAlts(e));
         break;
      case STGAPP:
         stgAppFun(e) = copyIn(letDefs,CTX_OTHER,stgAppFun(e));
         break;
      case CASEALT:
         stgCaseAltBody(e) = copyIn(letDefs,CTX_OTHER,stgCaseAltBody(e));
         break;
      case DEEFALT:
         stgDefaultBody(e) = copyIn(letDefs,CTX_OTHER,stgDefaultBody(e));
         break;
      case PRIMALT:
         stgPrimAltBody(e) = copyIn(letDefs,CTX_OTHER,stgPrimAltBody(e));
         break;
      case STGPRIM:
      case STGCON:
      case INTCELL:
      case STRCELL:
      case PTRCELL:
      case CHARCELL:
      case FLOATCELL:
         break;
      default:
         fprintf(stderr, "copyIn: unknown stuff %d\n",whatIsStg(e));
         ppStgExpr(e);
         printf("\n");
         print(e,1000);
         printf("\n");
         assert(0);
   }
   return e;
}



/* case (C a1 ... an) of
      B ...       -> ...
      C v1 ... vn -> e
      D ...       -> ...
   ==>
   e with v1/a1 ... vn/an
*/
StgExpr doCaseOfCon ( StgExpr expr, Bool* done )
{
   StgExpr    scrut, e;
   StgVar     apC;
   StgCaseAlt theAlt;
   List       alts, altvs, as, sub;

   *done  = FALSE;
   alts   = stgCaseAlts(expr);
   scrut  = stgCaseScrut(expr);

   apC    = stgConCon(scrut);

   theAlt = NIL;
   for (alts = stgCaseAlts(expr); nonNull(alts); alts=tl(alts))
      if (!isDefaultAlt(hd(alts)) && stgCaseAltCon(hd(alts)) == apC) {
         theAlt = hd(alts);
         break;
      }

   if (isNull(theAlt)) return expr;
   altvs  = stgCaseAltVars(theAlt);
   e      = stgCaseAltBody(theAlt);
   as     = stgConArgs(scrut);

   if (length(as)!=length(altvs)) return expr;

   sub = NIL;
   while (nonNull(altvs)) {
      sub   = cons(pair(hd(altvs),hd(as)),sub);
      as    = tl(as);
      altvs = tl(altvs);
   }
   nCaseOfCon++;
   *done = TRUE;
   return zubstExpr(sub,e);
}


/* case (let binds in e) of alts
   ===>
   let binds in case e of alts
*/
StgExpr doCaseOfLet ( StgExpr expr, Bool* done )
{
   StgExpr letexpr, e;
   List    binds, alts;

   letexpr = stgCaseScrut(expr);
   e       = stgLetBody(letexpr);
   binds   = stgLetBinds(letexpr);
   alts    = stgCaseAlts(expr);
   nCaseOfLet++;
   *done   = TRUE;
   return mkStgLet(binds,mkStgCase(e,alts));
}



/* case (case e of p1 -> e1 ... pn -> en) of
      q1 -> h1
      ...
      qk -> hk
   ===>
   case e of 
      p1 -> case e1 of q1 -> h1 ... qk -> hk
      ...
      pn -> case en of q1 -> h1 ... qk -> kl
*/
StgExpr doCaseOfCase ( StgExpr expr )
{
   StgExpr innercase, e, tmpcase, protocase;
   List ps_n_es, qs_n_hs, newAlts;
   StgCaseAlt newAlt, p_n_e;

   nCaseOfCase++;

   innercase = stgCaseScrut(expr);
   e = stgCaseScrut(innercase);
   ps_n_es = stgCaseAlts(innercase);
   qs_n_hs = stgCaseAlts(expr);

   /* protocase = case (hole-to-fill-in) of q1 -> h1 ... qk -> hk */
   protocase = mkStgCase( mkInt(0), qs_n_hs);

   newAlts = NIL;
   for (;nonNull(ps_n_es);ps_n_es = tl(ps_n_es)) {
      tmpcase = cloneStgTop(protocase);
      p_n_e = hd(ps_n_es);
      if (isDefaultAlt(p_n_e)) {
         stgCaseScrut(tmpcase) = stgDefaultBody(p_n_e);
         newAlt = mkStgDefault(stgDefaultVar(p_n_e), tmpcase);
      } else {
         stgCaseScrut(tmpcase) = stgCaseAltBody(p_n_e);
         newAlt = mkStgCaseAlt(stgCaseAltCon(p_n_e),stgCaseAltVars(p_n_e),tmpcase);
      }
      newAlts = cons(newAlt,newAlts);
   }
   newAlts = rev(newAlts);
   return
      mkStgCase(e, newAlts);
}



/* case (case# e of p1 -> e1 ... pn -> en) of
      q1 -> h1
      ...
      qk -> hk
   ===>
   case# e of 
      p1 -> case e1 of q1 -> h1 ... qk -> hk
      ...
      pn -> case en of q1 -> h1 ... qk -> kl
*/
StgExpr doCaseOfPrimCase ( StgExpr expr )
{
   StgExpr innercase, e, tmpcase, protocase;
   List ps_n_es, qs_n_hs, newAlts;
   StgCaseAlt newAlt, p_n_e;

   nCaseOfPrimCase++;

   innercase = stgCaseScrut(expr);
   e = stgPrimCaseScrut(innercase);
   ps_n_es = stgPrimCaseAlts(innercase);
   qs_n_hs = stgCaseAlts(expr);

   /* protocase = case (hole-to-fill-in) of q1 -> h1 ... qk -> hk */
   protocase = mkStgCase( mkInt(0), qs_n_hs);

   newAlts = NIL;
   for (;nonNull(ps_n_es);ps_n_es = tl(ps_n_es)) {
      tmpcase = cloneStgTop(protocase);
      p_n_e = hd(ps_n_es);
      stgPrimCaseScrut(tmpcase) = stgPrimAltBody(p_n_e);
      newAlt = mkStgPrimAlt(stgPrimAltVars(p_n_e),tmpcase);
      newAlts = cons(newAlt,newAlts);
   }
   newAlts = rev(newAlts);  
   return
      mkStgPrimCase(e, newAlts);
}


Bool isStgCaseWithSingleNonDefaultAlt ( StgExpr e )
{
   return
      whatIsStg(e)==CASE &&
      length(stgCaseAlts(e))==1 &&
      !isDefaultAlt(hd(stgCaseAlts(e)));
}


/* Do simplifications on an Stg tree.  Invariant is that the
   input and output trees should have no name shadowing.

   -- let { } in e
      ===>
      e

   -- dump individual let-bindings with usage counts of zero

   -- dump let-binding groups for which none of the bound vars
      occur in the let body

   -- (\v1 ... vn -> e) a1 ... am
      ===>
      -- the usual beta reduction.  There are no constraints on n and m, so
         the result can be a lambda term (if n > m), or an application of e 
         to the unused args (if n < m).


  Scheme is: bottom-up traversal of the tree.  First simplify child
  trees.  Then try to do local transformations.  If a local transformation 
  succeeds, jump to the local-transformation code for whatever node
  is produced -- so as to try and maximise the amount of work which
  happens on each call to simplify.
*/
StgExpr simplify ( List caseEnv, StgExpr e )
{
   List bs, bs2;
   Bool done;
   Int  n;

   restart:
   switch(whatIsStg(e)) {
      case STGVAR:
         return e;
      case NAME:
         return e;

      case LETREC:

         /* first dump dead binds, so as not to waste effort simplifying them */
         bs2=NIL;
         for (bs=stgLetBinds(e);nonNull(bs);bs=tl(bs))
            if (!isInt(stgVarInfo(hd(bs))) ||
                intOf(stgVarInfo(hd(bs))) > 0) {
               bs2=cons(hd(bs),bs2);
            } else {
               nLetBindsDropped++;
            }
         if (isNull(bs2)) { e = stgLetBody(e); goto restart; };
         stgLetBinds(e) = rev(bs2);

         for (bs=stgLetBinds(e);nonNull(bs);bs=tl(bs))
            stgVarBody(hd(bs)) = simplify(caseEnv,stgVarBody(hd(bs)));
         stgLetBody(e) = simplify(caseEnv,stgLetBody(e));

         /* Merge let ... in let ... in e.  Grouping lets together
            sometimes reduces the number of iterations needed.
            oaScc should do this anyway, but this just to make sure.
         */
         while (whatIsStg(stgLetBody(e))==LETREC) {
            stgLetBinds(e) = dupOnto(stgLetBinds(stgLetBody(e)),stgLetBinds(e));
            stgLetBody(e) = stgLetBody(stgLetBody(e));
         }

         let_local:
         /* let binds in case v-not-in-binds of singleAlt -> expr
            ===>
            case v-not-in-binds of singleAlt -> let binds in expr
	 */
         if (isStgCaseWithSingleNonDefaultAlt(stgLetBody(e)) &&
             whatIsStg(stgCaseScrut(stgLetBody(e)))==STGVAR &&
             isNull(cellIsMember(stgCaseScrut(stgLetBody(e)),stgLetBinds(e)))) {
            StgVar     v = stgCaseScrut(stgLetBody(e));
            StgCaseAlt a = hd(stgCaseAlts(stgLetBody(e)));
            nLetsFloatedIntoCase++;
            e = mkStgCase( 
                   v, 
                   singleton( 
                      mkStgCaseAlt(
                         stgCaseAltCon(a),
                         stgCaseAltVars(a), 
                         mkStgLet(stgLetBinds(e),stgCaseAltBody(a))
                      )
                   )
                );
            assert(whatIsStg(e)==CASE);
            goto case_local;
         }
          
         break;

      case LAMBDA:
         stgLambdaBody(e) = simplify(caseEnv,stgLambdaBody(e));

         lambda_local:
         while (whatIsStg(stgLambdaBody(e))==LAMBDA) {
            nLambdasMerged++;
            stgLambdaArgs(e) = appendOnto(stgLambdaArgs(e),
                                          stgLambdaArgs(stgLambdaBody(e)));
            stgLambdaBody(e) = stgLambdaBody(stgLambdaBody(e));
         }
         break;


      case CASE:
         stgCaseScrut(e) = simplify(caseEnv,stgCaseScrut(e));
         if (isStgCaseWithSingleNonDefaultAlt(e) &&
             (whatIsStg(stgCaseScrut(e))==STGVAR ||
              whatIsStg(stgCaseScrut(e))==NAME)) {
            List caseEnv2 = cons(
                               pair(stgCaseScrut(e),stgCaseAltVars(hd(stgCaseAlts(e)))),
                               caseEnv
                            );
            map1Over(simplify,caseEnv2,stgCaseAlts(e));
         } else {
            map1Over(simplify,caseEnv,stgCaseAlts(e));
         }

         case_local:
         /* zap redundant default alternatives */
         if (stgAltsExhaustive(stgCaseAlts(e))) {
            Bool droppedDef = FALSE;
            bs2 = NIL;
            for (bs = dupList(stgCaseAlts(e));nonNull(bs);bs=tl(bs))
               if (!isDefaultAlt(hd(bs))) {
                  bs2=cons(hd(bs),bs2); 
               } else {
                  droppedDef = TRUE;
               }
            bs2 = rev(bs2);
            stgCaseAlts(e) = bs2;
            if (droppedDef) nCaseDefaultsDropped++;
         }
        
         switch (whatIsStg(stgCaseScrut(e))) {
            case CASE:
               /* attempt case-of-case */
               n = length(stgCaseAlts(e));
               if (n==1 || 
                           (n <= 3 && 
                            (stgSize(e)-stgSize(stgCaseScrut(e))) < 100 &&
                            constructsCon(stgCaseScrut(e)))
                  ) {
                  e = doCaseOfCase(e);
                  assert(whatIsStg(e)==CASE);
                  goto case_local;
               }
               break;
            case PRIMCASE:
               /* attempt case-of-case# */
               n = length(stgCaseAlts(e));
               if (n==1 || 
                           (n <= 3 && 
                            (stgSize(e)-stgSize(stgCaseScrut(e))) < 100 &&
                            constructsCon(stgCaseScrut(e)))
                  ) {
                  e = doCaseOfPrimCase(e);
                  assert(whatIsStg(e)==PRIMCASE);
                  goto primcase_local;
               }
               break;
            case LETREC:
               /* attempt case-of-let */
               e = doCaseOfLet(e,&done);
               if (done) { assert(whatIsStg(e)==LETREC); goto let_local; };
               break;
            case STGCON:
	       /* attempt case-of-constructor */
               e = doCaseOfCon(e,&done);
               /* we don't know what the result is, so can't jump to local */
               break;
            case NAME:
            case STGVAR: {
               /* attempt to remove case on something already cased on */
               List outervs, innervs, sub;
               Cell lookupResult;
               if (!isStgCaseWithSingleNonDefaultAlt(e)) break;
               lookupResult = cellAssoc(stgCaseScrut(e),caseEnv);
               if (isNull(lookupResult)) break;
               outervs = snd(lookupResult);
               nCaseOfOuter++;
               sub = NIL;
               innervs = stgCaseAltVars(hd(stgCaseAlts(e)));
               for (; nonNull(outervs) && nonNull(innervs);
                      outervs=tl(outervs), innervs=tl(innervs))
                  sub = cons(pair(hd(innervs),hd(outervs)),sub);
               assert (isNull(outervs) && isNull(innervs));
               return zubstExpr(sub, stgCaseAltBody(hd(stgCaseAlts(e))));
	       }
            default:
               break;
         }
         break;
      case CASEALT:
         stgCaseAltBody(e) = simplify(caseEnv,stgCaseAltBody(e));
         break;
      case DEEFALT:
         stgDefaultBody(e) = simplify(caseEnv,stgDefaultBody(e));
         break;
      case PRIMALT:
         stgPrimAltBody(e) = simplify(caseEnv,stgPrimAltBody(e));
         break;
      case PRIMCASE:
         stgPrimCaseScrut(e) = simplify(caseEnv,stgPrimCaseScrut(e));
         map1Over(simplify,caseEnv,stgPrimCaseAlts(e));
         primcase_local:
         break;
      case STGAPP: {
         List    sub, formals;
         StgExpr subd_body;
         StgExpr fun;
         List    args;

         stgAppFun(e) = simplify(caseEnv,stgAppFun(e));
         map1Over(simplify,caseEnv,stgAppArgs(e));

         fun  = stgAppFun(e);
         args = stgAppArgs(e);

         switch (whatIsStg(fun)) {
            case STGAPP:
               nAppsMerged++;
               stgAppArgs(e) = appendOnto(stgAppArgs(fun),args);
               stgAppFun(e) = stgAppFun(fun);
               break;
            case LETREC:
               /* (let binds in f) args  ==> let binds in (f args) */
               nLetsFloatedOutOfFn++;
               e = mkStgLet(stgLetBinds(fun),mkStgApp(stgLetBody(fun),args));
               assert(whatIsStg(e)==LETREC);
               goto let_local;
               break;
            case CASE:
               if (length(stgCaseAlts(fun))==1 && 
                   !isDefaultAlt(hd(stgCaseAlts(fun)))) {
                  StgCaseAlt theAlt = hd(stgCaseAlts(fun));
                  /* (case e of alt -> f) args  ==> case e of alt -> f args */
                  e = mkStgCase(
                         stgCaseScrut(fun),
                         singleton(mkStgCaseAlt(stgCaseAltCon(theAlt),
                                                stgCaseAltVars(theAlt),
                                                 mkStgApp(stgCaseAltBody(theAlt),args))
                         )
                      );
                  nCasesFloatedOutOfFn++;
                  assert(whatIsStg(e)==CASE);
                  goto case_local;
	       }
               break;
            case LAMBDA: {
               sub      = NIL;
               formals  = stgLambdaArgs(fun);
               while (nonNull(formals) && nonNull(args)) {
                  sub     = cons(pair(hd(formals),hd(args)),sub);
                  formals = tl(formals);
                  args    = tl(args);
               }
               subd_body = zubstExpr(sub,stgLambdaBody(fun));

               nBetaReductions++;
               assert(isNull(formals) || isNull(args));
               if (isNull(formals) && isNull(args)) {
                  /* fn and args match exactly */
                  e = subd_body;
                  return e;
               }
               else
               if (isNull(formals) && nonNull(args)) {
                  /* more args than we could deal with.  Build a new Ap. */
                  e = mkStgApp(subd_body,args);
                  return e;
               }
               else
	       if (nonNull(formals) && isNull(args)) {
                  /* partial application.  We get a new Lambda */
                  e = mkStgLambda(formals,subd_body);
                  return e;
	       }
	       }
               break;
            default:
               break;
         }
         }
         break;
      case STGPRIM:
         break;
      case STGCON:
         break;
      case INTCELL:
      case STRCELL:
      case PTRCELL:
      case CHARCELL:
      case FLOATCELL:
         break;
      default:
         fprintf(stderr, "simplify: unknown stuff %d\n",whatIsStg(e));
         ppStgExpr(e);
         printf("\n");
         print(e,1000);
         printf("\n");
         assert(0);
   }
   return e;
}


/* Restore STG representation invariants broken by simplify.
   -- Let-bind any constructor applications which appear
      anywhere other than a let.
   -- Let-bind non-atomic case scrutinees (ToDo).
*/
StgExpr restoreStg ( StgExpr e )
{
   List bs;
   StgVar newv;

   if (isNull(e)) return e;

   switch(whatIsStg(e)) {
      case LETREC:
         for (bs=stgLetBinds(e); nonNull(bs); bs=tl(bs)) {
            if (whatIsStg(stgVarBody(hd(bs))) == STGCON) {
	      /* do nothing */
            } 
            else
            if (whatIsStg(stgVarBody(hd(bs))) == LAMBDA) {
               stgLambdaBody(stgVarBody(hd(bs))) 
                  = restoreStg(stgLambdaBody(stgVarBody(hd(bs))));
            }
            else {
               stgVarBody(hd(bs)) = restoreStg(stgVarBody(hd(bs)));
            }
         }      
         stgLetBody(e) = restoreStg(stgLetBody(e));
         break;
      case LAMBDA:
	 /* note that the check in LETREC above ensures we won't
            get here for legitimate (let-bound) lambdas. */
         stgLambdaBody(e) = restoreStg(stgLambdaBody(e));
         newv = mkStgVar(e,NIL);
         e = mkStgLet(singleton(newv),newv);
         break;
      case CASE:
         stgCaseScrut(e) = restoreStg(stgCaseScrut(e));
         mapOver(restoreStg,stgCaseAlts(e));
         if (!isAtomic(stgCaseScrut(e))) {
            newv = mkStgVar(stgCaseScrut(e),NIL);
            return mkStgLet(singleton(newv),mkStgCase(newv,stgCaseAlts(e)));
         }
         break;
      case PRIMCASE:
         stgPrimCaseScrut(e) = restoreStg(stgPrimCaseScrut(e));
         mapOver(restoreStg,stgPrimCaseAlts(e));
         break;
      case STGAPP:
         stgAppFun(e) = restoreStg(stgAppFun(e));
         mapOver(restoreStg,stgAppArgs(e)); /* probably incorrect */
         if (!isAtomic(stgAppFun(e))) {
            newv = mkStgVar(stgAppFun(e),NIL);
            e = mkStgLet(singleton(newv),mkStgApp(newv,stgAppArgs(e)));
         }
         break;
      case STGPRIM:
         mapOver(restoreStg,stgPrimArgs(e));
         break;
      case STGCON:
	 /* note that the check in LETREC above ensures we won't
            get here for legitimate constructor applications. */
         mapOver(restoreStg,stgConArgs(e));
         newv = mkStgVar(e,NIL);
         return mkStgLet(singleton(newv),newv);
         break;
      case CASEALT:
         stgCaseAltBody(e) = restoreStg(stgCaseAltBody(e));
         if (whatIsStg(stgCaseAltBody(e))==LAMBDA) {
            newv = mkStgVar(stgCaseAltBody(e),NIL);
            stgCaseAltBody(e) = mkStgLet(singleton(newv),newv);
         }
         break;
      case DEEFALT:
         stgDefaultBody(e) = restoreStg(stgDefaultBody(e));
         if (whatIsStg(stgDefaultBody(e))==LAMBDA) {
            newv = mkStgVar(stgDefaultBody(e),NIL);
            stgDefaultBody(e) = mkStgLet(singleton(newv),newv);
         }
         break;
      case PRIMALT:
         stgPrimAltBody(e) = restoreStg(stgPrimAltBody(e));
         break;
      case STGVAR:
      case NAME:
      case INTCELL:
      case STRCELL:
      case PTRCELL:
      case CHARCELL:
      case FLOATCELL:
         break;
      default:
         fprintf(stderr, "restoreStg: unknown stuff %d\n",whatIsStg(e));
         ppStgExpr(e);
         printf("\n");
         assert(0);
   }
   return e;
}


StgExpr restoreStgTop ( StgExpr e )
{
   if (whatIs(e)==LAMBDA)
      stgLambdaBody(e) = restoreStg(stgLambdaBody(e)); else
      e = restoreStg(e);
   return e;
}


void simplTopRefs ( StgExpr e )
{
   List bs;

   switch(whatIsStg(e)) {
     /* the only interesting case */
      case NAME:
         if (name(e).inlineMe && !name(e).simplified) {
            /* printf("\n((%d)) request for %s\n",rDepth, textToStr(name(e).text)); */
            name(e).simplified = TRUE;
            optimiseTopBind(name(e).stgVar);
            /* printf("((%d)) done    for %s\n",rDepth, textToStr(name(e).text)); */
         }
         break;
      case LETREC:
         simplTopRefs(stgLetBody(e));
         for (bs=stgLetBinds(e); nonNull(bs); bs=tl(bs))
            simplTopRefs(stgVarBody(hd(bs)));
         break;
      case LAMBDA:
         simplTopRefs(stgLambdaBody(e));
         break;
      case CASE:
         simplTopRefs(stgCaseScrut(e));
         mapProc(simplTopRefs,stgCaseAlts(e));
         break;
      case PRIMCASE:
         simplTopRefs(stgPrimCaseScrut(e));
         mapProc(simplTopRefs,stgPrimCaseAlts(e));
         break;
      case STGAPP:
         simplTopRefs(stgAppFun(e));
         mapProc(simplTopRefs,stgAppArgs(e));
         break;
      case STGCON:
         mapProc(simplTopRefs,stgConArgs(e));
         break;
      case STGPRIM:
         simplTopRefs(stgPrimOp(e));
         mapProc(simplTopRefs,stgPrimArgs(e));
         break;
      case CASEALT:
         simplTopRefs(stgCaseAltBody(e));
         break;
      case DEEFALT:
         simplTopRefs(stgDefaultBody(e));
         break;
      case PRIMALT:
         simplTopRefs(stgPrimAltBody(e));
         break;
      case INTCELL:
      case STRCELL:
      case PTRCELL:
      case BIGCELL:
      case CHARCELL:
      case FLOATCELL:
      case TUPLE:
      case STGVAR:
         break;
      default:
         fprintf(stderr, "simplTopRefs: unknown stuff %d\n",whatIsStg(e));
         ppStgExpr(e);
         printf("\n");
         print(e,1000);
         printf("\n");
         assert(0);
   }
}

char* maybeName ( StgVar v )
{
   Name n = nameFromStgVar(v);
   if (isNull(n)) return "(unknown)";
   return textToStr(name(n).text);
}


/* --------------------------------------------------------------------------
 * Sanity checking (weak :-(
 * ------------------------------------------------------------------------*/

Bool stgError;

int stgSanity_checkStack ( StgVar v )
{
   int i, j;
   j = 0;
   for (i = 0; i <= sp; i++)
      if (stack(i)==v) j++;
   return j;
}

void stgSanity_dropVar ( StgVar v )
{
   drop();
}

void stgSanity_pushVar ( StgVar v )
{
   if (stgSanity_checkStack(v) != 0) stgError = TRUE;
   push(v);
}


void stgSanity ( StgExpr e )
{
   List bs;

   switch(whatIsStg(e)) {
      case LETREC:
         mapProc(stgSanity_pushVar,stgLetBinds(e));
         stgSanity(stgLetBody(e));
         for (bs=stgLetBinds(e); nonNull(bs); bs=tl(bs))
             stgSanity(stgVarBody(hd(bs)));
         mapProc(stgSanity_dropVar,stgLetBinds(e));
         break;
      case LAMBDA:
         mapProc(stgSanity_pushVar,stgLambdaArgs(e));
         stgSanity(stgLambdaBody(e));
         mapProc(stgSanity_dropVar,stgLambdaArgs(e));
         break;
      case CASE:
         stgSanity(stgCaseScrut(e));
         mapProc(stgSanity,stgCaseAlts(e));
         break;
      case PRIMCASE:
         stgSanity(stgPrimCaseScrut(e));
         mapProc(stgSanity,stgPrimCaseAlts(e));
         break;
      case STGAPP:
         stgSanity(stgAppFun(e));
         mapProc(stgSanity,stgAppArgs(e));
         break;
      case STGCON:
         stgSanity(stgConCon(e));
         mapProc(stgSanity,stgConArgs(e));
         break;
      case STGPRIM:
         stgSanity(stgPrimOp(e));
         mapProc(stgSanity,stgPrimArgs(e));
         break;
      case CASEALT:
         mapProc(stgSanity_pushVar,stgCaseAltVars(e));
         stgSanity(stgCaseAltBody(e));
         mapProc(stgSanity_dropVar,stgCaseAltVars(e));
         break;
      case DEEFALT:
         stgSanity_pushVar(stgDefaultVar(e));
         stgSanity(stgDefaultBody(e));
         stgSanity_dropVar(stgDefaultVar(e));
         break;
      case PRIMALT:
         mapProc(stgSanity_pushVar,stgPrimAltVars(e));
         stgSanity(stgPrimAltBody(e));
         mapProc(stgSanity_dropVar,stgPrimAltVars(e));
         break;
      case STGVAR:
         if (stgSanity_checkStack(e) == 1) break;
         if (nonNull(nameFromStgVar(e))) return;
         break;
      case NAME:
      case INTCELL:
      case STRCELL:
      case PTRCELL:
      case CHARCELL:
      case FLOATCELL:
      case TUPLE:
         break;
      default:
         fprintf(stderr, "stgSanity: unknown stuff %d\n",whatIsStg(e));
         ppStgExpr(e);
         printf("\n");
         print(e,1000);
         printf("\n");
         assert(0);
   }
}


void stgTopSanity ( char* caller, StgExpr e )
{
return;
   clearStack();
   assert(sp == -1);
   stgError = FALSE;
   stgSanity(e);
   assert(sp == -1);
   if (stgError) {
      fprintf(stderr, "\n\nstgTopSanity (caller = %s):\n\n", caller );
      ppStgExpr ( e );
      printf( "\n\n" );
      assert(0);
   }
}


/* Check if e is in a form which the code generator can deal with.
 * stgexpr-ness is what we need to enforce.  The extended version,
 * expr, may only occur as the rhs of a let binding.
 *
 * stgexpr ::= case atom of alts
 *           | case# primop{atom*} of primalts
 *           | let v_i = expr_i in stgexpr
 *           | var{atom*}
 *
 * expr ::= stgexpr
 *        | \v_i -> stgexpr
 *        | con{atoms}
 *
 *  alt ::= con vars -> stgexpr      (primalt and default similarly)
 *
 * atom ::= var | int | char etc     (unboxed, that is)
 */
Bool isStgExpr     ( StgExpr e );
Bool isStgFullExpr ( StgExpr e );

Bool isStgExpr ( StgExpr e )
{
   List bs;
   switch (whatIs(e)) {
      case LAMBDA:
      case STGCON:
         return FALSE;
      case LETREC:
         for (bs=stgLetBinds(e); nonNull(bs); bs=tl(bs))
            if (!isStgFullExpr(stgVarBody(hd(bs))))
               return FALSE;
         return isStgExpr(stgLetBody(e));
      case CASE:
         for (bs=stgCaseAlts(e); nonNull(bs); bs=tl(bs))
            if (!isStgExpr(hd(bs))) return FALSE;
         return isAtomic(stgCaseScrut(e));
      case PRIMCASE:
         for (bs=stgPrimCaseAlts(e); nonNull(bs); bs=tl(bs))
            if (!isStgExpr(hd(bs))) return FALSE;
         if (isAtomic(stgPrimCaseScrut(e))) return TRUE;
         if (whatIs(stgPrimCaseScrut(e))==STGPRIM)
            return isStgExpr(stgPrimCaseScrut(e));
         return FALSE;
      case STGVAR:
      case NAME:
         return TRUE;
      case STGAPP:
         for (bs=stgAppArgs(e); nonNull(bs); bs=tl(bs))
            if (!isAtomic(hd(bs))) return FALSE;
         if (isStgVar(stgAppFun(e)) || isName(stgAppFun(e))) return TRUE;
         return FALSE;
      case STGPRIM:
         for (bs=stgPrimArgs(e); nonNull(bs); bs=tl(bs))
            if (!isAtomic(hd(bs))) return FALSE;
         if (isName(stgPrimOp(e))) return TRUE;
         return FALSE;
      case CASEALT:
         return isStgExpr(stgCaseAltBody(e));
      case DEEFALT:
         return isStgExpr(stgDefaultBody(e));
      case PRIMALT:
         return isStgExpr(stgPrimAltBody(e));
      default:
         return FALSE;
   }
}


Bool isStgFullExpr ( StgExpr e )
{
   List bs;
   switch (whatIs(e)) {
      case LAMBDA:
         return isStgExpr(stgLambdaBody(e));
      case STGCON:
         for (bs=stgConArgs(e); nonNull(bs); bs=tl(bs))
            if (!isAtomic(hd(bs))) return FALSE;
         if (isName(stgConCon(e)) || isTuple(stgConCon(e)))
            return TRUE;
         return FALSE;
      default:
         return isStgExpr(e);
   }
}


/* --------------------------------------------------------------------------
 * Top level calls
 * ------------------------------------------------------------------------*/

/* Set ddumpSimpl to TRUE if you want to see simplified code. */
static Bool ddumpSimpl = FALSE;

/* Leave this one alone ... */
static Bool noisy;


static void local optimiseTopBind( StgVar v )
{
   Bool ppPrel = FALSE;
   Int  n, m;
   Name naam;
   Int  oldSize, newSize;
   Bool me;

   /* printf( "[[%d]] looking at %s\n", rDepth, maybeName(v)); */
   assert(whatIsStg(v)==STGVAR);

   rDepth++;
   if (nonNull(stgVarBody(v))) simplTopRefs(stgVarBody(v));
   rDepth--;

   /* debugging ... */
   //me= 0&& 0==strcmp("tcUnify",maybeName(v));
   me= 0&& 0==strcmp("ttt",maybeName(v));

   nTotSizeIn += stgSize(stgVarBody(v));
   if (noisy) {
      printf( "%28s: in %4d    ", maybeName(v),stgSize(stgVarBody(v))); 
      fflush(stdout);
   }

   inDBuilder = FALSE;
   naam = nameFromStgVar(v);
   if (nonNull(naam) && name(naam).isDBuilder) inDBuilder = TRUE;

#if DEBUG_OPTIMISE
   if (nonNull(naam)) {
      assert(name(naam).stgSize == stgSize(stgVarBody(name(naam).stgVar)));
   }
#endif

   if (me) {
      fflush(stdout); fflush(stderr);
      fprintf ( stderr, "{{%d}}-----------------------------\n", -v );fflush(stderr);
      printStg ( stderr, v );
      fprintf(stderr, "\n" );
   }

   stgTopSanity ( "initial", stgVarBody(v));

   if (nonNull(stgVarBody(v))) {
      oldSize = -1;

      for (n = 0; n < 8; n++) { // originally 7
         if (noisy) printf("%4d", stgSize(stgVarBody(v)));
         copyInTopvar = TRUE;
         stgTopSanity ( "outer-1", stgVarBody(v));
         oaTop ( v );
         stgTopSanity ( "outer-2", stgVarBody(v));
         stgVarBody(v) = copyIn ( NIL, CTX_OTHER, stgVarBody(v) );
         stgTopSanity ( "outer-3", stgVarBody(v));
         stgVarBody(v) = simplify ( NIL, stgVarBody(v) );
         stgTopSanity ( "outer-4", stgVarBody(v));

         for (m = 0; m < 3; m++) { // oprignally 3
            if (noisy) printf("."); 
            fflush(stdout);
            copyInTopvar = FALSE;
            stgTopSanity ( "inner-1", stgVarBody(v));
            oaTop ( v );
            stgTopSanity ( "inner-2", stgVarBody(v));
            stgVarBody(v) = copyIn ( NIL, CTX_OTHER, stgVarBody(v) );
            stgTopSanity ( "inner-3", stgVarBody(v));
            stgVarBody(v) = simplify ( NIL, stgVarBody(v) );

            if (me && 0) {
               fprintf(stderr,"\n-%d- - - - - - - - - - - - - -\n", n+1);
               printStg ( stderr,v );
            }
            stgTopSanity ( "inner-post", stgVarBody(v));

         }

         if (me && 1) {
            fprintf(stderr,"\n-%d-=-=-=-=-=-=-=-=-=-=-=-=-=-\n", n+1);
            printStg ( stderr,v );
         }

         stgTopSanity ( "outer-post", stgVarBody(v));

         newSize = stgSize ( stgVarBody(v) );
         if (newSize == oldSize) break;
         oldSize = newSize;
      }
      n++; for (; n < 8; n++) for (m = 0; m <= 3+3; m++) if (noisy) printf ( " " );
      if (noisy) printf(" --> %4d\n", stgSize(stgVarBody(v)) );
      stgVarBody(v) = restoreStgTop ( stgVarBody(v) );

      if (nonNull(naam)) {
         assert(name(naam).stgVar == v);
         name(naam).stgSize = stgSize(stgVarBody(v));
      }

#if DEBUG_OPTIMISE
      /* debugging ... */
      if (!isStgFullExpr(stgVarBody(v))) {
         fprintf(stderr, "\n\nrestoreStg failed!\n\n" );
         printStg(stderr, v);
         fprintf(stderr, "\n" );
         exit(1);
      }
#endif
   }

   nTotSizeOut += stgSize(stgVarBody(v));

   if (me) {
      fprintf(stderr,"\n=============================\n");
      printStg ( stderr,v );
      fprintf(stderr, "\n\n" );
      fflush(stderr);
      if (me) exit(1);
   }
}


void optimiseTopBinds ( List bs )
{
   List t;
   Name n;
   Target ta = 0;

   noisy = ddumpSimpl && (lastModule() != modulePrelude);

   optimiser(RESET);
   if (noisy) printf("\n");
   initOptStats();

   for (t = bs; nonNull(t); t=tl(t)) {
      n = nameFromStgVar(hd(t));
      if (isNull(n) || !name(n).simplified) {
         rDepth = 0;
         optimiseTopBind(hd(t));
      }
      soFar(ta++);
   }
   if (noisy) printOptStats ( stderr );
   optimiser(RESET);
}


/* --------------------------------------------------------------------------
 * Optimiser control:
 * ------------------------------------------------------------------------*/

Void optimiser(what)
Int what; {

    switch (what) {
        case INSTALL :
        case RESET   : spClone = SP_NOT_IN_USE;
                       initStgVarSets();
                       daSccs = NIL;
                       break;

        case MARK    : markPairs();
                       markStgVarSets();
                       mark(daSccs);
                       break;

        case GCDONE  : checkStgVarSets();
                       break;
    }
}

/*-------------------------------------------------------------------------*/
