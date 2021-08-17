#ifndef __GHCJS_RTS_H_
#define __GHCJS_RTS_H_

#include "constants.h"

/*
 * low-level heap object manipulation macros
 */

#ifdef GHCJS_PROF
#define MK_TUP2(x1,x2)                           (h$c2(h$ghczmprimZCGHCziTupleziZLz2cUZR_con_e,(x1),(x2),h$currentThread?h$currentThread.ccs:h$CCS_SYSTEM))
#define MK_TUP3(x1,x2,x3)                        (h$c3(h$ghczmprimZCGHCziTupleziZLz2cUz2cUZR_con_e,(x1),(x2),(x3),h$currentThread?h$currentThread.ccs:h$CCS_SYSTEM))
#define MK_TUP4(x1,x2,x3,x4)                     (h$c4(h$ghczmprimZCGHCziTupleziZLz2cUz2cUz2cUZR_con_e,(x1),(x2),(x3),(x4),h$currentThread?h$currentThread.ccs:h$CCS_SYSTEM))
#define MK_TUP5(x1,x2,x3,x4,x5)                  (h$c5(h$ghczmprimZCGHCziTupleziZLz2cUz2cUz2cUz2cUZR_con_e,(x1),(x2),(x3),(x4),(x5),h$currentThread?h$currentThread.ccs:h$CCS_SYSTEM))
#define MK_TUP6(x1,x2,x3,x4,x5,x6)               (h$c6(h$ghczmprimZCGHCziTupleziZLz2cUz2cUz2cUz2cUz2cUZR_con_e,(x1),(x2),(x3),(x4),(x5),(x6),h$currentThread?h$currentThread.ccs:h$CCS_SYSTEM))
#define MK_TUP7(x1,x2,x3,x4,x5,x6,x7)            (h$c7(h$ghczmprimZCGHCziTupleziZLz2cUz2cUz2cUz2cUz2cUz2cUZR_con_e,(x1),(x2),(x3),(x4),(x5),(x6),(x7),h$currentThread?h$currentThread.ccs:h$CCS_SYSTEM))
#define MK_TUP8(x1,x2,x3,x4,x5,x6,x7,x8)         (h$c8(h$ghczmprimZCGHCziTupleziZLz2cUz2cUz2cUz2cUz2cUz2cUz2cUZR_con_e,(x1),(x2),(x3),(x4),(x5),(x6),(x7),(x8),h$currentThread?h$currentThread.ccs:h$CCS_SYSTEM))
#define MK_TUP9(x1,x2,x3,x4,x5,x6,x7,x8,x9)      (h$c9(h$ghczmprimZCGHCziTupleziZLz2cUz2cUz2cUz2cUz2cUz2cUz2cUz2cUZR_con_e,(x1),(x2),(x3),(x4),(x5),(x6),(x7),(x8),(x9),h$currentThread?h$currentThread.ccs:h$CCS_SYSTEM))
#define MK_TUP10(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) (h$c10(h$ghczmprimZCGHCziTupleziZLz2cUz2cUz2cUz2cUz2cUz2cUz2cUz2cUz2cUZR_con_e,(x1),(x2),(x3),(x4),(x5),(x6),(x7),(x8),(x9),(x10),h$currentThread?h$currentThread.ccs:h$CCS_SYSTEM))
#else
#define MK_TUP2(x1,x2)                           (h$c2(h$ghczmprimZCGHCziTupleziZLz2cUZR_con_e,(x1),(x2)))
#define MK_TUP3(x1,x2,x3)                        (h$c3(h$ghczmprimZCGHCziTupleziZLz2cUz2cUZR_con_e,(x1),(x2),(x3)))
#define MK_TUP4(x1,x2,x3,x4)                     (h$c4(h$ghczmprimZCGHCziTupleziZLz2cUz2cUz2cUZR_con_e,(x1),(x2),(x3),(x4)))
#define MK_TUP5(x1,x2,x3,x4,x5)                  (h$c5(h$ghczmprimZCGHCziTupleziZLz2cUz2cUz2cUz2cUZR_con_e,(x1),(x2),(x3),(x4),(x5)))
#define MK_TUP6(x1,x2,x3,x4,x5,x6)               (h$c6(h$ghczmprimZCGHCziTupleziZLz2cUz2cUz2cUz2cUz2cUZR_con_e,(x1),(x2),(x3),(x4),(x5),(x6)))
#define MK_TUP7(x1,x2,x3,x4,x5,x6,x7)            (h$c7(h$ghczmprimZCGHCziTupleziZLz2cUz2cUz2cUz2cUz2cUz2cUZR_con_e,(x1),(x2),(x3),(x4),(x5),(x6),(x7)))
#define MK_TUP8(x1,x2,x3,x4,x5,x6,x7,x8)         (h$c8(h$ghczmprimZCGHCziTupleziZLz2cUz2cUz2cUz2cUz2cUz2cUz2cUZR_con_e,(x1),(x2),(x3),(x4),(x5),(x6),(x7),(x8)))
#define MK_TUP9(x1,x2,x3,x4,x5,x6,x7,x8,x9)      (h$c9(h$ghczmprimZCGHCziTupleziZLz2cUz2cUz2cUz2cUz2cUz2cUz2cUz2cUZR_con_e,(x1),(x2),(x3),(x4),(x5),(x6),(x7),(x8),(x9)))
#define MK_TUP10(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) (h$c10(h$ghczmprimZCGHCziTupleziZLz2cUz2cUz2cUz2cUz2cUz2cUz2cUz2cUz2cUZR_con_e,(x1),(x2),(x3),(x4),(x5),(x6),(x7),(x8),(x9),(x10)))
#endif

#define TUP2_1(x) ((x).d1)
#define TUP2_2(x) ((x).d2)



// GHCJS.Prim.JSVal
#ifdef GHCJS_PROF
#define MK_JSVAL(x) (h$c1(h$baseZCGHCJSziPrimziJSVal_con_e, (x), h$CCS_SYSTEM))
#else
#define MK_JSVAL(x) (h$c1(h$baseZCGHCJSziPrimziJSVal_con_e, (x)))
#endif
#define JSVAL_VAL(x) ((x).d1)

// GHCJS.Prim.JSException
#ifdef GHCJS_PROF
#define MK_JSEXCEPTION(msg,hsMsg) (h$c2(h$baseZCGHCJSziPrimziJSException_con_e,(msg),(hsMsg),h$CCS_SYSTEM))
#else
#define MK_JSEXCEPTION(msg,hsMsg) (h$c2(h$baseZCGHCJSziPrimziJSException_con_e,(msg),(hsMsg)))
#endif
// Exception dictionary for JSException
#define HS_JSEXCEPTION_EXCEPTION h$baseZCGHCJSziPrimzizdfExceptionJSException

// SomeException
#ifdef GHCJS_PROF
#define MK_SOMEEXCEPTION(dict,except) (h$c2(h$baseZCGHCziExceptionziTypeziSomeException_con_e,(dict),(except),h$CCS_SYSTEM))
#else
#define MK_SOMEEXCEPTION(dict,except) (h$c2(h$baseZCGHCziExceptionziTypeziSomeException_con_e,(dict),(except)))
#endif

// GHC.Ptr.Ptr
#ifdef GHCJS_PROF
#define MK_PTR(val,offset) (h$c2(h$baseZCGHCziPtrziPtr_con_e, (val), (offset), h$CCS_SYSTEM))
#else
#define MK_PTR(val,offset) (h$c2(h$baseZCGHCziPtrziPtr_con_e, (val), (offset)))
#endif

// GHC.Integer.GMP.Internals
#define IS_INTEGER_S(cl) ((cl).f === h$integerzmwiredzminZCGHCziIntegerziTypeziSzh_con_e)
#define IS_INTEGER_Jp(cl) ((cl).f === h$integerzmwiredzminZCGHCziIntegerziTypeziJpzh_con_e)
#define IS_INTEGER_Jn(cl) ((cl).f === h$integerzmwiredzminZCGHCziIntegerziTypeziJnzh_con_e)
#define INTEGER_S_DATA(cl) ((cl).d1)
#define INTEGER_J_DATA(cl) ((cl).d1)
#ifdef GHCJS_PROF
#define MK_INTEGER_S(iii) (h$c1(h$integerzmwiredzminZCGHCziIntegerziTypeziSzh_con_e, (iii), h$CCS_SYSTEM));
#define MK_INTEGER_Jp(iii) (h$c1(h$integerzmwiredzminZCGHCziIntegerziTypeziJpzh_con_e, (iii), h$CCS_SYSTEM));
#define MK_INTEGER_Jn(iii) (h$c1(h$integerzmwiredzminZCGHCziIntegerziTypeziJnzh_con_e, (iii), h$CCS_SYSTEM));
#else
#define MK_INTEGER_S(iii) (h$c1(h$integerzmwiredzminZCGHCziIntegerziTypeziSzh_con_e, (iii)));
#define MK_INTEGER_Jp(iii) (h$c1(h$integerzmwiredzminZCGHCziIntegerziTypeziJpzh_con_e, (iii)));
#define MK_INTEGER_Jn(iii) (h$c1(h$integerzmwiredzminZCGHCziIntegerziTypeziJnzh_con_e, (iii)));
#endif

// Data.Maybe.Maybe
#define HS_NOTHING h$baseZCGHCziBaseziNothing
#define IS_NOTHING(cl) ((cl).f === h$baseZCGHCziBaseziNothing_con_e)
#define IS_JUST(cl) ((cl).f === h$baseZCGHCziBaseziJust_con_e)
#define JUST_VAL(jj) ((jj).d1)
// #define HS_NOTHING h$nothing
#ifdef GHCJS_PROF
#define MK_JUST(val) (h$c1(h$baseZCGHCziBaseziJust_con_e, (val), h$CCS_SYSTEM))
#else
#define MK_JUST(val) (h$c1(h$baseZCGHCziBaseziJust_con_e, (val)))
#endif

// Data.List
#define HS_NIL h$ghczmprimZCGHCziTypesziZMZN
#define HS_NIL_CON h$ghczmprimZCGHCziTypesziZMZN_con_e
#define IS_CONS(cl) ((cl).f === h$ghczmprimZCGHCziTypesziZC_con_e)
#define IS_NIL(cl) ((cl).f === h$ghczmprimZCGHCziTypesziZMZN_con_e)
#define CONS_HEAD(cl) ((cl).d1)
#define CONS_TAIL(cl) ((cl).d2)
#ifdef GHCJS_PROF
#define MK_CONS(head,tail) (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (head), (tail), h$CCS_SYSTEM))
#define MK_CONS_CC(head,tail,cc) (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (head), (tail), (cc)))
#else
#define MK_CONS(head,tail) (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (head), (tail)))
#define MK_CONS_CC(head,tail,cc) (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (head), (tail)))
#endif

// Data.Text
#define DATA_TEXT_ARRAY(x) ((x).d1)
#define DATA_TEXT_OFFSET(x) ((x).d2.d1)
#define DATA_TEXT_LENGTH(x) ((x).d2.d2)

// Data.Text.Lazy
#define LAZY_TEXT_IS_CHUNK(x) ((x).f.a === 2)
#define LAZY_TEXT_IS_NIL(x) ((x).f.a === 1)
#define LAZY_TEXT_CHUNK_HEAD(x) ((x))
#define LAZY_TEXT_CHUNK_TAIL(x) ((x).d2.d3)

// black holes
// can we skip the indirection for black holes?
#define IS_BLACKHOLE(x) (typeof (x) === 'object' && (x) && (x).f && (x).f.t === CLOSURE_TYPE_BLACKHOLE)
#define BLACKHOLE_TID(bh) ((bh).d1)
#define SET_BLACKHOLE_TID(bh,tid) ((bh).d1 = (tid))
#define BLACKHOLE_QUEUE(bh) ((bh).d2)
#define SET_BLACKHOLE_QUEUE(bh,val) ((bh).d2 = (val))

// resumable thunks
#define MAKE_RESUMABLE(closure,stack) { (closure).f = h$resume_e; (closure).d1 = (stack), (closure).d2 = null; }

// general deconstruction
#define IS_THUNK(x) ((x).f.t === CLOSURE_TYPE_THUNK)
#define CONSTR_TAG(x) ((x).f.a)

// retrieve  a numeric value that's possibly stored as an indirection
#define IS_WRAPPED_NUMBER(val) ((typeof(val)==='object')&&(val).f === h$unbox_e)
#define UNWRAP_NUMBER(val) ((typeof(val) === 'number')?(val):(val).d1)

// generic lazy values
#ifdef GHCJS_PROF
#define MK_LAZY(fun) (h$c1(h$lazy_e, (fun), h$CCS_SYSTEM))
#define MK_LAZY_CC(fun,cc) (h$c1(h$lazy_e, (fun), (cc)))
#else
#define MK_LAZY(fun) (h$c1(h$lazy_e, (fun)))
#define MK_LAZY_CC(fun,cc) (h$c1(h$lazy_e, (fun)))
#endif

// generic data constructors and selectors
#ifdef GHCJS_PROF
#define MK_DATA1_1(val) (h$c1(h$data1_e, (val), h$CCS_SYSTEM))
#define MK_DATA1_2(val1,val2) (h$c2(h$data1_e, (val1), (val2), h$CCS_SYSTEM))
#define MK_DATA2_1(val) (h$c1(h$data2_e, (val), h$CCS_SYSTEM))
#define MK_DATA2_2(val1,val2) (h$c2(h$data1_e, (val1), (val2), h$CCS_SYSTEM))
#define MK_SELECT1(val) (h$c1(h$select1_e, (val), h$CCS_SYSTEM))
#define MK_SELECT2(val) (h$c1(h$select2_e, (val), h$CCS_SYSTEM))
#define MK_AP1(fun,val) (h$c2(h$ap1_e, (fun), (val), h$CCS_SYSTEM))
#define MK_AP2(fun,val1,val2) (h$c3(h$ap2_e, (fun), (val1), (val2), h$CCS_SYSTEM))
#define MK_AP3(fun,val1,val2,val3) (h$c4(h$ap3_e, (fun), (val1), (val2), (val3), h$CCS_SYSTEM))
#else
#define MK_DATA1_1(val) (h$c1(h$data1_e, (val)))
#define MK_DATA1_2(val1,val2) (h$c2(h$data1_e, (val1), (val2)))
#define MK_DATA2_1(val) (h$c1(h$data2_e, (val)))
#define MK_DATA2_2(val1,val2) (h$c2(h$data2_e, (val1), (val2)))
#define MK_SELECT1(val) (h$c1(h$select1_e, (val)))
#define MK_SELECT2(val) (h$c1(h$select2_e, (val)))
#define MK_AP1(fun,val) (h$c2(h$ap1_e,(fun),(val)))
#define MK_AP2(fun,val1,val2) (h$c3(h$ap2_e,(fun),(val1),(val2)))
#define MK_AP3(fun,val1,val2,val3) (h$c4(h$ap3_e, (fun), (val1), (val2), (val3)))
#endif

// unboxed tuple returns
// #define RETURN_UBX_TUP1(x) return x;
#define RETURN_UBX_TUP2(x1,x2) { h$ret1 = (x2); return (x1); }
#define RETURN_UBX_TUP3(x1,x2,x3) { h$ret1 = (x2); h$ret2 = (x3); return (x1); }
#define RETURN_UBX_TUP4(x1,x2,x3,x4) { h$ret1 = (x2); h$ret2 = (x3); h$ret3 = (x4); return (x1); }
#define RETURN_UBX_TUP5(x1,x2,x3,x4,x5) { h$ret1 = (x2); h$ret2 = (x3); h$ret3 = (x4); h$ret4 = (x5); return (x1); }
#define RETURN_UBX_TUP6(x1,x2,x3,x4,x5,x6) { h$ret1 = (x2); h$ret2 = (x3); h$ret3 = (x4); h$ret4 = (x5); h$ret5 = (x6); return (x1); }
#define RETURN_UBX_TUP7(x1,x2,x3,x4,x5,x6,x7) { h$ret1 = (x2); h$ret2 = (x3); h$ret3 = (x4); h$ret4 = (x5); h$ret5 = (x6); h$ret6 = (x7); return (x1); }
#define RETURN_UBX_TUP8(x1,x2,x3,x4,x5,x6,x7,x8) { h$ret1 = (x2); h$ret2 = (x3); h$ret3 = (x4); h$ret4 = (x5); h$ret5 = (x6); h$ret6 = (x7); h$ret7 = (x8); return (x1); }
#define RETURN_UBX_TUP9(x1,x2,x3,x4,x5,x6,x7,x8,x9) { h$ret1 = (x2); h$ret2 = (x3); h$ret3 = (x4); h$ret4 = (x5); h$ret5 = (x6); h$ret6 = (x7); h$ret7 = (x8); h$ret8 = (x9); return (x1); }
#define RETURN_UBX_TUP10(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) { h$ret1 = (x2); h$ret2 = (x3); h$ret3 = (x4); h$ret4 = (x5); h$ret5 = (x6); h$ret6 = (x7); h$ret7 = (x8); h$ret8 = (x9); h$ret9 = (x10); return (x1); }

#define CALL_UBX_TUP2(r1,r2,c) { (r1) = (c); (r2) = h$ret1; }
#define CALL_UBX_TUP3(r1,r2,r3,c) { (r1) = (c); (r2) = h$ret1; (r3) = h$ret2; }
#define CALL_UBX_TUP4(r1,r2,r3,r4,c) { (r1) = (c); (r2) = h$ret1; (r3) = h$ret2; (r4) = h$ret3; }
#define CALL_UBX_TUP5(r1,r2,r3,r4,r5,c) { (r1) = (c); (r2) = h$ret1; (r3) = h$ret2; (r4) = h$ret3; (r5) = h$ret4; }
#define CALL_UBX_TUP6(r1,r2,r3,r4,r5,r6,c) { (r1) = (c); (r2) = h$ret1; (r3) = h$ret2; (r4) = h$ret3; (r5) = h$ret4; (r6) = h$ret5; }
#define CALL_UBX_TUP7(r1,r2,r3,r4,r5,r6,r7,c) { (r1) = (c); (r2) = h$ret1; (r3) = h$ret2; (r4) = h$ret3; (r5) = h$ret4; (r6) = h$ret5; (r7) = h$ret6; }
#define CALL_UBX_TUP8(r1,r2,r3,r4,r5,r6,r7,r8,c) { (r1) = (c); (r2) = h$ret1; (r3) = h$ret2; (r4) = h$ret3; (r5) = h$ret4; (r6) = h$ret5; (r7) = h$ret6; (r8) = h$ret7; }
#define CALL_UBX_TUP9(r1,r2,r3,r4,r5,r6,r7,r8,r9,c) { (r1) = (c); (r2) = h$ret1; (r3) = h$ret2; (r4) = h$ret3; (r5) = h$ret4; (r6) = h$ret5; (r7) = h$ret6; (r8) = h$ret7; (r9) = h$ret8; }
#define CALL_UBX_TUP10(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,c) { (r1) = (c); (r2) = h$ret1; (r3) = h$ret2; (r4) = h$ret3; (r5) = h$ret4; (r6) = h$ret5; (r7) = h$ret6; (r8) = h$ret7; (r9) = h$ret8; (r10) = h$ret9; }


#endif
