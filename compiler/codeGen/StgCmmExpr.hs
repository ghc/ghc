{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
--
-- Stg to C-- code generation: expressions
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmmExpr ( cgExpr ) where

#include "HsVersions.h"

import GhcPrelude hiding ((<*>))

import {-# SOURCE #-} StgCmmBind ( cgBind )

import StgCmmMonad
import StgCmmHeap
import StgCmmEnv
import StgCmmCon
import StgCmmProf (saveCurrentCostCentre, restoreCurrentCostCentre, emitSetCCC)
import StgCmmLayout
import StgCmmPrim
import StgCmmHpc
import StgCmmTicky
import StgCmmUtils
import StgCmmClosure

import StgSyn

import MkGraph
import BlockId
import Cmm
import CmmInfo
import CoreSyn
import DataCon
import ForeignCall
import Id
import PrimOp
import TyCon
import Type             ( isUnliftedType )
import RepType          ( isVoidTy, countConRepArgs, primRepSlot )
import CostCentre       ( CostCentreStack, currentCCS )
import Maybes
import Util
import FastString
import Outputable

import Control.Monad (unless,void)
import Control.Arrow (first)
import Data.Function ( on )

------------------------------------------------------------------------
--              cgExpr: the main function
------------------------------------------------------------------------

cgExpr  :: StgExpr -> FCode ReturnKind

cgExpr (StgApp fun args)     = cgIdApp fun args

-- seq# a s ==> a
-- See Note [seq# magic] in PrelRules
cgExpr (StgOpApp (StgPrimOp SeqOp) [StgVarArg a, _] _res_ty) =
  cgIdApp a []

cgExpr (StgOpApp op args ty) = cgOpApp op args ty
cgExpr (StgConApp con args _)= cgConApp con args
cgExpr (StgTick t e)         = cgTick t >> cgExpr e
cgExpr (StgLit lit)       = do cmm_lit <- cgLit lit
                               emitReturn [CmmLit cmm_lit]

cgExpr (StgLet binds expr)             = do { cgBind binds;     cgExpr expr }
cgExpr (StgLetNoEscape binds expr) =
  do { u <- newUnique
     ; let join_id = mkBlockId u
     ; cgLneBinds join_id binds
     ; r <- cgExpr expr
     ; emitLabel join_id
     ; return r }

cgExpr (StgCase expr bndr alt_type alts) =
  cgCase expr bndr alt_type alts

cgExpr (StgLam {}) = panic "cgExpr: StgLam"

------------------------------------------------------------------------
--              Let no escape
------------------------------------------------------------------------

{- Generating code for a let-no-escape binding, aka join point is very
very similar to what we do for a case expression.  The duality is
between
        let-no-escape x = b
        in e
and
        case e of ... -> b

That is, the RHS of 'x' (ie 'b') will execute *later*, just like
the alternative of the case; it needs to be compiled in an environment
in which all volatile bindings are forgotten, and the free vars are
bound only to stable things like stack locations..  The 'e' part will
execute *next*, just like the scrutinee of a case. -}

-------------------------
cgLneBinds :: BlockId -> StgBinding -> FCode ()
cgLneBinds join_id (StgNonRec bndr rhs)
  = do  { local_cc <- saveCurrentCostCentre
                -- See Note [Saving the current cost centre]
        ; (info, fcode) <- cgLetNoEscapeRhs join_id local_cc bndr rhs
        ; fcode
        ; addBindC info }

cgLneBinds join_id (StgRec pairs)
  = do  { local_cc <- saveCurrentCostCentre
        ; r <- sequence $ unzipWith (cgLetNoEscapeRhs join_id local_cc) pairs
        ; let (infos, fcodes) = unzip r
        ; addBindsC infos
        ; sequence_ fcodes
        }

-------------------------
cgLetNoEscapeRhs
    :: BlockId          -- join point for successor of let-no-escape
    -> Maybe LocalReg   -- Saved cost centre
    -> Id
    -> StgRhs
    -> FCode (CgIdInfo, FCode ())

cgLetNoEscapeRhs join_id local_cc bndr rhs =
  do { (info, rhs_code) <- cgLetNoEscapeRhsBody local_cc bndr rhs
     ; let (bid, _) = expectJust "cgLetNoEscapeRhs" $ maybeLetNoEscape info
     ; let code = do { (_, body) <- getCodeScoped rhs_code
                     ; emitOutOfLine bid (first (<*> mkBranch join_id) body) }
     ; return (info, code)
     }

cgLetNoEscapeRhsBody
    :: Maybe LocalReg   -- Saved cost centre
    -> Id
    -> StgRhs
    -> FCode (CgIdInfo, FCode ())
cgLetNoEscapeRhsBody local_cc bndr (StgRhsClosure cc _bi _ _upd args body)
  = cgLetNoEscapeClosure bndr local_cc cc (nonVoidIds args) body
cgLetNoEscapeRhsBody local_cc bndr (StgRhsCon cc con args)
  = cgLetNoEscapeClosure bndr local_cc cc []
      (StgConApp con args (pprPanic "cgLetNoEscapeRhsBody" $
                           text "StgRhsCon doesn't have type args"))
        -- For a constructor RHS we want to generate a single chunk of
        -- code which can be jumped to from many places, which will
        -- return the constructor. It's easy; just behave as if it
        -- was an StgRhsClosure with a ConApp inside!

-------------------------
cgLetNoEscapeClosure
        :: Id                   -- binder
        -> Maybe LocalReg       -- Slot for saved current cost centre
        -> CostCentreStack      -- XXX: *** NOT USED *** why not?
        -> [NonVoid Id]         -- Args (as in \ args -> body)
        -> StgExpr              -- Body (as in above)
        -> FCode (CgIdInfo, FCode ())

cgLetNoEscapeClosure bndr cc_slot _unused_cc args body
  = do dflags <- getDynFlags
       return ( lneIdInfo dflags bndr args
              , code )
  where
   code = forkLneBody $ do {
            ; withNewTickyCounterLNE (idName bndr) args $ do
            ; restoreCurrentCostCentre cc_slot
            ; arg_regs <- bindArgsToRegs args
            ; void $ noEscapeHeapCheck arg_regs (tickyEnterLNE >> cgExpr body) }


------------------------------------------------------------------------
--              Case expressions
------------------------------------------------------------------------

{- Note [Compiling case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is quite interesting to decide whether to put a heap-check at the
start of each alternative.  Of course we certainly have to do so if
the case forces an evaluation, or if there is a primitive op which can
trigger GC.

A more interesting situation is this (a Plan-B situation)

        !P!;
        ...P...
        case x# of
          0#      -> !Q!; ...Q...
          default -> !R!; ...R...

where !x! indicates a possible heap-check point. The heap checks
in the alternatives *can* be omitted, in which case the topmost
heapcheck will take their worst case into account.

In favour of omitting !Q!, !R!:

 - *May* save a heap overflow test,
   if ...P... allocates anything.

 - We can use relative addressing from a single Hp to
   get at all the closures so allocated.

 - No need to save volatile vars etc across heap checks
   in !Q!, !R!

Against omitting !Q!, !R!

  - May put a heap-check into the inner loop.  Suppose
        the main loop is P -> R -> P -> R...
        Q is the loop exit, and only it does allocation.
    This only hurts us if P does no allocation.  If P allocates,
    then there is a heap check in the inner loop anyway.

  - May do more allocation than reqd.  This sometimes bites us
    badly.  For example, nfib (ha!) allocates about 30\% more space if the
    worst-casing is done, because many many calls to nfib are leaf calls
    which don't need to allocate anything.

    We can un-allocate, but that costs an instruction

Neither problem hurts us if there is only one alternative.

Suppose the inner loop is P->R->P->R etc.  Then here is
how many heap checks we get in the *inner loop* under various
conditions

  Alloc   Heap check in branches (!Q!, !R!)?
  P Q R      yes     no (absorb to !P!)
--------------------------------------
  n n n      0          0
  n y n      0          1
  n . y      1          1
  y . y      2          1
  y . n      1          1

Best choices: absorb heap checks from Q and R into !P! iff
  a) P itself does some allocation
or
  b) P does allocation, or there is exactly one alternative

We adopt (b) because that is more likely to put the heap check at the
entry to a function, when not many things are live.  After a bunch of
single-branch cases, we may have lots of things live

Hence: two basic plans for

        case e of r { alts }

------ Plan A: the general case ---------

        ...save current cost centre...

        ...code for e,
           with sequel (SetLocals r)

        ...restore current cost centre...
        ...code for alts...
        ...alts do their own heap checks

------ Plan B: special case when ---------
  (i)  e does not allocate or call GC
  (ii) either upstream code performs allocation
       or there is just one alternative

  Then heap allocation in the (single) case branch
  is absorbed by the upstream check.
  Very common example: primops on unboxed values

        ...code for e,
           with sequel (SetLocals r)...

        ...code for alts...
        ...no heap check...
-}



-------------------------------------
data GcPlan
  = GcInAlts            -- Put a GC check at the start the case alternatives,
        [LocalReg]      -- which binds these registers
  | NoGcInAlts          -- The scrutinee is a primitive value, or a call to a
                        -- primitive op which does no GC.  Absorb the allocation
                        -- of the case alternative(s) into the upstream check

-------------------------------------
cgCase :: StgExpr -> Id -> AltType -> [StgAlt] -> FCode ReturnKind

cgCase (StgOpApp (StgPrimOp op) args _) bndr (AlgAlt tycon) alts
  | isEnumerationTyCon tycon -- Note [case on bool]
  = do { tag_expr <- do_enum_primop op args

       -- If the binder is not dead, convert the tag to a constructor
       -- and assign it.
       ; unless (isDeadBinder bndr) $ do
            { dflags <- getDynFlags
            ; tmp_reg <- bindArgToReg (NonVoid bndr)
            ; emitAssign (CmmLocal tmp_reg)
                         (tagToClosure dflags tycon tag_expr) }

       ; (mb_deflt, branches) <- cgAlgAltRhss (NoGcInAlts,AssignedDirectly)
                                              (NonVoid bndr) alts
                                 -- See Note [GC for conditionals]
       ; emitSwitch tag_expr branches mb_deflt 0 (tyConFamilySize tycon - 1)
       ; return AssignedDirectly
       }
  where
    do_enum_primop :: PrimOp -> [StgArg] -> FCode CmmExpr
    do_enum_primop TagToEnumOp [arg]  -- No code!
      = getArgAmode (NonVoid arg)
    do_enum_primop primop args
      = do dflags <- getDynFlags
           tmp <- newTemp (bWord dflags)
           cgPrimOp [tmp] primop args
           return (CmmReg (CmmLocal tmp))

{-
Note [case on bool]
~~~~~~~~~~~~~~~~~~~
This special case handles code like

  case a <# b of
    True ->
    False ->

-->  case tagToEnum# (a <$# b) of
        True -> .. ; False -> ...

--> case (a <$# b) of r ->
    case tagToEnum# r of
        True -> .. ; False -> ...

If we let the ordinary case code handle it, we'll get something like

 tmp1 = a < b
 tmp2 = Bool_closure_tbl[tmp1]
 if (tmp2 & 7 != 0) then ... // normal tagged case

but this junk won't optimise away.  What we really want is just an
inline comparison:

 if (a < b) then ...

So we add a special case to generate

 tmp1 = a < b
 if (tmp1 == 0) then ...

and later optimisations will further improve this.

Now that #6135 has been resolved it should be possible to remove that
special case. The idea behind this special case and pre-6135 implementation
of Bool-returning primops was that tagToEnum# was added implicitly in the
codegen and then optimized away. Now the call to tagToEnum# is explicit
in the source code, which allows to optimize it away at the earlier stages
of compilation (i.e. at the Core level).

Note [Scrutinising VoidRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have this STG code:
   f = \[s : State# RealWorld] ->
       case s of _ -> blah
This is very odd.  Why are we scrutinising a state token?  But it
can arise with bizarre NOINLINE pragmas (Trac #9964)
    crash :: IO ()
    crash = IO (\s -> let {-# NOINLINE s' #-}
                          s' = s
                      in (# s', () #))

Now the trouble is that 's' has VoidRep, and we do not bind void
arguments in the environment; they don't live anywhere.  See the
calls to nonVoidIds in various places.  So we must not look up
's' in the environment.  Instead, just evaluate the RHS!  Simple.
-}

cgCase (StgApp v []) _ (PrimAlt _) alts
  | isVoidRep (idPrimRep v)  -- See Note [Scrutinising VoidRep]
  , [(DEFAULT, _, rhs)] <- alts
  = cgExpr rhs

{- Note [Dodgy unsafeCoerce 1]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    case (x :: HValue) |> co of (y :: MutVar# Int)
        DEFAULT -> ...
We want to gnerate an assignment
     y := x
We want to allow this assignment to be generated in the case when the
types are compatible, because this allows some slightly-dodgy but
occasionally-useful casts to be used, such as in RtClosureInspect
where we cast an HValue to a MutVar# so we can print out the contents
of the MutVar#.  If instead we generate code that enters the HValue,
then we'll get a runtime panic, because the HValue really is a
MutVar#.  The types are compatible though, so we can just generate an
assignment.
-}
cgCase (StgApp v []) bndr alt_type@(PrimAlt _) alts
  | isUnliftedType (idType v)  -- Note [Dodgy unsafeCoerce 1]
  || reps_compatible
  = -- assignment suffices for unlifted types
    do { dflags <- getDynFlags
       ; unless reps_compatible $
           pprPanic "cgCase: reps do not match, perhaps a dodgy unsafeCoerce?"
                    (pp_bndr v $$ pp_bndr bndr)
       ; v_info <- getCgIdInfo v
       ; emitAssign (CmmLocal (idToReg dflags (NonVoid bndr)))
                    (idInfoToAmode v_info)
       -- Add bndr to the environment
       ; _ <- bindArgToReg (NonVoid bndr)
       ; cgAlts (NoGcInAlts,AssignedDirectly) (NonVoid bndr) alt_type alts }
  where
    reps_compatible = ((==) `on` (primRepSlot . idPrimRep)) v bndr
      -- Must compare SlotTys, not proper PrimReps, because with unboxed sums,
      -- the types of the binders are generated from slotPrimRep and might not
      -- match. Test case:
      --   swap :: (# Int | Int #) -> (# Int | Int #)
      --   swap (# x | #) = (# | x #)
      --   swap (# | y #) = (# y | #)

    pp_bndr id = ppr id <+> dcolon <+> ppr (idType id) <+> parens (ppr (idPrimRep id))

{- Note [Dodgy unsafeCoerce 2, #3132]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In all other cases of a lifted Id being cast to an unlifted type, the
Id should be bound to bottom, otherwise this is an unsafe use of
unsafeCoerce.  We can generate code to enter the Id and assume that
it will never return.  Hence, we emit the usual enter/return code, and
because bottom must be untagged, it will be entered.  The Sequel is a
type-correct assignment, albeit bogus.  The (dead) continuation loops;
it would be better to invoke some kind of panic function here.
-}
cgCase scrut@(StgApp v []) _ (PrimAlt _) _
  = do { dflags <- getDynFlags
       ; mb_cc <- maybeSaveCostCentre True
       ; _ <- withSequel
                  (AssignTo [idToReg dflags (NonVoid v)] False) (cgExpr scrut)
       ; restoreCurrentCostCentre mb_cc
       ; emitComment $ mkFastString "should be unreachable code"
       ; l <- newBlockId
       ; emitLabel l
       ; emit (mkBranch l)  -- an infinite loop
       ; return AssignedDirectly
       }

{- Note [Handle seq#]
~~~~~~~~~~~~~~~~~~~~~
See Note [seq# magic] in PrelRules.
The special case for seq# in cgCase does this:

  case seq# a s of v
    (# s', a' #) -> e
==>
  case a of v
    (# s', a' #) -> e

(taking advantage of the fact that the return convention for (# State#, a #)
is the same as the return convention for just 'a')
-}

cgCase (StgOpApp (StgPrimOp SeqOp) [StgVarArg a, _] _) bndr alt_type alts
  = -- Note [Handle seq#]
    -- And see Note [seq# magic] in PrelRules
    -- Use the same return convention as vanilla 'a'.
    cgCase (StgApp a []) bndr alt_type alts

cgCase scrut bndr alt_type alts
  = -- the general case
    do { dflags <- getDynFlags
       ; up_hp_usg <- getVirtHp        -- Upstream heap usage
       ; let ret_bndrs = chooseReturnBndrs bndr alt_type alts
             alt_regs  = map (idToReg dflags) ret_bndrs
       ; simple_scrut <- isSimpleScrut scrut alt_type
       ; let do_gc  | is_cmp_op scrut  = False  -- See Note [GC for conditionals]
                    | not simple_scrut = True
                    | isSingleton alts = False
                    | up_hp_usg > 0    = False
                    | otherwise        = True
               -- cf Note [Compiling case expressions]
             gc_plan = if do_gc then GcInAlts alt_regs else NoGcInAlts

       ; mb_cc <- maybeSaveCostCentre simple_scrut

       ; let sequel = AssignTo alt_regs do_gc{- Note [scrut sequel] -}
       ; ret_kind <- withSequel sequel (cgExpr scrut)
       ; restoreCurrentCostCentre mb_cc
       ; _ <- bindArgsToRegs ret_bndrs
       ; cgAlts (gc_plan,ret_kind) (NonVoid bndr) alt_type alts
       }
  where
    is_cmp_op (StgOpApp (StgPrimOp op) _ _) = isComparisonPrimOp op
    is_cmp_op _                             = False

{- Note [GC for conditionals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For boolean conditionals it seems that we have always done NoGcInAlts.
That is, we have always done the GC check before the conditional.
This is enshrined in the special case for
   case tagToEnum# (a>b) of ...
See Note [case on bool]

It's odd, and it's flagrantly inconsistent with the rules described
Note [Compiling case expressions].  However, after eliminating the
tagToEnum# (Trac #13397) we will have:
   case (a>b) of ...
Rather than make it behave quite differently, I am testing for a
comparison operator here in in the general case as well.

ToDo: figure out what the Right Rule should be.

Note [scrut sequel]
~~~~~~~~~~~~~~~~~~~
The job of the scrutinee is to assign its value(s) to alt_regs.
Additionally, if we plan to do a heap-check in the alternatives (see
Note [Compiling case expressions]), then we *must* retreat Hp to
recover any unused heap before passing control to the sequel.  If we
don't do this, then any unused heap will become slop because the heap
check will reset the heap usage. Slop in the heap breaks LDV profiling
(+RTS -hb) which needs to do a linear sweep through the nursery.


Note [Inlining out-of-line primops and heap checks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If shouldInlinePrimOp returns True when called from StgCmmExpr for the
purpose of heap check placement, we *must* inline the primop later in
StgCmmPrim. If we don't things will go wrong.
-}

-----------------
maybeSaveCostCentre :: Bool -> FCode (Maybe LocalReg)
maybeSaveCostCentre simple_scrut
  | simple_scrut = return Nothing
  | otherwise    = saveCurrentCostCentre


-----------------
isSimpleScrut :: StgExpr -> AltType -> FCode Bool
-- Simple scrutinee, does not block or allocate; hence safe to amalgamate
-- heap usage from alternatives into the stuff before the case
-- NB: if you get this wrong, and claim that the expression doesn't allocate
--     when it does, you'll deeply mess up allocation
isSimpleScrut (StgOpApp op args _) _       = isSimpleOp op args
isSimpleScrut (StgLit _)       _           = return True       -- case 1# of { 0# -> ..; ... }
isSimpleScrut (StgApp _ [])    (PrimAlt _) = return True       -- case x# of { 0# -> ..; ... }
isSimpleScrut _                _           = return False

isSimpleOp :: StgOp -> [StgArg] -> FCode Bool
-- True iff the op cannot block or allocate
isSimpleOp (StgFCallOp (CCall (CCallSpec _ _ safe)) _) _ = return $! not (playSafe safe)
isSimpleOp (StgPrimOp op) stg_args                  = do
    arg_exprs <- getNonVoidArgAmodes stg_args
    dflags <- getDynFlags
    -- See Note [Inlining out-of-line primops and heap checks]
    return $! isJust $ shouldInlinePrimOp dflags op arg_exprs
isSimpleOp (StgPrimCallOp _) _                           = return False

-----------------
chooseReturnBndrs :: Id -> AltType -> [StgAlt] -> [NonVoid Id]
-- These are the binders of a case that are assigned by the evaluation of the
-- scrutinee.
-- They're non-void, see Note [Post-unarisation invariants] in UnariseStg.
chooseReturnBndrs bndr (PrimAlt _) _alts
  = assertNonVoidIds [bndr]

chooseReturnBndrs _bndr (MultiValAlt n) [(_, ids, _)]
  = ASSERT2(ids `lengthIs` n, ppr n $$ ppr ids $$ ppr _bndr)
    assertNonVoidIds ids     -- 'bndr' is not assigned!

chooseReturnBndrs bndr (AlgAlt _) _alts
  = assertNonVoidIds [bndr]  -- Only 'bndr' is assigned

chooseReturnBndrs bndr PolyAlt _alts
  = assertNonVoidIds [bndr]  -- Only 'bndr' is assigned

chooseReturnBndrs _ _ _ = panic "chooseReturnBndrs"
                             -- MultiValAlt has only one alternative

-------------------------------------
cgAlts :: (GcPlan,ReturnKind) -> NonVoid Id -> AltType -> [StgAlt]
       -> FCode ReturnKind
-- At this point the result of the case are in the binders
cgAlts gc_plan _bndr PolyAlt [(_, _, rhs)]
  = maybeAltHeapCheck gc_plan (cgExpr rhs)

cgAlts gc_plan _bndr (MultiValAlt _) [(_, _, rhs)]
  = maybeAltHeapCheck gc_plan (cgExpr rhs)
        -- Here bndrs are *already* in scope, so don't rebind them

cgAlts gc_plan bndr (PrimAlt _) alts
  = do  { dflags <- getDynFlags

        ; tagged_cmms <- cgAltRhss gc_plan bndr alts

        ; let bndr_reg = CmmLocal (idToReg dflags bndr)
              (DEFAULT,deflt) = head tagged_cmms
                -- PrimAlts always have a DEFAULT case
                -- and it always comes first

              tagged_cmms' = [(lit,code)
                             | (LitAlt lit, code) <- tagged_cmms]
        ; emitCmmLitSwitch (CmmReg bndr_reg) tagged_cmms' deflt
        ; return AssignedDirectly }

cgAlts gc_plan bndr (AlgAlt tycon) alts
  = do  { dflags <- getDynFlags

        ; (mb_deflt, branches) <- cgAlgAltRhss gc_plan bndr alts

        ; let fam_sz   = tyConFamilySize tycon
              bndr_reg = CmmLocal (idToReg dflags bndr)

                    -- Is the constructor tag in the node reg?
        ; if isSmallFamily dflags fam_sz
          then do
                let   -- Yes, bndr_reg has constr. tag in ls bits
                   tag_expr = cmmConstrTag1 dflags (CmmReg bndr_reg)
                   branches' = [(tag+1,branch) | (tag,branch) <- branches]
                emitSwitch tag_expr branches' mb_deflt 1 fam_sz

           else -- No, get tag from info table
                let -- Note that ptr _always_ has tag 1
                    -- when the family size is big enough
                    untagged_ptr = cmmRegOffB bndr_reg (-1)
                    tag_expr = getConstrTag dflags (untagged_ptr)
                in emitSwitch tag_expr branches mb_deflt 0 (fam_sz - 1)

        ; return AssignedDirectly }

cgAlts _ _ _ _ = panic "cgAlts"
        -- UbxTupAlt and PolyAlt have only one alternative


-- Note [alg-alt heap check]
--
-- In an algebraic case with more than one alternative, we will have
-- code like
--
-- L0:
--   x = R1
--   goto L1
-- L1:
--   if (x & 7 >= 2) then goto L2 else goto L3
-- L2:
--   Hp = Hp + 16
--   if (Hp > HpLim) then goto L4
--   ...
-- L4:
--   call gc() returns to L5
-- L5:
--   x = R1
--   goto L1

-------------------
cgAlgAltRhss :: (GcPlan,ReturnKind) -> NonVoid Id -> [StgAlt]
             -> FCode ( Maybe CmmAGraphScoped
                      , [(ConTagZ, CmmAGraphScoped)] )
cgAlgAltRhss gc_plan bndr alts
  = do { tagged_cmms <- cgAltRhss gc_plan bndr alts

       ; let { mb_deflt = case tagged_cmms of
                           ((DEFAULT,rhs) : _) -> Just rhs
                           _other              -> Nothing
                            -- DEFAULT is always first, if present

              ; branches = [ (dataConTagZ con, cmm)
                           | (DataAlt con, cmm) <- tagged_cmms ]
              }

       ; return (mb_deflt, branches)
       }


-------------------
cgAltRhss :: (GcPlan,ReturnKind) -> NonVoid Id -> [StgAlt]
          -> FCode [(AltCon, CmmAGraphScoped)]
cgAltRhss gc_plan bndr alts = do
  dflags <- getDynFlags
  let
    base_reg = idToReg dflags bndr
    cg_alt :: StgAlt -> FCode (AltCon, CmmAGraphScoped)
    cg_alt (con, bndrs, rhs)
      = getCodeScoped             $
        maybeAltHeapCheck gc_plan $
        do { _ <- bindConArgs con base_reg (assertNonVoidIds bndrs)
                    -- alt binders are always non-void,
                    -- see Note [Post-unarisation invariants] in UnariseStg
           ; _ <- cgExpr rhs
           ; return con }
  forkAlts (map cg_alt alts)

maybeAltHeapCheck :: (GcPlan,ReturnKind) -> FCode a -> FCode a
maybeAltHeapCheck (NoGcInAlts,_)  code = code
maybeAltHeapCheck (GcInAlts regs, AssignedDirectly) code =
  altHeapCheck regs code
maybeAltHeapCheck (GcInAlts regs, ReturnedTo lret off) code =
  altHeapCheckReturnsTo regs lret off code

-----------------------------------------------------------------------------
--      Tail calls
-----------------------------------------------------------------------------

cgConApp :: DataCon -> [StgArg] -> FCode ReturnKind
cgConApp con stg_args
  | isUnboxedTupleCon con       -- Unboxed tuple: assign and return
  = do { arg_exprs <- getNonVoidArgAmodes stg_args
       ; tickyUnboxedTupleReturn (length arg_exprs)
       ; emitReturn arg_exprs }

  | otherwise   --  Boxed constructors; allocate and return
  = ASSERT2( stg_args `lengthIs` countConRepArgs con, ppr con <> parens (ppr (countConRepArgs con)) <+> ppr stg_args )
    do  { (idinfo, fcode_init) <- buildDynCon (dataConWorkId con) False
                                     currentCCS con (assertNonVoidStgArgs stg_args)
                                     -- con args are always non-void,
                                     -- see Note [Post-unarisation invariants] in UnariseStg
                -- The first "con" says that the name bound to this
                -- closure is "con", which is a bit of a fudge, but
                -- it only affects profiling (hence the False)

        ; emit =<< fcode_init
        ; tickyReturnNewCon (length stg_args)
        ; emitReturn [idInfoToAmode idinfo] }

cgIdApp :: Id -> [StgArg] -> FCode ReturnKind
cgIdApp fun_id args = do
    dflags         <- getDynFlags
    fun_info       <- getCgIdInfo fun_id
    self_loop_info <- getSelfLoop
    let cg_fun_id   = cg_id fun_info
           -- NB: use (cg_id fun_info) instead of fun_id, because
           -- the former may be externalised for -split-objs.
           -- See Note [Externalise when splitting] in StgCmmMonad

        fun_arg     = StgVarArg cg_fun_id
        fun_name    = idName    cg_fun_id
        fun         = idInfoToAmode fun_info
        lf_info     = cg_lf         fun_info
        n_args      = length args
        v_args      = length $ filter (isVoidTy . stgArgType) args
        node_points dflags = nodeMustPointToIt dflags lf_info
    case getCallMethod dflags fun_name cg_fun_id lf_info n_args v_args (cg_loc fun_info) self_loop_info of
            -- A value in WHNF, so we can just return it.
        ReturnIt
          | isVoidTy (idType fun_id) -> emitReturn []
          | otherwise                -> emitReturn [fun]
          -- ToDo: does ReturnIt guarantee tagged?

        EnterIt -> ASSERT( null args )  -- Discarding arguments
                   emitEnter fun

        SlowCall -> do      -- A slow function call via the RTS apply routines
                { tickySlowCall lf_info args
                ; emitComment $ mkFastString "slowCall"
                ; slowCall fun args }

        -- A direct function call (possibly with some left-over arguments)
        DirectEntry lbl arity -> do
                { tickyDirectCall arity args
                ; if node_points dflags
                     then directCall NativeNodeCall   lbl arity (fun_arg:args)
                     else directCall NativeDirectCall lbl arity args }

        -- Let-no-escape call or self-recursive tail-call
        JumpToIt blk_id lne_regs -> do
          { adjustHpBackwards -- always do this before a tail-call
          ; cmm_args <- getNonVoidArgAmodes args
          ; emitMultiAssign lne_regs cmm_args
          ; emit (mkBranch blk_id)
          ; return AssignedDirectly }

-- Note [Self-recursive tail calls]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Self-recursive tail calls can be optimized into a local jump in the same
-- way as let-no-escape bindings (see Note [What is a non-escaping let] in
-- stgSyn/CoreToStg.hs). Consider this:
--
-- foo.info:
--     a = R1  // calling convention
--     b = R2
--     goto L1
-- L1: ...
--     ...
-- ...
-- L2: R1 = x
--     R2 = y
--     call foo(R1,R2)
--
-- Instead of putting x and y into registers (or other locations required by the
-- calling convention) and performing a call we can put them into local
-- variables a and b and perform jump to L1:
--
-- foo.info:
--     a = R1
--     b = R2
--     goto L1
-- L1: ...
--     ...
-- ...
-- L2: a = x
--     b = y
--     goto L1
--
-- This can be done only when function is calling itself in a tail position
-- and only if the call passes number of parameters equal to function's arity.
-- Note that this cannot be performed if a function calls itself with a
-- continuation.
--
-- This in fact implements optimization known as "loopification". It was
-- described in "Low-level code optimizations in the Glasgow Haskell Compiler"
-- by Krzysztof Woś, though we use different approach. Krzysztof performed his
-- optimization at the Cmm level, whereas we perform ours during code generation
-- (Stg-to-Cmm pass) essentially making sure that optimized Cmm code is
-- generated in the first place.
--
-- Implementation is spread across a couple of places in the code:
--
--   * FCode monad stores additional information in its reader environment
--     (cgd_self_loop field). This information tells us which function can
--     tail call itself in an optimized way (it is the function currently
--     being compiled), what is the label of a loop header (L1 in example above)
--     and information about local registers in which we should arguments
--     before making a call (this would be a and b in example above).
--
--   * Whenever we are compiling a function, we set that information to reflect
--     the fact that function currently being compiled can be jumped to, instead
--     of called. This is done in closureCodyBody in StgCmmBind.
--
--   * We also have to emit a label to which we will be jumping. We make sure
--     that the label is placed after a stack check but before the heap
--     check. The reason is that making a recursive tail-call does not increase
--     the stack so we only need to check once. But it may grow the heap, so we
--     have to repeat the heap check in every self-call. This is done in
--     do_checks in StgCmmHeap.
--
--   * When we begin compilation of another closure we remove the additional
--     information from the environment. This is done by forkClosureBody
--     in StgCmmMonad. Other functions that duplicate the environment -
--     forkLneBody, forkAlts, codeOnly - duplicate that information. In other
--     words, we only need to clean the environment of the self-loop information
--     when compiling right hand side of a closure (binding).
--
--   * When compiling a call (cgIdApp) we use getCallMethod to decide what kind
--     of call will be generated. getCallMethod decides to generate a self
--     recursive tail call when (a) environment stores information about
--     possible self tail-call; (b) that tail call is to a function currently
--     being compiled; (c) number of passed non-void arguments is equal to
--     function's arity. (d) loopification is turned on via -floopification
--     command-line option.
--
--   * Command line option to turn loopification on and off is implemented in
--     DynFlags.
--
--
-- Note [Void arguments in self-recursive tail calls]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- State# tokens can get in the way of the loopification optimization as seen in
-- #11372. Consider this:
--
-- foo :: [a]
--     -> (a -> State# s -> (# State s, Bool #))
--     -> State# s
--     -> (# State# s, Maybe a #)
-- foo [] f s = (# s, Nothing #)
-- foo (x:xs) f s = case f x s of
--      (# s', b #) -> case b of
--          True -> (# s', Just x #)
--          False -> foo xs f s'
--
-- We would like to compile the call to foo as a local jump instead of a call
-- (see Note [Self-recursive tail calls]). However, the generated function has
-- an arity of 2 while we apply it to 3 arguments, one of them being of void
-- type. Thus, we mustn't count arguments of void type when checking whether
-- we can turn a call into a self-recursive jump.
--

emitEnter :: CmmExpr -> FCode ReturnKind
emitEnter fun = do
  { dflags <- getDynFlags
  ; adjustHpBackwards
  ; sequel <- getSequel
  ; updfr_off <- getUpdFrameOff
  ; case sequel of
      -- For a return, we have the option of generating a tag-test or
      -- not.  If the value is tagged, we can return directly, which
      -- is quicker than entering the value.  This is a code
      -- size/speed trade-off: when optimising for speed rather than
      -- size we could generate the tag test.
      --
      -- Right now, we do what the old codegen did, and omit the tag
      -- test, just generating an enter.
      Return -> do
        { let entry = entryCode dflags $ closureInfoPtr dflags $ CmmReg nodeReg
        ; emit $ mkJump dflags NativeNodeCall entry
                        [cmmUntag dflags fun] updfr_off
        ; return AssignedDirectly
        }

      -- The result will be scrutinised in the sequel.  This is where
      -- we generate a tag-test to avoid entering the closure if
      -- possible.
      --
      -- The generated code will be something like this:
      --
      --    R1 = fun  -- copyout
      --    if (fun & 7 != 0) goto Lret else goto Lcall
      --  Lcall:
      --    call [fun] returns to Lret
      --  Lret:
      --    fun' = R1  -- copyin
      --    ...
      --
      -- Note in particular that the label Lret is used as a
      -- destination by both the tag-test and the call.  This is
      -- because Lret will necessarily be a proc-point, and we want to
      -- ensure that we generate only one proc-point for this
      -- sequence.
      --
      -- Furthermore, we tell the caller that we generated a native
      -- return continuation by returning (ReturnedTo Lret off), so
      -- that the continuation can be reused by the heap-check failure
      -- code in the enclosing case expression.
      --
      AssignTo res_regs _ -> do
       { lret <- newBlockId
       ; let (off, _, copyin) = copyInOflow dflags NativeReturn (Young lret) res_regs []
       ; lcall <- newBlockId
       ; updfr_off <- getUpdFrameOff
       ; let area = Young lret
       ; let (outArgs, regs, copyout) = copyOutOflow dflags NativeNodeCall Call area
                                          [fun] updfr_off []
         -- refer to fun via nodeReg after the copyout, to avoid having
         -- both live simultaneously; this sometimes enables fun to be
         -- inlined in the RHS of the R1 assignment.
       ; let entry = entryCode dflags (closureInfoPtr dflags (CmmReg nodeReg))
             the_call = toCall entry (Just lret) updfr_off off outArgs regs
       ; tscope <- getTickScope
       ; emit $
           copyout <*>
           mkCbranch (cmmIsTagged dflags (CmmReg nodeReg))
                     lret lcall Nothing <*>
           outOfLine lcall (the_call,tscope) <*>
           mkLabel lret tscope <*>
           copyin
       ; return (ReturnedTo lret off)
       }
  }

------------------------------------------------------------------------
--              Ticks
------------------------------------------------------------------------

-- | Generate Cmm code for a tick. Depending on the type of Tickish,
-- this will either generate actual Cmm instrumentation code, or
-- simply pass on the annotation as a @CmmTickish@.
cgTick :: Tickish Id -> FCode ()
cgTick tick
  = do { dflags <- getDynFlags
       ; case tick of
           ProfNote   cc t p -> emitSetCCC cc t p
           HpcTick    m n    -> emit (mkTickBox dflags m n)
           SourceNote s n    -> emitTick $ SourceNote s n
           _other            -> return () -- ignore
       }
