
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE MultiWayIf       #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-2012

Note [Unarisation]
~~~~~~~~~~~~~~~~~~
The idea of this pass is to translate away *all* unboxed-tuple and unboxed-sum
binders. So for example:

  f (x :: (# Int, Bool #)) = f x + f (# 1, True #)

  ==>

  f (x1 :: Int) (x2 :: Bool) = f x1 x2 + f 1 True

It is important that we do this at the STG level and NOT at the Core level
because it would be very hard to make this pass Core-type-preserving. In this
example the type of 'f' changes, for example.

STG fed to the code generators *must* be unarised because the code generators do
not support unboxed tuple and unboxed sum binders natively.

In more detail: (see next note for unboxed sums)

Suppose that a variable x : (# t1, t2 #).

  * At the binding site for x, make up fresh vars  x1:t1, x2:t2

  * Extend the UnariseEnv   x :-> MultiVal [x1,x2]

  * Replace the binding with a curried binding for x1,x2

       Lambda:   \x.e                ==>   \x1 x2. e
       Case alt: MkT a b x c d -> e  ==>   MkT a b x1 x2 c d -> e

  * Replace argument occurrences with a sequence of args via a lookup in
    UnariseEnv

       f a b x c d   ==>   f a b x1 x2 c d

  * Replace tail-call occurrences with an unboxed tuple via a lookup in
    UnariseEnv

       x  ==>  (# x1, x2 #)

    So, for example

       f x = x    ==>   f x1 x2 = (# x1, x2 #)

  * We /always/ eliminate a case expression when

       - It scrutinises an unboxed tuple or unboxed sum

       - The scrutinee is a variable (or when it is an explicit tuple, but the
         simplifier eliminates those)

    The case alternative (there can be only one) can be one of these two
    things:

      - An unboxed tuple pattern. e.g.

          case v of x { (# x1, x2, x3 #) -> ... }

        Scrutinee has to be in form `(# t1, t2, t3 #)` so we just extend the
        environment with

          x :-> MultiVal [t1,t2,t3]
          x1 :-> UnaryVal t1, x2 :-> UnaryVal t2, x3 :-> UnaryVal t3

      - A DEFAULT alternative. Just the same, without the bindings for x1,x2,x3

By the end of this pass, we only have unboxed tuples in return positions.
Unboxed sums are completely eliminated, see next note.

Note [Translating unboxed sums to unboxed tuples]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unarise also eliminates unboxed sum binders, and translates unboxed sums in
return positions to unboxed tuples. We want to overlap fields of a sum when
translating it to a tuple to have efficient memory layout. When translating a
sum pattern to a tuple pattern, we need to translate it so that binders of sum
alternatives will be mapped to right arguments after the term translation. So
translation of sum DataCon applications to tuple DataCon applications and
translation of sum patterns to tuple patterns need to be in sync.

These translations work like this. Suppose we have

  (# x1 | | ... #) :: (# t1 | t2 | ... #)

remember that t1, t2 ... can be sums and tuples too. So we first generate
layouts of those. Then we "merge" layouts of each alternative, which gives us a
sum layout with best overlapping possible.

Layout of a flat type 'ty1' is just [ty1].
Layout of a tuple is just concatenation of layouts of its fields.

For layout of a sum type,

  - We first get layouts of all alternatives.
  - We sort these layouts based on their "slot types".
  - We merge all the alternatives.

For example, say we have (# (# Int#, Char #) | (# Int#, Int# #) | Int# #)

  - Layouts of alternatives: [ [Word, LiftedPtr], [Word, Word], [Word] ]
  - Sorted: [ [LiftedPtr, Word], [Word, Word], [Word] ]
  - Merge all alternatives together: [ LiftedPtr, Word, Word ]

We add a slot for the tag to the first position. So our tuple type is

  (# Tag#, Any, Word#, Word# #)
  (we use Any for pointer slots)

Now, any term of this sum type needs to generate a tuple of this type instead.
The translation works by simply putting arguments to first slots that they fit
in. Suppose we had

  (# (# 42#, 'c' #) | | #)

42# fits in Word#, 'c' fits in Any, so we generate this application:

  (# 1#, 'c', 42#, rubbish #)

Another example using the same type: (# | (# 2#, 3# #) | #). 2# fits in Word#,
3# fits in Word #, so we get:

  (# 2#, rubbish, 2#, 3# #).


Note [Don't merge lifted and unlifted slots]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When merging slots, one might be tempted to collapse lifted and unlifted
pointers. However, as seen in #19645, this is wrong. Imagine that you have
the program:

  test :: (# Char | ByteArray# #) -> ByteArray#
  test (# c | #) = doSomething c
  test (# | ba #) = ba

Collapsing the Char and ByteArray# slots would produce STG like:

  test :: forall {t}. (# t | GHC.Prim.ByteArray# #) -> GHC.Prim.ByteArray#
    = {} \r [ (tag :: Int#) (slot0 :: (Any :: Type)) ]
          case tag of tag'
            1# -> doSomething slot0
            2# -> slot0;

Note how `slot0` has a lifted type, despite being bound to an unlifted
ByteArray# in the 2# alternative. This liftedness would cause the code generator to
attempt to enter it upon returning. As unlifted objects do not have entry code,
this causes a runtime crash.

For this reason, Unarise treats unlifted and lifted things as distinct slot
types, despite both being GC pointers. This approach is a slight pessimisation
(since we need to pass more arguments) but appears to be the simplest way to
avoid #19645. Other alternatives considered include:

 a. Giving unlifted objects "trivial" entry code. However, we ultimately
    concluded that the value of the "unlifted things are never entered" invariant
    outweighed the simplicity of this approach.

 b. Annotating occurrences with calling convention information instead of
    relying on the binder's type. This seemed like a very complicated
    way to fix what is ultimately a corner-case.


Note [Representations in StgConApp]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have this unboxed sum term:

  (# 123 | #)

What will be the unboxed tuple representation? We can't tell without knowing the
type of this term. For example, these are all valid tuples for this:

  (# 1#, 123 #)          -- when type is (# Int | String #)
  (# 1#, 123, rubbish #) -- when type is (# Int | Float# #)
  (# 1#, 123, rubbish, rubbish #)
                         -- when type is (# Int | (# Int, Int, Int #) #)

Therefore, in StgConApp we store a list [[PrimRep]] of representations
to decide what layout to use.
Given (# T_1 | ... | T_n #), this list will be
[typePrimRep T_1, ..., typePrimRep T_n].
For example, given type
  (# Int | String #)              we will store [[LiftedRep], [LiftedRep]]
  (# Int | Float# #)              we will store [[LiftedRep], [FloatRep]]
  (# Int | (# Int, Int, Int #) #) we will store [[LiftedRep], [LiftedRep, LiftedRep, LiftedRep]].

This field is used for unboxed sums only and it's an empty list otherwise.
Perhaps it would be more elegant to have a separate StgUnboxedSumCon,
but that would require duplication of code in cases where the logic is shared.

Note that unlifted values can't be let-bound, so we don't need
representations in StgRhsCon.

Note [Casting slot arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this function which selects between Int32# and Int64# from a unboxed sum.

    foo ::  (# Int32# | Int64#  #) -> FD
    foo x = case x of
        (# x1 | #) -> F x1
        (# | x2 #) -> D x2

Naturally we would expect x1 to have a PrimRep of Int32Rep and x2 of DoubleRep.
However we used to generate this (bogus) code after Unarise giving rise to #22208:

    M.foo :: (# GHC.Prim.Int32# | GHC.Prim.Int64# #) -> M.FD
    [GblId, Arity=1, Unf=OtherCon []] =
        {} \r [sum_tag sum_field]
            case sum_tag of tag_gsc {
              __DEFAULT -> M.F [sum_field];
              2# -> M.D [sum_field];
            };

Where sum_field is used both as Int32# and Int64# depending on the branch
because they share the same SlotTy.
This usually works out since we put all int's in the same sort of register.
So even if the reps where wrong (x :: bits32) = (y :: bits64) would produce
correct code in the most cases.
However there are cases where this goes wrong, causing lint errors,in the case of #22208
compiler panics or in some cases incorrect results in the C backend.
For now our solution is to construct proper casts between the PrimRep of the slot and
the variables we want to store in, or read out of these slots.

This means when we have a sum (# Int32# | Int64# #) if we want to store a Int32
we convert it to a Int64 on construction of the tuple value, and convert it back
to a Int32 once when want to use the field. On most backends these coversions should
be no-ops at runtime so this seems reasonable.

Conversion for values coming out of a strict field happen in mapSumIdBinders. While
conversion during the construction of sums happen inside mkUbxSum.

------------- A full example of casting during sum construction ----------------

To compile a constructor application of a unboxed sum of type (# Int32# | Int64# )
in an expression like  `let sum = (# x | #)` we will call mkUbxSum to determine
which binders we have to replace sum with at use sites during unarise.
See also Note [Translating unboxed sums to unboxed tuples].

Int32# and Int64# in this case will share the same slot in the unboxed sum. This means
the sum after unarise will be represented by two binders. One for the tag and one for
the field. The later having Int64Rep.
However our input for the field is of Int32Rep. So in order to soundly construct
`(# x | #) :: (# Int32# | Int64# )` we must upcast `x` to Int64#.
To do this mkUbxSum will produce an expression with a hole for constructor application
to go into. That is the call to mkUbxSum and it's result will look something like:

  >>> mkUbxSum (#|#) [Int32#, Int64#] (x::Int32#) us (x')
  ([1#::Int#, x'::Int64#], \rhs -> case int32ToInt# x of x' -> rhs )

We will use the returned arguments to construct an application to an unboxed tuple:

  >>> mkTuple [tag::Int#, x'::Int64#]
  (# tag, x' #)

Which we will then use as the rhs to pass into the casting wrapper to
construct an expression that casts `x` to the right type before constructing the
tuple

  >>> (\rhs -> case int32ToInt# x of x' -> rhs ) (# tag, x' #)
  case int32ToInt# x of x' -> (# #) 1# x'

Which results in the this definition for `sum` after all is said and done:

  let sum = case int32ToInt# x of { x' -> (# #) 1# x' }

Not that the renaming is not optional. Cmm requires binders of different uniques
to have at least different types. See Note [CorePrep Overview]: 6. Clone all local Ids

------------- A full example of casting during sum matching --------------------

When matching on an unboxed sum constructor we start out with
something like this the pre-unarise:

    f :: (# Int32 | Int64# ) -> ...
    f sum = case sum of
        (# x |#) -> alt_rhs
        ...

We unarise the function arguments and get:

    f sum_tag sum_slot1 = case sum_tag of
        1# -> ???

Now we need to match up the original alternative binders with the sum slots passed
to the function. This is done by mapSumIdBinders which we we call for our
example alternative like this:

    >>> mapSumIdBinders [x] [sum_slot1] alt_rhs env
    (env', alt_rhs')

mapSumIdBinders first matches up the list of binders with the slots passed to
the function which is trivial in this case. Then we check if the slot and the
variable residing inside it agree on their Rep. If alternative binders and
the function arguments agree in their slot reps we we just extend the environment
with a mapping from `x` to `sum_slot1` and we return the rhs as is.

If the reps of the sum_slots do not agree with alternative binders they represent
then we need to wrap the whole RHS in nested cases which cast the sum_slot<n>
variables to the correct rep. Here `x` is of Int32Rep while `sum_slot1` will be
of Int64Rep. This means instead of retuning the original alt_rhs we will return:

  >>> mapSumIdBinders [x] [sum_slot1] alt_rhs env
  ( env'[x=x']
  , case int64ToInt32# (sum_slot1 :: Int64#) of
      (x' :: Int32#) -> alt_rhs
  )

We then run unarise on alt_rhs within that expression, which will replace the first occurrence
of `x` with sum_slot_arg_1 giving us post-unarise:

    f sum_tag sum_slot1 =
      case sum_tag of
        1# -> case int64ToInt32# sum_slot1 of
          x' -> ... x' ...
        ...

Note [UnariseEnv]
~~~~~~~~~~~~~~~~~~
At any variable occurrence 'v',
* If the UnariseEnv has a binding for 'v', the binding says what 'v' is bound to
* If not, 'v' stands just for itself.

Most variables are unaffected by unarisation, and (for efficiency) we don't put
them in the UnariseEnv at all.  But NB: when we go under a binding for 'v' we must
remember to delete 'v' from the UnariseEnv, lest occurrences of 'v' see the outer
binding for the variable (#21396).


Note [UnariseEnv can map to literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To avoid redundant case expressions when unarising unboxed sums, UnariseEnv
needs to map variables to literals too. Suppose we have this Core:

  f (# x | #)

  ==> (CorePrep)

  case (# x | #) of y {
    _ -> f y
  }

  ==> (MultiVal)

  case (# 1#, x #) of [x1, x2] {
    _ -> f x1 x2
  }

To eliminate this case expression we need to map x1 to 1# in UnariseEnv:

  x1 :-> UnaryVal 1#, x2 :-> UnaryVal x

so that `f x1 x2` becomes `f 1# x`.

Note [Unarisation and arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because of unarisation, the arity that will be recorded in the generated info
table for an Id may be larger than the idArity. Instead we record what we call
the RepArity, which is the Arity taking into account any expanded arguments, and
corresponds to the number of (possibly-void) *registers* arguments will arrive
in.

Note [Post-unarisation invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
STG programs after unarisation have these invariants:

 1. No unboxed sums at all.

 2. No unboxed tuple binders. Tuples only appear in return position.

 3. Binders and literals always have zero (for void arguments) or one PrimRep.
    (i.e. typePrimRep1 won't crash; see Note [VoidRep] in GHC.Types.RepType.)

 4. DataCon applications (StgRhsCon and StgConApp) don't have void arguments.
    This means that it's safe to wrap `StgArg`s of DataCon applications with
    `GHC.StgToCmm.Env.NonVoid`, for example.

 5. Alt binders (binders in patterns) are always non-void.
-}

module GHC.Stg.Unarise (unarise) where

import GHC.Prelude

import GHC.Types.Basic
import GHC.Core
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Data.FastString (FastString, mkFastString, fsLit)
import GHC.Types.Id
import GHC.Types.Literal
import GHC.Core.Make (aBSENT_SUM_FIELD_ERROR_ID)
import GHC.Types.Id.Make (voidPrimId, voidArgId)
import GHC.Utils.Monad (mapAccumLM)
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.RepType
import GHC.Stg.Syntax
import GHC.Stg.Utils
import GHC.Stg.Make
import GHC.Core.Type
import GHC.Builtin.Types.Prim (intPrimTy)
import GHC.Builtin.Types
import GHC.Types.Unique.Supply
import GHC.Types.Unique
import GHC.Utils.Misc
import GHC.Types.Var.Env

import Data.Bifunctor (second)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import qualified Data.IntMap as IM
import GHC.Builtin.PrimOps
import GHC.Builtin.PrimOps.Casts
import Data.List (mapAccumL)

-- import GHC.Utils.Trace
--------------------------------------------------------------------------------

-- | A mapping from binders to the Ids they were expanded/renamed to.
--
--   x :-> MultiVal [a,b,c] in rho
--
-- iff  x's typePrimRep is not a singleton, or equivalently
--      x's type is an unboxed tuple, sum or void.
--
--    x :-> UnaryVal x'
--
-- iff x's RepType is UnaryRep or equivalently
--     x's type is not unboxed tuple, sum or void.
--
-- So
--     x :-> MultiVal [a] in rho
-- means x is represented by singleton tuple.
--
--     x :-> MultiVal [] in rho
-- means x is void.
--
-- INVARIANT: OutStgArgs in the range only have NvUnaryTypes
--            (i.e. no unboxed tuples, sums or voids)
--
data UnariseEnv = UnariseEnv
  { ue_rho                 :: (VarEnv UnariseVal)
  , ue_allow_static_conapp :: DataCon -> [StgArg] -> Bool
  }

initUnariseEnv :: VarEnv UnariseVal -> (DataCon -> [StgArg] -> Bool) -> UnariseEnv
initUnariseEnv = UnariseEnv

data UnariseVal
  = MultiVal [OutStgArg] -- MultiVal to tuple. Can be empty list (void).
  | UnaryVal OutStgArg   -- See Note [Renaming during unarisation].

instance Outputable UnariseVal where
  ppr (MultiVal args) = text "MultiVal" <+> ppr args
  ppr (UnaryVal arg)   = text "UnaryVal" <+> ppr arg

-- | Extend the environment, checking the UnariseEnv invariant.
-- The id is mapped to one or more things.
-- See Note [UnariseEnv]
extendRho :: UnariseEnv -> Id -> UnariseVal -> UnariseEnv
extendRho env x (MultiVal args)
  = assert (all (isNvUnaryRep . stgArgRep) args)
    env { ue_rho = extendVarEnv (ue_rho env) x (MultiVal args) }
extendRho env x (UnaryVal val)
  = assert (isNvUnaryRep (stgArgRep val))
    env { ue_rho = extendVarEnv (ue_rho env) x (UnaryVal val) }
-- Properly shadow things from an outer scope.
-- See Note [UnariseEnv]

-- The id stands for itself so we don't record a mapping.
-- See Note [UnariseEnv]
extendRhoWithoutValue :: UnariseEnv -> Id -> UnariseEnv
extendRhoWithoutValue env x = env { ue_rho = delVarEnv (ue_rho env) x }

lookupRho :: UnariseEnv -> Id -> Maybe UnariseVal
lookupRho env v = lookupVarEnv (ue_rho env) v

--------------------------------------------------------------------------------

unarise :: UniqSupply -> (DataCon -> [StgArg] -> Bool) -> [StgTopBinding] -> [StgTopBinding]
unarise us is_dll_con_app binds = initUs_ us (mapM (unariseTopBinding (initUnariseEnv emptyVarEnv is_dll_con_app)) binds)

unariseTopBinding :: UnariseEnv -> StgTopBinding -> UniqSM StgTopBinding
unariseTopBinding rho (StgTopLifted bind)
  = StgTopLifted <$> unariseBinding rho True bind
unariseTopBinding _ bind@StgTopStringLit{} = return bind

unariseBinding :: UnariseEnv -> Bool -> StgBinding -> UniqSM StgBinding
unariseBinding rho top_level (StgNonRec x rhs)
  = StgNonRec x <$> unariseRhs rho top_level rhs
unariseBinding rho top_level (StgRec xrhss)
  = StgRec <$> mapM (\(x, rhs) -> (x,) <$> unariseRhs rho top_level rhs) xrhss

unariseRhs :: UnariseEnv -> Bool -> StgRhs -> UniqSM StgRhs
unariseRhs rho top_level (StgRhsClosure ext ccs update_flag args expr typ)
  = do (rho', args1) <- unariseFunArgBinders rho args
       expr' <- unariseExpr rho' expr
       -- Unarisation can lead to a StgRhsClosure becoming a StgRhsCon.
       -- Hence, we call `mk(Top)StgRhsCon_maybe` rather than just building
       -- another `StgRhsClosure`.
       --
       -- For example with unboxed sums (#25166):
       --
       --     foo = \u [] case (# | _ | #) [(##)] of tag { __DEFAULT -> D [True tag] }
       --
       --  ====> {unarisation}
       --
       --     foo = D [True 2#]
       --
       -- Transforming an appropriate StgRhsClosure into a StgRhsCon is
       -- important as top-level StgRhsCon are statically allocated.
       --
       let mk_rhs = MkStgRhs
            { rhs_args = args1
            , rhs_expr = expr'
            , rhs_type = typ
            , rhs_is_join = update_flag == JumpedTo
            }
       if | top_level
          , Just rhs_con <- mkTopStgRhsCon_maybe (ue_allow_static_conapp rho) mk_rhs
          -> pure rhs_con

          | not top_level
          , Just rhs_con <- mkStgRhsCon_maybe mk_rhs
          -> pure rhs_con

          | otherwise
          -> pure (StgRhsClosure ext ccs update_flag args1 expr' typ)

unariseRhs rho _top (StgRhsCon ccs con mu ts args typ)
  = assert (not (isUnboxedTupleDataCon con || isUnboxedSumDataCon con))
    return (StgRhsCon ccs con mu ts (unariseConArgs rho args) typ)

--------------------------------------------------------------------------------

unariseExpr :: UnariseEnv -> StgExpr -> UniqSM StgExpr

unariseExpr rho e@(StgApp f [])
  = case lookupRho rho f of
      Just (MultiVal args)  -- Including empty tuples
        -> return (mkTuple args)
      Just (UnaryVal (StgVarArg f'))
        -> return (StgApp f' [])
      Just (UnaryVal (StgLitArg f'))
        -> return (StgLit f')
      Nothing
        -> return e

unariseExpr rho e@(StgApp f args)
  = return (StgApp f' (unariseFunArgs rho args))
  where
    f' = case lookupRho rho f of
           Just (UnaryVal (StgVarArg f')) -> f'
           Nothing -> f
           err -> pprPanic "unariseExpr - app2" (pprStgExpr panicStgPprOpts e $$ ppr err)
               -- Can't happen because 'args' is non-empty, and
               -- a tuple or sum cannot be applied to anything

unariseExpr _ (StgLit l)
  = return (StgLit l)

unariseExpr rho (StgConApp dc n args ty_args)
  | isUnboxedSumDataCon dc || isUnboxedTupleDataCon dc
  = do
      us <- getUniqueSupplyM
      case unariseUbxSumOrTupleArgs rho us dc args ty_args of
        (args', Just cast_wrapper)
          -> return $ cast_wrapper (mkTuple args')
        (args', Nothing)
          -> return $ (mkTuple args')
  | otherwise =
      let args' = unariseConArgs rho args in
      return $ (StgConApp dc n args' [])

unariseExpr rho (StgOpApp op args ty)
  = return (StgOpApp op (unariseFunArgs rho args) ty)

unariseExpr rho (StgCase scrut bndr alt_ty alts)
  -- tuple/sum binders in the scrutinee can always be eliminated
  | StgApp v [] <- scrut
  , Just (MultiVal xs) <- lookupRho rho v
  = elimCase rho xs bndr alt_ty alts

  -- Handle strict lets for tuples and sums:
  --   case (# a,b #) of r -> rhs
  -- and analogously for sums
  | StgConApp dc _n args ty_args <- scrut
  , isUnboxedSumDataCon dc || isUnboxedTupleDataCon dc
  = do
    us <- getUniqueSupplyM
    case unariseUbxSumOrTupleArgs rho us dc args ty_args of
      (args',Just wrapper) -> wrapper <$> elimCase rho args' bndr alt_ty alts
      (args',Nothing) -> elimCase rho args' bndr alt_ty alts

  -- See (3) of Note [Rubbish literals] in GHC.Types.Literal
  | StgLit lit <- scrut
  , Just args' <- unariseLiteral_maybe lit
  = elimCase rho args' bndr alt_ty alts

  -- general case
  | otherwise
  = do scrut' <- unariseExpr rho scrut
       alts'  <- unariseAlts rho alt_ty bndr alts
       return (StgCase scrut' bndr alt_ty alts')
                       -- bndr may have a unboxed sum/tuple type but it will be
                       -- dead after unarise (checked in GHC.Stg.Lint)

unariseExpr rho (StgLet ext bind e)
  = StgLet ext <$> unariseBinding rho False bind <*> unariseExpr rho e

unariseExpr rho (StgLetNoEscape ext bind e)
  = StgLetNoEscape ext <$> unariseBinding rho False bind <*> unariseExpr rho e

unariseExpr rho (StgTick tick e)
  = StgTick tick <$> unariseExpr rho e

-- Doesn't return void args.
unariseUbxSumOrTupleArgs :: UnariseEnv -> UniqSupply -> DataCon -> [InStgArg] -> [[PrimRep]]
                   -> ( [OutStgArg]           -- Arguments representing the unboxed sum
                      , Maybe (StgExpr -> StgExpr)) -- Transformation to apply to the arguments, to bring them
                                                    -- into the right Rep
unariseUbxSumOrTupleArgs rho us dc args ty_args
  | isUnboxedTupleDataCon dc
  = (unariseConArgs rho args, Nothing)

  | isUnboxedSumDataCon dc
  , let args1 = assert (isSingleton args) (unariseConArgs rho args)
  = let (args2, cast_wrapper) = mkUbxSum dc ty_args args1 us
    in (args2, Just cast_wrapper)

  | otherwise
  = panic "unariseUbxSumOrTupleArgs: Constructor not a unboxed sum or tuple"

-- Returns @Nothing@ if the given literal is already unary (exactly
-- one PrimRep).  Doesn't return void args.
--
-- This needs to exist because rubbish literals can have any representation.
-- See also Note [Rubbish literals] in GHC.Types.Literal.
unariseLiteral_maybe :: Literal -> Maybe [OutStgArg]
unariseLiteral_maybe (LitRubbish torc rep)
  | [_] <- preps
  = Nothing   -- Single PrimRep. Nothing to do!

  | otherwise -- Multiple reps, or zero. Eliminate via elimCase
  = Just [ StgLitArg (LitRubbish torc (primRepToRuntimeRep prep))
         | prep <- preps ]
  where
    preps = runtimeRepPrimRep (text "unariseLiteral_maybe") rep

unariseLiteral_maybe _ = Nothing

--------------------------------------------------------------------------------

elimCase :: UnariseEnv
         -> [OutStgArg] -- non-void args
         -> InId -> AltType -> [InStgAlt] -> UniqSM OutStgExpr

elimCase rho args bndr (MultiValAlt _) [GenStgAlt{ alt_con   = _
                                                 , alt_bndrs = bndrs
                                                 , alt_rhs   = rhs}]
  = do let rho1 = extendRho rho bndr (MultiVal args)
       (rho2, rhs') <- case () of
           _
             | isUnboxedTupleBndr bndr
             -> return (mapTupleIdBinders bndrs args rho1, rhs)
             | otherwise
             -> assert (isUnboxedSumBndr bndr) $
               case bndrs of
                -- Sum with a void-type binder?
                [] -> return (rho1, rhs)
                [alt_bndr] -> mapSumIdBinders alt_bndr args rhs rho1
                _ -> pprPanic "mapSumIdBinders" (ppr bndrs $$ ppr args)

       unariseExpr rho2 rhs'

elimCase rho args@(tag_arg : real_args) bndr (MultiValAlt _) alts
  | isUnboxedSumBndr bndr
  = do tag_bndr <- mkId (mkFastString "tag") tagTy
          -- this won't be used but we need a binder anyway
       let rho1 = extendRho rho bndr (MultiVal args)
           scrut' = case tag_arg of
                      StgVarArg v     -> StgApp v []
                      StgLitArg l     -> StgLit l

       alts' <- unariseSumAlts rho1 real_args alts
       return (StgCase scrut' tag_bndr tagAltTy alts')

elimCase _ args bndr alt_ty alts
  = pprPanic "elimCase - unhandled case"
      (ppr args <+> ppr bndr <+> ppr alt_ty $$ pprPanicAlts alts)

--------------------------------------------------------------------------------

unariseAlts :: UnariseEnv -> AltType -> InId -> [StgAlt] -> UniqSM [StgAlt]
unariseAlts rho (MultiValAlt n) bndr [GenStgAlt{ alt_con   = DEFAULT
                                               , alt_bndrs = []
                                               , alt_rhs   = e}]
  | isUnboxedTupleBndr bndr
  = do (rho', ys) <- unariseConArgBinder rho bndr
       !e' <- unariseExpr rho' e
       return [GenStgAlt (DataAlt (tupleDataCon Unboxed n)) ys e']

unariseAlts rho (MultiValAlt n) bndr [GenStgAlt{ alt_con   = DataAlt _
                                               , alt_bndrs = ys
                                               , alt_rhs   = e}]
  | isUnboxedTupleBndr bndr
  = do (rho', ys1) <- unariseConArgBinders rho ys
       massert (ys1 `lengthIs` n)
       let rho'' = extendRho rho' bndr (MultiVal (map StgVarArg ys1))
       !e' <- unariseExpr rho'' e
       return [GenStgAlt (DataAlt (tupleDataCon Unboxed n)) ys1 e']

unariseAlts _ (MultiValAlt _) bndr alts
  | isUnboxedTupleBndr bndr
  = pprPanic "unariseExpr: strange multi val alts" (pprPanicAlts alts)

-- In this case we don't need to scrutinize the tag bit
unariseAlts rho (MultiValAlt _) bndr [GenStgAlt{ alt_con    = DEFAULT
                                               , alt_bndrs = []
                                               , alt_rhs   = rhs}]
  | isUnboxedSumBndr bndr
  = do (rho_sum_bndrs, sum_bndrs) <- unariseConArgBinder rho bndr
       rhs' <- unariseExpr rho_sum_bndrs rhs
       return [GenStgAlt (DataAlt (tupleDataCon Unboxed (length sum_bndrs))) sum_bndrs rhs']

unariseAlts rho (MultiValAlt _) bndr alts
  | isUnboxedSumBndr bndr
  = do (rho_sum_bndrs, scrt_bndrs) <- unariseConArgBinder rho bndr
       let tag_bndr:|real_bndrs = expectNonEmpty scrt_bndrs
       alts' <- unariseSumAlts rho_sum_bndrs (map StgVarArg real_bndrs) alts
       let inner_case = StgCase (StgApp tag_bndr []) tag_bndr tagAltTy alts'
       return [GenStgAlt{ alt_con   = DataAlt (tupleDataCon Unboxed (length scrt_bndrs))
                        , alt_bndrs = scrt_bndrs
                        , alt_rhs   = inner_case
                        }]

unariseAlts rho _ _ alts
  = mapM (\alt -> unariseAlt rho alt) alts

unariseAlt :: UnariseEnv -> StgAlt -> UniqSM StgAlt
unariseAlt rho alt@GenStgAlt{alt_con=_,alt_bndrs=xs,alt_rhs=e}
  = do (rho', xs') <- unariseConArgBinders rho xs
       !e' <- unariseExpr rho' e
       return $! alt {alt_bndrs = xs', alt_rhs = e'}

--------------------------------------------------------------------------------

-- | Make alternatives that match on the tag of a sum
-- (i.e. generate LitAlts for the tag)
unariseSumAlts :: UnariseEnv
               -> [StgArg] -- sum components _excluding_ the tag bit.
               -> [StgAlt] -- original alternative with sum LHS
               -> UniqSM [StgAlt]
unariseSumAlts env args alts
  = do alts' <- mapM (unariseSumAlt env args) alts
       return (mkDefaultLitAlt alts')

unariseSumAlt :: UnariseEnv
              -> [StgArg] -- sum components _excluding_ the tag bit.
              -> StgAlt   -- original alternative with sum LHS
              -> UniqSM StgAlt
unariseSumAlt rho _ GenStgAlt{alt_con=DEFAULT,alt_bndrs=_,alt_rhs=e}
  = GenStgAlt DEFAULT mempty <$> unariseExpr rho e

unariseSumAlt rho args alt@GenStgAlt{ alt_con   = DataAlt sumCon
                                , alt_bndrs = bs
                                , alt_rhs   = e
                                }

  = do (rho',e') <- case bs of
              [b] -> mapSumIdBinders b args e rho
              -- Sums must have one binder
              _ -> pprPanic "unariseSumAlt2" (ppr args $$ pprPanicAlt alt)
       let lit_case   = LitAlt (LitNumber LitNumInt (fromIntegral (dataConTag sumCon)))
       GenStgAlt lit_case mempty <$> unariseExpr rho' e'

unariseSumAlt _ scrt alt
  = pprPanic "unariseSumAlt3" (ppr scrt $$ pprPanicAlt alt)

--------------------------------------------------------------------------------
-- Mapping binders when matching und a unboxed sum/tuple

mapTupleIdBinders
  :: [InId]       -- Un-processed binders of a tuple alternative.
                  -- Can have void binders.
  -> [OutStgArg]  -- Arguments that form the tuple (after unarisation).
                  -- Can't have void args.
  -> UnariseEnv
  -> UnariseEnv
mapTupleIdBinders ids args0 rho0
  = assert (not (any (null . stgArgRep) args0)) $
    let
      map_ids :: UnariseEnv -> [Id] -> [StgArg] -> UnariseEnv
      map_ids rho [] _  = rho
      map_ids rho (x : xs) args =
        let
          x_reps = typePrimRep (idType x)
          x_arity = length x_reps
          (x_args, args') =
            assert (args `lengthAtLeast` x_arity)
            splitAt x_arity args

          rho'
            | x_arity == 1
            = assert (x_args `lengthIs` 1)
              extendRho rho x (UnaryVal (head x_args))
            | otherwise
            = extendRho rho x (MultiVal x_args)
        in
          map_ids rho' xs args'
    in
      map_ids rho0 ids args0

mapSumIdBinders
  :: InId        -- Binder (in the case alternative).
  -> [OutStgArg] -- Arguments that form the sum (NOT including the tag).
                 -- Can't have void args.
  -> InStgExpr
  -> UnariseEnv
  -> UniqSM (UnariseEnv, OutStgExpr)

mapSumIdBinders alt_bndr args rhs rho0
  = assert (not (any (null . stgArgRep) args)) $ do
    uss <- listSplitUniqSupply <$> getUniqueSupplyM
    let
      fld_reps = typePrimRep (idType alt_bndr)

      -- Slots representing the whole sum
      arg_slots = map primRepSlot $ concatMap stgArgRep args
      -- The slots representing the field of the sum we bind.
      id_slots  = map primRepSlot $ fld_reps
      layout1   = layoutUbxSum arg_slots id_slots

      -- See Note [Casting slot arguments]
      -- Most of the code here is just to make sure our binders are of the
      -- right type.
      -- Select only the args which contain parts of the current field.
      id_arg_exprs   = [ args !! i | i <- layout1 ]
      id_vars   = [v | StgVarArg v <- id_arg_exprs]

      typed_id_arg_input = assert (equalLength id_vars fld_reps) $
                           zip3 id_vars fld_reps uss

      mkCastInput :: (Id,PrimRep,UniqSupply) -> ([(PrimOp,Type,Unique)],Id,Id)
      mkCastInput (id,rep,bndr_us) =
        let (ops,types) = unzip $ getCasts (typePrimRepU $ idType id) rep
            cst_opts = zip3 ops types $ uniqListFromSupply bndr_us
            out_id = case cst_opts of
              [] -> id
              _ ->  let (_,ty,uq) = last cst_opts
                    in mkCastVar uq ty
        in (cst_opts,id,out_id)

      cast_inputs = map mkCastInput typed_id_arg_input
      (rhs_with_casts,typed_ids) = mapAccumL cast_arg (\x->x) cast_inputs
        where
          cast_arg rhs_in (cast_ops,in_id,out_id) =
            let rhs_out = castArgRename cast_ops (StgVarArg in_id)
            in (rhs_in . rhs_out, out_id)

      typed_id_args = map StgVarArg typed_ids

      -- pprTrace "mapSumIdBinders"
      --           (text "fld_reps" <+> ppr fld_reps $$
      --           text "id_args" <+> ppr id_arg_exprs $$
      --           text "rhs" <+> ppr rhs $$
      --           text "rhs_with_casts" <+> ppr rhs_with_casts
      --           ) $
    if isMultiValBndr alt_bndr
      then return (extendRho rho0 alt_bndr (MultiVal typed_id_args), rhs_with_casts rhs)
      else assert (typed_id_args `lengthIs` 1) $
            return (extendRho rho0 alt_bndr (UnaryVal (head typed_id_args)), rhs_with_casts rhs)

-- Convert the argument to the given type, and wrap the conversion
-- around the given expression. Use the given Id as a name for the
-- converted value.
castArgRename :: [(PrimOp,Type,Unique)] -> StgArg -> StgExpr -> StgExpr
castArgRename ops in_arg rhs =
  case ops of
    [] -> rhs
    ((op,ty,uq):rest_ops) ->
      let out_id' = mkCastVar uq ty -- out_name `setIdUnique` uq `setIdType` ty
          sub_cast = castArgRename rest_ops (StgVarArg out_id')
      in mkCast in_arg op out_id' ty $ sub_cast rhs

-- Construct a case binder used when casting sums, of a given type and unique.
mkCastVar :: Unique -> Type -> Id
mkCastVar uq ty = mkSysLocal (fsLit "cst_sum") uq ManyTy ty

mkCast :: StgArg -> PrimOp -> OutId -> Type -> StgExpr -> StgExpr
mkCast arg_in cast_op out_id out_ty in_rhs =
  let r2 = typePrimRepU out_ty
      scrut = StgOpApp (StgPrimOp cast_op) [arg_in] out_ty
      alt = GenStgAlt { alt_con = DEFAULT, alt_bndrs = [], alt_rhs = in_rhs}
      alt_ty = PrimAlt r2
  in (StgCase scrut out_id alt_ty [alt])

-- | Build a unboxed sum term from arguments of an alternative.
--
-- Example, for (# x | #) :: (# (# #) | Int #) we call
--
--   mkUbxSum (# _ | #) [ [], [LiftedRep] ] [ voidPrimId ]
--
-- which returns
--
--   [ 1#, rubbish ]
--
mkUbxSum
  :: HasDebugCallStack
  => DataCon      -- Sum data con
  -> [[PrimRep]]  -- Representations of type arguments of the sum data con
  -> [OutStgArg]  -- Actual arguments of the alternative.
  -> UniqSupply
  -> ([OutStgArg] -- Final tuple arguments
     ,(StgExpr->StgExpr) -- We might need to cast the args first
     )
mkUbxSum dc ty_args args0 us
  = let
      _ :| sum_slots = ubxSumRepType ty_args
      -- drop tag slot
      field_slots = (mapMaybe (repSlotTy . stgArgRep) args0)
      tag = dataConTag dc
      layout'  = layoutUbxSum sum_slots field_slots

      tag_arg  = StgLitArg (LitNumber LitNumInt (fromIntegral tag))
      arg_idxs = IM.fromList (zipEqual layout' args0)

      ((_idx,_idx_map,_us,wrapper),slot_args)
        = assert (length arg_idxs <= length sum_slots ) $
          mapAccumL mkTupArg (0,arg_idxs,us,id) sum_slots

      mkTupArg  :: (Int, IM.IntMap StgArg,UniqSupply,StgExpr->StgExpr)
                -> SlotTy
                -> ((Int,IM.IntMap StgArg,UniqSupply,StgExpr->StgExpr), StgArg)
      mkTupArg (arg_idx, arg_map, us, wrapper) slot
         | Just stg_arg <- IM.lookup arg_idx arg_map
         =  case castArg us slot stg_arg of
              -- Slot and arg type mismatched, do a cast
              Just (casted_arg,us',wrapper') ->
                ( (arg_idx+1, arg_map, us', wrapper . wrapper')
                , casted_arg)
              -- Use the arg as-is
              Nothing ->
                ( (arg_idx+1, arg_map, us, wrapper)
                , stg_arg)
         -- Garbage slot, fill with rubbish
         | otherwise
         =  ( (arg_idx+1, arg_map, us, wrapper)
            , ubxSumRubbishArg slot)

      castArg :: UniqSupply -> SlotTy -> StgArg -> Maybe (StgArg,UniqSupply,StgExpr -> StgExpr)
      castArg us slot_ty arg
        -- Cast the argument to the type of the slot if required
        | slotPrimRep slot_ty /= stgArgRepU arg
        , (ops,types) <- unzip $ getCasts (stgArgRepU arg) $ slotPrimRep slot_ty
        , not . null $ ops
        = let (us1,us2) = splitUniqSupply us
              cast_uqs = uniqListFromSupply us1
              cast_opts = zip3 ops types cast_uqs
              (_op,out_ty,out_uq) = last cast_opts
              casts = castArgRename cast_opts arg :: StgExpr -> StgExpr
          in Just (StgVarArg (mkCastVar out_uq out_ty),us2,casts)
        -- No need for casting
        | otherwise = Nothing

      tup_args = tag_arg : slot_args
    in
      -- pprTrace "mkUbxSum" (
      --   text "ty_args (slots)" <+> ppr ty_args $$
      --   text "args0" <+> ppr args0 $$
      --   text "wrapper" <+>
      --       (ppr $ wrapper $ StgLit $ LitChar '_'))
      (tup_args, wrapper)


-- | Return a rubbish value for the given slot type.
--
-- We use the following rubbish values:
--    * Literals: 0 or 0.0
--    * Pointers: `ghc-prim:GHC.Prim.Panic.absentSumFieldError`
--
-- See Note [aBSENT_SUM_FIELD_ERROR_ID] in "GHC.Core.Make"
--
ubxSumRubbishArg :: SlotTy -> StgArg
ubxSumRubbishArg PtrLiftedSlot   = StgVarArg aBSENT_SUM_FIELD_ERROR_ID
ubxSumRubbishArg PtrUnliftedSlot = StgVarArg aBSENT_SUM_FIELD_ERROR_ID
ubxSumRubbishArg WordSlot        = StgLitArg (LitNumber LitNumWord 0)
ubxSumRubbishArg Word64Slot      = StgLitArg (LitNumber LitNumWord64 0)
ubxSumRubbishArg FloatSlot       = StgLitArg (LitFloat 0)
ubxSumRubbishArg DoubleSlot      = StgLitArg (LitDouble 0)
ubxSumRubbishArg (VecSlot n e)   = StgLitArg (LitRubbish TypeLike vec_rep)
  where vec_rep = primRepToRuntimeRep (VecRep n e)

--------------------------------------------------------------------------------

{-
Note [Unarisation of Void binders and arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For arguments (StgArg) and binders (Id) we have two kind of unarisation:

  - When unarising function arg binders and arguments, we don't want to remove
    void binders and arguments. For example,

      f :: (# (# #), (# #) #) -> Void# -> State# RealWorld -> ...
      f x y z = <body>

    Here after unarise we should still get a function with arity 3. Similarly
    in the call site we shouldn't remove void arguments:

      f (# (# #), (# #) #) void# realWorld#

    When unarising <body>, we extend the environment with these binders:

      x :-> MultiVal [], y :-> MultiVal [], z :-> MultiVal []

    Because their rep types are `MultiRep []` (aka. void). This means that when
    we see `x` in a function argument position, we actually replace it with a
    void argument. When we see it in a DataCon argument position, we just get
    rid of it, because DataCon applications in STG are always saturated.

  - When unarising case alternative binders we remove void binders, but we
    still update the environment the same way, because those binders may be
    used in the RHS. Example:

      case x of y {
        (# x1, x2, x3 #) -> <RHS>
      }

    We know that y can't be void, because we don't scrutinize voids, so x will
    be unarised to some number of arguments, and those arguments will have at
    least one non-void thing. So in the rho we will have something like:

      x :-> MultiVal [xu1, xu2]

    Now, after we eliminate void binders in the pattern, we get exactly the same
    number of binders, and extend rho again with these:

      x1 :-> UnaryVal xu1
      x2 :-> MultiVal [] -- x2 is void
      x3 :-> UnaryVal xu2

    Now when we see x2 in a function argument position or in return position, we
    generate void#. In constructor argument position, we just remove it.

So in short, when we have a void id,

  - We keep it if it's a lambda argument binder or
                       in argument position of an application.

  - We remove it if it's a DataCon field binder or
                         in argument position of a DataCon application.
-}

unariseArgBinder
    :: Bool -- data con arg?
    -> UnariseEnv -> Id -> UniqSM (UnariseEnv, [Id])
unariseArgBinder is_con_arg rho x =
  case typePrimRep (idType x) of
    []
      | is_con_arg
      -> return (extendRho rho x (MultiVal []), [])
      | otherwise -- fun arg, do not remove void binders
      -> return (extendRho rho x (MultiVal []), [voidArgId])

    [rep]
      -- Arg represented as single variable, but original type may still be an
      -- unboxed sum/tuple, e.g. (# Void# | Void# #).
      --
      -- While not unarising the binder in this case does not break any programs
      -- (because it unarises to a single variable), it triggers StgLint as we
      -- break the post-unarisation invariant that says unboxed tuple/sum
      -- binders should vanish. See Note [Post-unarisation invariants].
      | isUnboxedSumType (idType x) || isUnboxedTupleType (idType x)
      -> do x' <- mkId (mkFastString "us") (primRepToType rep)
            return (extendRho rho x (MultiVal [StgVarArg x']), [x'])
      | otherwise
      -> return (extendRhoWithoutValue rho x, [x])

    reps -> do
      xs <- mkIds (mkFastString "us") (map primRepToType reps)
      return (extendRho rho x (MultiVal (map StgVarArg xs)), xs)

--------------------------------------------------------------------------------

-- | MultiVal a function argument. Never returns an empty list.
unariseFunArg :: UnariseEnv -> StgArg -> [StgArg]
unariseFunArg rho (StgVarArg x) =
  case lookupRho rho x of
    Just (MultiVal [])  -> [voidArg]   -- NB: do not remove void args
    Just (MultiVal as)  -> as
    Just (UnaryVal arg) -> [arg]
    Nothing             -> [StgVarArg x]
unariseFunArg _ arg@(StgLitArg lit) = case unariseLiteral_maybe lit of
  -- forgetting to unariseLiteral_maybe here caused #23914
  Just [] -> [voidArg]
  Just as -> as
  Nothing -> [arg]

unariseFunArgs :: UnariseEnv -> [StgArg] -> [StgArg]
unariseFunArgs = concatMap . unariseFunArg

unariseFunArgBinders :: UnariseEnv -> [Id] -> UniqSM (UnariseEnv, [Id])
unariseFunArgBinders rho xs = second concat <$> mapAccumLM unariseFunArgBinder rho xs

-- Result list of binders is never empty
unariseFunArgBinder :: UnariseEnv -> Id -> UniqSM (UnariseEnv, [Id])
unariseFunArgBinder = unariseArgBinder False

--------------------------------------------------------------------------------

-- | MultiVal a DataCon argument. Returns an empty list when argument is void.
unariseConArg :: UnariseEnv -> InStgArg -> [OutStgArg]
unariseConArg rho (StgVarArg x) =
  case lookupRho rho x of
    Just (UnaryVal arg) -> [arg]
    Just (MultiVal as) -> as      -- 'as' can be empty
    Nothing
      | isZeroBitTy (idType x) -> [] -- e.g. C realWorld#
                                     -- Here realWorld# is not in the envt, but
                                     -- is a void, and so should be eliminated
      | otherwise -> [StgVarArg x]
unariseConArg _ arg@(StgLitArg lit)
  | Just as <- unariseLiteral_maybe lit
  = as
  | otherwise
  = assert (isNvUnaryRep (typePrimRep (literalType lit))) -- We have no non-rubbish non-unary literals
    [arg]

unariseConArgs :: UnariseEnv -> [InStgArg] -> [OutStgArg]
unariseConArgs = concatMap . unariseConArg

unariseConArgBinders :: UnariseEnv -> [Id] -> UniqSM (UnariseEnv, [Id])
unariseConArgBinders rho xs = second concat <$> mapAccumLM unariseConArgBinder rho xs

-- Different from `unariseFunArgBinder`: result list of binders may be empty.
-- See DataCon applications case in Note [Post-unarisation invariants].
unariseConArgBinder :: UnariseEnv -> Id -> UniqSM (UnariseEnv, [Id])
unariseConArgBinder = unariseArgBinder True

--------------------------------------------------------------------------------

mkIds :: FastString -> [NvUnaryType] -> UniqSM [Id]
mkIds fs tys = mkUnarisedIds fs tys

mkId :: FastString -> NvUnaryType -> UniqSM Id
mkId s t = mkUnarisedId s t

isMultiValBndr :: Id -> Bool
isMultiValBndr id
  | [_] <- typePrimRep (idType id)
  = False
  | otherwise
  = True

isUnboxedSumBndr :: Id -> Bool
isUnboxedSumBndr = isUnboxedSumType . idType

isUnboxedTupleBndr :: Id -> Bool
isUnboxedTupleBndr = isUnboxedTupleType . idType

mkTuple :: [StgArg] -> StgExpr
mkTuple args = StgConApp (tupleDataCon Unboxed (length args)) NoNumber args []

tagAltTy :: AltType
tagAltTy = PrimAlt IntRep

tagTy :: Type
tagTy = intPrimTy

voidArg :: StgArg
voidArg = StgVarArg voidPrimId

mkDefaultLitAlt :: [StgAlt] -> [StgAlt]
-- We have an exhaustive list of literal alternatives
--    1# -> e1
--    2# -> e2
-- Since they are exhaustive, we can replace one with DEFAULT, to avoid
-- generating a final test. Remember, the DEFAULT comes first if it exists.
mkDefaultLitAlt [] = pprPanic "elimUbxSumExpr.mkDefaultAlt" (text "Empty alts")
mkDefaultLitAlt alts@(GenStgAlt{alt_con=DEFAULT,alt_bndrs=_,alt_rhs=_} : _)   = alts
mkDefaultLitAlt (alt@GenStgAlt{alt_con=LitAlt{}, alt_bndrs=[]} : alts) = alt {alt_con = DEFAULT} : alts
mkDefaultLitAlt alts = pprPanic "mkDefaultLitAlt" (text "Not a lit alt:" <+> pprPanicAlts alts)

pprPanicAlts :: OutputablePass pass => [GenStgAlt pass] -> SDoc
pprPanicAlts alts = ppr (map pprPanicAlt alts)

pprPanicAlt :: OutputablePass pass => GenStgAlt pass -> SDoc
pprPanicAlt GenStgAlt{alt_con=c,alt_bndrs=b,alt_rhs=e} = ppr (c,b,pprStgExpr panicStgPprOpts e)
