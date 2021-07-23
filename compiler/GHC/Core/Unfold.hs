{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998


Core-syntax unfoldings

Unfoldings (which can travel across module boundaries) are in Core
syntax (namely @CoreExpr@s).

The type @Unfolding@ sits ``above'' simply-Core-expressions
unfoldings, capturing ``higher-level'' things we know about a binding,
usually things that the simplifier found out (e.g., ``it's a
literal'').  In the corner of a @CoreUnfolding@ unfolding, you will
find, unsurprisingly, a Core expression.
-}


{-# LANGUAGE BangPatterns #-}

module GHC.Core.Unfold (
        Unfolding, UnfoldingGuidance,   -- Abstract types

        UnfoldingOpts (..), defaultUnfoldingOpts,
        updateCreationThreshold, updateUseThreshold,
        updateFunAppDiscount, updateDictDiscount,
        updateVeryAggressive, updateCaseScaling,
        updateCaseThreshold, updateReportPrefix,

        ArgSummary(..),

        couldBeSmallEnoughToInline, inlineBoringOk,
        smallEnoughToInline,

        callSiteInline, CallCtxt(..),
        calcUnfoldingGuidance
    ) where

import GHC.Prelude

import GHC.Driver.Flags

import GHC.Core
import GHC.Core.Utils
import GHC.Types.Id
import GHC.Core.DataCon
import GHC.Types.Literal
import GHC.Builtin.PrimOps
import GHC.Types.Id.Info
import GHC.Types.Basic  ( Arity, RecFlag(..) )
import GHC.Core.Type
import GHC.Builtin.Names
import GHC.Builtin.Types.Prim ( realWorldStatePrimTy )
import GHC.Data.Bag
import GHC.Utils.Logger
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Types.ForeignCall
import GHC.Types.Name
import GHC.Types.Tickish

import qualified Data.ByteString as BS
import Data.List (isPrefixOf)


-- | Unfolding options
data UnfoldingOpts = UnfoldingOpts
   { unfoldingCreationThreshold :: !Int
      -- ^ Threshold above which unfoldings are not *created*

   , unfoldingUseThreshold :: !Int
      -- ^ Threshold above which unfoldings are not *inlined*

   , unfoldingFunAppDiscount :: !Int
      -- ^ Discount for lambdas that are used (applied)

   , unfoldingDictDiscount :: !Int
      -- ^ Discount for dictionaries

   , unfoldingVeryAggressive :: !Bool
      -- ^ Force inlining in many more cases

   , unfoldingCaseThreshold :: !Int
      -- ^ Don't consider depth up to x

   , unfoldingCaseScaling :: !Int
      -- ^ Penalize depth with 1/x

   , unfoldingReportPrefix :: !(Maybe String)
      -- ^ Only report inlining decisions for names with this prefix
   }

defaultUnfoldingOpts :: UnfoldingOpts
defaultUnfoldingOpts = UnfoldingOpts
   { unfoldingCreationThreshold = 750
      -- The unfoldingCreationThreshold threshold must be reasonably high
      -- to take account of possible discounts.
      -- E.g. 450 is not enough in 'fulsom' for Interval.sqr to
      -- inline into Csg.calc (The unfolding for sqr never makes it
      -- into the interface file.)

   , unfoldingUseThreshold   = 90
      -- Last adjusted upwards in #18282, when I reduced
      -- the result discount for constructors.

   , unfoldingFunAppDiscount = 60
      -- Be fairly keen to inline a function if that means
      -- we'll be able to pick the right method from a dictionary

   , unfoldingDictDiscount   = 30
      -- Be fairly keen to inline a function if that means
      -- we'll be able to pick the right method from a dictionary

   , unfoldingVeryAggressive = False

      -- Only apply scaling once we are deeper than threshold cases
      -- in an RHS.
   , unfoldingCaseThreshold = 2

      -- Penalize depth with (size*depth)/scaling
   , unfoldingCaseScaling = 30

      -- Don't filter inlining decision reports
   , unfoldingReportPrefix = Nothing
   }

-- Helpers for "GHC.Driver.Session"

updateCreationThreshold :: Int -> UnfoldingOpts -> UnfoldingOpts
updateCreationThreshold n opts = opts { unfoldingCreationThreshold = n }

updateUseThreshold :: Int -> UnfoldingOpts -> UnfoldingOpts
updateUseThreshold n opts = opts { unfoldingUseThreshold = n }

updateFunAppDiscount :: Int -> UnfoldingOpts -> UnfoldingOpts
updateFunAppDiscount n opts = opts { unfoldingFunAppDiscount = n }

updateDictDiscount :: Int -> UnfoldingOpts -> UnfoldingOpts
updateDictDiscount n opts = opts { unfoldingDictDiscount = n }

updateVeryAggressive :: Bool -> UnfoldingOpts -> UnfoldingOpts
updateVeryAggressive n opts = opts { unfoldingVeryAggressive = n }


updateCaseThreshold :: Int -> UnfoldingOpts -> UnfoldingOpts
updateCaseThreshold n opts = opts { unfoldingCaseThreshold = n }

updateCaseScaling :: Int -> UnfoldingOpts -> UnfoldingOpts
updateCaseScaling n opts = opts { unfoldingCaseScaling = n }

updateReportPrefix :: Maybe String -> UnfoldingOpts -> UnfoldingOpts
updateReportPrefix n opts = opts { unfoldingReportPrefix = n }

{-
Note [Occurrence analysis of unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do occurrence-analysis of unfoldings once and for all, when the
unfolding is built, rather than each time we inline them.

But given this decision it's vital that we do
*always* do it.  Consider this unfolding
    \x -> letrec { f = ...g...; g* = f } in body
where g* is (for some strange reason) the loop breaker.  If we don't
occ-anal it when reading it in, we won't mark g as a loop breaker, and
we may inline g entirely in body, dropping its binding, and leaving
the occurrence in f out of scope. This happened in #8892, where
the unfolding in question was a DFun unfolding.

But more generally, the simplifier is designed on the
basis that it is looking at occurrence-analysed expressions, so better
ensure that they actually are.

Note [Calculate unfolding guidance on the non-occ-anal'd expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that we give the non-occur-analysed expression to
calcUnfoldingGuidance.  In some ways it'd be better to occur-analyse
first; for example, sometimes during simplification, there's a large
let-bound thing which has been substituted, and so is now dead; so
'expr' contains two copies of the thing while the occurrence-analysed
expression doesn't.

Nevertheless, we *don't* and *must not* occ-analyse before computing
the size because

a) The size computation bales out after a while, whereas occurrence
   analysis does not.

b) Residency increases sharply if you occ-anal first.  I'm not
   100% sure why, but it's a large effect.  Compiling Cabal went
   from residency of 534M to over 800M with this one change.

This can occasionally mean that the guidance is very pessimistic;
it gets fixed up next round.  And it should be rare, because large
let-bound things that are dead are usually caught by preInlineUnconditionally


************************************************************************
*                                                                      *
\subsection{The UnfoldingGuidance type}
*                                                                      *
************************************************************************
-}

inlineBoringOk :: CoreExpr -> Bool
-- See Note [INLINE for small functions]
-- True => the result of inlining the expression is
--         no bigger than the expression itself
--     eg      (\x y -> f y x)
-- This is a quick and dirty version. It doesn't attempt
-- to deal with  (\x y z -> x (y z))
-- The really important one is (x `cast` c)
inlineBoringOk e
  = go 0 e
  where
    go :: Int -> CoreExpr -> Bool
    go credit (Lam x e) | isId x           = go (credit+1) e
                        | otherwise        = go credit e
        -- See Note [Count coercion arguments in boring contexts]
    go credit (App f (Type {}))            = go credit f
    go credit (App f a) | credit > 0
                        , exprIsTrivial a  = go (credit-1) f
    go credit (Tick _ e)                   = go credit e -- dubious
    go credit (Cast e _)                   = go credit e
    go credit (Case scrut _ _ [Alt _ _ rhs]) -- See Note [Inline unsafeCoerce]
      | isUnsafeEqualityProof scrut        = go credit rhs
    go _      (Var {})                     = boringCxtOk
    go _      _                            = boringCxtNotOk

calcUnfoldingGuidance
        :: UnfoldingOpts
        -> Bool          -- Definitely a top-level, bottoming binding
        -> CoreExpr      -- Expression to look at
        -> UnfoldingGuidance
calcUnfoldingGuidance opts is_top_bottoming (Tick t expr)
  | not (tickishIsCode t)  -- non-code ticks don't matter for unfolding
  = calcUnfoldingGuidance opts is_top_bottoming expr
calcUnfoldingGuidance opts is_top_bottoming expr
  = case sizeExpr opts bOMB_OUT_SIZE val_bndrs body of
      TooBig -> UnfNever
      SizeIs size cased_bndrs scrut_discount
        | uncondInline expr n_val_bndrs size
        -> UnfWhen { ug_unsat_ok = unSaturatedOk
                   , ug_boring_ok =  boringCxtOk
                   , ug_arity = n_val_bndrs }   -- Note [INLINE for small functions]

        | is_top_bottoming
        -> UnfNever   -- See Note [Do not inline top-level bottoming functions]

        | otherwise
        -> UnfIfGoodArgs { ug_args  = map (mk_discount cased_bndrs) val_bndrs
                         , ug_size  = size
                         , ug_res   = scrut_discount }

  where
    (bndrs, body) = collectBinders expr
    bOMB_OUT_SIZE = unfoldingCreationThreshold opts
           -- Bomb out if size gets bigger than this
    val_bndrs   = filter isId bndrs
    n_val_bndrs = length val_bndrs

    mk_discount :: Bag (Id,Int) -> Id -> Int
    mk_discount cbs bndr = foldl' combine 0 cbs
           where
             combine acc (bndr', disc)
               | bndr == bndr' = acc `plus_disc` disc
               | otherwise     = acc

             plus_disc :: Int -> Int -> Int
             plus_disc | isFunTy (idType bndr) = max
                       | otherwise             = (+)
             -- See Note [Function and non-function discounts]

{- Note [Inline unsafeCoerce]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We really want to inline unsafeCoerce, even when applied to boring
arguments.  It doesn't look as if its RHS is smaller than the call
   unsafeCoerce x = case unsafeEqualityProof @a @b of UnsafeRefl -> x
but that case is discarded -- see Note [Implementing unsafeCoerce]
in base:Unsafe.Coerce.

Moreover, if we /don't/ inline it, we may be left with
          f (unsafeCoerce x)
which will build a thunk -- bad, bad, bad.

Conclusion: we really want inlineBoringOk to be True of the RHS of
unsafeCoerce.  This is (U4) in Note [Implementing unsafeCoerce].

Note [Computing the size of an expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic idea of sizeExpr is obvious enough: count nodes.  But getting the
heuristics right has taken a long time.  Here's the basic strategy:

    * Variables, literals: 0
      (Exception for string literals, see litSize.)

    * Function applications (f e1 .. en): 1 + #value args

    * Constructor applications: 1, regardless of #args

    * Let(rec): 1 + size of components

    * Note, cast: 0

Examples

  Size  Term
  --------------
    0     42#
    0     x
    0     True
    2     f x
    1     Just x
    4     f (g x)

Notice that 'x' counts 0, while (f x) counts 2.  That's deliberate: there's
a function call to account for.  Notice also that constructor applications
are very cheap, because exposing them to a caller is so valuable.

[25/5/11] All sizes are now multiplied by 10, except for primops
(which have sizes like 1 or 4.  This makes primops look fantastically
cheap, and seems to be almost universally beneficial.  Done partly as a
result of #4978.

Note [Do not inline top-level bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The FloatOut pass has gone to some trouble to float out calls to 'error'
and similar friends.  See Note [Bottoming floats] in GHC.Core.Opt.SetLevels.
Do not re-inline them!  But we *do* still inline if they are very small
(the uncondInline stuff).

Note [INLINE for small functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider        {-# INLINE f #-}
                f x = Just x
                g y = f y
Then f's RHS is no larger than its LHS, so we should inline it into
even the most boring context.  In general, f the function is
sufficiently small that its body is as small as the call itself, the
inline unconditionally, regardless of how boring the context is.

Things to note:

(1) We inline *unconditionally* if inlined thing is smaller (using sizeExpr)
    than the thing it's replacing.  Notice that
      (f x) --> (g 3)             -- YES, unconditionally
      (f x) --> x : []            -- YES, *even though* there are two
                                  --      arguments to the cons
      x     --> g 3               -- NO
      x     --> Just v            -- NO

    It's very important not to unconditionally replace a variable by
    a non-atomic term.

(2) We do this even if the thing isn't saturated, else we end up with the
    silly situation that
       f x y = x
       ...map (f 3)...
    doesn't inline.  Even in a boring context, inlining without being
    saturated will give a lambda instead of a PAP, and will be more
    efficient at runtime.

(3) However, when the function's arity > 0, we do insist that it
    has at least one value argument at the call site.  (This check is
    made in the UnfWhen case of callSiteInline.) Otherwise we find this:
         f = /\a \x:a. x
         d = /\b. MkD (f b)
    If we inline f here we get
         d = /\b. MkD (\x:b. x)
    and then prepareRhs floats out the argument, abstracting the type
    variables, so we end up with the original again!

(4) We must be much more cautious about arity-zero things. Consider
       let x = y +# z in ...
    In *size* terms primops look very small, because the generate a
    single instruction, but we do not want to unconditionally replace
    every occurrence of x with (y +# z).  So we only do the
    unconditional-inline thing for *trivial* expressions.

    NB: you might think that PostInlineUnconditionally would do this
    but it doesn't fire for top-level things; see GHC.Core.Opt.Simplify.Utils
    Note [Top level and postInlineUnconditionally]

Note [Count coercion arguments in boring contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In inlineBoringOK, we ignore type arguments when deciding whether an
expression is okay to inline into boring contexts. This is good, since
if we have a definition like

  let y = x @Int in f y y

there’s no reason not to inline y at both use sites — no work is
actually duplicated. It may seem like the same reasoning applies to
coercion arguments, and indeed, in #17182 we changed inlineBoringOK to
treat coercions the same way.

However, this isn’t a good idea: unlike type arguments, which have
no runtime representation, coercion arguments *do* have a runtime
representation (albeit the zero-width VoidRep, see Note [Coercion tokens]
in "GHC.CoreToStg"). This caused trouble in #17787 for DataCon wrappers for
nullary GADT constructors: the wrappers would be inlined and each use of
the constructor would lead to a separate allocation instead of just
sharing the wrapper closure.

The solution: don’t ignore coercion arguments after all.
-}

uncondInline :: CoreExpr -> Arity -> Int -> Bool
-- Inline unconditionally if there no size increase
-- Size of call is arity (+1 for the function)
-- See Note [INLINE for small functions]
uncondInline rhs arity size
  | arity > 0 = size <= 10 * (arity + 1) -- See Note [INLINE for small functions] (1)
  | otherwise = exprIsTrivial rhs        -- See Note [INLINE for small functions] (4)

sizeExpr :: UnfoldingOpts
         -> Int             -- Bomb out if it gets bigger than this
         -> [Id]            -- Arguments; we're interested in which of these
                            -- get case'd
         -> CoreExpr
         -> ExprSize

-- Note [Computing the size of an expression]

-- Forcing bOMB_OUT_SIZE early prevents repeated
-- unboxing of the Int argument.
sizeExpr opts !bOMB_OUT_SIZE top_args expr
  = size_up expr
  where
    size_up (Cast e _) = size_up e
    size_up (Tick _ e) = size_up e
    size_up (Type _)   = sizeZero           -- Types cost nothing
    size_up (Coercion _) = sizeZero
    size_up (Lit lit)  = sizeN (litSize lit)
    size_up (Var f) | isRealWorldId f = sizeZero
                      -- Make sure we get constructor discounts even
                      -- on nullary constructors
                    | otherwise       = size_up_call f [] 0

    size_up (App fun arg)
      | isTyCoArg arg = size_up fun
      | otherwise     = size_up arg  `addSizeNSD`
                        size_up_app fun [arg] (if isRealWorldExpr arg then 1 else 0)

    size_up (Lam b e)
      | isId b && not (isRealWorldId b) = lamScrutDiscount opts (size_up e `addSizeN` 10)
      | otherwise = size_up e

    size_up (Let (NonRec binder rhs) body)
      = size_up_rhs (binder, rhs) `addSizeNSD`
        size_up body              `addSizeN`
        size_up_alloc binder

    size_up (Let (Rec pairs) body)
      = foldr (addSizeNSD . size_up_rhs)
              (size_up body `addSizeN` sum (map (size_up_alloc . fst) pairs))
              pairs

    size_up (Case e _ _ alts)
        | null alts
        = size_up e    -- case e of {} never returns, so take size of scrutinee

    size_up (Case e _ _ alts)
        -- Now alts is non-empty
        | Just v <- is_top_arg e -- We are scrutinising an argument variable
        = let
            alt_sizes = map size_up_alt alts

                  -- alts_size tries to compute a good discount for
                  -- the case when we are scrutinising an argument variable
            alts_size (SizeIs tot tot_disc tot_scrut)
                          -- Size of all alternatives
                      (SizeIs max _        _)
                          -- Size of biggest alternative
                  = SizeIs tot (unitBag (v, 20 + tot - max)
                      `unionBags` tot_disc) tot_scrut
                          -- If the variable is known, we produce a
                          -- discount that will take us back to 'max',
                          -- the size of the largest alternative The
                          -- 1+ is a little discount for reduced
                          -- allocation in the caller
                          --
                          -- Notice though, that we return tot_disc,
                          -- the total discount from all branches.  I
                          -- think that's right.

            alts_size tot_size _ = tot_size
          in
          alts_size (foldr1 addAltSize alt_sizes)  -- alts is non-empty
                    (foldr1 maxSize    alt_sizes)
                -- Good to inline if an arg is scrutinised, because
                -- that may eliminate allocation in the caller
                -- And it eliminates the case itself
        where
          is_top_arg (Var v) | v `elem` top_args = Just v
          is_top_arg (Cast e _) = is_top_arg e
          is_top_arg _ = Nothing


    size_up (Case e _ _ alts) = size_up e  `addSizeNSD`
                                foldr (addAltSize . size_up_alt) case_size alts
      where
          case_size
           | is_inline_scrut e, lengthAtMost alts 1 = sizeN (-10)
           | otherwise = sizeZero
                -- Normally we don't charge for the case itself, but
                -- we charge one per alternative (see size_up_alt,
                -- below) to account for the cost of the info table
                -- and comparisons.
                --
                -- However, in certain cases (see is_inline_scrut
                -- below), no code is generated for the case unless
                -- there are multiple alts.  In these cases we
                -- subtract one, making the first alt free.
                -- e.g. case x# +# y# of _ -> ...   should cost 1
                --      case touch# x# of _ -> ...  should cost 0
                -- (see #4978)
                --
                -- I would like to not have the "lengthAtMost alts 1"
                -- condition above, but without that some programs got worse
                -- (spectral/hartel/event and spectral/para).  I don't fully
                -- understand why. (SDM 24/5/11)

                -- unboxed variables, inline primops and unsafe foreign calls
                -- are all "inline" things:
          is_inline_scrut (Var v) = isUnliftedType (idType v)
          is_inline_scrut scrut
              | (Var f, _) <- collectArgs scrut
                = case idDetails f of
                    FCallId fc    -> not (isSafeForeignCall fc)
                    PrimOpId op _ -> not (primOpOutOfLine op)
                    _other        -> False
              | otherwise
                = False

    size_up_rhs (bndr, rhs)
      | Just join_arity <- isJoinId_maybe bndr
        -- Skip arguments to join point
      , (_bndrs, body) <- collectNBinders join_arity rhs
      = size_up body
      | otherwise
      = size_up rhs

    ------------
    -- size_up_app is used when there's ONE OR MORE value args
    size_up_app (App fun arg) args voids
        | isTyCoArg arg                  = size_up_app fun args voids
        | isRealWorldExpr arg            = size_up_app fun (arg:args) (voids + 1)
        | otherwise                      = size_up arg  `addSizeNSD`
                                           size_up_app fun (arg:args) voids
    size_up_app (Var fun)     args voids = size_up_call fun args voids
    size_up_app (Tick _ expr) args voids = size_up_app expr args voids
    size_up_app (Cast expr _) args voids = size_up_app expr args voids
    size_up_app other         args voids = size_up other `addSizeN`
                                           callSize (length args) voids
       -- if the lhs is not an App or a Var, or an invisible thing like a
       -- Tick or Cast, then we should charge for a complete call plus the
       -- size of the lhs itself.

    ------------
    size_up_call :: Id -> [CoreExpr] -> Int -> ExprSize
    size_up_call fun val_args voids
       = case idDetails fun of
           FCallId _        -> sizeN (callSize (length val_args) voids)
           DataConWorkId dc -> conSize    dc (length val_args)
           PrimOpId op _    -> primOpSize op (length val_args)
           ClassOpId _      -> classOpSize opts top_args val_args
           _                -> funSize opts top_args fun (length val_args) voids

    ------------
    size_up_alt (Alt _con _bndrs rhs) = size_up rhs `addSizeN` 10
        -- Don't charge for args, so that wrappers look cheap
        -- (See comments about wrappers with Case)
        --
        -- IMPORTANT: *do* charge 1 for the alternative, else we
        -- find that giant case nests are treated as practically free
        -- A good example is Foreign.C.Error.errnoToIOError

    ------------
    -- Cost to allocate binding with given binder
    size_up_alloc bndr
      |  isTyVar bndr                 -- Doesn't exist at runtime
      || isJoinId bndr                -- Not allocated at all
      || isUnliftedType (idType bndr) -- Doesn't live in heap
      = 0
      | otherwise
      = 10

    ------------
        -- These addSize things have to be here because
        -- I don't want to give them bOMB_OUT_SIZE as an argument
    addSizeN TooBig          _  = TooBig
    addSizeN (SizeIs n xs d) m  = mkSizeIs bOMB_OUT_SIZE (n + m) xs d

        -- addAltSize is used to add the sizes of case alternatives
    addAltSize TooBig            _      = TooBig
    addAltSize _                 TooBig = TooBig
    addAltSize (SizeIs n1 xs d1) (SizeIs n2 ys d2)
        = mkSizeIs bOMB_OUT_SIZE (n1 + n2)
                                 (xs `unionBags` ys)
                                 (d1 + d2) -- Note [addAltSize result discounts]

        -- This variant ignores the result discount from its LEFT argument
        -- It's used when the second argument isn't part of the result
    addSizeNSD TooBig            _      = TooBig
    addSizeNSD _                 TooBig = TooBig
    addSizeNSD (SizeIs n1 xs _) (SizeIs n2 ys d2)
        = mkSizeIs bOMB_OUT_SIZE (n1 + n2)
                                 (xs `unionBags` ys)
                                 d2  -- Ignore d1

    isRealWorldId id = idType id `eqType` realWorldStatePrimTy

    -- an expression of type State# RealWorld must be a variable
    isRealWorldExpr (Var id)   = isRealWorldId id
    isRealWorldExpr (Tick _ e) = isRealWorldExpr e
    isRealWorldExpr _          = False

-- | Finds a nominal size of a string literal.
litSize :: Literal -> Int
-- Used by GHC.Core.Unfold.sizeExpr
litSize (LitNumber LitNumBigNat _)  = 100
litSize (LitString str) = 10 + 10 * ((BS.length str + 3) `div` 4)
        -- If size could be 0 then @f "x"@ might be too small
        -- [Sept03: make literal strings a bit bigger to avoid fruitless
        --  duplication of little strings]
litSize _other = 0    -- Must match size of nullary constructors
                      -- Key point: if  x |-> 4, then x must inline unconditionally
                      --            (eg via case binding)

classOpSize :: UnfoldingOpts -> [Id] -> [CoreExpr] -> ExprSize
-- See Note [Conlike is interesting]
classOpSize _ _ []
  = sizeZero
classOpSize opts top_args (arg1 : other_args)
  = SizeIs size arg_discount 0
  where
    size = 20 + (10 * length other_args)
    -- If the class op is scrutinising a lambda bound dictionary then
    -- give it a discount, to encourage the inlining of this function
    -- The actual discount is rather arbitrarily chosen
    arg_discount = case arg1 of
                     Var dict | dict `elem` top_args
                              -> unitBag (dict, unfoldingDictDiscount opts)
                     _other   -> emptyBag

-- | The size of a function call
callSize
 :: Int  -- ^ number of value args
 -> Int  -- ^ number of value args that are void
 -> Int
callSize n_val_args voids = 10 * (1 + n_val_args - voids)
        -- The 1+ is for the function itself
        -- Add 1 for each non-trivial arg;
        -- the allocation cost, as in let(rec)

-- | The size of a jump to a join point
jumpSize
 :: Int  -- ^ number of value args
 -> Int  -- ^ number of value args that are void
 -> Int
jumpSize n_val_args voids = 2 * (1 + n_val_args - voids)
  -- A jump is 20% the size of a function call. Making jumps free reopens
  -- bug #6048, but making them any more expensive loses a 21% improvement in
  -- spectral/puzzle. TODO Perhaps adjusting the default threshold would be a
  -- better solution?

funSize :: UnfoldingOpts -> [Id] -> Id -> Int -> Int -> ExprSize
-- Size for functions that are not constructors or primops
-- Note [Function applications]
funSize opts top_args fun n_val_args voids
  | fun `hasKey` buildIdKey   = buildSize
  | fun `hasKey` augmentIdKey = augmentSize
  | otherwise = SizeIs size arg_discount res_discount
  where
    some_val_args = n_val_args > 0
    is_join = isJoinId fun

    size | is_join              = jumpSize n_val_args voids
         | not some_val_args    = 0
         | otherwise            = callSize n_val_args voids

        --                  DISCOUNTS
        --  See Note [Function and non-function discounts]
    arg_discount | some_val_args && fun `elem` top_args
                 = unitBag (fun, unfoldingFunAppDiscount opts)
                 | otherwise = emptyBag
        -- If the function is an argument and is applied
        -- to some values, give it an arg-discount

    res_discount | idArity fun > n_val_args = unfoldingFunAppDiscount opts
                 | otherwise                = 0
        -- If the function is partially applied, show a result discount
-- XXX maybe behave like ConSize for eval'd variable

conSize :: DataCon -> Int -> ExprSize
conSize dc n_val_args
  | n_val_args == 0 = SizeIs 0 emptyBag 10    -- Like variables

-- See Note [Unboxed tuple size and result discount]
  | isUnboxedTupleDataCon dc = SizeIs 0 emptyBag 10

-- See Note [Constructor size and result discount]
  | otherwise = SizeIs 10 emptyBag 10

{- Note [Constructor size and result discount]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Treat a constructors application as size 10, regardless of how many
arguments it has; we are keen to expose them (and we charge separately
for their args).  We can't treat them as size zero, else we find that
(Just x) has size 0, which is the same as a lone variable; and hence
'v' will always be replaced by (Just x), where v is bound to Just x.

The "result discount" is applied if the result of the call is
scrutinised (say by a case).  For a constructor application that will
mean the constructor application will disappear, so we don't need to
charge it to the function.  So the discount should at least match the
cost of the constructor application, namely 10.

Historical note 1: Until Jun 2020 we gave it a "bit of extra
incentive" via a discount of 10*(1 + n_val_args), but that was FAR too
much (#18282).  In particular, consider a huge case tree like

   let r = case y1 of
          Nothing -> B1 a b c
          Just v1 -> case y2 of
                      Nothing -> B1 c b a
                      Just v2 -> ...

If conSize gives a cost of 10 (regardless of n_val_args) and a
discount of 10, that'll make each alternative RHS cost zero.  We
charge 10 for each case alternative (see size_up_alt).  If we give a
bigger discount (say 20) in conSize, we'll make the case expression
cost *nothing*, and that can make a huge case tree cost nothing. This
leads to massive, sometimes exponenial inlinings (#18282).  In short,
don't give a discount that give a negative size to a sub-expression!

Historical note 2: Much longer ago, Simon M tried a MUCH bigger
discount: (10 * (10 + n_val_args)), and said it was an "unambiguous
win", but its terribly dangerous because a function with many many
case branches, each finishing with a constructor, can have an
arbitrarily large discount.  This led to terrible code bloat: see #6099.

Note [Unboxed tuple size and result discount]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
However, unboxed tuples count as size zero. I found occasions where we had
        f x y z = case op# x y z of { s -> (# s, () #) }
and f wasn't getting inlined.

I tried giving unboxed tuples a *result discount* of zero (see the
commented-out line).  Why?  When returned as a result they do not
allocate, so maybe we don't want to charge so much for them. If you
have a non-zero discount here, we find that workers often get inlined
back into wrappers, because it look like
    f x = case $wf x of (# a,b #) -> (a,b)
and we are keener because of the case.  However while this change
shrank binary sizes by 0.5% it also made spectral/boyer allocate 5%
more. All other changes were very small. So it's not a big deal but I
didn't adopt the idea.

When fixing #18282 (see Note [Constructor size and result discount])
I changed the result discount to be just 10, not 10*(1+n_val_args).

Note [Function and non-function discounts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want a discount if the function is applied. A good example is
monadic combinators with continuation arguments, where inlining is
quite important.

But we don't want a big discount when a function is called many times
(see the detailed comments with #6048) because if the function is
big it won't be inlined at its many call sites and no benefit results.
Indeed, we can get exponentially big inlinings this way; that is what
#6048 is about.

On the other hand, for data-valued arguments, if there are lots of
case expressions in the body, each one will get smaller if we apply
the function to a constructor application, so we *want* a big discount
if the argument is scrutinised by many case expressions.

Conclusion:
  - For functions, take the max of the discounts
  - For data values, take the sum of the discounts


Note [Literal integer size]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Literal integers *can* be big (mkInteger [...coefficients...]), but
need not be (IS n).  We just use an arbitrary big-ish constant here
so that, in particular, we don't inline top-level defns like
   n = IS 5
There's no point in doing so -- any optimisations will see the IS
through n's unfolding.  Nor will a big size inhibit unfoldings functions
that mention a literal Integer, because the float-out pass will float
all those constants to top level.
-}

primOpSize :: PrimOp -> Int -> ExprSize
primOpSize op n_val_args
 = if primOpOutOfLine op
      then sizeN (op_size + n_val_args)
      else sizeN op_size
 where
   op_size = primOpCodeSize op


buildSize :: ExprSize
buildSize = SizeIs 0 emptyBag 40
        -- We really want to inline applications of build
        -- build t (\cn -> e) should cost only the cost of e (because build will be inlined later)
        -- Indeed, we should add a result_discount because build is
        -- very like a constructor.  We don't bother to check that the
        -- build is saturated (it usually is).  The "-2" discounts for the \c n,
        -- The "4" is rather arbitrary.

augmentSize :: ExprSize
augmentSize = SizeIs 0 emptyBag 40
        -- Ditto (augment t (\cn -> e) ys) should cost only the cost of
        -- e plus ys. The -2 accounts for the \cn

-- When we return a lambda, give a discount if it's used (applied)
lamScrutDiscount :: UnfoldingOpts -> ExprSize -> ExprSize
lamScrutDiscount opts (SizeIs n vs _) = SizeIs n vs (unfoldingFunAppDiscount opts)
lamScrutDiscount _      TooBig          = TooBig

{-
Note [addAltSize result discounts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When adding the size of alternatives, we *add* the result discounts
too, rather than take the *maximum*.  For a multi-branch case, this
gives a discount for each branch that returns a constructor, making us
keener to inline.  I did try using 'max' instead, but it makes nofib
'rewrite' and 'puzzle' allocate significantly more, and didn't make
binary sizes shrink significantly either.

Note [Discounts and thresholds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Constants for discounts and thresholds are defined in 'UnfoldingOpts'. They are:

unfoldingCreationThreshold
     At a definition site, if the unfolding is bigger than this, we
     may discard it altogether

unfoldingUseThreshold
     At a call site, if the unfolding, less discounts, is smaller than
     this, then it's small enough inline

unfoldingDictDiscount
     The discount for each occurrence of a dictionary argument
     as an argument of a class method.  Should be pretty small
     else big functions may get inlined

unfoldingFunAppDiscount
     Discount for a function argument that is applied.  Quite
     large, because if we inline we avoid the higher-order call.

unfoldingVeryAggressive
     If True, the compiler ignores all the thresholds and inlines very
     aggressively. It still adheres to arity, simplifier phase control and
     loop breakers.


Historical Note: Before April 2020 we had another factor,
ufKeenessFactor, which would scale the discounts before they were subtracted
from the size. This was justified with the following comment:

  -- We multiply the raw discounts (args_discount and result_discount)
  -- ty opt_UnfoldingKeenessFactor because the former have to do with
  --  *size* whereas the discounts imply that there's some extra
  --  *efficiency* to be gained (e.g. beta reductions, case reductions)
  -- by inlining.

However, this is highly suspect since it means that we subtract a *scaled* size
from an absolute size, resulting in crazy (e.g. negative) scores in some cases
(#15304). We consequently killed off ufKeenessFactor and bumped up the
ufUseThreshold to compensate.


Note [Function applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a function application (f a b)

  - If 'f' is an argument to the function being analysed,
    and there's at least one value arg, record a FunAppDiscount for f

  - If the application if a PAP (arity > 2 in this example)
    record a *result* discount (because inlining
    with "extra" args in the call may mean that we now
    get a saturated application)

Code for manipulating sizes
-}

-- | The size of a candidate expression for unfolding
data ExprSize
    = TooBig
    | SizeIs { _es_size_is  :: {-# UNPACK #-} !Int -- ^ Size found
             , _es_args     :: !(Bag (Id,Int))
               -- ^ Arguments cased herein, and discount for each such
             , _es_discount :: {-# UNPACK #-} !Int
               -- ^ Size to subtract if result is scrutinised by a case
               -- expression
             }

instance Outputable ExprSize where
  ppr TooBig         = text "TooBig"
  ppr (SizeIs a _ c) = brackets (int a <+> int c)

-- subtract the discount before deciding whether to bale out. eg. we
-- want to inline a large constructor application into a selector:
--      tup = (a_1, ..., a_99)
--      x = case tup of ...
--
mkSizeIs :: Int -> Int -> Bag (Id, Int) -> Int -> ExprSize
mkSizeIs max n xs d | (n - d) > max = TooBig
                    | otherwise     = SizeIs n xs d

maxSize :: ExprSize -> ExprSize -> ExprSize
maxSize TooBig         _                                  = TooBig
maxSize _              TooBig                             = TooBig
maxSize s1@(SizeIs n1 _ _) s2@(SizeIs n2 _ _) | n1 > n2   = s1
                                              | otherwise = s2

sizeZero :: ExprSize
sizeN :: Int -> ExprSize

sizeZero = SizeIs 0 emptyBag 0
sizeN n  = SizeIs n emptyBag 0

{-
************************************************************************
*                                                                      *
\subsection[considerUnfolding]{Given all the info, do (not) do the unfolding}
*                                                                      *
************************************************************************

We use 'couldBeSmallEnoughToInline' to avoid exporting inlinings that
we ``couldn't possibly use'' on the other side.  Can be overridden w/
flaggery.  Just the same as smallEnoughToInline, except that it has no
actual arguments.
-}

couldBeSmallEnoughToInline :: UnfoldingOpts -> Int -> CoreExpr -> Bool
couldBeSmallEnoughToInline opts threshold rhs
  = case sizeExpr opts threshold [] body of
       TooBig -> False
       _      -> True
  where
    (_, body) = collectBinders rhs

----------------
smallEnoughToInline :: UnfoldingOpts -> Unfolding -> Bool
smallEnoughToInline opts (CoreUnfolding {uf_guidance = guidance})
  = case guidance of
       UnfIfGoodArgs {ug_size = size} -> size <= unfoldingUseThreshold opts
       UnfWhen {} -> True
       UnfNever   -> False
smallEnoughToInline _ _
  = False

{-
************************************************************************
*                                                                      *
\subsection{callSiteInline}
*                                                                      *
************************************************************************

This is the key function.  It decides whether to inline a variable at a call site

callSiteInline is used at call sites, so it is a bit more generous.
It's a very important function that embodies lots of heuristics.
A non-WHNF can be inlined if it doesn't occur inside a lambda,
and occurs exactly once or
    occurs once in each branch of a case and is small

If the thing is in WHNF, there's no danger of duplicating work,
so we can inline if it occurs once, or is small

NOTE: we don't want to inline top-level functions that always diverge.
It just makes the code bigger.  Tt turns out that the convenient way to prevent
them inlining is to give them a NOINLINE pragma, which we do in
StrictAnal.addStrictnessInfoToTopId
-}

data ArgSummary = TrivArg       -- Nothing interesting
                | NonTrivArg    -- Arg has structure
                | ValueArg      -- Arg is a con-app or PAP
                                -- ..or con-like. Note [Conlike is interesting]

instance Outputable ArgSummary where
  ppr TrivArg    = text "TrivArg"
  ppr NonTrivArg = text "NonTrivArg"
  ppr ValueArg   = text "ValueArg"

nonTriv ::  ArgSummary -> Bool
nonTriv TrivArg = False
nonTriv _       = True

data CallCtxt
  = BoringCtxt
  | RhsCtxt RecFlag     -- Rhs of a let-binding; see Note [RHS of lets]
  | DiscArgCtxt         -- Argument of a function with non-zero arg discount
  | RuleArgCtxt         -- We are somewhere in the argument of a function with rules

  | ValAppCtxt          -- We're applied to at least one value arg
                        -- This arises when we have ((f x |> co) y)
                        -- Then the (f x) has argument 'x' but in a ValAppCtxt

  | CaseCtxt            -- We're the scrutinee of a case
                        -- that decomposes its scrutinee

instance Outputable CallCtxt where
  ppr CaseCtxt    = text "CaseCtxt"
  ppr ValAppCtxt  = text "ValAppCtxt"
  ppr BoringCtxt  = text "BoringCtxt"
  ppr (RhsCtxt ir)= text "RhsCtxt" <> parens (ppr ir)
  ppr DiscArgCtxt = text "DiscArgCtxt"
  ppr RuleArgCtxt = text "RuleArgCtxt"

callSiteInline :: Logger
               -> UnfoldingOpts
               -> Int                   -- Case depth
               -> Id                    -- The Id
               -> Bool                  -- True <=> unfolding is active
               -> Bool                  -- True if there are no arguments at all (incl type args)
               -> [ArgSummary]          -- One for each value arg; True if it is interesting
               -> CallCtxt              -- True <=> continuation is interesting
               -> Maybe CoreExpr        -- Unfolding, if any
callSiteInline logger opts !case_depth id active_unfolding lone_variable arg_infos cont_info
  = case idUnfolding id of
      -- idUnfolding checks for loop-breakers, returning NoUnfolding
      -- Things with an INLINE pragma may have an unfolding *and*
      -- be a loop breaker  (maybe the knot is not yet untied)
        CoreUnfolding { uf_tmpl = unf_template
                      , uf_is_work_free = is_wf
                      , uf_guidance = guidance, uf_expandable = is_exp }
          | active_unfolding -> tryUnfolding logger opts case_depth id lone_variable
                                    arg_infos cont_info unf_template
                                    is_wf is_exp guidance
          | otherwise -> traceInline logger opts id "Inactive unfolding:" (ppr id) Nothing
        NoUnfolding      -> Nothing
        BootUnfolding    -> Nothing
        OtherCon {}      -> Nothing
        DFunUnfolding {} -> Nothing     -- Never unfold a DFun

-- | Report the inlining of an identifier's RHS to the user, if requested.
traceInline :: Logger -> UnfoldingOpts -> Id -> String -> SDoc -> a -> a
traceInline logger opts inline_id str doc result
  -- We take care to ensure that doc is used in only one branch, ensuring that
  -- the simplifier can push its allocation into the branch. See Note [INLINE
  -- conditional tracing utilities].
  | enable    = logTraceMsg logger str doc result
  | otherwise = result
  where
    enable
      | logHasDumpFlag logger Opt_D_dump_verbose_inlinings
      = True
      | Just prefix <- unfoldingReportPrefix opts
      = prefix `isPrefixOf` occNameString (getOccName inline_id)
      | otherwise
      = False
{-# INLINE traceInline #-} -- see Note [INLINE conditional tracing utilities]

{- Note [Avoid inlining into deeply nested cases]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider a function f like this:

  f arg1 arg2 =
    case ...
      ... -> g arg1
      ... -> g arg2

This function is small. So should be safe to inline.
However sometimes this doesn't quite work out like that.
Consider this code:

f1 arg1 arg2 ... = ...
    case _foo of
      alt1 -> ... f2 arg1 ...
      alt2 -> ... f2 arg2 ...

f2 arg1 arg2 ... = ...
    case _foo of
      alt1 -> ... f3 arg1 ...
      alt2 -> ... f3 arg2 ...

f3 arg1 arg2 ... = ...

... repeats up to n times. And then f1 is
applied to some arguments:

foo = ... f1 <interestingArgs> ...

Initially f2..fn are not interesting to inline so we don't.
However we see that f1 is applied to interesting args.
So it's an obvious choice to inline those:

foo =
    ...
      case _foo of
        alt1 -> ... f2 <interestingArg> ...
        alt2 -> ... f2 <interestingArg> ...

As a result we go and inline f2 both mentions of f2 in turn are now applied to interesting
arguments and f2 is small:

foo =
    ...
      case _foo of
        alt1 -> ... case _foo of
            alt1 -> ... f3 <interestingArg> ...
            alt2 -> ... f3 <interestingArg> ...

        alt2 -> ... case _foo of
            alt1 -> ... f3 <interestingArg> ...
            alt2 -> ... f3 <interestingArg> ...

The same thing happens for each binding up to f_n, duplicating the amount of inlining
done in each step. Until at some point we are either done or run out of simplifier
ticks/RAM. This pattern happened #18730.

To combat this we introduce one more heuristic when weighing inlining decision.
We keep track of a "case-depth". Which increases each time we look inside a case
expression with more than one alternative.

We then apply a penalty to inlinings based on the case-depth at which they would
be inlined. Bounding the number of inlinings in such a scenario.

The heuristic can be tuned in two ways:

* We can ignore the first n levels of case nestings for inlining decisions using
  -funfolding-case-threshold.
* The penalty grows linear with the depth. It's computed as size*(depth-threshold)/scaling.
  Scaling can be set with -funfolding-case-scaling.

Some guidance on setting these defaults:

* A low treshold (<= 2) is needed to prevent exponential cases from spiraling out of
  control. We picked 2 for no particular reason.
* Scaling the penalty by any more than 30 means the reproducer from
  T18730 won't compile even with reasonably small values of n. Instead
  it will run out of runs/ticks. This means to positively affect the reproducer
  a scaling <= 30 is required.
* A scaling of >= 15 still causes a few very large regressions on some nofib benchmarks.
  (+80% for gc/fulsom, +90% for real/ben-raytrace, +20% for spectral/fibheaps)
* A scaling of >= 25 showed no regressions on nofib. However it showed a number of
  (small) regression for compiler perf benchmarks.

The end result is that we are settling for a scaling of 30, with a threshold of 2.
This gives us minimal compiler perf regressions. No nofib runtime regressions and
will still avoid this pattern sometimes. This is a "safe" default, where we err on
the side of compiler blowup instead of risking runtime regressions.

For cases where the default falls short the flag can be changed to allow more/less inlining as
needed on a per-module basis.

-}

tryUnfolding :: Logger -> UnfoldingOpts -> Int -> Id -> Bool -> [ArgSummary] -> CallCtxt
             -> CoreExpr -> Bool -> Bool -> UnfoldingGuidance
             -> Maybe CoreExpr
tryUnfolding logger opts !case_depth id lone_variable
             arg_infos cont_info unf_template
             is_wf is_exp guidance
 = case guidance of
     UnfNever -> traceInline logger opts id str (text "UnfNever") Nothing

     UnfWhen { ug_arity = uf_arity, ug_unsat_ok = unsat_ok, ug_boring_ok = boring_ok }
        | enough_args && (boring_ok || some_benefit || unfoldingVeryAggressive opts)
                -- See Note [INLINE for small functions (3)]
        -> traceInline logger opts id str (mk_doc some_benefit empty True) (Just unf_template)
        | otherwise
        -> traceInline logger opts id str (mk_doc some_benefit empty False) Nothing
        where
          some_benefit = calc_some_benefit uf_arity
          enough_args = (n_val_args >= uf_arity) || (unsat_ok && n_val_args > 0)

     UnfIfGoodArgs { ug_args = arg_discounts, ug_res = res_discount, ug_size = size }
        | unfoldingVeryAggressive opts
        -> traceInline logger opts id str (mk_doc some_benefit extra_doc True) (Just unf_template)
        | is_wf && some_benefit && small_enough
        -> traceInline logger opts id str (mk_doc some_benefit extra_doc True) (Just unf_template)
        | otherwise
        -> traceInline logger opts id str (mk_doc some_benefit extra_doc False) Nothing
        where
          some_benefit = calc_some_benefit (length arg_discounts)
          extra_doc = vcat [ text "case depth =" <+> int case_depth
                           , text "depth based penalty =" <+> int depth_penalty
                           , text "discounted size =" <+> int adjusted_size ]
          -- See Note [Avoid inlining into deeply nested cases]
          depth_treshold = unfoldingCaseThreshold opts
          depth_scaling = unfoldingCaseScaling opts
          depth_penalty | case_depth <= depth_treshold = 0
                        | otherwise       = (size * (case_depth - depth_treshold)) `div` depth_scaling
          adjusted_size = size + depth_penalty - discount
          small_enough = adjusted_size <= unfoldingUseThreshold opts
          discount = computeDiscount arg_discounts res_discount arg_infos cont_info

  where
    mk_doc some_benefit extra_doc yes_or_no
      = vcat [ text "arg infos" <+> ppr arg_infos
             , text "interesting continuation" <+> ppr cont_info
             , text "some_benefit" <+> ppr some_benefit
             , text "is exp:" <+> ppr is_exp
             , text "is work-free:" <+> ppr is_wf
             , text "guidance" <+> ppr guidance
             , extra_doc
             , text "ANSWER =" <+> if yes_or_no then text "YES" else text "NO"]

    ctx = log_default_dump_context (logFlags logger)
    str = "Considering inlining: " ++ renderWithContext ctx (ppr id)
    n_val_args = length arg_infos

           -- some_benefit is used when the RHS is small enough
           -- and the call has enough (or too many) value
           -- arguments (ie n_val_args >= arity). But there must
           -- be *something* interesting about some argument, or the
           -- result context, to make it worth inlining
    calc_some_benefit :: Arity -> Bool   -- The Arity is the number of args
                                         -- expected by the unfolding
    calc_some_benefit uf_arity
       | not saturated = interesting_args       -- Under-saturated
                                        -- Note [Unsaturated applications]
       | otherwise = interesting_args   -- Saturated or over-saturated
                  || interesting_call
      where
        saturated      = n_val_args >= uf_arity
        over_saturated = n_val_args > uf_arity
        interesting_args = any nonTriv arg_infos
                -- NB: (any nonTriv arg_infos) looks at the
                -- over-saturated args too which is "wrong";
                -- but if over-saturated we inline anyway.

        interesting_call
          | over_saturated
          = True
          | otherwise
          = case cont_info of
              CaseCtxt   -> not (lone_variable && is_exp)  -- Note [Lone variables]
              ValAppCtxt -> True                           -- Note [Cast then apply]
              RuleArgCtxt -> uf_arity > 0  -- See Note [Unfold info lazy contexts]
              DiscArgCtxt -> uf_arity > 0  -- Note [Inlining in ArgCtxt]
              RhsCtxt NonRecursive
                          -> uf_arity > 0  -- See Note [RHS of lets]
              _other      -> False         -- See Note [Nested functions]


{-
Note [Unfold into lazy contexts], Note [RHS of lets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the call is the argument of a function with a RULE, or the RHS of a let,
we are a little bit keener to inline (in tryUnfolding).  For example
     f y = (y,y,y)
     g y = let x = f y in ...(case x of (a,b,c) -> ...) ...
We'd inline 'f' if the call was in a case context, and it kind-of-is,
only we can't see it.  Also
     x = f v
could be expensive whereas
     x = case v of (a,b) -> a
is patently cheap and may allow more eta expansion.

So we treat the RHS of a /non-recursive/ let as not-totally-boring.
A /recursive/ let isn't going be inlined so there is much less point.
Hence the RecFlag in RhsCtxt

Note [Unsaturated applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a call is not saturated, we *still* inline if one of the
arguments has interesting structure.  That's sometimes very important.
A good example is the Ord instance for Bool in Base:

 Rec {
    $fOrdBool =GHC.Classes.D:Ord
                 @ Bool
                 ...
                 $cmin_ajX

    $cmin_ajX [Occ=LoopBreaker] :: Bool -> Bool -> Bool
    $cmin_ajX = GHC.Classes.$dmmin @ Bool $fOrdBool
  }

But the defn of GHC.Classes.$dmmin is:

  $dmmin :: forall a. GHC.Classes.Ord a => a -> a -> a
    {- Arity: 3, HasNoCafRefs, Strictness: SLL,
       Unfolding: (\ @ a $dOrd :: GHC.Classes.Ord a x :: a y :: a ->
                   case @ a GHC.Classes.<= @ a $dOrd x y of wild {
                     GHC.Types.False -> y GHC.Types.True -> x }) -}

We *really* want to inline $dmmin, even though it has arity 3, in
order to unravel the recursion.


Note [Things to watch]
~~~~~~~~~~~~~~~~~~~~~~
*   { y = I# 3; x = y `cast` co; ...case (x `cast` co) of ... }
    Assume x is exported, so not inlined unconditionally.
    Then we want x to inline unconditionally; no reason for it
    not to, and doing so avoids an indirection.

*   { x = I# 3; ....f x.... }
    Make sure that x does not inline unconditionally!
    Lest we get extra allocation.

Note [Inlining an InlineRule]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An InlineRules is used for
  (a) programmer INLINE pragmas
  (b) inlinings from worker/wrapper

For (a) the RHS may be large, and our contract is that we *only* inline
when the function is applied to all the arguments on the LHS of the
source-code defn.  (The uf_arity in the rule.)

However for worker/wrapper it may be worth inlining even if the
arity is not satisfied (as we do in the CoreUnfolding case) so we don't
require saturation.

Note [Nested functions]
~~~~~~~~~~~~~~~~~~~~~~~
At one time we treated a call of a non-top-level function as
"interesting" (regardless of how boring the context) in the hope
that inlining it would eliminate the binding, and its allocation.
Specifically, in the default case of interesting_call we had
   _other -> not is_top && uf_arity > 0

But actually postInlineUnconditionally does some of this and overall
it makes virtually no difference to nofib.  So I simplified away this
special case

Note [Cast then apply]
~~~~~~~~~~~~~~~~~~~~~~
Consider
   myIndex = __inline_me ( (/\a. <blah>) |> co )
   co :: (forall a. a -> a) ~ (forall a. T a)
     ... /\a.\x. case ((myIndex a) |> sym co) x of { ... } ...

We need to inline myIndex to unravel this; but the actual call (myIndex a) has
no value arguments.  The ValAppCtxt gives it enough incentive to inline.

Note [Inlining in ArgCtxt]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The condition (arity > 0) here is very important, because otherwise
we end up inlining top-level stuff into useless places; eg
   x = I# 3#
   f = \y.  g x
This can make a very big difference: it adds 16% to nofib 'integer' allocs,
and 20% to 'power'.

At one stage I replaced this condition by 'True' (leading to the above
slow-down).  The motivation was test eyeball/inline1.hs; but that seems
to work ok now.

NOTE: arguably, we should inline in ArgCtxt only if the result of the
call is at least CONLIKE.  At least for the cases where we use ArgCtxt
for the RHS of a 'let', we only profit from the inlining if we get a
CONLIKE thing (modulo lets).

Note [Lone variables]   See also Note [Interaction of exprIsWorkFree and lone variables]
~~~~~~~~~~~~~~~~~~~~~   which appears below
The "lone-variable" case is important.  I spent ages messing about
with unsatisfactory variants, but this is nice.  The idea is that if a
variable appears all alone

        as an arg of lazy fn, or rhs    BoringCtxt
        as scrutinee of a case          CaseCtxt
        as arg of a fn                  ArgCtxt
AND
        it is bound to a cheap expression

then we should not inline it (unless there is some other reason,
e.g. it is the sole occurrence).  That is what is happening at
the use of 'lone_variable' in 'interesting_call'.

Why?  At least in the case-scrutinee situation, turning
        let x = (a,b) in case x of y -> ...
into
        let x = (a,b) in case (a,b) of y -> ...
and thence to
        let x = (a,b) in let y = (a,b) in ...
is bad if the binding for x will remain.

Another example: I discovered that strings
were getting inlined straight back into applications of 'error'
because the latter is strict.
        s = "foo"
        f = \x -> ...(error s)...

Fundamentally such contexts should not encourage inlining because, provided
the RHS is "expandable" (see Note [exprIsExpandable] in GHC.Core.Utils) the
context can ``see'' the unfolding of the variable (e.g. case or a
RULE) so there's no gain.

However, watch out:

 * Consider this:
        foo = _inline_ (\n. [n])
        bar = _inline_ (foo 20)
        baz = \n. case bar of { (m:_) -> m + n }
   Here we really want to inline 'bar' so that we can inline 'foo'
   and the whole thing unravels as it should obviously do.  This is
   important: in the NDP project, 'bar' generates a closure data
   structure rather than a list.

   So the non-inlining of lone_variables should only apply if the
   unfolding is regarded as cheap; because that is when exprIsConApp_maybe
   looks through the unfolding.  Hence the "&& is_wf" in the
   InlineRule branch.

 * Even a type application or coercion isn't a lone variable.
   Consider
        case $fMonadST @ RealWorld of { :DMonad a b c -> c }
   We had better inline that sucker!  The case won't see through it.

   For now, I'm treating treating a variable applied to types
   in a *lazy* context "lone". The motivating example was
        f = /\a. \x. BIG
        g = /\a. \y.  h (f a)
   There's no advantage in inlining f here, and perhaps
   a significant disadvantage.  Hence some_val_args in the Stop case

Note [Interaction of exprIsWorkFree and lone variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The lone-variable test says "don't inline if a case expression
scrutinises a lone variable whose unfolding is cheap".  It's very
important that, under these circumstances, exprIsConApp_maybe
can spot a constructor application. So, for example, we don't
consider
        let x = e in (x,x)
to be cheap, and that's good because exprIsConApp_maybe doesn't
think that expression is a constructor application.

In the 'not (lone_variable && is_wf)' test, I used to test is_value
rather than is_wf, which was utterly wrong, because the above
expression responds True to exprIsHNF, which is what sets is_value.

This kind of thing can occur if you have

        {-# INLINE foo #-}
        foo = let x = e in (x,x)

which Roman did.


-}

computeDiscount :: [Int] -> Int -> [ArgSummary] -> CallCtxt
                -> Int
computeDiscount arg_discounts res_discount arg_infos cont_info

  = 10          -- Discount of 10 because the result replaces the call
                -- so we count 10 for the function itself

    + 10 * length actual_arg_discounts
               -- Discount of 10 for each arg supplied,
               -- because the result replaces the call

    + total_arg_discount + res_discount'
  where
    actual_arg_discounts = zipWith mk_arg_discount arg_discounts arg_infos
    total_arg_discount   = sum actual_arg_discounts

    mk_arg_discount _        TrivArg    = 0
    mk_arg_discount _        NonTrivArg = 10
    mk_arg_discount discount ValueArg   = discount

    res_discount'
      | LT <- arg_discounts `compareLength` arg_infos
      = res_discount   -- Over-saturated
      | otherwise
      = case cont_info of
           BoringCtxt  -> 0
           CaseCtxt    -> res_discount  -- Presumably a constructor
           ValAppCtxt  -> res_discount  -- Presumably a function
           _           -> 40 `min` res_discount
                -- ToDo: this 40 `min` res_discount doesn't seem right
                --   for DiscArgCtxt it shouldn't matter because the function will
                --       get the arg discount for any non-triv arg
                --   for RuleArgCtxt we do want to be keener to inline; but not only
                --       constructor results
                --   for RhsCtxt I suppose that exposing a data con is good in general
                --   And 40 seems very arbitrary
                --
                -- res_discount can be very large when a function returns
                -- constructors; but we only want to invoke that large discount
                -- when there's a case continuation.
                -- Otherwise we, rather arbitrarily, threshold it.  Yuk.
                -- But we want to avoid inlining large functions that return
                -- constructors into contexts that are simply "interesting"
