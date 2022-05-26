{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | GHC.Core holds all the main data types for use by for the Glasgow Haskell Compiler midsection
module GHC.Core.Unfoldings (
        -- * Unfolding data types
        Unfolding(..),  UnfoldingCache(..), UnfoldingGuidance(..), UnfoldingSource(..),

        -- ** Constructing 'Unfolding's
        noUnfolding, bootUnfolding, evaldUnfolding, mkOtherCon,
        unSaturatedOk, needSaturated, boringCxtOk, boringCxtNotOk,

        -- ** Predicates and deconstruction on 'Unfolding'
        unfoldingTemplate, expandUnfolding_maybe,
        maybeUnfoldingTemplate, otherCons,
        isValueUnfolding, isEvaldUnfolding, isCheapUnfolding,
        isExpandableUnfolding, isConLikeUnfolding, isCompulsoryUnfolding,
        isStableUnfolding, isStableUserUnfolding, isStableSystemUnfolding,
        isInlineUnfolding, isBootUnfolding,
        hasCoreUnfolding, hasSomeUnfolding,
        canUnfold, neverUnfoldGuidance, isStableSource,
    ) where

import GHC.Prelude

import GHC.Types.Var
import GHC.Core
import GHC.Core.DataCon
import GHC.Types.Basic

{-
The @Unfolding@ type is declared here to avoid numerous loops

Note [Never put `OtherCon` unfoldings on lambda binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Based on #21496 we never attach unfoldings of any kind to lambda binders.
It's just too easy for the call site to change and invalidate the unfolding.
E.g. the caller of the lambda drops a seq (e.g. because the lambda is strict in it's binder)
which in turn makes the OtherCon[] unfolding a lie.
So unfoldings on lambda binders can never really be trusted when on lambda binders if there
is the chance of the call site to change. So it's easiest to just never attach any
to lambda binders to begin with, as well as stripping them off if we e.g. float out
and expression while abstracting over some arguments.
-}

-- | Records the /unfolding/ of an identifier, which is approximately the form the
-- identifier would have if we substituted its definition in for the identifier.
-- This type should be treated as abstract everywhere except in "GHC.Core.Unfold"
data Unfolding
  = NoUnfolding        -- ^ We have no information about the unfolding.

  | BootUnfolding      -- ^ We have no information about the unfolding, because
                       -- this 'Id' came from an @hi-boot@ file.
                       -- See Note [Inlining and hs-boot files] in "GHC.CoreToIface"
                       -- for what this is used for.

  | OtherCon [AltCon]  -- ^ It ain't one of these constructors.
                       -- @OtherCon xs@ also indicates that something has been evaluated
                       -- and hence there's no point in re-evaluating it.
                       -- @OtherCon []@ is used even for non-data-type values
                       -- to indicated evaluated-ness.  Notably:
                       --
                       -- > data C = C !(Int -> Int)
                       -- > case x of { C f -> ... }
                       --
                       -- Here, @f@ gets an @OtherCon []@ unfolding.

  | DFunUnfolding {     -- The Unfolding of a DFunId
                        -- See Note [DFun unfoldings]
                        --     df = /\a1..am. \d1..dn. MkD t1 .. tk
                        --                                 (op1 a1..am d1..dn)
                        --                                 (op2 a1..am d1..dn)
        df_bndrs :: [Var],      -- The bound variables [a1..m],[d1..dn]
        df_con   :: DataCon,    -- The dictionary data constructor (never a newtype datacon)
        df_args  :: [CoreExpr]  -- Args of the data con: types, superclasses and methods,
    }                           -- in positional order

  | CoreUnfolding {             -- An unfolding for an Id with no pragma,
                                -- or perhaps a NOINLINE pragma
                                -- (For NOINLINE, the phase, if any, is in the
                                -- InlinePragInfo for this Id.)
        uf_tmpl       :: CoreExpr,        -- Template; occurrence info is correct
        uf_src        :: UnfoldingSource, -- Where the unfolding came from
        uf_is_top     :: Bool,          -- True <=> top level binding
        uf_cache      :: UnfoldingCache,        -- Cache of flags computable from the expr
                                                -- See Note [Tying the 'CoreUnfolding' knot]
        uf_guidance   :: UnfoldingGuidance      -- Tells about the *size* of the template.
    }
  -- ^ An unfolding with redundant cached information. Parameters:
  --
  --  uf_tmpl: Template used to perform unfolding;
  --           NB: Occurrence info is guaranteed correct:
  --               see Note [OccInfo in unfoldings and rules]
  --
  --  uf_is_top: Is this a top level binding?
  --
  --  uf_is_value: 'exprIsHNF' template (cached); it is ok to discard a 'seq' on
  --     this variable
  --
  --  uf_is_work_free:  Does this waste only a little work if we expand it inside an inlining?
  --     Basically this is a cached version of 'exprIsWorkFree'
  --
  --  uf_guidance:  Tells us about the /size/ of the unfolding template


-- | Properties of a 'CoreUnfolding' that could be computed on-demand from its template.
-- See Note [UnfoldingCache]
data UnfoldingCache
  = UnfoldingCache {
        uf_is_value   :: !Bool,         -- exprIsHNF template (cached); it is ok to discard
                                        --      a `seq` on this variable
        uf_is_conlike :: !Bool,         -- True <=> applicn of constructor or CONLIKE function
                                        --      Cached version of exprIsConLike
        uf_is_work_free :: !Bool,       -- True <=> doesn't waste (much) work to expand
                                        --          inside an inlining
                                        --      Cached version of exprIsCheap
        uf_expandable :: !Bool          -- True <=> can expand in RULE matching
                                        --      Cached version of exprIsExpandable
    }
  deriving (Eq)

-- | 'UnfoldingGuidance' says when unfolding should take place
data UnfoldingGuidance
  = UnfWhen {   -- Inline without thinking about the *size* of the uf_tmpl
                -- Used (a) for small *and* cheap unfoldings
                --      (b) for INLINE functions
                -- See Note [INLINE for small functions] in GHC.Core.Unfold
      ug_arity    :: Arity,     -- Number of value arguments expected

      ug_unsat_ok  :: Bool,     -- True <=> ok to inline even if unsaturated
      ug_boring_ok :: Bool      -- True <=> ok to inline even if the context is boring
                -- So True,True means "always"
    }

  | UnfIfGoodArgs {     -- Arose from a normal Id; the info here is the
                        -- result of a simple analysis of the RHS

      ug_args ::  [Int],  -- Discount if the argument is evaluated.
                          -- (i.e., a simplification will definitely
                          -- be possible).  One elt of the list per *value* arg.

      ug_size :: Int,     -- The "size" of the unfolding.

      ug_res :: Int       -- Scrutinee discount: the discount to subtract if the thing is in
    }                     -- a context (case (thing args) of ...),
                          -- (where there are the right number of arguments.)

  | UnfNever        -- The RHS is big, so don't inline it
  deriving (Eq)

{- Note [UnfoldingCache]
~~~~~~~~~~~~~~~~~~~~~~~~
The UnfoldingCache field of an Unfolding holds four (strict) booleans,
all derived from the uf_tmpl field of the unfolding.

* We serialise the UnfoldingCache to and from interface files, for
  reasons described in  Note [Tying the 'CoreUnfolding' knot] in
  GHC.IfaceToCore

* Because it is a strict data type, we must be careful not to
  pattern-match on it until we actually want its values.  E.g
  GHC.Core.Unfold.callSiteInline/tryUnfolding are careful not to force
  it unnecessarily.  Just saves a bit of work.

* When `seq`ing Core to eliminate space leaks, to suffices to `seq` on
  the cache, but not its fields, because it is strict in all fields.

Note [Historical note: unfoldings for wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to have a nice clever scheme in interface files for
wrappers. A wrapper's unfolding can be reconstructed from its worker's
id and its strictness. This decreased .hi file size (sometimes
significantly, for modules like GHC.Classes with many high-arity w/w
splits) and had a slight corresponding effect on compile times.

However, when we added the second demand analysis, this scheme lead to
some Core lint errors. The second analysis could change the strictness
signatures, which sometimes resulted in a wrapper's regenerated
unfolding applying the wrapper to too many arguments.

Instead of repairing the clever .hi scheme, we abandoned it in favor
of simplicity. The .hi sizes are usually insignificant (excluding the
+1M for base libraries), and compile time barely increases (~+1% for
nofib). The nicer upshot is that the UnfoldingSource no longer mentions
an Id, so, eg, substitutions need not traverse them.


Note [DFun unfoldings]
~~~~~~~~~~~~~~~~~~~~~~
The Arity in a DFunUnfolding is total number of args (type and value)
that the DFun needs to produce a dictionary.  That's not necessarily
related to the ordinary arity of the dfun Id, esp if the class has
one method, so the dictionary is represented by a newtype.  Example

     class C a where { op :: a -> Int }
     instance C a -> C [a] where op xs = op (head xs)

The instance translates to

     $dfCList :: forall a. C a => C [a]  -- Arity 2!
     $dfCList = /\a.\d. $copList {a} d |> co

     $copList :: forall a. C a => [a] -> Int  -- Arity 2!
     $copList = /\a.\d.\xs. op {a} d (head xs)

Now we might encounter (op (dfCList {ty} d) a1 a2)
and we want the (op (dfList {ty} d)) rule to fire, because $dfCList
has all its arguments, even though its (value) arity is 2.  That's
why we record the number of expected arguments in the DFunUnfolding.

Note that although it's an Arity, it's most convenient for it to give
the *total* number of arguments, both type and value.  See the use
site in exprIsConApp_maybe.
-}

-- Constants for the UnfWhen constructor
needSaturated, unSaturatedOk :: Bool
needSaturated = False
unSaturatedOk = True

boringCxtNotOk, boringCxtOk :: Bool
boringCxtOk    = True
boringCxtNotOk = False

------------------------------------------------
noUnfolding :: Unfolding
-- ^ There is no known 'Unfolding'
evaldUnfolding :: Unfolding
-- ^ This unfolding marks the associated thing as being evaluated

noUnfolding    = NoUnfolding
evaldUnfolding = OtherCon []

-- | There is no known 'Unfolding', because this came from an
-- hi-boot file.
bootUnfolding :: Unfolding
bootUnfolding = BootUnfolding

mkOtherCon :: [AltCon] -> Unfolding
mkOtherCon = OtherCon

-- | Retrieves the template of an unfolding: panics if none is known
unfoldingTemplate :: Unfolding -> CoreExpr
unfoldingTemplate = uf_tmpl

-- | Retrieves the template of an unfolding if possible
-- maybeUnfoldingTemplate is used mainly when specialising, and we do
-- want to specialise DFuns, so it's important to return a template
-- for DFunUnfoldings
maybeUnfoldingTemplate :: Unfolding -> Maybe CoreExpr
maybeUnfoldingTemplate (CoreUnfolding { uf_tmpl = expr })
  = Just expr
maybeUnfoldingTemplate (DFunUnfolding { df_bndrs = bndrs, df_con = con, df_args = args })
  = Just (mkLams bndrs (mkApps (Var (dataConWorkId con)) args))
maybeUnfoldingTemplate _
  = Nothing

-- | The constructors that the unfolding could never be:
-- returns @[]@ if no information is available
otherCons :: Unfolding -> [AltCon]
otherCons (OtherCon cons) = cons
otherCons _               = []

-- | Determines if it is certainly the case that the unfolding will
-- yield a value (something in HNF): returns @False@ if unsure
isValueUnfolding :: Unfolding -> Bool
        -- Returns False for OtherCon
isValueUnfolding (CoreUnfolding { uf_cache = cache }) = uf_is_value cache
isValueUnfolding (DFunUnfolding {})                   = True
isValueUnfolding _                                    = False

-- | Determines if it possibly the case that the unfolding will
-- yield a value. Unlike 'isValueUnfolding' it returns @True@
-- for 'OtherCon'
isEvaldUnfolding :: Unfolding -> Bool
        -- Returns True for OtherCon
isEvaldUnfolding (OtherCon _)                         = True
isEvaldUnfolding (DFunUnfolding {})                   = True
isEvaldUnfolding (CoreUnfolding { uf_cache = cache }) = uf_is_value cache
isEvaldUnfolding _                                    = False

-- | @True@ if the unfolding is a constructor application, the application
-- of a CONLIKE function or 'OtherCon'
isConLikeUnfolding :: Unfolding -> Bool
isConLikeUnfolding (OtherCon _)                         = True
isConLikeUnfolding (CoreUnfolding { uf_cache = cache }) = uf_is_conlike cache
isConLikeUnfolding _                                    = False

-- | Is the thing we will unfold into certainly cheap?
isCheapUnfolding :: Unfolding -> Bool
isCheapUnfolding (CoreUnfolding { uf_cache = cache }) = uf_is_work_free cache
isCheapUnfolding _                                    = False

isExpandableUnfolding :: Unfolding -> Bool
isExpandableUnfolding (CoreUnfolding { uf_cache = cache }) = uf_expandable cache
isExpandableUnfolding _                                    = False

expandUnfolding_maybe :: Unfolding -> Maybe CoreExpr
-- Expand an expandable unfolding; this is used in rule matching
--   See Note [Expanding variables] in GHC.Core.Rules
-- The key point here is that CONLIKE things can be expanded
expandUnfolding_maybe (CoreUnfolding { uf_cache = cache, uf_tmpl = rhs })
  | uf_expandable cache
    = Just rhs
expandUnfolding_maybe _ = Nothing

isCompulsoryUnfolding :: Unfolding -> Bool
isCompulsoryUnfolding (CoreUnfolding { uf_src = src }) = isCompulsorySource src
isCompulsoryUnfolding _                                = False

isStableUnfolding :: Unfolding -> Bool
-- True of unfoldings that should not be overwritten
-- by a CoreUnfolding for the RHS of a let-binding
isStableUnfolding (CoreUnfolding { uf_src = src }) = isStableSource src
isStableUnfolding (DFunUnfolding {})               = True
isStableUnfolding _                                = False

isStableUserUnfolding :: Unfolding -> Bool
-- True of unfoldings that arise from an INLINE or INLINEABLE pragma
isStableUserUnfolding (CoreUnfolding { uf_src = src }) = isStableUserSource src
isStableUserUnfolding _                                = False

isStableSystemUnfolding :: Unfolding -> Bool
-- True of unfoldings that arise from an INLINE or INLINEABLE pragma
isStableSystemUnfolding (CoreUnfolding { uf_src = src }) = isStableSystemSource src
isStableSystemUnfolding _                                = False

isInlineUnfolding :: Unfolding -> Bool
-- ^ True of a /stable/ unfolding that is
--   (a) always inlined; that is, with an `UnfWhen` guidance, or
--   (b) a DFunUnfolding which never needs to be inlined
isInlineUnfolding (CoreUnfolding { uf_src = src, uf_guidance = guidance })
  | isStableSource src
  , UnfWhen {} <- guidance
  = True

isInlineUnfolding (DFunUnfolding {})
  = True

-- Default case
isInlineUnfolding _ = False


-- | Only returns False if there is no unfolding information available at all
hasSomeUnfolding :: Unfolding -> Bool
hasSomeUnfolding NoUnfolding   = False
hasSomeUnfolding BootUnfolding = False
hasSomeUnfolding _             = True

isBootUnfolding :: Unfolding -> Bool
isBootUnfolding BootUnfolding = True
isBootUnfolding _             = False

neverUnfoldGuidance :: UnfoldingGuidance -> Bool
neverUnfoldGuidance UnfNever = True
neverUnfoldGuidance _        = False

hasCoreUnfolding :: Unfolding -> Bool
-- An unfolding "has Core" if it contains a Core expression, which
-- may mention free variables. See Note [Fragile unfoldings]
hasCoreUnfolding (CoreUnfolding {}) = True
hasCoreUnfolding (DFunUnfolding {}) = True
hasCoreUnfolding _                  = False
  -- NoUnfolding, BootUnfolding, OtherCon have no Core

canUnfold :: Unfolding -> Bool
canUnfold (CoreUnfolding { uf_guidance = g }) = not (neverUnfoldGuidance g)
canUnfold _                                   = False

{- Note [Fragile unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An unfolding is "fragile" if it mentions free variables (and hence would
need substitution) or might be affected by optimisation.  The non-fragile
ones are

   NoUnfolding, BootUnfolding

   OtherCon {}    If we know this binder (say a lambda binder) will be
                  bound to an evaluated thing, we want to retain that
                  info in simpleOptExpr; see #13077.

We consider even a StableUnfolding as fragile, because it needs substitution.

Note [Stable unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~
When you say
      {-# INLINE f #-}
      f x = <rhs>
you intend that calls (f e) are replaced by <rhs>[e/x] So we
should capture (\x.<rhs>) in the Unfolding of 'f', and never meddle
with it.  Meanwhile, we can optimise <rhs> to our heart's content,
leaving the original unfolding intact in Unfolding of 'f'. For example
        all xs = foldr (&&) True xs
        any p = all . map p  {-# INLINE any #-}
We optimise any's RHS fully, but leave the stable unfolding for `any`
saying "all . map p", which deforests well at the call site.

So INLINE pragma gives rise to a stable unfolding, which captures the
original RHS.

Moreover, it's only used when 'f' is applied to the
specified number of arguments; that is, the number of argument on
the LHS of the '=' sign in the original source definition.
For example, (.) is now defined in the libraries like this
   {-# INLINE (.) #-}
   (.) f g = \x -> f (g x)
so that it'll inline when applied to two arguments. If 'x' appeared
on the left, thus
   (.) f g x = f (g x)
it'd only inline when applied to three arguments.  This slightly-experimental
change was requested by Roman, but it seems to make sense.

Note [OccInfo in unfoldings and rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In unfoldings and rules, we guarantee that the template is occ-analysed,
so that the occurrence info on the binders is correct.  This is important,
because the Simplifier does not re-analyse the template when using it. If
the occurrence info is wrong
  - We may get more simplifier iterations than necessary, because
    once-occ info isn't there
  - More seriously, we may get an infinite loop if there's a Rec
    without a loop breaker marked

-}
