{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}

-- | 'RoughMap' is an approximate finite map data structure keyed on
-- @['RoughMatchTc']@. This is useful when keying maps on lists of 'Type's
-- (e.g. an instance head).
module GHC.Core.RoughMap
  ( -- * RoughMatchTc
    RoughMatchTc(..)
  , isRoughWildcard
  , typeToRoughMatchTc
  , RoughMatchLookupTc(..)
  , typeToRoughMatchLookupTc
  , roughMatchTcToLookup

    -- * RoughMap
  , RoughMap
  , emptyRM
  , lookupRM
  , lookupRM'
  , insertRM
  , filterRM
  , filterMatchingRM
  , elemsRM
  , sizeRM
  , foldRM
  , unionRM
  ) where

import GHC.Prelude

import GHC.Data.Bag
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.Type
import GHC.Utils.Outputable
import GHC.Types.Name
import GHC.Types.Name.Env

import Control.Monad (join)
import Data.Data (Data)
import GHC.Utils.Misc
import Data.Bifunctor
import GHC.Utils.Panic

{-
Note [RoughMap]
~~~~~~~~~~~~~~~
We often want to compute whether one type matches another. That is, given
`ty1` and `ty2`, we want to know whether `ty1` is a substitution instance of `ty2`.

We can bail out early by taking advantage of the following observation:

  If `ty2` is headed by a generative type constructor, say `tc`,
  but `ty1` is not headed by that same type constructor,
  then `ty1` does not match `ty2`.

The idea is that we can use a `RoughMap` as a pre-filter, to produce a
short-list of candidates to examine more closely.

This means we can avoid computing a full substitution if we represent types
as applications of known generative type constructors. So, after type synonym
expansion, we classify application heads into two categories ('RoughMatchTc')

  - `RM_KnownTc tc`: the head is the generative type constructor `tc`,
  - `RM_Wildcard`: anything else.

A (RoughMap val) is semantically a list of (key,[val]) pairs, where
   key :: [RoughMatchTc]
So, writing # for `OtherTc`, and Int for `KnownTc "Int"`, we might have
    [ ([#, Int, Maybe, #, Int], v1)
    , ([Int, #, List], v2 ]

This map is stored as a trie, so looking up a key is very fast.
See Note [Matching a RoughMap] and Note [Simple Matching Semantics] for details on
lookup.

We lookup a key of type [RoughMatchLookupTc], and return the list of all values whose
keys "match":

Given the above map, here are the results of some lookups:
   Lookup key       Result
   -------------------------
   [Int, Int]       [v1,v2] -- Matches because the prefix of both entries matches
   [Int,Int,List]   [v2]
   [Bool]           []

Notice that a single key can map to /multiple/ values.  E.g. if we started
with (Maybe Int, val1) and (Maybe Bool, val2), we'd generate a RoughMap
that is semantically the list   [( Maybe, [val1,val2] )]

Note [RoughMap and beta reduction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is one tricky case we have to account for when matching a rough map due
to Note [Eta reduction for data families] in `GHC.Core.Coercion.Axiom`:
Consider that the user has written a program containing a data family:

> data family Fam a b
> data instance Fam Int a = SomeType  -- known henceforth as FamIntInst

The LHS of this instance will be eta reduced, as described in Note [Eta
reduction for data families]. Consequently, we will end up with a `FamInst`
with `fi_tcs = [KnownTc Int]`. Naturally, we need RoughMap to return this
instance when queried for an instance with template, e.g., `[KnownTc Fam,
KnownTc Int, KnownTc Char]`.

This explains the third clause of the mightMatch specification in Note [Simple Matching Semantics].
As soon as the the lookup key runs out, the remaining instances might match.

Note [Matching a RoughMap]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The /lookup key/ into a rough map (RoughMatchLookupTc) is slightly
different to the /insertion key/ (RoughMatchTc).  Like the insertion
key each lookup argument is classified to a simpler key which
describes what could match that position. There are three
possibilities:

* RML_KnownTc Name: The argument is headed by a known type
  constructor.  Example: 'Bool' is classified as 'RML_KnownTc Bool'
  and '[Int]' is classified as `RML_KnownTc []`

* RML_NoKnownTc: The argument is definitely not headed by any known
  type constructor.  Example: For instance matching 'a[sk], a[tau]' and 'F a[sk], F a[tau]'
  are classified as 'RML_NoKnownTc', for family instance matching no examples.

* RML_WildCard: The argument could match anything, we don't know
  enough about it. For instance matching no examples, for type family matching,
  things to do with variables.

The interesting case for instance matching is the second case, because it does not appear in
an insertion key. The second case arises in two situations:

1. The head of the application is a type variable. The type variable definitely
   doesn't match with any of the KnownTC instances so we can discard them all. For example:
    Show a[sk] or Show (a[sk] b[sk]). One place constraints like this arise is when
    typechecking derived instances.
2. The head of the application is a known type family.
   For example: F a[sk]. The application of F is stuck, and because
   F is a type family it won't match any KnownTC instance so it's safe to discard
   all these instances.

Of course, these two cases can still match instances of the form `forall a . Show a =>`,
and those instances are retained as they are classified as RM_WildCard instances.

Note [Matches vs Unifiers]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The lookupRM' function returns a pair of potential /matches/ and potential /unifiers/.
The potential matches is likely to be much smaller than the bag of potential unifiers due
to the reasoning about rigid type variables described in Note [Matching a RoughMap].
On the other hand, the instances captured by the RML_NoKnownTC case can still potentially unify
with any instance (depending on the substituion of said rigid variable) so they can't be discounted
from the list of potential unifiers. This is achieved by the RML_NoKnownTC case continuing
the lookup for unifiers by replacing RML_NoKnownTC with RML_LookupOtherTC.

This distinction between matches and unifiers is also important for type families.
During normal type family lookup, we care about matches and when checking for consistency
we care about the unifiers. This is evident in the code as `lookup_fam_inst_env` is
parameterised over a lookup function which either performs matching checking or unification
checking.

In addition to this, we only care whether there are zero or non-zero potential
unifiers, even if we have many candidates, the search can stop before consulting
each candidate. We only need the full list of unifiers when displaying error messages.
Therefore the list is computed lazily so much work can be avoided constructing the
list in the first place.

Note [Simple Matching Semantics]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose `rm` is a RoughMap representing a set of (key,vals) pairs,
  where key::[RoughMapTc] and val::a.
Suppose I look up a key lk :: [RoughMapLookupTc] in `rm`
Then I get back (matches, unifiers) where
   matches  = [ vals | (key,vals) <- rm, key `mightMatch` lk ]
   unifiers = [ vals | (key,vals) <- rm, key `mightUnify` lk ]

Where mightMatch is defined like this:

  mightMatch :: [RoughMapTc] -> [RoughMapLookupTc] -> Bool
  mightMatch []  []    = True   -- A perfectly sized match might match
  mightMatch key []    = True   -- A shorter lookup key matches everything
  mightMatch []  (_:_) = True   -- If the lookup key is longer, then still might match
                                -- Note [RoughMatch and beta reduction]
  mightMatch (k:ks) (lk:lks) =
    = case (k,lk) of
         -- Standard case, matching on a specific known TyCon.
         (RM_KnownTc n1, RML_KnownTc n2) -> n1==n2 && mightMatch ks lks
         -- For example, if the key for 'Show Bool' is [RM_KnownTc Show, RM_KnownTc Bool]
         ---and we match against (Show a[sk]) [RM_KnownTc Show, RML_NoKnownTc]
         -- then Show Bool can never match Show a[sk] so return False.
         (RM_KnownTc _, RML_NoKnownTc)   -> False
         -- Wildcard cases don't inform us anything about the match.
         (RM_WildCard, _ )    -> mightMatch ks lks
         (_, RML_WildCard)    -> mightMatch ks lks

  -- Might unify is very similar to mightMatch apart from RML_NoKnownTc may
  -- unify with any instance.
  mightUnify :: [RoughMapTc] -> [RoughMapLookupTc] -> Bool
  mightUnify []  []    = True   -- A perfectly sized match might unify
  mightUnify key []    = True   -- A shorter lookup key matches everything
  mightUnify []  (_:_) = True
  mightUnify (k:ks) (lk:lks) =
    = case (k,lk) of
         (RM_KnownTc n1, RML_KnownTc n2) -> n1==n2 && mightUnify ks lks
         (RM_KnownTc _, RML_NoKnownTc)   -> mightUnify (k:ks) (RML_WildCard:lks)
         (RM_WildCard, _ )    -> mightUnify ks lks
         (_, RML_WildCard)    -> mightUnify ks lks


The guarantee that RoughMap provides is that

if
   insert_ty `tcMatchTy` lookup_ty
then definitely
   typeToRoughMatchTc insert_ty `mightMatch` typeToRoughMatchLookupTc lookup_ty
but not vice versa

this statement encodes the intuition that the RoughMap is used as a quick pre-filter
to remove instances from the matching pool. The contrapositive states that if the
RoughMap reports that the instance doesn't match then `tcMatchTy` will report that the
types don't match as well.

-}

-- Key for insertion into a RoughMap
data RoughMatchTc
  = RM_KnownTc Name   -- INVARIANT: Name refers to a TyCon tc that responds
                   -- true to `isGenerativeTyCon tc Nominal`. See
                   -- Note [Rough matching in class and family instances]
  | RM_WildCard    -- e.g. type variable at the head
  deriving( Data )

-- Key for lookup into a RoughMap
-- See Note [Matching a RoughMap]
data RoughMatchLookupTc
  = RML_KnownTc Name -- ^ The position only matches the specified KnownTc
  | RML_NoKnownTc -- ^ The position definitely doesn't match any KnownTc
  | RML_WildCard -- ^ The position can match anything
  deriving ( Data )

instance Outputable RoughMatchLookupTc where
    ppr (RML_KnownTc nm) = text "RML_KnownTc" <+> ppr nm
    ppr RML_NoKnownTc = text "RML_NoKnownTC"
    ppr RML_WildCard = text "_"

roughMatchTcToLookup :: RoughMatchTc -> RoughMatchLookupTc
roughMatchTcToLookup (RM_KnownTc n) = RML_KnownTc n
roughMatchTcToLookup RM_WildCard = RML_WildCard

instance Outputable RoughMatchTc where
    ppr (RM_KnownTc nm) = text "KnownTc" <+> ppr nm
    ppr RM_WildCard = text "OtherTc"

isRoughWildcard :: RoughMatchTc -> Bool
isRoughWildcard RM_WildCard  = True
isRoughWildcard (RM_KnownTc {}) = False

typeToRoughMatchLookupTc :: Type -> RoughMatchLookupTc
typeToRoughMatchLookupTc ty
  | Just (ty', _) <- splitCastTy_maybe ty   = typeToRoughMatchLookupTc ty'
  | otherwise =
      case splitAppTys ty of
        -- Case 1: Head of application is a type variable, does not match any KnownTc.
        (TyVarTy {}, _) -> RML_NoKnownTc
        (TyConApp tc _, _)
          -- Case 2: Head of application is a known type constructor, hence KnownTc.
          | not (isTypeFamilyTyCon tc) -> RML_KnownTc $! tyConName tc
          -- Case 3: Head is a type family so it's stuck and therefore doesn't match
          -- any KnownTc
          | isTypeFamilyTyCon tc -> RML_NoKnownTc
        -- Fallthrough: Otherwise, anything might match this position
        _ -> RML_WildCard

typeToRoughMatchTc :: Type -> RoughMatchTc
typeToRoughMatchTc ty
  | Just (ty', _) <- splitCastTy_maybe ty   = typeToRoughMatchTc ty'
  | Just (tc,_)   <- splitTyConApp_maybe ty
  , not (isTypeFamilyTyCon tc)              = assertPpr (isGenerativeTyCon tc Nominal) (ppr tc)
                                              RM_KnownTc $! tyConName tc
    -- See Note [Rough matching in class and family instances]
  | otherwise                               = RM_WildCard

-- | Trie of @[RoughMatchTc]@
--
-- *Examples*
-- @
-- insert [OtherTc] 1
-- insert [OtherTc] 2
-- lookup [OtherTc] == [1,2]
-- @
data RoughMap a = RM { rm_empty   :: Bag a
                     , rm_known   :: DNameEnv (RoughMap a)
                        -- See Note [InstEnv determinism] in GHC.Core.InstEnv
                     , rm_unknown :: RoughMap a }
                | RMEmpty -- an optimised (finite) form of emptyRM
                          -- invariant: Empty RoughMaps are always represented with RMEmpty

                deriving (Functor)

instance Outputable a => Outputable (RoughMap a) where
  ppr (RM empty known unknown) =
      vcat [text "RM"
           , nest 2 (vcat [ text "Empty:" <+> ppr empty
                          , text "Known:" <+> ppr known
                          , text "Unknown:" <+> ppr unknown])]
  ppr RMEmpty = text "{}"

emptyRM :: RoughMap a
emptyRM = RMEmpty

-- | Order of result is deterministic.
lookupRM :: [RoughMatchLookupTc] -> RoughMap a -> [a]
lookupRM tcs rm = bagToList (fst $ lookupRM' tcs rm)


-- | N.B. Returns a 'Bag' for matches, which allows us to avoid rebuilding all of the lists
-- we find in 'rm_empty', which would otherwise be necessary due to '++' if we
-- returned a list. We use a list for unifiers becuase the tail is computed lazily and
-- we often only care about the first couple of potential unifiers. Constructing a
-- bag forces the tail which performs much too much work.
--
-- See Note [Matching a RoughMap]
-- See Note [Matches vs Unifiers]
lookupRM' :: [RoughMatchLookupTc] -> RoughMap a -> (Bag a -- Potential matches
                                                   , [a]) -- Potential unifiers
lookupRM' _                  RMEmpty = (emptyBag, [])
-- See Note [Simple Matching Semantics] about why we return everything when the lookup
-- key runs out.
lookupRM' []                 rm      = let m = elemsRM rm
                                       in (listToBag m, m)
lookupRM' (RML_KnownTc tc : tcs) rm  =
  let (common_m, common_u) = lookupRM' tcs (rm_unknown rm)
      (m, u) = maybe (emptyBag, []) (lookupRM' tcs) (lookupDNameEnv (rm_known rm) tc)
  in (rm_empty rm `unionBags` common_m `unionBags` m
     , bagToList (rm_empty rm) ++ common_u ++ u)
-- A RML_NoKnownTC does **not** match any KnownTC but can unify
lookupRM' (RML_NoKnownTc : tcs)  rm      =

  let (u_m, _u_u) = lookupRM' tcs (rm_unknown rm)
  in (rm_empty rm `unionBags` u_m -- Definitely don't match
     , snd $ lookupRM' (RML_WildCard : tcs) rm) -- But could unify..

lookupRM' (RML_WildCard : tcs)    rm  =
  let (m, u) = bimap unionManyBags concat (mapAndUnzip (lookupRM' tcs) (eltsDNameEnv $ rm_known rm))
      (u_m, u_u) = lookupRM' tcs (rm_unknown rm)
  in (rm_empty rm `unionBags` u_m `unionBags` m
     , bagToList (rm_empty rm) ++ u_u ++ u)

unionRM :: RoughMap a -> RoughMap a -> RoughMap a
unionRM RMEmpty a = a
unionRM a RMEmpty = a
unionRM a b =
  RM { rm_empty = rm_empty a `unionBags` rm_empty b
     , rm_known = plusDNameEnv_C unionRM (rm_known a) (rm_known b)
     , rm_unknown = rm_unknown a `unionRM` rm_unknown b
     }


insertRM :: [RoughMatchTc] -> a -> RoughMap a -> RoughMap a
insertRM k v RMEmpty =
    insertRM k v $ RM { rm_empty = emptyBag
                      , rm_known = emptyDNameEnv
                      , rm_unknown = emptyRM }
insertRM [] v rm@(RM {}) =
    -- See Note [Simple Matching Semantics]
    rm { rm_empty = v `consBag` rm_empty rm }
insertRM (RM_KnownTc k : ks) v rm@(RM {}) =
    rm { rm_known = alterDNameEnv f (rm_known rm) k }
  where
    f Nothing  = Just $ (insertRM ks v emptyRM)
    f (Just m) = Just $ (insertRM ks v m)
insertRM (RM_WildCard : ks) v rm@(RM {}) =
    rm { rm_unknown = insertRM ks v (rm_unknown rm) }

filterRM :: (a -> Bool) -> RoughMap a -> RoughMap a
filterRM _ RMEmpty = RMEmpty
filterRM pred rm =
    normalise $ RM {
      rm_empty = filterBag pred (rm_empty rm),
      rm_known = mapDNameEnv (filterRM pred) (rm_known rm),
      rm_unknown = filterRM pred (rm_unknown rm)
    }

-- | Place a 'RoughMap' in normal form, turning all empty 'RM's into
-- 'RMEmpty's. Necessary after removing items.
normalise :: RoughMap a -> RoughMap a
normalise RMEmpty = RMEmpty
normalise (RM empty known RMEmpty)
  | isEmptyBag empty
  , isEmptyDNameEnv known = RMEmpty
normalise rm = rm

-- | Filter all elements that might match a particular key with the given
-- predicate.
filterMatchingRM :: (a -> Bool) -> [RoughMatchTc] -> RoughMap a -> RoughMap a
filterMatchingRM _    _  RMEmpty = RMEmpty
filterMatchingRM pred [] rm      = filterRM pred rm
filterMatchingRM pred (RM_KnownTc tc : tcs) rm =
    normalise $ RM {
      rm_empty = filterBag pred (rm_empty rm),
      rm_known = alterDNameEnv (join . fmap (dropEmpty . filterMatchingRM pred tcs)) (rm_known rm) tc,
      rm_unknown = filterMatchingRM pred tcs (rm_unknown rm)
    }
filterMatchingRM pred (RM_WildCard : tcs) rm =
    normalise $ RM {
      rm_empty = filterBag pred (rm_empty rm),
      rm_known = mapDNameEnv (filterMatchingRM pred tcs) (rm_known rm),
      rm_unknown = filterMatchingRM pred tcs (rm_unknown rm)
    }

dropEmpty :: RoughMap a -> Maybe (RoughMap a)
dropEmpty RMEmpty = Nothing
dropEmpty rm = Just rm

elemsRM :: RoughMap a -> [a]
elemsRM = foldRM (:) []

foldRM :: (a -> b -> b) -> b -> RoughMap a -> b
foldRM f = go
  where
    -- N.B. local worker ensures that the loop can be specialised to the fold
    -- function.
    go z RMEmpty = z
    go z (RM{ rm_unknown = unk, rm_known = known, rm_empty = empty}) =
      foldr
        f
        (foldDNameEnv
           (flip go)
           (go z unk)
           known
        )
        empty

nonDetStrictFoldRM :: (b -> a -> b) -> b -> RoughMap a -> b
nonDetStrictFoldRM f = go
  where
    -- N.B. local worker ensures that the loop can be specialised to the fold
    -- function.
    go !z RMEmpty = z
    go  z rm@(RM{}) =
      foldl'
        f
        (nonDetStrictFoldDNameEnv
           (flip go)
           (go z (rm_unknown rm))
           (rm_known rm)
        )
        (rm_empty rm)

sizeRM :: RoughMap a -> Int
sizeRM = nonDetStrictFoldRM (\acc _ -> acc + 1) 0
