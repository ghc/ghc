{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}

-- | 'RoughMap' is an approximate finite map data structure keyed on
-- @['RoughMatchTc']@. This is useful when keying maps on lists of 'Type's
-- (e.g. an instance head).
module GHC.Core.RoughMap
  ( -- * RoughMatchTc
    RoughMatchTc(..)
  , isRoughOtherTc
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
import {-# SOURCE #-} GHC.Tc.Utils.TcType

import Control.Monad (join)
import Data.Data (Data)
import GHC.Types.Var.Set
import GHC.Utils.Misc
import Data.Bifunctor

{-
Note [Rough maps of Types]
~~~~~~~~~~~~~~~~~~~~~~~~~~

Note [Matching a RoughMap]
~~~~~~~~~~~~~~~~~~~~~~~~~~

The lookup key into a rough map is slightly different to the insertion key.
Like the insertion key each argument is classified to a simpler key which describes
what could match that position. There are three possibilities:

* LookupKnownTc Name: The argument is headed by a known type constructor.
* NoKnownTc: The argument is definitely not headed by any known type constructor.
* LookupOtherTc: The argument could match anything, we don't know enough about it.

The interesting case is the second case as that's different to the insertion key
for a rough map. The second case arises in two situations:

1. The head of the application is a skolem type variable. The type variable definitely
   doesn't match with any of the KnownTC instances so we can discard them all. For example:
    Show a[sk] or Show (a[sk] b[sk]). One place constraints like this arise is when
    typechecking derived instances.
2. The head of the application is a known type family whose arguments contain no
   meta variables. For example: F a[sk]. The application of F is stuck, and because
   F is a type family it won't match any KnownTC instance so it's safe to discard
   all these instances.


The lookup key into a rough map is slightly different to the insertion key.
Like the insertion key each argument is classified to a simpler key which describes
what could match that position. There are three possibilities:

* LookupKnownTc Name: The argument is headed by a known type constructor.
* NoKnownTc: The argument is definitely not headed by any known type constructor.
* LookupOtherTc: The argument could match anything, we don't know enough about it.

The interesting case is the second case as that's different to the insertion key
for a rough map. The second case arises in two situations:

1. The head of the application is a skolem type variable. The type variable definitely
   doesn't match with any of the KnownTC instances so we can discard them all. For example:
    Show a[sk] or Show (a[sk] b[sk]). One place constraints like this arise is when
    typechecking derived instances.
2. The head of the application is a known type family whose arguments contain no
   meta variables. For example: F a[sk]. The application of F is stuck, and because
   F is a type family it won't match any KnownTC instance so it's safe to discard
   all these instances.

Of course, these two cases can still match instances of the form `forall a . Show a =>`,
and those instances are retained as they are not classified as KnownTC instances.


Note [Matches vs Unifiers]
~~~~~~~~~~~~~~~~~~~~~~~~~~

The lookupRM' function returns a pair of potential matches and potential unifiers.
The potential matches is likely to be much smaller than the bag of potential unifiers due
to the reasoning about rigid type variables described in Note [Matching a RoughMap].
On the other hand, the instances captured by the NoKnownTC case can still potentially unify
with any instance (depending on the substituion of said rigid variable) so they can't be discounted
from the list of potential unifiers. This is achieved by the NoKnownTC case continuing
the lookup for unifiers by replacing NoKnownTC with LookupOtherTC.

In addition to this, we usually only care whether there are zero, one or two potential
unifiers, even if we have many candidates, the search can stop before consulting
each candidate. We only need the full list of unifiers when displaying error messages.
Therefore the bag is computed lazily so much work can be avoided constructing the
bag in the first place.

-}

-- Key for insertion into a RoughMap
data RoughMatchTc
  = KnownTc Name   -- INVARIANT: Name refers to a TyCon tc that responds
                   -- true to `isGenerativeTyCon tc Nominal`. See
                   -- Note [Rough matching in class and family instances]
  | OtherTc        -- e.g. type variable at the head
  deriving( Data )

-- Key for lookup into a RoughMap
-- See Note [Matching a RoughMap]
data RoughMatchLookupTc
  = LookupKnownTc Name -- ^ The position only matches the specified KnownTc
  | NoKnownTc -- ^ The position definitely doesn't match any KnownTc
  | LookupOtherTc -- ^ The position can match anything
  deriving ( Data )

instance Outputable RoughMatchLookupTc where
    ppr (LookupKnownTc nm) = text "LookupKnownTc" <+> ppr nm
    ppr LookupOtherTc = text "LookupOtherTc"
    ppr NoKnownTc = text "NoKnownTc"

roughMatchTcToLookup :: RoughMatchTc -> RoughMatchLookupTc
roughMatchTcToLookup (KnownTc n) = LookupKnownTc n
roughMatchTcToLookup OtherTc = LookupOtherTc

instance Outputable RoughMatchTc where
    ppr (KnownTc nm) = text "KnownTc" <+> ppr nm
    ppr OtherTc = text "OtherTc"

isRoughOtherTc :: RoughMatchTc -> Bool
isRoughOtherTc OtherTc      = True
isRoughOtherTc (KnownTc {}) = False

typeToRoughMatchLookupTc :: Type -> RoughMatchLookupTc
typeToRoughMatchLookupTc ty
  | Just (ty', _) <- splitCastTy_maybe ty   = typeToRoughMatchLookupTc ty'
  | otherwise =
      case splitAppTys ty of
        -- Case 1: Head of application is a SkolemTV, does not match any KnownTc.
        (TyVarTy tv, _) | isSkolemTyVar tv -> NoKnownTc
        (TyConApp tc [], tys)
          -- Case 2: Head of application is a known type constructor, hence KnownTc.
          | not (isTypeFamilyTyCon tc) -> LookupKnownTc $! tyConName tc
          -- Case 3: Head is a type family, but it's applied to skolem types so
          --         it's stuck and can't match any KnownTC instances.
          | not (anyVarSet isMetaTyVar (tyCoVarsOfTypes tys)) -> NoKnownTc
        -- Fallthrough: Otherwise, anything might match this position
        _ -> LookupOtherTc

typeToRoughMatchTc :: Type -> RoughMatchTc
typeToRoughMatchTc ty
  | Just (ty', _) <- splitCastTy_maybe ty   = typeToRoughMatchTc ty'
  | Just (tc,_)   <- splitTyConApp_maybe ty
  , not (isTypeFamilyTyCon tc)              = -- assertPpr (isGenerativeTyCon tc Nominal) (ppr tc)
                                              KnownTc $! tyConName tc
    -- See Note [Rough matching in class and family instances]
  | otherwise                               = OtherTc

-- | Trie of @[RoughMatchTc]@
--
-- *Examples*
-- @
-- insert [OtherTc] 1
-- insert [OtherTc] 2
-- lookup [OtherTc] == [1,2]
-- @
data RoughMap a = RM { rm_empty   :: Bag a
                     , rm_known   :: !(DNameEnv (RoughMap a))
                        -- See Note [InstEnv determinism] in GHC.Core.InstEnv
                     , rm_unknown :: !(RoughMap a) }
                | RMEmpty -- an optimised (finite) form of emptyRM
                          -- invariant: Empty RoughMaps are always represented with RMEmpty
                deriving (Functor)

emptyRM :: RoughMap a
emptyRM = RMEmpty

-- | Order of result is deterministic.
lookupRM :: [RoughMatchLookupTc] -> RoughMap a -> [a]
lookupRM tcs rm = bagToList (fst $ lookupRM' tcs rm)


-- | N.B. Returns a 'Bag', which allows us to avoid rebuilding all of the lists
-- we find in 'rm_empty', which would otherwise be necessary due to '++' if we
-- returned a list.
--
-- See Note [Matching a RoughMap]
-- See Note [Matces vs Unifiers]
lookupRM' :: [RoughMatchLookupTc] -> RoughMap a -> (Bag a -- Potential matches
                                                   , Bag a) -- Potential unifiers
lookupRM' _                  RMEmpty = (emptyBag, emptyBag)
lookupRM' []                 rm      = let m = listToBag $ elemsRM rm
                                       in (m, m)
lookupRM' (LookupKnownTc tc : tcs) rm  =
  let (common_m, common_u) = lookupRM' tcs (rm_unknown rm)
      (m, u) = maybe (emptyBag, emptyBag) (lookupRM' tcs) (lookupDNameEnv (rm_known rm) tc)
  in (rm_empty rm `unionBags` common_m `unionBags` m
     , rm_empty rm `unionBags` common_u `unionBags` u)
-- A SkolemTC does **not** match any KnownTC but can unify
lookupRM' (NoKnownTc : tcs)  rm      =

  let (u_m, _u_u) = lookupRM' tcs (rm_unknown rm)
      empty = rm_empty rm
  in (u_m `unionBags` empty -- Definitely don't match
     , snd $ lookupRM' (LookupOtherTc : tcs) rm) -- But could unify..

lookupRM' (LookupOtherTc : tcs)    rm  =
  let (m, u) = bimap unionManyBags unionManyBags (mapAndUnzip (lookupRM' tcs) (eltsDNameEnv $ rm_known rm))
      (u_m, u_u) = lookupRM' tcs (rm_unknown rm)
  in (rm_empty rm `unionBags` u_m `unionBags` m
     , rm_empty rm `unionBags` u_u `unionBags` u)

unionRM :: RoughMap a -> RoughMap a -> RoughMap a
unionRM RMEmpty a = a
unionRM a RMEmpty = a
unionRM a b =
  RM { rm_empty = rm_empty a `unionBags` rm_empty b
     , rm_known = plusDNameEnv_C unionRM (rm_known a) (rm_known b)
     , rm_unknown = rm_unknown a `unionRM` rm_unknown b
     }

{-
Note [RoughMap]
~~~~~~~~~~~~~~~
A RoughMap is semantically a list of (key,value) pairs, where
   key :: [RoughMatchTc]
So, writing # for `OtherTc`, and Int for `KnownTc "Int"`, we might have
    [ ([#, Int, Maybe, #, Int], v1)
    , ([Int, #, List], v2 ]

We lookup a key of type [RoughMatchTc], and return the list of all values whose
keys "match", where matching means:
  * OtherTc matches anything
  * `KnownTc n1` matches OtherTc, or `KnownTc n2` if n1=n2
  * If the lists are of different length, extend the shorter with OtherTc

Given the above map, here are the results of some lookups:
   Lookup key       Result
   -------------------------
   [Int, Int]       [v1,v2]
   [Int,Int,List]   [v2]
   [Bool]           []

The idea is that we can use a `RoughMap` as a pre-filter, to produce a
short-list of candidates to examine more closely.
-}

    -- TODO: Including rm_empty due to Note [Eta reduction for data families]
    -- in GHC.Core.Coercion.Axiom. e.g., we may have an environment which includes
    --     data instance Fam Int a = ...
    -- which will result in `axiom ax :: Fam Int ~ FamInt` and an FamInst with
    -- `fi_tcs = [Int]`, `fi_eta_tvs = [a]`. We need to make sure that this
    -- instance matches when we are looking for an instance `Fam Int a`.

insertRM :: [RoughMatchTc] -> a -> RoughMap a -> RoughMap a
insertRM k v RMEmpty =
    insertRM k v $ RM { rm_empty = emptyBag
                      , rm_known = emptyDNameEnv
                      , rm_unknown = emptyRM }
insertRM [] v rm@(RM {}) =
    rm { rm_empty = v `consBag` rm_empty rm }
insertRM (KnownTc k : ks) v rm@(RM {}) =
    rm { rm_known = alterDNameEnv f (rm_known rm) k }
  where
    f Nothing  = Just $ insertRM ks v emptyRM
    f (Just m) = Just $ insertRM ks v m
insertRM (OtherTc : ks) v rm@(RM {}) =
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
filterMatchingRM pred (KnownTc tc : tcs) rm =
    normalise $ RM {
      rm_empty = filterBag pred (rm_empty rm),
      rm_known = alterDNameEnv (join . fmap (dropEmpty . filterMatchingRM pred tcs)) (rm_known rm) tc,
      rm_unknown = filterMatchingRM pred tcs (rm_unknown rm)
    }
filterMatchingRM pred (OtherTc : tcs) rm =
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
    go z rm@(RM{}) =
      foldr
        f
        (foldDNameEnv
           (flip go)
           (go z (rm_unknown rm))
           (rm_known rm)
        )
        (rm_empty rm)

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
