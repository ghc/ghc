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

{-
Note [Rough maps of Types]
~~~~~~~~~~~~~~~~~~~~~~~~~~

-}

-- Key for insertion into a RoughMap
data RoughMatchTc
  = KnownTc Name   -- INVARIANT: Name refers to a TyCon tc that responds
                   -- true to `isGenerativeTyCon tc Nominal`. See
                   -- Note [Rough matching in class and family instances]
  | OtherTc        -- e.g. type variable at the head
  deriving( Data )

-- Key for lookup into a RoughMap
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
  | (TyVarTy tv, _) <- splitAppTys ty, isSkolemTyVar tv = NoKnownTc
  | Just (tc,_)   <- splitTyConApp_maybe ty
  , not (isTypeFamilyTyCon tc)              = -- assertPpr (isGenerativeTyCon tc Nominal) (ppr tc)
                                              LookupKnownTc $! tyConName tc
  | Just (tc, tys)   <- splitTyConApp_maybe ty
  , isTypeFamilyTyCon tc
  , not (anyVarSet isMetaTyVar (tyCoVarsOfTypes tys))
    = NoKnownTc
  | otherwise = LookupOtherTc

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
lookupRM tcs rm = bagToList (lookupRM' tcs rm)

-- | N.B. Returns a 'Bag', which allows us to avoid rebuilding all of the lists
-- we find in 'rm_empty', which would otherwise be necessary due to '++' if we
-- returned a list.
lookupRM' :: [RoughMatchLookupTc] -> RoughMap a -> Bag a
lookupRM' _                  RMEmpty = emptyBag
lookupRM' []                 rm      = listToBag $ elemsRM rm
lookupRM' (LookupKnownTc tc : tcs) rm  = foldl' unionBags emptyBag
                                       [ maybe emptyBag (lookupRM' tcs) (lookupDNameEnv (rm_known rm) tc)
                                       , lookupRM' tcs (rm_unknown rm)
                                       , rm_empty rm
                                       ]
-- A SkolemTC does **not** match any KnownTC
lookupRM' (NoKnownTc : tcs)  rm      = foldl' unionBags emptyBag
                                       [ lookupRM' tcs (rm_unknown rm)
                                       , rm_empty rm ]

lookupRM' (LookupOtherTc : tcs)    rm  = foldl' unionBags emptyBag
                                       [ foldl' unionBags emptyBag $ map (lookupRM' tcs) (eltsDNameEnv $ rm_known rm)
                                       , lookupRM' tcs (rm_unknown rm)
                                       , rm_empty rm
                                       ]

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
