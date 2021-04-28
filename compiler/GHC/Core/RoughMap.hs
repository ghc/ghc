{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | 'RoughMap' is an approximate finite map data structure keyed on
-- @['RoughMatchTc']@. This is useful when keying maps on lists of 'Type's
-- (e.g. an instance head).
module GHC.Core.RoughMap
  ( -- * RoughMatchTc
    RoughMatchTc(..)
  , isRoughOtherTc
  , typeToRoughMatchTc

    -- * RoughMap
  , RoughMap
  , emptyRM
  , lookupRM
  , insertRM
  , filterRM
  , elemsRM
  , sizeRM
  ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Unique.FM (NonDetUniqFM(..))

import Data.Monoid
import Data.Data (Data)

{-
Note [Rough maps of Types]
~~~~~~~~~~~~~~~~~~~~~~~~~~

-}

data RoughMatchTc
  = KnownTc Name   -- INVARIANT: Name refers to a TyCon tc that responds
                   -- true to `isGenerativeTyCon tc Nominal`. See
                   -- Note [Rough matching in class and family instances]
  | OtherTc        -- e.g. type variable at the head
  deriving( Data )

isRoughOtherTc :: RoughMatchTc -> Bool
isRoughOtherTc OtherTc      = True
isRoughOtherTc (KnownTc {}) = False

typeToRoughMatchTc :: Type -> RoughMatchTc
typeToRoughMatchTc ty
  | Just (ty', _) <- splitCastTy_maybe ty   = typeToRoughMatchTc ty'
  | Just (tc,_)   <- splitTyConApp_maybe ty
  , not (isTypeFamilyTyCon tc)              = ASSERT2( isGenerativeTyCon tc Nominal, ppr tc )
                                              KnownTc (tyConName tc)
    -- See Note [Rough matching in class and family instances]
  | otherwise                               = OtherTc

-- trie of [RoughTc]
--
-- insert [UnknownTc] 1
-- insert [UnknownTc] 2
-- lookup [UnknownTc] == [1,2]
data RoughMap a = RM { rm_empty   :: [a]
                     , rm_known   :: !(NameEnv (RoughMap a))
                     , rm_unknown :: !(RoughMap a) }
                | RMEmpty -- an optimised (finite) form of emptyRM

emptyRM :: RoughMap a
emptyRM = RMEmpty

lookupRM :: [RoughMatchTc] -> RoughMap a -> [a]
lookupRM _                  RMEmpty = []
lookupRM []                 rm      = elemsRM rm
lookupRM (KnownTc tc : tcs) rm      = maybe [] (lookupRM tcs) (lookupNameEnv (rm_known rm) tc)
                                      ++ lookupRM tcs (rm_unknown rm)
                                      ++ rm_empty rm
lookupRM (OtherTc : tcs)    rm      = [ x
                                      | m <- nameEnvElts (rm_known rm)
                                      , x <- lookupRM tcs m ]
                                      ++ lookupRM tcs (rm_unknown rm)
                                      ++ rm_empty rm

{-
Note [RoughMap]
~~~~~~~~~~~~~~~
RoughMap is a finite map keyed on the rough "shape" of a list of type
applications. This allows efficient (yet approximate) instance look-up.

-}

    -- TODO: Including rm_empty due to Note [Eta reduction for data families]
    -- in GHC.Core.Coercion.Axiom. e.g., we may have an environment which includes
    --     data instance Fam Int a = ...
    -- which will result in `axiom ax :: Fam Int ~ FamInt` and an FamInst with
    -- `fi_tcs = [Int]`, `fi_eta_tvs = [a]`. We need to make sure that this
    -- instance matches when we are looking for an instance `Fam Int a`.

insertRM :: [RoughMatchTc] -> a -> RoughMap a -> RoughMap a
insertRM k v RMEmpty =
    insertRM k v $ RM { rm_empty = []
                      , rm_known = emptyNameEnv
                      , rm_unknown = emptyRM }
insertRM [] v rm@(RM {}) =
    rm { rm_empty = v : rm_empty rm }
insertRM (KnownTc k : ks) v rm@(RM {}) =
    rm { rm_known = alterNameEnv f (rm_known rm) k }
  where
    f Nothing  = Just $ insertRM ks v emptyRM
    f (Just m) = Just $ insertRM ks v m
insertRM (OtherTc : ks) v rm@(RM {}) =
    rm { rm_unknown = insertRM ks v (rm_unknown rm) }

filterRM :: (a -> Bool) -> RoughMap a -> RoughMap a
filterRM _ RMEmpty = RMEmpty
filterRM pred rm = norm $ RM { rm_empty = filter pred (rm_empty rm)
                             , rm_known = mapNameEnv (filterRM pred) (rm_known rm)
                             , rm_unknown = filterRM pred (rm_unknown rm)
                             }
  where
    norm RMEmpty = RMEmpty
    norm (RM [] known RMEmpty)
      | isEmptyNameEnv known = RMEmpty
    norm rm = rm

elemsRM :: RoughMap a -> [a]
elemsRM RMEmpty = []
elemsRM rm      = rm_empty rm ++ concatMap elemsRM (nameEnvElts $ rm_known rm) ++ elemsRM (rm_unknown rm)

sizeRM :: RoughMap a -> Int
sizeRM RMEmpty = 0
sizeRM rm      = length (rm_empty rm)
                 + getSum (foldMap (Sum . sizeRM) (NonDetUniqFM $ rm_known rm))
                 + sizeRM (rm_unknown rm)
