-----------------------------------------------------------------------------
-- |
-- Module      :  Debug.QuickCheck.Poly
-- Copyright   :  (c) Andy Gill 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Control.Exception, Control.Concurrent)
--
-- This is an attempt to emulate polymorphic types for the 
-- purposes of testing by using abstract monomorphic types.
-- 
-- It is likely that future versions of QuickCheck will
-- include some polymorphic emulation testing facility,
-- but this module can be used for now.
--
-----------------------------------------------------------------------------

module Debug.QuickCheck.Poly
  ( ALPHA
  , BETA
  , GAMMA
  , OrdALPHA
  , OrdBETA
  , OrdGAMMA
  ) where

import Prelude

import Debug.QuickCheck
import Debug.QuickCheck.Utils

{- This is the basic pseudo-polymorphic object.
 - The idea is you can't cheat, and use the integer
 - directly, but need to use the abstraction.
 - 
 - We use phantom types (ref: Domain Specific Embedded Compilers,
 - Daan Leijen & Erik Meijer, 2nd Conference of Domain Specific
 - Languages, Austin, TX, 1999)
 -}

newtype Poly a = Poly Int

instance Show (Poly a) where
        show (Poly a) = "_" ++ show a

instance Arbitrary (Poly a) where
    arbitrary            = sized $ \n -> (choose (1,n) >>= return . Poly)
    coarbitrary (Poly n) = variant (if n >= 0 then 2*n else 2*(-n) + 1)

instance Eq a => Eq (Poly a) where
        (Poly a) == (Poly b) = a == b

instance Ord a => Ord (Poly a) where
        (Poly a) `compare` (Poly b) = a `compare` b

{-
 - These are what we export, our pseudo-polymorphic instances.
 -}

type ALPHA = Poly ALPHA_
data ALPHA_ = ALPHA_ deriving (Eq)

type BETA = Poly BETA_
data BETA_ = BETA_ deriving (Eq)

type GAMMA = Poly GAMMA_
data GAMMA_ = GAMMA_ deriving (Eq)

type OrdALPHA = Poly OrdALPHA_
data OrdALPHA_ = OrdALPHA_ deriving (Eq,Ord)

type OrdBETA = Poly OrdBETA_
data OrdBETA_ = OrdBETA_ deriving (Eq,Ord)

type OrdGAMMA = Poly OrdGAMMA_
data OrdGAMMA_ = OrdGAMMA_ deriving (Eq,Ord)

{-
 - This is a condition on OrdALPHA, OrdBETA, etc, itself.
 - It states that all OrdALPHA objects obey total ordering.
 -}

prop_OrdPOLY x y = isTotalOrder x y
    where types = (x :: OrdALPHA, y :: OrdALPHA)
