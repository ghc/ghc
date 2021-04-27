{- Vendored from https://hackage.haskell.org/package/Tainted-0.1.0.2:

Copyright (c) 2015, Ross Meikleham
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of Tainted nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Data.Tainted
-- Copyright   :  Ross Meikleham, Sebastian Graf
-- License     :  BSD-style
--
-- Maintainer  :  RossMeikleham@hotmail.co.uk, sgraf1337@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The Tainted type, and associated operations.
-----------------------------------------------------------------------------

module GHC.Data.Tainted
  ( Tainted(Dirty, Clean)
  , dirtyIf, forgetTaint, setWhenClean, untaintPart
  , isClean, isDirty
  , cleans, dirtys, partitionTaints
  ) where

import GHC.Prelude
import GHC.Show (appPrec, appPrec1)
import GHC.Utils.Outputable
import Data.Bifunctor (bimap, second)
import Data.List (partition)
import Data.Ord (comparing)
import qualified Data.Semigroup as S

-- | The 'Tainted' type encapsulates a value. A value of type @'Tainted' a@
-- either contains a \"clean\" value of type @a@ (represented as @'Clean' a@),
-- or it contains a \"dirty\" value of type @a@ (represented as @'Dirty' a@).
--
-- The 'Tainted' type is also a monad. Once the \"dirty\" state has been
-- reached, and clean operations performed themselves create a \"dirty\" value.
data Tainted a
  = Tainted_ !Bool a -- internal data constructor, only accessed by patsyns below

pattern Dirty :: a -> Tainted a -- exported pattern synonyms
pattern Dirty a <- Tainted_ True a where
  Dirty a = a `seq` Tainted_ True a -- NB: strict builder
pattern Clean :: a -> Tainted a
pattern Clean a = Tainted_ False a
{-# COMPLETE Dirty, Clean #-}

-- | Mark the given value as 'Dirty' if the condition is 'True'.
dirtyIf :: Bool -> a -> Tainted a
dirtyIf True a = Dirty a
dirtyIf _    a = Clean a

-- | Forget whether a value is tainted and return the value.
forgetTaint :: Tainted a -> a
forgetTaint (Dirty a) = a
forgetTaint (Clean a) = a

------------------------
-- Type class instances:

taintedTag :: Tainted a -> Bool
taintedTag Dirty{} = True
taintedTag Clean{} = False

instance Eq a => Eq (Tainted a) where
  ta == tb = taintedTag ta == taintedTag tb && forgetTaint ta == forgetTaint tb

instance Ord a => Ord (Tainted a) where
  compare = comparing taintedTag S.<> comparing forgetTaint

instance Show a => Show (Tainted a) where
  showsPrec prec ta = showParen (prec > appPrec) $ case ta of
    Clean a -> showString "Clean " . showsPrec appPrec1 a
    Dirty a -> showString "Dirty " . showsPrec appPrec1 a

instance Outputable a => Outputable (Tainted a) where
  ppr (Clean a) = text "Clean " <> ppr a
  ppr (Dirty a) = text "Dirty " <> ppr a

instance Functor Tainted where
    fmap f (Dirty a) = Dirty (f a)
    fmap f (Clean a) = Clean (f a)

-- | @combineTaint (Clean _) (Clean _) = 'Clean'@ and 'Dirty' otherwise.
combineTaint :: Tainted a -> Tainted b -> c -> Tainted c
combineTaint (Clean _) (Clean _) = Clean
combineTaint _         !_        = Dirty

instance Applicative Tainted where
    pure = Clean
    tf <*> ta = combineTaint tf ta (forgetTaint tf $ forgetTaint ta)

instance Monad Tainted where
    tx >>= f = case f (forgetTaint tx) of
      ty -> combineTaint tx ty (forgetTaint ty)

---------------------------
-- Other useful operations:

-- | Replaces the wrapped value of a 'Clean' by the given one. Useful if 'Clean'
-- means unmodified, in which case there often is an old value that we can reuse
-- instead of computing the same value all over again.
--
-- Example: Updating a key in a map that isn't present in the map. In that case,
-- we can just re-use the old map. If @Map.update f k map@ would return a
-- @Clean map@ if @k@ is not present, we could just call
-- @setWhenClean map (Map.update f k map)@ and extract the original map with
-- 'forgetTaint' afterwards.
setWhenClean :: a -> Tainted a -> Tainted a
setWhenClean a (Clean _) = Clean a
setWhenClean _ (Dirty a) = Dirty a

-- | Untaints a part of the tainted value, as defined by the projection in left
-- (untainted) and right (tainted) part.
untaintPart :: (a -> (b, c)) -> Tainted a -> (b, Tainted c)
untaintPart f (Dirty a) = second Dirty (f a)
untaintPart f (Clean a) = second Clean (f a)

-- | Returns 'True' iff its argument is of the form 'Clean _.
isClean :: Tainted a -> Bool
isClean (Clean _) = True
isClean _         = False

-- | Returns 'True' iff its argument is of the form Dirty _.
isDirty :: Tainted a -> Bool
isDirty = not . isClean

-- | Extracts from a list of 'Tainted' all the 'Clean' elements.
--   All the 'Clean' elements are extracted in order.
cleans :: [Tainted a] -> [a]
cleans = map forgetTaint . filter isClean

-- | Extracts from a list of 'Tainted' all the 'Dirty' elements.
--   All the 'Dirty' elements are extracted in order.
dirtys :: [Tainted a] -> [a]
dirtys = map forgetTaint . filter isDirty

-- | Partitions a list of 'Tainted' into two lists.
--   All the 'Dirty' elements are extracted, in order, to the first component of the output.
--   Similarly the 'Clean' elements are extracted to the second component of the output.
partitionTaints :: [Tainted a] -> ([a], [a])
partitionTaints = bimap (map forgetTaint) (map forgetTaint) . partition isClean
