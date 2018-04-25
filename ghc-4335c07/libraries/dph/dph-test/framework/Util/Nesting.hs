{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
module Util.Nesting where
import Control.Monad
import Util.Array               (Array)
import Data.Vector              (Vector)
import qualified Util.Array     as A
import qualified Data.Vector    as V


-- | Class of nested structures.
class Nested c a e i | c a -> e, c a -> i where
 rank     :: c a -> Int
 nesting  :: c a -> i
 flatten  :: c a -> Vector e


instance ( Array c1 (c2 a)
         , Array c2 a
         , Nested c2 a e i)
 =>      Nested c1 (c2 a) e (Vector i) where
         
 rank xs  = 1 + rank (A.index xs 0)
 nesting  = V.map nesting . A.toVector
 flatten  = join . V.map flatten . A.toVector


-- | Instances for atomic types need to be written out manually. If GHC had
--   backtracking constraint resolution then we could make an Atom class for
--   this, but it doesn't. 
instance Array a Int => Nested a Int Int Int where
 rank _      = 1
 nesting     = A.length . A.toVector
 flatten xs  = A.toVector xs

instance Array a Float => Nested a Float Float Int where
 rank _      = 1
 nesting     = A.length . A.toVector
 flatten xs  = A.toVector xs

instance Array a Bool => Nested a Bool Bool Int where
 rank _      = 1
 nesting     = A.length . A.toVector
 flatten xs  = A.toVector xs


-- | Compare two nested structures for equality, not caring exactly what
--   data structure defines the nesting. Provided the elements are equal,
--   and the two structures are congruent, then we'll call them equal.
(~=) :: ( Eq i, Eq e
        , Nested c1 a1 e i
        , Nested c2 a2 e i)
     => c1 a1 -> c2 a2 -> Bool
   
(~=) xs ys
        =   nesting xs == nesting ys
        &&  flatten xs == flatten ys

 