{- Union-find data structure compiled from Distribution.Utils.UnionFind -}
module GHC.Data.UnionFind where

import GHC.Prelude
import Data.STRef
import Control.Monad.ST
import Control.Monad

-- | A variable which can be unified; alternately, this can be thought
-- of as an equivalence class with a distinguished representative.
newtype Point s a = Point (STRef s (Link s a))
    deriving (Eq)

-- | Mutable write to a 'Point'
writePoint :: Point s a -> Link s a -> ST s ()
writePoint (Point v) = writeSTRef v

-- | Read the current value of 'Point'.
readPoint :: Point s a -> ST s (Link s a)
readPoint (Point v) = readSTRef v

-- | The internal data structure for a 'Point', which either records
-- the representative element of an equivalence class, or a link to
-- the 'Point' that actually stores the representative type.
data Link s a
    -- NB: it is too bad we can't say STRef Int#; the weights remain boxed
    = Info {-# UNPACK #-} !(STRef s Int) {-# UNPACK #-} !(STRef s a)
    | Link {-# UNPACK #-} !(Point s a)

-- | Create a fresh equivalence class with one element.
fresh :: a -> ST s (Point s a)
fresh desc = do
    weight <- newSTRef 1
    descriptor <- newSTRef desc
    Point `fmap` newSTRef (Info weight descriptor)

-- | Flatten any chains of links, returning a 'Point'
-- which points directly to the canonical representation.
repr :: Point s a -> ST s (Point s a)
repr point = readPoint point >>= \r ->
  case r of
    Link point' -> do
        point'' <- repr point'
        when (point'' /= point') $ do
            writePoint point =<< readPoint point'
        return point''
    Info _ _ -> return point

-- | Return the canonical element of an equivalence
-- class 'Point'.
find :: Point s a -> ST s a
find point =
    -- Optimize length 0 and 1 case at expense of
    -- general case
    readPoint point >>= \r ->
      case r of
        Info _ d_ref -> readSTRef d_ref
        Link point' -> readPoint point' >>= \r' ->
          case r' of
            Info _ d_ref -> readSTRef d_ref
            Link _ -> repr point >>= find

-- | Unify two equivalence classes, so that they share
-- a canonical element. Keeps the descriptor of point2.
union :: Point s a -> Point s a -> ST s ()
union refpoint1 refpoint2 = do
    point1 <- repr refpoint1
    point2 <- repr refpoint2
    when (point1 /= point2) $ do
      l1 <- readPoint point1
      l2 <- readPoint point2
      case (l1, l2) of
          (Info wref1 dref1, Info wref2 dref2) -> do
              weight1 <- readSTRef wref1
              weight2 <- readSTRef wref2
              -- Should be able to optimize the == case separately
              if weight1 >= weight2
                  then do
                      writePoint point2 (Link point1)
                      -- The weight calculation here seems a bit dodgy
                      writeSTRef wref1 (weight1 + weight2)
                      writeSTRef dref1 =<< readSTRef dref2
                  else do
                      writePoint point1 (Link point2)
                      writeSTRef wref2 (weight1 + weight2)
          _ -> error "UnionFind.union: repr invariant broken"

-- | Test if two points are in the same equivalence class.
equivalent :: Point s a -> Point s a -> ST s Bool
equivalent point1 point2 = liftM2 (==) (repr point1) (repr point2)

