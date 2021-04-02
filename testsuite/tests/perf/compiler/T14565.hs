-- GHC-8.2.2 gets stuck in SpecConstr while compiling this module
module T14565 where

import           Control.Monad (forM_, when)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

data Dimensions = Dimensions Int Int Int Int
  deriving (Show)

-- Changing this to `toIdx = const 0` removes the issue.
toIdx :: Dimensions -> Int
toIdx (Dimensions _ _ _ _) = 0

data Grid = Grid Dimensions (V.Vector Double)
  deriving (Show)

pointsGrid
  :: Dimensions
  -> Grid
  -> Grid
pointsGrid scs (Grid _ gv) = Grid scs $ V.create $ do
  let v0 = V.fromList [(0::Int)]

  v <- VM.replicate 100 0

  -- Removing one of `V.forM_ v0` branches seems to remove the issue.
  V.forM_ v0 $ \_ ->
    V.forM_ v0 $ \_ ->
      V.forM_ v0 $ \_ ->
        V.forM_ v0 $ \_ -> VM.unsafeModify v (+ 0) (toIdx scs)

  pure v
