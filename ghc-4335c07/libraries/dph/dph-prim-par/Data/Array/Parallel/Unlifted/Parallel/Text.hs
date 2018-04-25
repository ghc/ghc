{-# OPTIONS -fno-warn-orphans #-}
-- | Read\/Show instances for segmented unlifted arrays.
module Data.Array.Parallel.Unlifted.Parallel.Text ()
where
import Data.Array.Parallel.Base.Text (showsApp)
import Data.Array.Parallel.Unlifted.Parallel.UPSegd (UPSegd, takeLengths)

instance Show UPSegd where
  showsPrec k = showsApp k "toUPSegd" . takeLengths
