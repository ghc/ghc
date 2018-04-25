{-# LANGUAGE CPP #-}
{-# OPTIONS -fvectorise #-}
module CommonVectorised where
import Data.Array.Parallel.Prelude.Bool
import Data.Array.Parallel.Prelude.Double
import qualified Prelude    as P

#include "Common.inc.hs"
