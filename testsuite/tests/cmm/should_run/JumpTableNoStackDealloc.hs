{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

import Data.Foldable
import GHC.Exts
import GHC.Int

foreign import prim "foo" foo :: Int64# -> Int64#

main :: IO ()
main = for_ [0 .. 9] $ \(I64# x#) -> print $ I64# (foo x#)
