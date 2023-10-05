{-# LANGUAGE ForeignFunctionInterface, MagicHash #-}
module T12076lit where

-- This test-case demonstrates that cpeApp's collect_args can
-- be invoked on a literal

import Foreign.C
import Foreign
import GHC.Exts

main = do let y = Ptr "LOL"#
          x <- strlen y
          x2 <- strlen y -- don't inline y
          case (x,x2) of
            (3,3) -> putStrLn "Yes"
            _ -> putStrLn "No"

foreign import ccall unsafe "strlen"
  strlen :: Ptr a -> IO Int
