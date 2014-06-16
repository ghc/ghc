{-# LANGUAGE MagicHash, UnboxedTuples #-}
-- test the representation of unboxed literals

module Main
where

import GHC.Exts
import GHC.Float
import Language.Haskell.TH
import Text.PrettyPrint
import System.IO

main :: IO ()
main = do putStrLn $ show $ $( do e <- [| 20# |]
                                  [| I# $(return e) |] )
          putStrLn $ show $ $( do e <- [| 32## |]
                                  [| W# $(return e) |] )
          putStrLn $ show $ $( do e <- [| 12.3# |]
                                  [| F# $(return e) |] )
          putStrLn $ show $ $( do e <- [| 24.6## |]
                                  [| D# $(return e) |] )


