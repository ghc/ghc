{-# LANGUAGE MagicHash, UnboxedTuples #-}
-- test the representation of unboxed literals

module Main where

import GHC.Exts
import GHC.Float
import Language.Haskell.TH
import Text.PrettyPrint
import System.IO

main :: IO ()
main = do putStrLn $ show $ $( do e <- [| 20# |]
                                  runIO $ putStrLn $ show e
                                  runIO $ putStrLn $ pprint e
                                  runIO $ hFlush stdout
                                  [| I# $( return e) |] )
          putStrLn $ show $ $( do e <- [| 32## |]
                                  runIO $ putStrLn $ show e
                                  runIO $ putStrLn $ pprint e
                                  runIO $ hFlush stdout
                                  [| W# $(return e) |] )
          putStrLn $ show $ $( do e <- [| 12.3# |]
                                  runIO $ putStrLn $ show e
                                  runIO $ putStrLn $ pprint e
                                  runIO $ hFlush stdout
                                  [| F# $(return e) |] )
          putStrLn $ show $ $( do e <- [| 24.6## |]
                                  runIO $ putStrLn $ show e
                                  runIO $ putStrLn $ pprint e
                                  runIO $ hFlush stdout
                                  [| D# $(return e) |] )

