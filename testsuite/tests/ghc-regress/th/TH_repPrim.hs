-- test the representation of unboxed literals

module Main
where

import GHC.Base
import GHC.Float
import Language.Haskell.THSyntax
import Text.PrettyPrint

main :: IO ()
main = do putStrLn $ show $ $( do e <- [| I# 20# |]
                                  qIO $ putStrLn $ show e
                                  qIO $ putStrLn $ render $ pprExp e
                                  return e )
          putStrLn $ show $ $( do e <- [| F# 12.3# |]
                                  qIO $ putStrLn $ show e
                                  qIO $ putStrLn $ render $ pprExp e
                                  return e )
          putStrLn $ show $ $( do e <- [| D# 24.6## |]
                                  qIO $ putStrLn $ show e
                                  qIO $ putStrLn $ render $ pprExp e
                                  return e )

