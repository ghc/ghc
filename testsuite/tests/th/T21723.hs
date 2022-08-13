module Main where

import Language.Haskell.TH

main :: IO ()
main = do
    putStrLn $ pprint (InfixT (ArrowT `AppT` StarT `AppT` StarT) (mkName ":>:") StarT)
    putStrLn $ pprint (InfixT (ParensT $ ArrowT `AppT` StarT `AppT` StarT) (mkName ":>:") StarT)
