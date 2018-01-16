{-# LANGUAGE MagicHash, TemplateHaskell #-}
module Main where

import Language.Haskell.TH

main :: IO ()
main = do
    putStrLn $([| 'a'#   |] >>= stringE . show)
    putStrLn $([| "abc"# |] >>= stringE . show)
