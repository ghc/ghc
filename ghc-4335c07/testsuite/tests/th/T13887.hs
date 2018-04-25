{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Proxy
import GHC.Generics
import Language.Haskell.TH

main :: IO ()
main = do
  putStrLn $([t| Proxy  (:*:) |] >>= stringE . pprint)
  putStrLn $([t| Proxy '(:*:) |] >>= stringE . pprint)
  putStrLn $([t| Proxy '(:)   |] >>= stringE . pprint)
