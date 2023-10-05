{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
module Main where

import Data.Kind (Type)
import Language.Haskell.TH hiding (Type)

type Id1    a       = a
type Id2 k (a :: k) = a
data Proxy1 (a :: Id1      k) = Proxy1
data Proxy2 (a :: Id2 Type k) = Proxy2

$(return [])

main :: IO ()
main = do
  putStrLn $(reify ''Proxy1 >>= stringE . pprint)
  putStrLn $(reify ''Proxy2 >>= stringE . pprint)
