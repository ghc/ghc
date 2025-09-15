{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE MagicHash             #-}

import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import GHC.Prim (Addr#)

instance KnownSymbol symbol => IsLabel symbol String where
  fromLabel = symbolVal (Proxy :: Proxy symbol)

(#), (#.) :: String -> Int -> String
(#) _ i = show i
_ #. i = show i

f :: Addr# -> Int -> String
f _ i = show i

main :: IO ()
main = traverse_ putStrLn
  [ #a
  , #number17
  , #do
  , #type
  , #Foo
  , #3
  , #"199.4"
  , #17a23b
  , #f'a'
  , #'a'
  , #'
  , #''notTHSplice
  , #"..."
  , #привет
  , #こんにちは
  , #"3"
  , #":"
  , #"Foo"
  , #"The quick brown fox"
  , #"\""
  , (++) #hello#world
  , (++) #"hello"#"world"
  , #"hello"# 1 -- equivalent to `(fromLabel @"hello") # 1`
  , f "hello"#2 -- equivalent to `f ("hello"# :: Addr#) 2`
  ]
