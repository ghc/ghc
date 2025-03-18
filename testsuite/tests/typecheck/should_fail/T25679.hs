{-# LANGUAGE DataKinds #-}

module T25679 where

import Data.Coerce
import GHC.Records

data Foo = Foo
  { foo :: String
  }

coerceString :: forall a. (Coercible a String) => a -> String
coerceString = coerce

main :: IO ()
main = putStrLn $ coerce $ getField @"bar" (Foo {foo = "hi"})
