{-# LANGUAGE GADTs, TypeInType, TemplateHaskell, RankNTypes #-}
module T13642 where

import Data.Kind (Type)
import Language.Haskell.TH (stringE, pprint)

foo :: IO ()
foo = putStrLn $([d| data Foo :: forall a. a -> Type where MkFoo :: Foo Int |]
                 >>= \d -> stringE (pprint d))
