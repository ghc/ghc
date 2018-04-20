{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
module Main where

import Data.Kind
import Data.Proxy
import Language.Haskell.TH hiding (Type)

-- Anonymous tyvar binder example
newtype Foo1 = Foo1 (Proxy '[False, True, False])

-- Required (dependent) tyvar binder example
type family Wurble k (a :: k) :: k
newtype Foo2 a = Foo2 (Proxy (Wurble (Maybe a) Nothing))

-- Non-injective type family example
type family Foo3Fam1 (a :: Type) :: Type where
  Foo3Fam1 a = a
type family Foo3Fam2 (a :: Foo3Fam1 b) :: b
newtype Foo3 = Foo3 (Proxy (Foo3Fam2 Int))

-- Injective type family example
type family Foo4Fam1 (a :: Type) = (r :: Type) | r -> a where
  Foo4Fam1 a = a
type family Foo4Fam2 (a :: Foo4Fam1 b) :: b
newtype Foo4 = Foo4 (Proxy (Foo4Fam2 Int))

$(return [])

main :: IO ()
main = do
  putStrLn $(reify ''Foo1 >>= stringE . pprint)
  putStrLn $(reify ''Foo2 >>= stringE . pprint)
  putStrLn $(reify ''Foo3 >>= stringE . pprint)
  putStrLn $(reify ''Foo4 >>= stringE . pprint)
