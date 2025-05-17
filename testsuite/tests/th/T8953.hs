{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TemplateHaskell,
             FlexibleInstances, UndecidableInstances #-}

module T8953 where

import Data.Proxy
import Language.Haskell.TH hiding (Type)
import Data.Kind (Type)
import System.IO

type family Poly (a :: k) :: Type
type instance Poly (x :: Bool) = Int
type instance Poly (x :: Maybe k) = Double

type family Silly :: k -> Type
type instance Silly @Type = Proxy
type instance Silly @(Type -> Type) = Proxy

a :: Proxy (Proxy :: Type -> Type)
b :: Proxy (Proxy :: (Type -> Type) -> Type)
a = undefined
b = undefined

type StarProxy (a :: Type) = Proxy a

class PC (a :: k)
instance PC (a :: Type)
instance PC (Proxy :: (k -> Type) -> Type)

data T1 :: k1 -> k2 -> Type
data T2 :: k1 -> k2 -> Type
type family F a :: k
type family G (a :: k) :: k
type instance G T1 = T2
type instance F Char = (G T1 Bool :: (Type -> Type) -> Type)

$( do infos <- mapM reify [''Poly, ''Silly, 'a, 'b, ''StarProxy, ''PC, ''F, ''G]
      runIO $ mapM (putStrLn . pprint) infos
      runIO $ hFlush stdout
      return [] )
