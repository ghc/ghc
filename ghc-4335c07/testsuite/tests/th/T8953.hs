{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TemplateHaskell,
             FlexibleInstances, UndecidableInstances #-}

module T8953 where

import Data.Proxy
import Language.Haskell.TH
import System.IO

type family Poly (a :: k) :: *
type instance Poly (x :: Bool) = Int
type instance Poly (x :: Maybe k) = Double

type family Silly :: k -> *
type instance Silly = (Proxy :: * -> *)
type instance Silly = (Proxy :: (* -> *) -> *)

a :: Proxy (Proxy :: * -> *)
b :: Proxy (Proxy :: (* -> *) -> *)
a = undefined
b = undefined

type StarProxy (a :: *) = Proxy a

class PC (a :: k)
instance PC (a :: *)
instance PC (Proxy :: (k -> *) -> *)

data T1 :: k1 -> k2 -> *
data T2 :: k1 -> k2 -> *
type family F a :: k
type family G (a :: k) :: k
type instance G T1 = T2
type instance F Char = (G T1 Bool :: (* -> *) -> *)

$( do infos <- mapM reify [''Poly, ''Silly, 'a, 'b, ''StarProxy, ''PC, ''F, ''G]
      runIO $ mapM (putStrLn . pprint) infos
      runIO $ hFlush stdout
      return [] )
