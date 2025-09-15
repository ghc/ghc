{-# LANGUAGE TemplateHaskell, DataKinds, NoListTuplePuns #-}

module T25179 where

import Prelude.Experimental
import GHC.TypeLits
import Data.Proxy
import Language.Haskell.TH hiding (Type)
import Data.Kind (Type)

p1 :: Proxy ((:) Int [])
p1 = Proxy

p2 :: Proxy ($(conT (mkName ":")) Int [])
p2 = Proxy

p3 :: Proxy ((:) Int $(conT (mkName "[]")))
p3 = Proxy

q1 :: Proxy ((,) Int "hello" :: Tuple2 Type Symbol)
q1 = Proxy

q2 :: Proxy ($(conT (mkName "(,)")) Int "hello" :: Tuple2 Type Symbol)
q2 = Proxy

