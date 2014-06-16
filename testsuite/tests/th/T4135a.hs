{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies,
             FlexibleInstances, OverlappingInstances #-}

module T4135a where

import Control.Monad
import Language.Haskell.TH

class Foo a where
    type FooType a

createInstance' :: Q Type -> Q Dec
createInstance' t = liftM head [d|
    instance Foo $t where
      type FooType $t = String |]
