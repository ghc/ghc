{-# LANGUAGE TemplateHaskell, KindSignatures, FlexibleInstances #-}

module T5452 where

import Language.Haskell.TH hiding (Type)
import Data.Kind (Type)

class C (f :: Type -> Type)
class D (f :: Type -> Type)

instance C ((,) Int)

$(do { ClassI _ [inst_dec] <- reify ''C
     ; let InstanceD o cxt (AppT _ ty) _ = inst_dec
     ; return [InstanceD o cxt
                         (foldl AppT (ConT ''D) [ty]) 
                         []
              ] })

