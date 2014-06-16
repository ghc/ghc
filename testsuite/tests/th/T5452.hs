{-# LANGUAGE TemplateHaskell, KindSignatures, FlexibleInstances #-}

module T5452 where
import Language.Haskell.TH

class C (f :: * -> *)
class D (f :: * -> *)

instance C ((,) Int)

$(do { ClassI _ [inst_dec] <- reify ''C
     ; let InstanceD cxt (AppT _ ty) _ = inst_dec
     ; return [InstanceD cxt
                         (foldl AppT (ConT ''D) [ty]) 
                         []
              ] })

