{-# Language CPP                   #-}
{-# Language QuantifiedConstraints #-}
{-# Language TypeApplications      #-}
{-# Language PolyKinds             #-}
{-# Language TypeOperators         #-}
{-# Language DataKinds             #-}
{-# Language TypeFamilies          #-}
{-# Language TypeSynonymInstances  #-}
{-# Language FlexibleInstances     #-}
{-# Language GADTs                 #-}
{-# Language UndecidableInstances  #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleContexts      #-}
{-# LANGUAGE UnliftedNewtypes      #-}

module Bug where
import Data.Coerce
import Data.Kind

type Cat ob = ob -> ob -> Type

type Obj = Type

class
  Ríki (obj :: Obj) where
  type (-->) :: obj -> obj -> Type

  ið :: a --> (a::obj)

data Op a = Op a

type family UnOp op where UnOp ('Op obj) = obj

newtype Y :: Cat (Op a) where
  Y :: (UnOp b --> UnOp a) -> Y a b
