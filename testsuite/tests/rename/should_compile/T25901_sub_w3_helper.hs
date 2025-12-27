{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeData #-}

module T25901_sub_w3_helper
  ( T(.., data ..)   -- T(MkT, unT)
  , C(data .., F)    -- C(f, g, data (#), F) excluding type (#)
  , K(TNum, data .., TStr) -- K(TNum, TStr) excluding TBool
  ) where

data T = MkT { unT :: () }

class C a b where
  type F a b
  type G a b
  type a # b
  f, g, (#) :: a -> b -> ()

type data K = TNum | TStr | TBool
