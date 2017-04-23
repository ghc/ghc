{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module T13555 where

import Data.Functor.Identity (Identity(..))

data T a
type Polynomial a = T a
newtype GF fp d = GF (Polynomial fp)
type CRTInfo r = (Int -> r, r)
type Tagged s b = TaggedT s Identity b
newtype TaggedT s m b = TagT { untagT :: m b }

class Reflects a i where
  value :: Tagged a i

class CRTrans mon r where
  crtInfo :: Reflects m Int => TaggedT m mon (CRTInfo r)

instance CRTrans Maybe (GF fp d) where
  crtInfo :: forall m . (Reflects m Int) => TaggedT m Maybe (CRTInfo (GF fp d))
  crtInfo = undefined
