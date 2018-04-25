{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module T12545a
  ( ElemWitness(..)
  , ElemAt(..)
  , JustElemPath
  , FindElem
  , IsElem
  , ElemOf
  , ElemsOf
  ) where

import Data.Proxy (Proxy(..))

data ElemPath = HeadElem
              | TailElem ElemPath

data MaybeElemPath = NotElem
                   | Elem ElemPath

type family FindElem (p :: ElemPath) (a :: k) (l :: [k]) :: MaybeElemPath where
  FindElem p a (a ': t) = 'Elem p
  FindElem p a (b ': t) = FindElem ('TailElem p) a t
  FindElem p a '[] = 'NotElem

type family JustElemPath (p :: MaybeElemPath) :: ElemPath where
  JustElemPath ('Elem p) = p

data ElemWitness (p :: ElemPath) (a :: k) (l :: [k])  where
  ElemHeadWitness :: ElemWitness 'HeadElem a (a ': t)
  ElemTailWitness :: (ElemAt p a t,
                      FindElem 'HeadElem a (b ': t) ~ 'Elem ('TailElem p))
                  => ElemWitness p a t -> ElemWitness ('TailElem p) a (b ': t)

class (FindElem 'HeadElem a l ~ 'Elem p) => ElemAt p (a :: k) (l :: [k]) where
  elemWitness :: Proxy a -> Proxy l -> ElemWitness p a l

instance ElemAt 'HeadElem a (a ': t) where
  elemWitness _ _ = ElemHeadWitness

instance (ElemAt p a t, FindElem 'HeadElem a (b ': t) ~ 'Elem ('TailElem p))
         => ElemAt ('TailElem p) a (b ': t) where
  elemWitness pa _ = ElemTailWitness (elemWitness pa (Proxy :: Proxy t))

type IsElem a l = ElemAt (JustElemPath (FindElem 'HeadElem a l)) a l

class IsElem t (ElemsOf a) => ElemOf a t where

type family ElemsOf a :: [*]
