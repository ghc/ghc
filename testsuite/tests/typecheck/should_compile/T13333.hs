{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module T13333 where

import Data.Data
import GHC.Exts (Constraint)

data T (phantom :: k) = T

data D (p :: k -> Constraint) (x :: j) where
  D :: forall k (p :: k -> Constraint) (x :: k). p x => D p x

class Possibly p x where
  possibly :: proxy1 p -> proxy2 x -> Maybe (D p x)

dataCast1T :: forall k c t (phantom :: k).
              (Typeable k, Typeable t, Typeable phantom, Possibly Data phantom)
           => (forall d. Data d => c (t d))
           -> Maybe (c (T phantom))
dataCast1T f = case possibly (Proxy :: Proxy Data) (Proxy :: Proxy phantom) of
                 Nothing -> Nothing
                 Just D  -> gcast1 f
