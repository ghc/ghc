{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
module T14863 where

data Dict c where
  Dict :: c => Dict c

class    (a => b) => Implies a b
instance (a => b) => Implies a b

uncurryCImpredic1 :: forall a b c. Implies a (b => c) => Dict (Implies (a, b) c)
uncurryCImpredic1 = Dict

uncurryCImpredic2 :: forall a b c. a => Implies b c => Dict (Implies (a, b) c)
uncurryCImpredic2 = Dict

uncurryC1 :: forall a b c. (a => b => c) => Dict (Implies (a, b) c)
uncurryC1 = Dict

uncurryC2 :: forall a b c. Implies a (Implies b c) => Dict (Implies (a, b) c)
uncurryC2 = Dict