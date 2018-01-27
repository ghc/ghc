{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
module Bug where

data Dict c where
  Dict :: c => Dict c

class    (a => b) => Implies a b
instance (a => b) => Implies a b

curryC1 :: ((a, b) => c) => Dict (Implies a (Implies b c))
curryC1 = Dict

curryC2 :: Implies (a, b) c => Dict (Implies a (Implies b c))
curryC2 = Dict
