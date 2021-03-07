{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
module Bug1063 where

class    (c => d) => Implies c d
instance (c => d) => Implies c d
