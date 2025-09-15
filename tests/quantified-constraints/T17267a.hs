{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module T17267a where

-- Now rejected
class C a b where
  op :: a -> b

uc :: a -> b
uc = oops where
  oops :: (C a b => C a b) => a -> b
  oops x = op x
