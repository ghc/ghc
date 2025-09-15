{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module T17267b where

-- Now rejected
uc :: a -> b
uc = oops where
  oops :: (a ~ b => a ~ b) => a -> b
  oops x = x

{-
Consider the ambiguity check for oops.

[G] (a ~ b => a ~ b)
[W] (a ~ b => a ~ b)
==>

[G] (a ~ b => a ~ b)
[G] (a ~# b)     was [G] (a ~ b) [G] a ~# b

kick out the QC and (old) (a~b)
[G] (b ~ b => b ~ b)    Quantified constraint
[G] (a ~# b)     was [G] (b ~ b) [G] a ~# b

[W] (a~b)   DictCt

Wanted is rewritten
   (b~b)    DictCt
-}
