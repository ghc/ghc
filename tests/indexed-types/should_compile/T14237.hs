{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fwarn-redundant-constraints #-}

module T14237 where

f :: (Integer ~ a) => a -> Integer
f = (+ 1)
