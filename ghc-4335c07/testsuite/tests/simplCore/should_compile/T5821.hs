{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module T5821 where

type family T a
type instance T Int = Bool

foo :: Num a => a -> T a
foo = undefined

{-# SPECIALISE foo :: Int -> Bool #-}
