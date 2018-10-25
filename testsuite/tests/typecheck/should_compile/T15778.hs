{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}

module T15778 where

f (x :: (Int :: a)) = True :: (Bool :: a)
