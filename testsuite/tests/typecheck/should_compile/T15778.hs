{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module T15778 where

f (x :: (Int :: a)) = True :: (Bool :: a)
