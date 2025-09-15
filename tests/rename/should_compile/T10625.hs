{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module T10625 where

data Ex a = forall a. Ex a
