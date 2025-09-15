{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module T16179 where

class C a
newtype T a = MkT a deriving C
