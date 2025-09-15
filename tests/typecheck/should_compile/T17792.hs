{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module T17792 where

class C a where
  m :: a

instance C Bool where
  m = notInScope @Word
