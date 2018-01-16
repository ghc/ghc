{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
module Bug where

class C a where
  type T a
  data D a
  m :: a

instance C Int
deriving instance C Bool
