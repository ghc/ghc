{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module T18321 where

import Data.Ix

data T = MkT deriving (Eq, Ord, Ix)
$(return [])
deriving instance Enum T

data S a = MkS
deriving instance Enum (S Int)
$(return [])
deriving instance Enum (S Bool)
