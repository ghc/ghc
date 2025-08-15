{-# OPTIONS_GHC -Wredundant-constraints #-}

module T25992 where

import Control.DeepSeq
import GHC.Generics (Generic)

data Foo a = Foo a
  deriving (Generic)

instance NFData a => NFData (Foo a)

