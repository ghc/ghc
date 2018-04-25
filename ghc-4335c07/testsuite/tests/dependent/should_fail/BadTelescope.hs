{-# LANGUAGE TypeInType #-}

module BadTelescope where

import Data.Kind

data SameKind :: k -> k -> *

data X a k (b :: k) (c :: SameKind a b)
