{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module T7354b where

type family Base t :: * -> *

class Unfoldable t where
  embed :: Base t t -> t
