{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module T8758a where

import T8758

newtype MyList a = Mk [a]
  deriving C