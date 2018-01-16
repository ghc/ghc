{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module T8865 where
import Control.Category

instance Category Either where
  id  = error "urk1"
  (.) = error "urk2"

newtype T a b = MkT (Either a b) deriving( Category )

