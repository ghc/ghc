{-# LANGUAGE NamedDefaults #-}

module T25857 (
    IsString, default IsString, default IsString
  ) where

import Data.String

instance IsString (Maybe ()) where
  fromString _ = Just ()

default IsString (Maybe ())
