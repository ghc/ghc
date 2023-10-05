{-# LANGUAGE NoImplicitPrelude #-}

module T13528 (
  GHC.Exts.IsList(
      Item
      , fromList
      , toList
      )
  , Data.Bool.Bool(True, False)
) where

import qualified GHC.Exts (IsList(..))
import qualified Data.Bool (Bool(..))
