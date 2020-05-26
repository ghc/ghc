{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Proposal229f_instances where

import GHC.Exts
import Data.String
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

instance IsList (Code Q String) where
  type Item (Code Q String) = Char
  fromList = liftTyped
  toList = undefined

instance IsList (Q Exp) where
  type Item (Q Exp) = Char
  fromList = lift
  toList = undefined

instance IsString (Code Q String) where
  fromString = liftTyped

instance IsString (Q Exp) where
  fromString = lift
