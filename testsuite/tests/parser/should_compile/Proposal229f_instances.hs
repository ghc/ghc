{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Proposal229f_instances where

import GHC.Exts
import Data.String
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

instance IsList (Q (TExp String)) where
  type Item (Q (TExp String)) = Char
  fromList = liftTyped
  toList = undefined

instance IsList (Q Exp) where
  type Item (Q Exp) = Char
  fromList = lift
  toList = undefined

instance IsString (Q (TExp String)) where
  fromString = liftTyped

instance IsString (Q Exp) where
  fromString = lift
