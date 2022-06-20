{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable, Binary
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Fixity
module GHC.Hs.Basic
   ( module Language.Haskell.Syntax.Basic
   ) where

import GHC.Prelude

import GHC.Utils.Outputable
import GHC.Utils.Binary

import Data.Data ()

import Language.Haskell.Syntax.Basic

instance Outputable LexicalFixity where
  ppr Prefix = text "Prefix"
  ppr Infix  = text "Infix"

instance Outputable FixityDirection where
    ppr InfixL = text "infixl"
    ppr InfixR = text "infixr"
    ppr InfixN = text "infix"

instance Outputable Fixity where
    ppr (Fixity prec dir) = hcat [ppr dir, space, int prec]


instance Binary Fixity where
    put_ bh (Fixity aa ab) = do
            put_ bh aa
            put_ bh ab
    get bh = do
          aa <- get bh
          ab <- get bh
          return (Fixity aa ab)

------------------------

instance Binary FixityDirection where
    put_ bh InfixL =
            putByte bh 0
    put_ bh InfixR =
            putByte bh 1
    put_ bh InfixN =
            putByte bh 2
    get bh = do
            h <- getByte bh
            case h of
              0 -> return InfixL
              1 -> return InfixR
              _ -> return InfixN
