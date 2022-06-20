{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable, Binary, Eq
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Fixity
module GHC.Hs.Basic
   ( module Language.Haskell.Syntax.Basic
   ) where

import GHC.Prelude

import GHC.Hs.Extension
import GHC.Types.SourceText

import GHC.Utils.Outputable
import GHC.Utils.Binary

import Data.Data ()

import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Basic

type instance XFixity (GhcPass _) = SourceText -- Note [Pragma source text]
type instance XXFixity (GhcPass _) = DataConCantHappen

instance Outputable FixityDirection where
    ppr InfixL = text "infixl"
    ppr InfixR = text "infixr"
    ppr InfixN = text "infix"

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

instance Outputable LexicalFixity where
  ppr Prefix = text "Prefix"
  ppr Infix  = text "Infix"

instance Outputable (Fixity (GhcPass p)) where
    ppr (Fixity _ prec dir) = hcat [ppr dir, space, int prec]

instance Binary (Fixity (GhcPass p)) where
    put_ bh (Fixity src aa ab) = do
            put_ bh src
            put_ bh aa
            put_ bh ab
    get bh = do
          src <- get bh
          aa <- get bh
          ab <- get bh
          return (Fixity src aa ab)

