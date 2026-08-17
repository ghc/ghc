{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Hs.Extension.Pass where

import Data.Data
import GHC.Utils.Panic.Plain

-- | Used as a data type index for the hsSyn AST; also serves
-- as a singleton type for Pass
data GhcPass (c :: Pass) where
  GhcPs :: GhcPass 'Parsed
  GhcRn :: GhcPass 'Renamed
  GhcTc :: GhcPass 'Typechecked

-- This really should never be entered, but the data-deriving machinery
-- needs the instance to exist.
instance Typeable p => Data (GhcPass p) where
  gunfold _ _ _ = panic "instance Data GhcPass"
  toConstr  _   = panic "instance Data GhcPass"
  dataTypeOf _  = panic "instance Data GhcPass"

data Pass = Parsed | Renamed | Typechecked
         deriving (Data)

-- Type synonyms as a shorthand for tagging
type GhcPs   = GhcPass 'Parsed      -- Output of parser
type GhcRn   = GhcPass 'Renamed     -- Output of renamer
type GhcTc   = GhcPass 'Typechecked -- Output of typechecker
