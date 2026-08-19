{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-} -- for IsPass; see Note [NoGhcTc] in GHC.Hs.Extension

module GHC.Hs.Extension.Pass where

import Data.Data
import Data.Type.Equality (type (~))
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

-- | Allows us to check what phase we're in at GHC's runtime.
-- For example, this class allows us to write
--
-- @
-- f :: forall p. IsPass p => HsExpr (GhcPass p) -> blah
-- f e = case ghcPass @p of
--         GhcPs ->    ... in this RHS we have HsExpr GhcPs...
--         GhcRn ->    ... in this RHS we have HsExpr GhcRn...
--         GhcTc ->    ... in this RHS we have HsExpr GhcTc...
-- @
--
-- which is very useful, for example, when pretty-printing.
-- See Note [IsPass] in GHC.Hs.Extension.
class ( NoGhcTcPass (NoGhcTcPass p) ~ NoGhcTcPass p
      , IsPass (NoGhcTcPass p)
      ) => IsPass p where
  ghcPass :: GhcPass p

instance IsPass 'Parsed where
  ghcPass = GhcPs
instance IsPass 'Renamed where
  ghcPass = GhcRn
instance IsPass 'Typechecked where
  ghcPass = GhcTc

type family NoGhcTcPass (p :: Pass) :: Pass where
  NoGhcTcPass 'Typechecked = 'Renamed
  NoGhcTcPass other        = other
