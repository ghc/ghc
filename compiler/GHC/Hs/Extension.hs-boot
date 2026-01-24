{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-} -- for IsPass; see Note [NoGhcTc]

module GHC.Hs.Extension where

import Data.Type.Equality (type (~))

-- | Used as a data type index for the hsSyn AST; also serves
-- as a singleton type for Pass
data GhcPass (c :: Pass) where
  GhcPs :: GhcPass 'Parsed
  GhcRn :: GhcPass 'Renamed
  GhcTc :: GhcPass 'Typechecked

data Pass = Parsed | Renamed | Typechecked

-- Type synonyms as a shorthand for tagging
type GhcPs   = GhcPass 'Parsed      -- Output of parser
type GhcRn   = GhcPass 'Renamed     -- Output of renamer
type GhcTc   = GhcPass 'Typechecked -- Output of typechecker

type family NoGhcTcPass (p :: Pass) :: Pass where
  NoGhcTcPass 'Typechecked = 'Renamed
  NoGhcTcPass other        = other

class ( NoGhcTcPass (NoGhcTcPass p) ~ NoGhcTcPass p
      , IsPass (NoGhcTcPass p)
      ) => IsPass p where
  ghcPass :: GhcPass p
