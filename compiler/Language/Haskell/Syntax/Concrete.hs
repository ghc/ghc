{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Bits of concrete syntax (tokens, layout).

module Language.Haskell.Syntax.Concrete
  ( LHsToken, LHsUniToken,
    HsToken(HsTok),
    HsUniToken(HsNormalTok, HsUnicodeTok),
    LayoutInfo(ExplicitBraces, VirtualBraces, NoLayoutInfo)
  ) where

import GHC.Prelude
import GHC.TypeLits (Symbol, KnownSymbol)
import Data.Data
import Language.Haskell.Syntax.Extension

type LHsToken tok p = XRec p (HsToken tok)
type LHsUniToken tok utok p = XRec p (HsUniToken tok utok)

-- | A token stored in the syntax tree. For example, when parsing a
-- let-expression, we store @HsToken "let"@ and @HsToken "in"@.
-- The locations of those tokens can be used to faithfully reproduce
-- (exactprint) the original program text.
data HsToken (tok :: Symbol) = HsTok

-- | With @UnicodeSyntax@, there might be multiple ways to write the same
-- token. For example an arrow could be either @->@ or @→@. This choice must be
-- recorded in order to exactprint such tokens, so instead of @HsToken "->"@ we
-- introduce @HsUniToken "->" "→"@.
--
-- See also @IsUnicodeSyntax@ in @GHC.Parser.Annotation@; we do not use here to
-- avoid a dependency.
data HsUniToken (tok :: Symbol) (utok :: Symbol) = HsNormalTok | HsUnicodeTok

deriving instance KnownSymbol tok => Data (HsToken tok)
deriving instance (KnownSymbol tok, KnownSymbol utok) => Data (HsUniToken tok utok)

-- | Layout information for declarations.
data LayoutInfo pass =

    -- | Explicit braces written by the user.
    --
    -- @
    -- class C a where { foo :: a; bar :: a }
    -- @
    ExplicitBraces !(LHsToken "{" pass) !(LHsToken "}" pass)
  |
    -- | Virtual braces inserted by the layout algorithm.
    --
    -- @
    -- class C a where
    --   foo :: a
    --   bar :: a
    -- @
    VirtualBraces
      !Int -- ^ Layout column (indentation level, begins at 1)
  |
    -- | Empty or compiler-generated blocks do not have layout information
    -- associated with them.
    NoLayoutInfo
