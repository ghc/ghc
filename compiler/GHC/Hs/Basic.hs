{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable, Binary
{-# LANGUAGE TypeFamilies #-}

-- | Fixity
module GHC.Hs.Basic
   ( module Language.Haskell.Syntax.Basic
   , NamespaceSpecifier(..)
   , overlappingNamespaceSpecifiers
   , coveredByNamespaceSpecifier
   ) where

import GHC.Prelude

import GHC.Utils.Outputable
import GHC.Utils.Binary
import GHC.Types.Name
import GHC.Parser.Annotation
import GHC.Utils.Misc ((<||>))

import Data.Data (Data)

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


-- | Optional namespace specifier for:
--
-- * import/export items
-- * fixity signatures
-- * @WARNING@ and @DEPRECATED@ pragmas
--
-- Examples:
--
-- @
-- module M (data ..) where
--        -- ↑ DataNamespaceSpecifier
--
-- import Data.Proxy as T (type ..)
--                      -- ↑ TypeNamespaceSpecifier
--
-- {-# WARNING in "x-partial" data Head "don't use this pattern synonym" #-}
--                          -- ↑ DataNamespaceSpecifier
--
-- {-# DEPRECATED type D "This type was deprecated" #-}
--              -- ↑ TypeNamespaceSpecifier
--
-- infixr 6 data $
--        -- ↑ DataNamespaceSpecifier
-- @
data NamespaceSpecifier
  = NoNamespaceSpecifier
  | TypeNamespaceSpecifier (EpToken "type")
  | DataNamespaceSpecifier (EpToken "data")
  deriving (Eq, Data)

-- | Check if namespace specifiers overlap, i.e. if they are equal or
-- if at least one of them doesn't specify a namespace
overlappingNamespaceSpecifiers :: NamespaceSpecifier -> NamespaceSpecifier -> Bool
overlappingNamespaceSpecifiers NoNamespaceSpecifier _ = True
overlappingNamespaceSpecifiers _ NoNamespaceSpecifier = True
overlappingNamespaceSpecifiers TypeNamespaceSpecifier{} TypeNamespaceSpecifier{} = True
overlappingNamespaceSpecifiers DataNamespaceSpecifier{} DataNamespaceSpecifier{} = True
overlappingNamespaceSpecifiers _ _ = False

-- | Check if namespace is covered by a namespace specifier:
--     * NoNamespaceSpecifier covers both namespaces
--     * TypeNamespaceSpecifier covers the type namespace only
--     * DataNamespaceSpecifier covers the data namespace only
coveredByNamespaceSpecifier :: NamespaceSpecifier -> NameSpace -> Bool
coveredByNamespaceSpecifier NoNamespaceSpecifier = const True
coveredByNamespaceSpecifier TypeNamespaceSpecifier{} = isTcClsNameSpace <||> isTvNameSpace
coveredByNamespaceSpecifier DataNamespaceSpecifier{} = isValNameSpace

instance Outputable NamespaceSpecifier where
  ppr NoNamespaceSpecifier = empty
  ppr TypeNamespaceSpecifier{} = text "type"
  ppr DataNamespaceSpecifier{} = text "data"
