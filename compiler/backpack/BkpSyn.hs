-- | This is the syntax for bkp files which are parsed in 'ghc --backpack'
-- mode.  This syntax is used purely for testing purposes.

module BkpSyn (
    -- * Backpack abstract syntax
    HsUnitId(..),
    LHsUnitId,
    HsModuleSubst,
    LHsModuleSubst,
    HsModuleId(..),
    LHsModuleId,
    HsComponentId(..),
    LHsUnit, HsUnit(..),
    LHsUnitDecl, HsUnitDecl(..),
    HsDeclType(..),
    IncludeDecl(..),
    LRenaming, Renaming(..),
    ) where

import GhcPrelude

import HsSyn
import SrcLoc
import Outputable
import Module
import PackageConfig

{-
************************************************************************
*                                                                      *
                        User syntax
*                                                                      *
************************************************************************
-}

data HsComponentId = HsComponentId {
    hsPackageName :: PackageName,
    hsComponentId :: ComponentId
    }

instance Outputable HsComponentId where
    ppr (HsComponentId _pn cid) = ppr cid -- todo debug with pn

data HsUnitId n = HsUnitId (Located n) [LHsModuleSubst n]
type LHsUnitId n = Located (HsUnitId n)

type HsModuleSubst n = (Located ModuleName, LHsModuleId n)
type LHsModuleSubst n = Located (HsModuleSubst n)

data HsModuleId n = HsModuleVar (Located ModuleName)
                  | HsModuleId (LHsUnitId n) (Located ModuleName)
type LHsModuleId n = Located (HsModuleId n)

-- | Top level @unit@ declaration in a Backpack file.
data HsUnit n = HsUnit {
        hsunitName :: Located n,
        hsunitBody :: [LHsUnitDecl n]
    }
type LHsUnit n = Located (HsUnit n)

-- | A declaration in a package, e.g. a module or signature definition,
-- or an include.
data HsDeclType = ModuleD | SignatureD
data HsUnitDecl n
    = DeclD   HsDeclType (Located ModuleName) (Maybe (Located (HsModule GhcPs)))
    | IncludeD   (IncludeDecl n)
type LHsUnitDecl n = Located (HsUnitDecl n)

-- | An include of another unit
data IncludeDecl n = IncludeDecl {
        idUnitId :: LHsUnitId n,
        idModRenaming :: Maybe [ LRenaming ],
        -- | Is this a @dependency signature@ include?  If so,
        -- we don't compile this include when we instantiate this
        -- unit (as there should not be any modules brought into
        -- scope.)
        idSignatureInclude :: Bool
    }

-- | Rename a module from one name to another.  The identity renaming
-- means that the module should be brought into scope.
data Renaming = Renaming { renameFrom :: Located ModuleName
                         , renameTo :: Maybe (Located ModuleName) }
type LRenaming = Located Renaming
