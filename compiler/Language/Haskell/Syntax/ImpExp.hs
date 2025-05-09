{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Syntax.ImpExp ( module Language.Haskell.Syntax.ImpExp, IsBootInterface(..) ) where

import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Module.Name
import Language.Haskell.Syntax.ImpExp.IsBoot ( IsBootInterface(..) )

import Data.Eq (Eq)
import Data.Data (Data)
import Data.Bool (Bool)
import Data.Maybe (Maybe)
import Data.String (String)
import Data.Int (Int)

import Control.DeepSeq
import {-# SOURCE #-} GHC.Hs.Doc (LHsDoc) -- ROMES:TODO Discuss in #21592 whether this is parsed AST or base AST

{-
************************************************************************
*                                                                      *
Import and export declaration lists
*                                                                      *
************************************************************************

One per import declaration in a module.
-}

-- | Located Import Declaration
type LImportDecl pass = XRec pass (ImportDecl pass)

-- | If/how an import is 'qualified'.
data ImportDeclQualifiedStyle
  = QualifiedPre  -- ^ 'qualified' appears in prepositive position.
  | QualifiedPost -- ^ 'qualified' appears in postpositive position.
  | NotQualified  -- ^ Not qualified.
  deriving (Eq, Data)

data ImportDeclLevelStyle
  = LevelStylePre ImportDeclLevel -- ^ 'splice' or 'quote' appears in prepositive position.
  | LevelStylePost ImportDeclLevel -- ^ 'splice' or 'quote' appears in postpositive position.
  | NotLevelled -- ^ Not levelled.
  deriving (Eq, Data)

data ImportDeclLevel = ImportDeclQuote | ImportDeclSplice deriving (Eq, Data)

-- | Import Declaration
--
-- A single Haskell @import@ declaration.
data ImportDecl pass
  = ImportDecl {
      ideclExt        :: XCImportDecl pass, -- ^ Locations of keywords like @import@, @qualified@, etc. are captured here.
      ideclName       :: XRec pass ModuleName, -- ^ Module name.
      ideclPkgQual    :: ImportDeclPkgQual pass,  -- ^ Package qualifier.
      ideclSource     :: IsBootInterface,      -- ^ IsBoot \<=> {-\# SOURCE \#-} import
      ideclLevelSpec  :: ImportDeclLevelStyle,
      ideclSafe       :: Bool,          -- ^ True => safe import
      ideclQualified  :: ImportDeclQualifiedStyle, -- ^ If/how the import is qualified.
      ideclAs         :: Maybe (XRec pass ModuleName),  -- ^ as Module
      ideclImportList :: Maybe (ImportListInterpretation, XRec pass [LIE pass])
                                       -- ^ Explicit import list (EverythingBut => hiding, names)
    }
  | XImportDecl !(XXImportDecl pass)

-- | Whether the import list is exactly what to import, or whether @hiding@ was
-- used, and therefore everything but what was listed should be imported
data ImportListInterpretation = Exactly | EverythingBut
    deriving (Eq, Data)

instance NFData ImportListInterpretation where
  rnf = rwhnf

-- | Located Import or Export
type LIE pass = XRec pass (IE pass)
        -- ^ When in a list this may have

-- | A docstring attached to an export list item.
type ExportDoc pass = LHsDoc pass

-- | Imported or exported entity.
data IE pass
  = IEVar (XIEVar pass) (LIEWrappedName pass) (Maybe (ExportDoc pass))
        -- ^ Imported or exported variable
        --
        -- @
        -- module Mod ( test )
        -- import Mod ( test )
        -- @

  | IEThingAbs (XIEThingAbs pass) (LIEWrappedName pass) (Maybe (ExportDoc pass))
        -- ^ Imported or exported Thing with absent subordinate list
        --
        -- The thing is a Class\/Type (can't tell)
        --
        -- @
        -- module Mod ( Test )
        -- import Mod ( Test )
        -- @

        -- See Note [Located RdrNames] in GHC.Hs.Expr
  | IEThingAll  (XIEThingAll pass) (LIEWrappedName pass) (Maybe (ExportDoc pass))
        -- ^ Imported or exported thing with wildcard subordinate list (e.g. @(..)@)
        --
        -- The thing is a Class\/Type and the All refers to methods\/constructors
        --
        -- @
        -- module Mod ( Test(..) )
        -- import Mod ( Test(..) )
        -- @

        -- See Note [Located RdrNames] in GHC.Hs.Expr
  | IEThingWith (XIEThingWith pass)
                (LIEWrappedName pass)
                IEWildcard
                [LIEWrappedName pass]
                (Maybe (ExportDoc pass))
        -- ^ Imported or exported thing with explicit subordinate list.
        --
        -- The thing is a Class\/Type (can't tell) and the imported or exported things are
        -- its children.
        --
        -- @
        -- module Mod ( Test(f, g) )
        -- import Mod ( Test(f, g) )
        -- @
  | IEModuleContents  (XIEModuleContents pass) (XRec pass ModuleName)
        -- ^ Export of entire module. Can only occur in export list.
        --
        -- @
        -- module Mod ( module Mod2 )
        -- @
  | IEGroup (XIEGroup pass) Int (LHsDoc pass)
        -- ^ A Haddock section in an export list.
        --
        -- @
        -- module Mod
        --   ( -- * Section heading
        --     ...
        --   )
        -- @
  | IEDoc (XIEDoc pass) (LHsDoc pass)
        -- ^ A bit of unnamed documentation.
        --
        -- @
        -- module Mod
        --   ( -- | Documentation
        --     ...
        --   )
        -- @
  | IEDocNamed (XIEDocNamed pass) String
        -- ^ A reference to a named documentation chunk.
        --
        -- @
        -- module Mod
        --   ( -- $chunkName
        --     ...
        --   )
        -- @
  | XIE !(XXIE pass)

-- | Wildcard in an import or export sublist, like the @..@ in
-- @import Mod ( T(Mk1, Mk2, ..) )@.
data IEWildcard
  = NoIEWildcard   -- ^ no wildcard in this list
  | IEWildcard Int -- ^ wildcard after the given \# of items in this list
                   -- The @Int@ is in the range [0..n], where n is the length
                   -- of the list.
  deriving (Eq, Data)

-- | A name in an import or export specification which may have
-- adornments. Used primarily for accurate pretty printing of
-- ParsedSource, and API Annotation placement.
data IEWrappedName p
  = IEName    (XIEName p)    (LIdP p)  -- ^ unadorned name, e.g @myFun@
  | IEDefault (XIEDefault p) (LIdP p)  -- ^ @default X ()@, see Note [Named default declarations] in GHC.Tc.Gen.Default
  | IEPattern (XIEPattern p) (LIdP p)  -- ^ @pattern X@
  | IEType    (XIEType p)    (LIdP p)  -- ^ @type (:+:)@
  | IEData    (XIEData p)    (LIdP p)  -- ^ @data (:+:)@
  | XIEWrappedName !(XXIEWrappedName p)

-- | Located name with possible adornment
type LIEWrappedName p = XRec p (IEWrappedName p)
