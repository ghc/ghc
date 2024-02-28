{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Syntax.ImpExp where

import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Module.Name

import Data.Eq (Eq)
import Data.Ord (Ord)
import Text.Show (Show)
import Data.Data (Data)
import Data.Bool (Bool)
import Data.Maybe (Maybe)
import Data.String (String)
import Data.Int (Int)

import Control.DeepSeq

import GHC.Hs.Doc -- ROMES:TODO Discuss in #21592 whether this is parsed AST or base AST

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
        -- ^ When in a list this may have
        --
        --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnSemi'

        -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

-- | If/how an import is 'qualified'.
data ImportDeclQualifiedStyle
  = QualifiedPre  -- ^ 'qualified' appears in prepositive position.
  | QualifiedPost -- ^ 'qualified' appears in postpositive position.
  | NotQualified  -- ^ Not qualified.
  deriving (Eq, Data)

-- | Indicates whether a module name is referring to a boot interface (hs-boot
-- file) or regular module (hs file). We need to treat boot modules specially
-- when building compilation graphs, since they break cycles. Regular source
-- files and signature files are treated equivalently.
data IsBootInterface = NotBoot | IsBoot
    deriving (Eq, Ord, Show, Data)

instance NFData IsBootInterface where
  rnf = rwhnf

-- | Import Declaration
--
-- A single Haskell @import@ declaration.
data ImportDecl pass
  = ImportDecl {
      ideclExt        :: XCImportDecl pass,
      ideclName       :: XRec pass ModuleName, -- ^ Module name.
      ideclPkgQual    :: ImportDeclPkgQual pass,  -- ^ Package qualifier.
      ideclSource     :: IsBootInterface,      -- ^ IsBoot <=> {-\# SOURCE \#-} import
      ideclSafe       :: Bool,          -- ^ True => safe import
      ideclQualified  :: ImportDeclQualifiedStyle, -- ^ If/how the import is qualified.
      ideclAs         :: Maybe (XRec pass ModuleName),  -- ^ as Module
      ideclImportList :: Maybe (ImportListInterpretation, XRec pass [LIE pass])
                                       -- ^ Explicit import list (EverythingBut => hiding, names)
    }
  | XImportDecl !(XXImportDecl pass)
     -- ^
     --  'GHC.Parser.Annotation.AnnKeywordId's
     --
     --  - 'GHC.Parser.Annotation.AnnImport'
     --
     --  - 'GHC.Parser.Annotation.AnnOpen', 'GHC.Parser.Annotation.AnnClose' for ideclSource
     --
     --  - 'GHC.Parser.Annotation.AnnSafe','GHC.Parser.Annotation.AnnQualified',
     --    'GHC.Parser.Annotation.AnnPackageName','GHC.Parser.Annotation.AnnAs',
     --    'GHC.Parser.Annotation.AnnVal'
     --
     --  - 'GHC.Parser.Annotation.AnnHiding','GHC.Parser.Annotation.AnnOpen',
     --    'GHC.Parser.Annotation.AnnClose' attached
     --     to location in ideclImportList

     -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

-- | Whether the import list is exactly what to import, or whether `hiding` was
-- used, and therefore everything but what was listed should be imported
data ImportListInterpretation = Exactly | EverythingBut
    deriving (Eq, Data)

instance NFData ImportListInterpretation where
  rnf = rwhnf

-- | Located Import or Export
type LIE pass = XRec pass (IE pass)
        -- ^ When in a list this may have
        --
        --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnComma'

        -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

-- | A docstring attached to an export list item.
type ExportDoc pass = LHsDoc pass

-- | Imported or exported entity.
data IE pass
  = IEVar       (XIEVar pass) (LIEWrappedName pass) (Maybe (ExportDoc pass))
        -- ^ Imported or exported variable
        --
        -- @
        -- module Mod ( test )
        -- import Mod ( test )
        -- @

  | IEThingAbs  (XIEThingAbs pass) (LIEWrappedName pass) (Maybe (ExportDoc pass))
        -- ^ Imported or exported Thing with absent subordinate list
        --
        -- The thing is a typeclass or type (can't tell)
        --  - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnPattern',
        --             'GHC.Parser.Annotation.AnnType','GHC.Parser.Annotation.AnnVal'
        --
        -- @
        -- module Mod ( Test )
        -- import Mod ( Test )
        -- @

        -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation
        -- See Note [Located RdrNames] in GHC.Hs.Expr
  | IEThingAll  (XIEThingAll pass) (LIEWrappedName pass) (Maybe (ExportDoc pass))
        -- ^ Imported or exported thing with wildcard subordinate list (e..g @(..)@)
        --
        -- The thing is a Class/Type and the All refers to methods/constructors
        --
        -- - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnOpen',
        --       'GHC.Parser.Annotation.AnnDotdot','GHC.Parser.Annotation.AnnClose',
        --                                 'GHC.Parser.Annotation.AnnType'
        -- @
        -- module Mod ( Test(..) )
        -- import Mod ( Test(..) )
        -- @

        -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation
        -- See Note [Located RdrNames] in GHC.Hs.Expr

  | IEThingWith (XIEThingWith pass)
                (LIEWrappedName pass)
                IEWildcard
                [LIEWrappedName pass]
                (Maybe (ExportDoc pass))
        -- ^ Imported or exported thing with explicit subordinate list.
        --
        -- The thing is a Class/Type and the imported or exported things are
        -- its children.
        -- - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnOpen',
        --                                   'GHC.Parser.Annotation.AnnClose',
        --                                   'GHC.Parser.Annotation.AnnComma',
        --                                   'GHC.Parser.Annotation.AnnType'
        -- @
        -- module Mod ( Test(..) )
        -- import Mod ( Test(..) )
        -- @

        -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation
  | IEModuleContents  (XIEModuleContents pass) (XRec pass ModuleName)
        -- ^ Imported or exported module contents
        --
        -- (Export Only)
        --
        -- - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnModule'
        --
        -- @
        -- module Mod ( module Mod2 )
        -- @

        -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation
  | IEGroup             (XIEGroup pass) Int (LHsDoc pass) -- ^ Doc section heading
        -- ^ A Haddock section in an export list.
        --
        -- @
        -- module Mod
        --   ( -- * Section heading
        --     ...
        --   )
        -- @
  | IEDoc               (XIEDoc pass) (LHsDoc pass)       -- ^ Some documentation
        -- ^ A bit of unnamed documentation.
        --
        -- @
        -- module Mod
        --   ( -- | Documentation
        --     ...
        --   )
        -- @
  | IEDocNamed          (XIEDocNamed pass) String    -- ^ Reference to named doc
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
-- ParsedSource, and API Annotation placement. The
-- 'GHC.Parser.Annotation' is the location of the adornment in
-- the original source.
data IEWrappedName p
  = IEName    (XIEName p)    (LIdP p)  -- ^ no extra
  | IEPattern (XIEPattern p) (LIdP p)  -- ^ pattern X
  | IEType    (XIEType p)    (LIdP p)  -- ^ type (:+:)
  | XIEWrappedName !(XXIEWrappedName p)

-- | Located name with possible adornment
-- - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnType',
--         'GHC.Parser.Annotation.AnnPattern'
type LIEWrappedName p = XRec p (IEWrappedName p)
-- For details on above see Note [exact print annotations] in GHC.Parser.Annotation
