{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


GHC.Hs.ImpExp: Abstract syntax: imports, exports, interfaces
-}

module GHC.Hs.ImpExp where

import GHC.Prelude

import GHC.Unit.Module        ( ModuleName, IsBootInterface(..) )
import GHC.Hs.Doc
import GHC.Types.SourceText   ( SourceText(..) )
import GHC.Types.FieldLabel   ( FieldLabel )

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.SrcLoc
import Language.Haskell.Syntax.Extension
import GHC.Hs.Extension
import GHC.Parser.Annotation
import GHC.Types.Name
import GHC.Types.PkgQual

import Data.Data
import Data.Maybe

{-
************************************************************************
*                                                                      *
\subsection{Import and export declaration lists}
*                                                                      *
************************************************************************

One per \tr{import} declaration in a module.
-}

-- | Located Import Declaration
type LImportDecl pass = XRec pass (ImportDecl pass)
        -- ^ When in a list this may have
        --
        --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnSemi'

        -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation
type instance Anno (ImportDecl (GhcPass p)) = SrcSpanAnnA

-- | If/how an import is 'qualified'.
data ImportDeclQualifiedStyle
  = QualifiedPre  -- ^ 'qualified' appears in prepositive position.
  | QualifiedPost -- ^ 'qualified' appears in postpositive position.
  | NotQualified  -- ^ Not qualified.
  deriving (Eq, Data)

-- | Given two possible located 'qualified' tokens, compute a style
-- (in a conforming Haskell program only one of the two can be not
-- 'Nothing'). This is called from "GHC.Parser".
importDeclQualifiedStyle :: Maybe EpaLocation
                         -> Maybe EpaLocation
                         -> (Maybe EpaLocation, ImportDeclQualifiedStyle)
importDeclQualifiedStyle mPre mPost =
  if isJust mPre then (mPre, QualifiedPre)
  else if isJust mPost then (mPost,QualifiedPost) else (Nothing, NotQualified)

-- | Convenience function to answer the question if an import decl. is
-- qualified.
isImportDeclQualified :: ImportDeclQualifiedStyle -> Bool
isImportDeclQualified NotQualified = False
isImportDeclQualified _ = True

-- | Import Declaration
--
-- A single Haskell @import@ declaration.
data ImportDecl pass
  = ImportDecl {
      ideclExt       :: XCImportDecl pass,
      ideclSourceSrc :: SourceText,
                                 -- Note [Pragma source text] in GHC.Types.SourceText
      ideclName      :: XRec pass ModuleName, -- ^ Module name.
      ideclPkgQual   :: ImportDeclPkgQual pass,  -- ^ Package qualifier.
      ideclSource    :: IsBootInterface,      -- ^ IsBoot <=> {-\# SOURCE \#-} import
      ideclSafe      :: Bool,          -- ^ True => safe import
      ideclQualified :: ImportDeclQualifiedStyle, -- ^ If/how the import is qualified.
      ideclImplicit  :: Bool,          -- ^ True => implicit import (of Prelude)
      ideclAs        :: Maybe (XRec pass ModuleName),  -- ^ as Module
      ideclHiding    :: Maybe (Bool, XRec pass [LIE pass])
                                       -- ^ (True => hiding, names)
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
     --     to location in ideclHiding

     -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

type family ImportDeclPkgQual pass
type instance ImportDeclPkgQual GhcPs = RawPkgQual
type instance ImportDeclPkgQual GhcRn = PkgQual
type instance ImportDeclPkgQual GhcTc = PkgQual

type instance XCImportDecl  GhcPs = EpAnn EpAnnImportDecl
type instance XCImportDecl  GhcRn = NoExtField
type instance XCImportDecl  GhcTc = NoExtField

type instance XXImportDecl  (GhcPass _) = DataConCantHappen

type instance Anno ModuleName = SrcSpanAnnA
type instance Anno [LocatedA (IE (GhcPass p))] = SrcSpanAnnL

-- ---------------------------------------------------------------------

-- API Annotations types

data EpAnnImportDecl = EpAnnImportDecl
  { importDeclAnnImport    :: EpaLocation
  , importDeclAnnPragma    :: Maybe (EpaLocation, EpaLocation)
  , importDeclAnnSafe      :: Maybe EpaLocation
  , importDeclAnnQualified :: Maybe EpaLocation
  , importDeclAnnPackage   :: Maybe EpaLocation
  , importDeclAnnAs        :: Maybe EpaLocation
  } deriving (Data)

-- ---------------------------------------------------------------------

simpleImportDecl :: ModuleName -> ImportDecl GhcPs
simpleImportDecl mn = ImportDecl {
      ideclExt       = noAnn,
      ideclSourceSrc = NoSourceText,
      ideclName      = noLocA mn,
      ideclPkgQual   = NoRawPkgQual,
      ideclSource    = NotBoot,
      ideclSafe      = False,
      ideclImplicit  = False,
      ideclQualified = NotQualified,
      ideclAs        = Nothing,
      ideclHiding    = Nothing
    }

instance (OutputableBndrId p
         , Outputable (Anno (IE (GhcPass p)))
         , Outputable (ImportDeclPkgQual (GhcPass p)))
       => Outputable (ImportDecl (GhcPass p)) where
    ppr (ImportDecl { ideclSourceSrc = mSrcText, ideclName = mod'
                    , ideclPkgQual = pkg
                    , ideclSource = from, ideclSafe = safe
                    , ideclQualified = qual, ideclImplicit = implicit
                    , ideclAs = as, ideclHiding = spec })
      = hang (hsep [text "import", ppr_imp from, pp_implicit implicit, pp_safe safe,
                    pp_qual qual False, ppr pkg, ppr mod', pp_qual qual True, pp_as as])
             4 (pp_spec spec)
      where
        pp_implicit False = empty
        pp_implicit True = text "(implicit)"

        pp_qual QualifiedPre False = text "qualified" -- Prepositive qualifier/prepositive position.
        pp_qual QualifiedPost True = text "qualified" -- Postpositive qualifier/postpositive position.
        pp_qual QualifiedPre True = empty -- Prepositive qualifier/postpositive position.
        pp_qual QualifiedPost False = empty -- Postpositive qualifier/prepositive position.
        pp_qual NotQualified _ = empty

        pp_safe False   = empty
        pp_safe True    = text "safe"

        pp_as Nothing   = empty
        pp_as (Just a)  = text "as" <+> ppr a

        ppr_imp IsBoot = case mSrcText of
                          NoSourceText   -> text "{-# SOURCE #-}"
                          SourceText src -> text src <+> text "#-}"
        ppr_imp NotBoot = empty

        pp_spec Nothing             = empty
        pp_spec (Just (False, (L _ ies))) = ppr_ies ies
        pp_spec (Just (True, (L _ ies))) = text "hiding" <+> ppr_ies ies

        ppr_ies []  = text "()"
        ppr_ies ies = char '(' <+> interpp'SP ies <+> char ')'

{-
************************************************************************
*                                                                      *
\subsection{Imported and exported entities}
*                                                                      *
************************************************************************
-}

-- | A name in an import or export specification which may have
-- adornments. Used primarily for accurate pretty printing of
-- ParsedSource, and API Annotation placement. The
-- 'GHC.Parser.Annotation' is the location of the adornment in
-- the original source.
data IEWrappedName name
  = IEName                (LocatedN name)  -- ^ no extra
  | IEPattern EpaLocation (LocatedN name)  -- ^ pattern X
  | IEType    EpaLocation (LocatedN name)  -- ^ type (:+:)
  deriving (Eq,Data)

-- | Located name with possible adornment
-- - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnType',
--         'GHC.Parser.Annotation.AnnPattern'
type LIEWrappedName name = LocatedA (IEWrappedName name)
-- For details on above see Note [exact print annotations] in GHC.Parser.Annotation


-- | Located Import or Export
type LIE pass = XRec pass (IE pass)
        -- ^ When in a list this may have
        --
        --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnComma'

        -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation
type instance Anno (IE (GhcPass p)) = SrcSpanAnnA

-- | Imported or exported entity.
data IE pass
  = IEVar       (XIEVar pass) (LIEWrappedName (IdP pass))
        -- ^ Imported or Exported Variable

  | IEThingAbs  (XIEThingAbs pass) (LIEWrappedName (IdP pass))
        -- ^ Imported or exported Thing with Absent list
        --
        -- The thing is a Class/Type (can't tell)
        --  - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnPattern',
        --             'GHC.Parser.Annotation.AnnType','GHC.Parser.Annotation.AnnVal'

        -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation
        -- See Note [Located RdrNames] in GHC.Hs.Expr
  | IEThingAll  (XIEThingAll pass) (LIEWrappedName (IdP pass))
        -- ^ Imported or exported Thing with All imported or exported
        --
        -- The thing is a Class/Type and the All refers to methods/constructors
        --
        -- - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnOpen',
        --       'GHC.Parser.Annotation.AnnDotdot','GHC.Parser.Annotation.AnnClose',
        --                                 'GHC.Parser.Annotation.AnnType'

        -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation
        -- See Note [Located RdrNames] in GHC.Hs.Expr

  | IEThingWith (XIEThingWith pass)
                (LIEWrappedName (IdP pass))
                IEWildcard
                [LIEWrappedName (IdP pass)]
        -- ^ Imported or exported Thing With given imported or exported
        --
        -- The thing is a Class/Type and the imported or exported things are
        -- methods/constructors and record fields; see Note [IEThingWith]
        -- - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnOpen',
        --                                   'GHC.Parser.Annotation.AnnClose',
        --                                   'GHC.Parser.Annotation.AnnComma',
        --                                   'GHC.Parser.Annotation.AnnType'

        -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation
  | IEModuleContents  (XIEModuleContents pass) (XRec pass ModuleName)
        -- ^ Imported or exported module contents
        --
        -- (Export Only)
        --
        -- - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnModule'

        -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation
  | IEGroup             (XIEGroup pass) Int (LHsDoc pass) -- ^ Doc section heading
  | IEDoc               (XIEDoc pass) (LHsDoc pass)       -- ^ Some documentation
  | IEDocNamed          (XIEDocNamed pass) String    -- ^ Reference to named doc
  | XIE !(XXIE pass)

type instance XIEVar             GhcPs = NoExtField
type instance XIEVar             GhcRn = NoExtField
type instance XIEVar             GhcTc = NoExtField

type instance XIEThingAbs        (GhcPass _) = EpAnn [AddEpAnn]
type instance XIEThingAll        (GhcPass _) = EpAnn [AddEpAnn]

-- See Note [IEThingWith]
type instance XIEThingWith       (GhcPass 'Parsed)      = EpAnn [AddEpAnn]
type instance XIEThingWith       (GhcPass 'Renamed)     = [Located FieldLabel]
type instance XIEThingWith       (GhcPass 'Typechecked) = NoExtField

type instance XIEModuleContents  GhcPs = EpAnn [AddEpAnn]
type instance XIEModuleContents  GhcRn = NoExtField
type instance XIEModuleContents  GhcTc = NoExtField

type instance XIEGroup           (GhcPass _) = NoExtField
type instance XIEDoc             (GhcPass _) = NoExtField
type instance XIEDocNamed        (GhcPass _) = NoExtField
type instance XXIE               (GhcPass _) = DataConCantHappen

type instance Anno (LocatedA (IE (GhcPass p))) = SrcSpanAnnA

-- | Imported or Exported Wildcard
data IEWildcard = NoIEWildcard | IEWildcard Int deriving (Eq, Data)

{-
Note [IEThingWith]
~~~~~~~~~~~~~~~~~~
A definition like

    {-# LANGUAGE DuplicateRecordFields #-}
    module M ( T(MkT, x) ) where
      data T = MkT { x :: Int }

gives rise to this in the output of the parser:

    IEThingWith NoExtField T [MkT, x] NoIEWildcard

But in the renamer we need to attach the correct field label,
because the selector Name is mangled (see Note [FieldLabel] in
GHC.Types.FieldLabel).  Hence we change this to:

    IEThingWith [FieldLabel "x" True $sel:x:MkT)] T [MkT] NoIEWildcard

using the TTG extension field to store the list of fields in renamed syntax
only.  (Record fields always appear in this list, regardless of whether
DuplicateRecordFields was in use at the definition site or not.)

See Note [Representing fields in AvailInfo] in GHC.Types.Avail for more details.
-}

ieName :: IE (GhcPass p) -> IdP (GhcPass p)
ieName (IEVar _ (L _ n))            = ieWrappedName n
ieName (IEThingAbs  _ (L _ n))      = ieWrappedName n
ieName (IEThingWith _ (L _ n) _ _)  = ieWrappedName n
ieName (IEThingAll  _ (L _ n))      = ieWrappedName n
ieName _ = panic "ieName failed pattern match!"

ieNames :: IE (GhcPass p) -> [IdP (GhcPass p)]
ieNames (IEVar       _ (L _ n)   )   = [ieWrappedName n]
ieNames (IEThingAbs  _ (L _ n)   )   = [ieWrappedName n]
ieNames (IEThingAll  _ (L _ n)   )   = [ieWrappedName n]
ieNames (IEThingWith _ (L _ n) _ ns) = ieWrappedName n
                                     : map (ieWrappedName . unLoc) ns
-- NB the above case does not include names of field selectors
ieNames (IEModuleContents {})     = []
ieNames (IEGroup          {})     = []
ieNames (IEDoc            {})     = []
ieNames (IEDocNamed       {})     = []

ieWrappedLName :: IEWrappedName name -> LocatedN name
ieWrappedLName (IEName      ln) = ln
ieWrappedLName (IEPattern _ ln) = ln
ieWrappedLName (IEType    _ ln) = ln

ieWrappedName :: IEWrappedName name -> name
ieWrappedName = unLoc . ieWrappedLName


lieWrappedName :: LIEWrappedName name -> name
lieWrappedName (L _ n) = ieWrappedName n

ieLWrappedName :: LIEWrappedName name -> LocatedN name
ieLWrappedName (L _ n) = ieWrappedLName n

replaceWrappedName :: IEWrappedName name1 -> name2 -> IEWrappedName name2
replaceWrappedName (IEName      (L l _)) n = IEName      (L l n)
replaceWrappedName (IEPattern r (L l _)) n = IEPattern r (L l n)
replaceWrappedName (IEType    r (L l _)) n = IEType    r (L l n)

replaceLWrappedName :: LIEWrappedName name1 -> name2 -> LIEWrappedName name2
replaceLWrappedName (L l n) n' = L l (replaceWrappedName n n')

instance OutputableBndrId p => Outputable (IE (GhcPass p)) where
    ppr (IEVar       _     var) = ppr (unLoc var)
    ppr (IEThingAbs  _   thing) = ppr (unLoc thing)
    ppr (IEThingAll  _   thing) = hcat [ppr (unLoc thing), text "(..)"]
    ppr (IEThingWith flds thing wc withs)
        = ppr (unLoc thing) <> parens (fsep (punctuate comma
                                              (ppWiths ++ ppFields) ))
      where
        ppWiths =
          case wc of
              NoIEWildcard ->
                map (ppr . unLoc) withs
              IEWildcard pos ->
                let (bs, as) = splitAt pos (map (ppr . unLoc) withs)
                in bs ++ [text ".."] ++ as
        ppFields =
          case ghcPass @p of
            GhcRn -> map ppr flds
            _     -> []
    ppr (IEModuleContents _ mod')
        = text "module" <+> ppr mod'
    ppr (IEGroup _ n _)           = text ("<IEGroup: " ++ show n ++ ">")
    ppr (IEDoc _ doc)             = ppr doc
    ppr (IEDocNamed _ string)     = text ("<IEDocNamed: " ++ string ++ ">")

instance (HasOccName name) => HasOccName (IEWrappedName name) where
  occName w = occName (ieWrappedName w)

instance (OutputableBndr name) => OutputableBndr (IEWrappedName name) where
  pprBndr bs   w = pprBndr bs   (ieWrappedName w)
  pprPrefixOcc w = pprPrefixOcc (ieWrappedName w)
  pprInfixOcc  w = pprInfixOcc  (ieWrappedName w)

instance (OutputableBndr name) => Outputable (IEWrappedName name) where
  ppr (IEName      n) = pprPrefixOcc (unLoc n)
  ppr (IEPattern _ n) = text "pattern" <+> pprPrefixOcc (unLoc n)
  ppr (IEType    _ n) = text "type"    <+> pprPrefixOcc (unLoc n)

pprImpExp :: (HasOccName name, OutputableBndr name) => name -> SDoc
pprImpExp name = type_pref <+> pprPrefixOcc name
    where
    occ = occName name
    type_pref | isTcOcc occ && isSymOcc occ = text "type"
              | otherwise                   = empty
