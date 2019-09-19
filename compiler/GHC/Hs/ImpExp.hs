{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


GHC.Hs.ImpExp: Abstract syntax: imports, exports, interfaces
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module GHC.Hs.PlaceHolder

module GHC.Hs.ImpExp where

import GhcPrelude

import Module           ( ModuleName, IsBootInterface )
import GHC.Hs.Doc       ( HsDocString )
import OccName          ( HasOccName(..), isTcOcc, isSymOcc )
import BasicTypes       ( SourceText(..), StringLiteral(..), pprWithSourceText )
import FieldLabel       ( FieldLbl(..) )

import Outputable
import FastString
import SrcLoc
import GHC.Hs.Extension

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
type LImportDecl pass = Located (ImportDecl pass)
        -- ^ When in a list this may have
        --
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi'

        -- For details on above see note [Api annotations] in ApiAnnotation

-- | If/how an import is 'qualified'.
data ImportDeclQualifiedStyle
  = QualifiedPre  -- ^ 'qualified' appears in prepositive position.
  | QualifiedPost -- ^ 'qualified' appears in postpositive position.
  | NotQualified  -- ^ Not qualified.
  deriving (Eq, Data)

-- | Given two possible located 'qualified' tokens, compute a style
-- (in a conforming Haskell program only one of the two can be not
-- 'Nothing'). This is called from 'Parser.y'.
importDeclQualifiedStyle :: Maybe (Located a)
                         -> Maybe (Located a)
                         -> ImportDeclQualifiedStyle
importDeclQualifiedStyle mPre mPost =
  if isJust mPre then QualifiedPre
  else if isJust mPost then QualifiedPost else NotQualified

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
                                 -- Note [Pragma source text] in BasicTypes
      ideclName      :: Located ModuleName, -- ^ Module name.
      ideclPkgQual   :: Maybe StringLiteral,  -- ^ Package qualifier.
      ideclSource    :: IsBootInterface, -- ^ True <=> {-\# SOURCE \#-} import
      ideclSafe      :: Bool,          -- ^ True => safe import
      ideclQualified :: ImportDeclQualifiedStyle, -- ^ If/how the import is qualified.
      ideclImplicit  :: Bool,          -- ^ True => implicit import (of Prelude)
      ideclAs        :: Maybe (Located ModuleName),  -- ^ as Module
      ideclHiding    :: Maybe (Bool, Located [LIE pass])
                                       -- ^ (True => hiding, names)
    }
  | XImportDecl (XXImportDecl pass)
     -- ^
     --  'ApiAnnotation.AnnKeywordId's
     --
     --  - 'ApiAnnotation.AnnImport'
     --
     --  - 'ApiAnnotation.AnnOpen', 'ApiAnnotation.AnnClose' for ideclSource
     --
     --  - 'ApiAnnotation.AnnSafe','ApiAnnotation.AnnQualified',
     --    'ApiAnnotation.AnnPackageName','ApiAnnotation.AnnAs',
     --    'ApiAnnotation.AnnVal'
     --
     --  - 'ApiAnnotation.AnnHiding','ApiAnnotation.AnnOpen',
     --    'ApiAnnotation.AnnClose' attached
     --     to location in ideclHiding

     -- For details on above see note [Api annotations] in ApiAnnotation

type instance XCImportDecl  (GhcPass _) = NoExtField
type instance XXImportDecl  (GhcPass _) = NoExtCon

simpleImportDecl :: ModuleName -> ImportDecl (GhcPass p)
simpleImportDecl mn = ImportDecl {
      ideclExt       = noExtField,
      ideclSourceSrc = NoSourceText,
      ideclName      = noLoc mn,
      ideclPkgQual   = Nothing,
      ideclSource    = False,
      ideclSafe      = False,
      ideclImplicit  = False,
      ideclQualified = NotQualified,
      ideclAs        = Nothing,
      ideclHiding    = Nothing
    }

instance OutputableBndrId p
       => Outputable (ImportDecl (GhcPass p)) where
    ppr (ImportDecl { ideclSourceSrc = mSrcText, ideclName = mod'
                    , ideclPkgQual = pkg
                    , ideclSource = from, ideclSafe = safe
                    , ideclQualified = qual, ideclImplicit = implicit
                    , ideclAs = as, ideclHiding = spec })
      = hang (hsep [text "import", ppr_imp from, pp_implicit implicit, pp_safe safe,
                    pp_qual qual False, pp_pkg pkg, ppr mod', pp_qual qual True, pp_as as])
             4 (pp_spec spec)
      where
        pp_implicit False = empty
        pp_implicit True = ptext (sLit ("(implicit)"))

        pp_pkg Nothing                    = empty
        pp_pkg (Just (StringLiteral st p))
          = pprWithSourceText st (doubleQuotes (ftext p))

        pp_qual QualifiedPre False = text "qualified" -- Prepositive qualifier/prepositive position.
        pp_qual QualifiedPost True = text "qualified" -- Postpositive qualifier/postpositive position.
        pp_qual QualifiedPre True = empty -- Prepositive qualifier/postpositive position.
        pp_qual QualifiedPost False = empty -- Postpositive qualifier/prepositive position.
        pp_qual NotQualified _ = empty

        pp_safe False   = empty
        pp_safe True    = text "safe"

        pp_as Nothing   = empty
        pp_as (Just a)  = text "as" <+> ppr a

        ppr_imp True  = case mSrcText of
                          NoSourceText   -> text "{-# SOURCE #-}"
                          SourceText src -> text src <+> text "#-}"
        ppr_imp False = empty

        pp_spec Nothing             = empty
        pp_spec (Just (False, (L _ ies))) = ppr_ies ies
        pp_spec (Just (True, (L _ ies))) = text "hiding" <+> ppr_ies ies

        ppr_ies []  = text "()"
        ppr_ies ies = char '(' <+> interpp'SP ies <+> char ')'
    ppr (XImportDecl x) = ppr x

{-
************************************************************************
*                                                                      *
\subsection{Imported and exported entities}
*                                                                      *
************************************************************************
-}

-- | A name in an import or export specification which may have adornments. Used
-- primarily for accurate pretty printing of ParsedSource, and API Annotation
-- placement.
data IEWrappedName name
  = IEName    (Located name)  -- ^ no extra
  | IEPattern (Located name)  -- ^ pattern X
  | IEType    (Located name)  -- ^ type (:+:)
  deriving (Eq,Data)

-- | Located name with possible adornment
-- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnType',
--         'ApiAnnotation.AnnPattern'
type LIEWrappedName name = Located (IEWrappedName name)
-- For details on above see note [Api annotations] in ApiAnnotation


-- | Located Import or Export
type LIE pass = Located (IE pass)
        -- ^ When in a list this may have
        --
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnComma'

        -- For details on above see note [Api annotations] in ApiAnnotation

-- | Imported or exported entity.
data IE pass
  = IEVar       (XIEVar pass) (LIEWrappedName (IdP pass))
        -- ^ Imported or Exported Variable

  | IEThingAbs  (XIEThingAbs pass) (LIEWrappedName (IdP pass))
        -- ^ Imported or exported Thing with Absent list
        --
        -- The thing is a Class/Type (can't tell)
        --  - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnPattern',
        --             'ApiAnnotation.AnnType','ApiAnnotation.AnnVal'

        -- For details on above see note [Api annotations] in ApiAnnotation
        -- See Note [Located RdrNames] in GHC.Hs.Expr
  | IEThingAll  (XIEThingAll pass) (LIEWrappedName (IdP pass))
        -- ^ Imported or exported Thing with All imported or exported
        --
        -- The thing is a Class/Type and the All refers to methods/constructors
        --
        -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen',
        --       'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnClose',
        --                                 'ApiAnnotation.AnnType'

        -- For details on above see note [Api annotations] in ApiAnnotation
        -- See Note [Located RdrNames] in GHC.Hs.Expr

  | IEThingWith (XIEThingWith pass)
                (LIEWrappedName (IdP pass))
                IEWildcard
                [LIEWrappedName (IdP pass)]
                [Located (FieldLbl (IdP pass))]
        -- ^ Imported or exported Thing With given imported or exported
        --
        -- The thing is a Class/Type and the imported or exported things are
        -- methods/constructors and record fields; see Note [IEThingWith]
        -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen',
        --                                   'ApiAnnotation.AnnClose',
        --                                   'ApiAnnotation.AnnComma',
        --                                   'ApiAnnotation.AnnType'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | IEModuleContents  (XIEModuleContents pass) (Located ModuleName)
        -- ^ Imported or exported module contents
        --
        -- (Export Only)
        --
        -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnModule'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | IEGroup             (XIEGroup pass) Int HsDocString -- ^ Doc section heading
  | IEDoc               (XIEDoc pass) HsDocString       -- ^ Some documentation
  | IEDocNamed          (XIEDocNamed pass) String    -- ^ Reference to named doc
  | XIE (XXIE pass)

type instance XIEVar             (GhcPass _) = NoExtField
type instance XIEThingAbs        (GhcPass _) = NoExtField
type instance XIEThingAll        (GhcPass _) = NoExtField
type instance XIEThingWith       (GhcPass _) = NoExtField
type instance XIEModuleContents  (GhcPass _) = NoExtField
type instance XIEGroup           (GhcPass _) = NoExtField
type instance XIEDoc             (GhcPass _) = NoExtField
type instance XIEDocNamed        (GhcPass _) = NoExtField
type instance XXIE               (GhcPass _) = NoExtCon

-- | Imported or Exported Wildcard
data IEWildcard = NoIEWildcard | IEWildcard Int deriving (Eq, Data)

{-
Note [IEThingWith]
~~~~~~~~~~~~~~~~~~

A definition like

    module M ( T(MkT, x) ) where
      data T = MkT { x :: Int }

gives rise to

    IEThingWith T [MkT] [FieldLabel "x" False x)]           (without DuplicateRecordFields)
    IEThingWith T [MkT] [FieldLabel "x" True $sel:x:MkT)]   (with    DuplicateRecordFields)

See Note [Representing fields in AvailInfo] in Avail for more details.
-}

ieName :: IE (GhcPass p) -> IdP (GhcPass p)
ieName (IEVar _ (L _ n))              = ieWrappedName n
ieName (IEThingAbs  _ (L _ n))        = ieWrappedName n
ieName (IEThingWith _ (L _ n) _ _ _)  = ieWrappedName n
ieName (IEThingAll  _ (L _ n))        = ieWrappedName n
ieName _ = panic "ieName failed pattern match!"

ieNames :: IE (GhcPass p) -> [IdP (GhcPass p)]
ieNames (IEVar       _ (L _ n)   )     = [ieWrappedName n]
ieNames (IEThingAbs  _ (L _ n)   )     = [ieWrappedName n]
ieNames (IEThingAll  _ (L _ n)   )     = [ieWrappedName n]
ieNames (IEThingWith _ (L _ n) _ ns _) = ieWrappedName n
                                       : map (ieWrappedName . unLoc) ns
ieNames (IEModuleContents {})     = []
ieNames (IEGroup          {})     = []
ieNames (IEDoc            {})     = []
ieNames (IEDocNamed       {})     = []
ieNames (XIE nec) = noExtCon nec

ieWrappedName :: IEWrappedName name -> name
ieWrappedName (IEName    (L _ n)) = n
ieWrappedName (IEPattern (L _ n)) = n
ieWrappedName (IEType    (L _ n)) = n

lieWrappedName :: LIEWrappedName name -> name
lieWrappedName (L _ n) = ieWrappedName n

ieLWrappedName :: LIEWrappedName name -> Located name
ieLWrappedName (L l n) = L l (ieWrappedName n)

replaceWrappedName :: IEWrappedName name1 -> name2 -> IEWrappedName name2
replaceWrappedName (IEName    (L l _)) n = IEName    (L l n)
replaceWrappedName (IEPattern (L l _)) n = IEPattern (L l n)
replaceWrappedName (IEType    (L l _)) n = IEType    (L l n)

replaceLWrappedName :: LIEWrappedName name1 -> name2 -> LIEWrappedName name2
replaceLWrappedName (L l n) n' = L l (replaceWrappedName n n')

instance OutputableBndrId p => Outputable (IE (GhcPass p)) where
    ppr (IEVar       _     var) = ppr (unLoc var)
    ppr (IEThingAbs  _   thing) = ppr (unLoc thing)
    ppr (IEThingAll  _   thing) = hcat [ppr (unLoc thing), text "(..)"]
    ppr (IEThingWith _ thing wc withs flds)
        = ppr (unLoc thing) <> parens (fsep (punctuate comma
                                              (ppWiths ++
                                              map (ppr . flLabel . unLoc) flds)))
      where
        ppWiths =
          case wc of
              NoIEWildcard ->
                map (ppr . unLoc) withs
              IEWildcard pos ->
                let (bs, as) = splitAt pos (map (ppr . unLoc) withs)
                in bs ++ [text ".."] ++ as
    ppr (IEModuleContents _ mod')
        = text "module" <+> ppr mod'
    ppr (IEGroup _ n _)           = text ("<IEGroup: " ++ show n ++ ">")
    ppr (IEDoc _ doc)             = ppr doc
    ppr (IEDocNamed _ string)     = text ("<IEDocNamed: " ++ string ++ ">")
    ppr (XIE x) = ppr x

instance (HasOccName name) => HasOccName (IEWrappedName name) where
  occName w = occName (ieWrappedName w)

instance (OutputableBndr name) => OutputableBndr (IEWrappedName name) where
  pprBndr bs   w = pprBndr bs   (ieWrappedName w)
  pprPrefixOcc w = pprPrefixOcc (ieWrappedName w)
  pprInfixOcc  w = pprInfixOcc  (ieWrappedName w)

instance (OutputableBndr name) => Outputable (IEWrappedName name) where
  ppr (IEName    n) = pprPrefixOcc (unLoc n)
  ppr (IEPattern n) = text "pattern" <+> pprPrefixOcc (unLoc n)
  ppr (IEType    n) = text "type"    <+> pprPrefixOcc (unLoc n)

pprImpExp :: (HasOccName name, OutputableBndr name) => name -> SDoc
pprImpExp name = type_pref <+> pprPrefixOcc name
    where
    occ = occName name
    type_pref | isTcOcc occ && isSymOcc occ = text "type"
              | otherwise                   = empty
