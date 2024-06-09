{-# OPTIONS_GHC -Wno-orphans      #-} -- Outputable and IEWrappedName
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE StandaloneDeriving   #-}
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

module GHC.Hs.ImpExp
    ( module Language.Haskell.Syntax.ImpExp
    , module GHC.Hs.ImpExp
    ) where

import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Module.Name
import Language.Haskell.Syntax.ImpExp

import GHC.Prelude

import GHC.Types.SourceText   ( SourceText(..) )
import GHC.Types.SrcLoc
import GHC.Types.Name
import GHC.Types.PkgQual

import GHC.Parser.Annotation
import GHC.Hs.Extension
import GHC.Hs.Lit ()

import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Unit.Module.Warnings

import Data.Data
import Data.Maybe
import GHC.Hs.Doc (LHsDoc)


{-
************************************************************************
*                                                                      *
    Import and export declaration lists
*                                                                      *
************************************************************************

One per import declaration in a module.
-}

type instance Anno (ImportDecl (GhcPass p)) = SrcSpanAnnA

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


type instance ImportDeclPkgQual GhcPs = RawPkgQual GhcPs
type instance ImportDeclPkgQual GhcRn = PkgQual
type instance ImportDeclPkgQual GhcTc = PkgQual

type instance XCImportDecl  GhcPs = XImportDeclPass
type instance XCImportDecl  GhcRn = XImportDeclPass
type instance XCImportDecl  GhcTc = DataConCantHappen
                                 -- Note [Pragma source text] in "GHC.Types.SourceText"

data XImportDeclPass = XImportDeclPass
    { ideclAnn        :: EpAnn EpAnnImportDecl
    , ideclSourceText :: SourceText
    , ideclImplicit   :: Bool
        -- ^ GHC generates an `ImportDecl` to represent the invisible `import Prelude`
        -- that appears in any file that omits `import Prelude`, setting
        -- this field to indicate that the import doesn't appear in the
        -- original source. True => implicit import (of Prelude)
    }
    deriving (Data)

type instance XXImportDecl  (GhcPass _) = DataConCantHappen

type instance Anno ModuleName = SrcSpanAnnA
type instance Anno [LocatedA (IE (GhcPass p))] = SrcSpanAnnL

deriving instance Data (IEWrappedName GhcPs)
deriving instance Data (IEWrappedName GhcRn)
deriving instance Data (IEWrappedName GhcTc)

deriving instance Eq (IEWrappedName GhcPs)
deriving instance Eq (IEWrappedName GhcRn)
deriving instance Eq (IEWrappedName GhcTc)

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

instance NoAnn EpAnnImportDecl where
  noAnn = EpAnnImportDecl noAnn  Nothing  Nothing  Nothing  Nothing  Nothing
-- ---------------------------------------------------------------------

simpleImportDecl :: ModuleName -> ImportDecl GhcPs
simpleImportDecl mn = ImportDecl {
      ideclExt        = XImportDeclPass noAnn NoSourceText False,
      ideclName       = noLocA mn,
      ideclPkgQual    = NoRawPkgQual,
      ideclSource     = NotBoot,
      ideclSafe       = False,
      ideclQualified  = NotQualified,
      ideclAs         = Nothing,
      ideclImportList = Nothing
    }

instance (OutputableBndrId p
         , Outputable (Anno (IE (GhcPass p)))
         , Outputable (ImportDeclPkgQual (GhcPass p)))
       => Outputable (ImportDecl (GhcPass p)) where
    ppr (ImportDecl { ideclExt = impExt, ideclName = mod'
                    , ideclPkgQual = pkg
                    , ideclSource = from, ideclSafe = safe
                    , ideclQualified = qual
                    , ideclAs = as, ideclImportList = spec })
      = hang (hsep [text "import", ppr_imp impExt from, pp_implicit impExt, pp_safe safe,
                    pp_qual qual False, ppr pkg, ppr mod', pp_qual qual True, pp_as as])
             4 (pp_spec spec)
      where
        pp_implicit ext =
            let implicit = case ghcPass @p of
                            GhcPs | XImportDeclPass { ideclImplicit = implicit } <- ext -> implicit
                            GhcRn | XImportDeclPass { ideclImplicit = implicit } <- ext -> implicit
                            GhcTc -> dataConCantHappen ext
            in if implicit then text "(implicit)"
                           else empty

        pp_qual QualifiedPre False = text "qualified" -- Prepositive qualifier/prepositive position.
        pp_qual QualifiedPost True = text "qualified" -- Postpositive qualifier/postpositive position.
        pp_qual QualifiedPre True = empty -- Prepositive qualifier/postpositive position.
        pp_qual QualifiedPost False = empty -- Postpositive qualifier/prepositive position.
        pp_qual NotQualified _ = empty

        pp_safe False   = empty
        pp_safe True    = text "safe"

        pp_as Nothing   = empty
        pp_as (Just a)  = text "as" <+> ppr a

        ppr_imp ext IsBoot =
            let mSrcText = case ghcPass @p of
                                GhcPs | XImportDeclPass { ideclSourceText = mst } <- ext -> mst
                                GhcRn | XImportDeclPass { ideclSourceText = mst } <- ext -> mst
                                GhcTc -> dataConCantHappen ext
            in case mSrcText of
                  NoSourceText   -> text "{-# SOURCE #-}"
                  SourceText src -> ftext src <+> text "#-}"
        ppr_imp _ NotBoot = empty

        pp_spec Nothing             = empty
        pp_spec (Just (Exactly, (L _ ies))) = ppr_ies ies
        pp_spec (Just (EverythingBut, (L _ ies))) = text "hiding" <+> ppr_ies ies

        ppr_ies []  = text "()"
        ppr_ies ies = char '(' <+> interpp'SP ies <+> char ')'

{-
************************************************************************
*                                                                      *
\subsection{Imported and exported entities}
*                                                                      *
************************************************************************
-}

type instance XIEName    (GhcPass _) = NoExtField
type instance XIEPattern (GhcPass _) = EpaLocation
type instance XIEType    (GhcPass _) = EpaLocation
type instance XXIEWrappedName (GhcPass _) = DataConCantHappen

type instance Anno (IEWrappedName (GhcPass _)) = SrcSpanAnnA

type instance Anno (IE (GhcPass p)) = SrcSpanAnnA

-- The additional field of type 'Maybe (WarningTxt pass)' holds information
-- about export deprecation annotations and is thus set to Nothing when `IE`
-- is used in an import list (since export deprecation can only be used in exports)
type instance XIEVar       GhcPs = Maybe (LWarningTxt GhcPs)
type instance XIEVar       GhcRn = Maybe (LWarningTxt GhcRn)
type instance XIEVar       GhcTc = NoExtField

-- The additional field of type 'Maybe (WarningTxt pass)' holds information
-- about export deprecation annotations and is thus set to Nothing when `IE`
-- is used in an import list (since export deprecation can only be used in exports)
type instance XIEThingAbs  GhcPs = (Maybe (LWarningTxt GhcPs), [AddEpAnn])
type instance XIEThingAbs  GhcRn = (Maybe (LWarningTxt GhcRn), [AddEpAnn])
type instance XIEThingAbs  GhcTc = [AddEpAnn]

-- The additional field of type 'Maybe (WarningTxt pass)' holds information
-- about export deprecation annotations and is thus set to Nothing when `IE`
-- is used in an import list (since export deprecation can only be used in exports)
type instance XIEThingAll  GhcPs = (Maybe (LWarningTxt GhcPs), [AddEpAnn])
type instance XIEThingAll  GhcRn = (Maybe (LWarningTxt GhcRn), [AddEpAnn])
type instance XIEThingAll  GhcTc = [AddEpAnn]

-- The additional field of type 'Maybe (WarningTxt pass)' holds information
-- about export deprecation annotations and is thus set to Nothing when `IE`
-- is used in an import list (since export deprecation can only be used in exports)
type instance XIEThingWith GhcPs = (Maybe (LWarningTxt GhcPs), [AddEpAnn])
type instance XIEThingWith GhcRn = (Maybe (LWarningTxt GhcRn), [AddEpAnn])
type instance XIEThingWith GhcTc = [AddEpAnn]

-- The additional field of type 'Maybe (WarningTxt pass)' holds information
-- about export deprecation annotations and is thus set to Nothing when `IE`
-- is used in an import list (since export deprecation can only be used in exports)
type instance XIEModuleContents  GhcPs = (Maybe (LWarningTxt GhcPs), [AddEpAnn])
type instance XIEModuleContents  GhcRn = Maybe (LWarningTxt GhcRn)
type instance XIEModuleContents  GhcTc = NoExtField

type instance XIEGroup           (GhcPass _) = NoExtField
type instance XIEDoc             (GhcPass _) = NoExtField
type instance XIEDocNamed        (GhcPass _) = NoExtField
type instance XXIE               (GhcPass _) = DataConCantHappen

type instance Anno (LocatedA (IE (GhcPass p))) = SrcSpanAnnA

ieName :: IE (GhcPass p) -> IdP (GhcPass p)
ieName (IEVar _ (L _ n) _)           = ieWrappedName n
ieName (IEThingAbs  _ (L _ n) _)     = ieWrappedName n
ieName (IEThingWith _ (L _ n) _ _ _) = ieWrappedName n
ieName (IEThingAll  _ (L _ n) _)     = ieWrappedName n
ieName _ = panic "ieName failed pattern match!"

ieNames :: IE (GhcPass p) -> [IdP (GhcPass p)]
ieNames (IEVar       _ (L _ n) _)      = [ieWrappedName n]
ieNames (IEThingAbs  _ (L _ n) _)      = [ieWrappedName n]
ieNames (IEThingAll  _ (L _ n) _)      = [ieWrappedName n]
ieNames (IEThingWith _ (L _ n) _ ns _) = ieWrappedName n : map (ieWrappedName . unLoc) ns
-- NB the above case does not include names of field selectors
ieNames (IEModuleContents {})     = []
ieNames (IEGroup          {})     = []
ieNames (IEDoc            {})     = []
ieNames (IEDocNamed       {})     = []

ieDeprecation :: forall p. IsPass p => IE (GhcPass p) -> Maybe (WarningTxt (GhcPass p))
ieDeprecation = fmap unLoc . ie_deprecation (ghcPass @p)
  where
    ie_deprecation :: GhcPass p -> IE (GhcPass p) -> Maybe (LWarningTxt (GhcPass p))
    ie_deprecation GhcPs (IEVar xie _ _) = xie
    ie_deprecation GhcPs (IEThingAbs (xie, _) _ _) = xie
    ie_deprecation GhcPs (IEThingAll (xie, _) _ _) = xie
    ie_deprecation GhcPs (IEThingWith (xie, _) _ _ _ _) = xie
    ie_deprecation GhcPs (IEModuleContents (xie, _) _) = xie
    ie_deprecation GhcRn (IEVar xie _ _) = xie
    ie_deprecation GhcRn (IEThingAbs (xie, _) _ _) = xie
    ie_deprecation GhcRn (IEThingAll (xie, _) _ _) = xie
    ie_deprecation GhcRn (IEThingWith (xie, _) _ _ _ _) = xie
    ie_deprecation GhcRn (IEModuleContents xie _) = xie
    ie_deprecation _ _ = Nothing

ieWrappedLName :: IEWrappedName (GhcPass p) -> LIdP (GhcPass p)
ieWrappedLName (IEName    _ (L l n)) = L l n
ieWrappedLName (IEPattern _ (L l n)) = L l n
ieWrappedLName (IEType    _ (L l n)) = L l n

ieWrappedName :: IEWrappedName (GhcPass p) -> IdP (GhcPass p)
ieWrappedName = unLoc . ieWrappedLName


lieWrappedName :: LIEWrappedName (GhcPass p) -> IdP (GhcPass p)
lieWrappedName (L _ n) = ieWrappedName n

ieLWrappedName :: LIEWrappedName (GhcPass p) -> LIdP (GhcPass p)
ieLWrappedName (L _ n) = ieWrappedLName n

replaceWrappedName :: IEWrappedName GhcPs -> IdP GhcRn -> IEWrappedName GhcRn
replaceWrappedName (IEName    x (L l _)) n = IEName    x (L l n)
replaceWrappedName (IEPattern r (L l _)) n = IEPattern r (L l n)
replaceWrappedName (IEType    r (L l _)) n = IEType    r (L l n)

replaceLWrappedName :: LIEWrappedName GhcPs -> IdP GhcRn -> LIEWrappedName GhcRn
replaceLWrappedName (L l n) n' = L l (replaceWrappedName n n')

exportDocstring :: LHsDoc pass -> SDoc
exportDocstring doc = braces (text "docstring: " <> ppr doc)

instance OutputableBndrId p => Outputable (IE (GhcPass p)) where
    ppr ie@(IEVar       _     var doc) =
      sep $ catMaybes [ ppr <$> ieDeprecation ie
                      , Just $ ppr (unLoc var)
                      , exportDocstring <$> doc
                      ]
    ppr ie@(IEThingAbs  _   thing doc) =
      sep $ catMaybes [ ppr <$> ieDeprecation ie
                      , Just $ ppr (unLoc thing)
                      , exportDocstring <$> doc
                      ]
    ppr ie@(IEThingAll  _   thing doc) =
      sep $ catMaybes [ ppr <$> ieDeprecation ie
                      , Just $ hcat [ppr (unLoc thing)
                      , text "(..)"]
                      , exportDocstring <$> doc
                      ]
    ppr ie@(IEThingWith _ thing wc withs doc) =
      sep $ catMaybes [ ppr <$> ieDeprecation ie
                      , Just $ ppr (unLoc thing) <> parens (fsep (punctuate comma ppWiths))
                      , exportDocstring <$> doc
                      ]
      where
        ppWiths =
          case wc of
              NoIEWildcard ->
                map (ppr . unLoc) withs
              IEWildcard pos ->
                let (bs, as) = splitAt pos (map (ppr . unLoc) withs)
                in bs ++ [text ".."] ++ as
    ppr ie@(IEModuleContents _ mod')
        = sep $ catMaybes [ppr <$> ieDeprecation ie, Just $ text "module" <+> ppr mod']
    ppr (IEGroup _ n _)           = text ("<IEGroup: " ++ show n ++ ">")
    ppr (IEDoc _ doc)             = ppr doc
    ppr (IEDocNamed _ string)     = text ("<IEDocNamed: " ++ string ++ ">")

instance (HasOccName (IdP (GhcPass p)), OutputableBndrId p) => HasOccName (IEWrappedName (GhcPass p)) where
  occName w = occName (ieWrappedName w)

instance OutputableBndrId p => OutputableBndr (IEWrappedName (GhcPass p)) where
  pprBndr bs   w = pprBndr bs   (ieWrappedName w)
  pprPrefixOcc w = pprPrefixOcc (ieWrappedName w)
  pprInfixOcc  w = pprInfixOcc  (ieWrappedName w)

instance OutputableBndrId p => Outputable (IEWrappedName (GhcPass p)) where
  ppr (IEName    _ (L _ n)) = pprPrefixOcc n
  ppr (IEPattern _ (L _ n)) = text "pattern" <+> pprPrefixOcc n
  ppr (IEType    _ (L _ n)) = text "type"    <+> pprPrefixOcc n

pprImpExp :: (HasOccName name, OutputableBndr name) => name -> SDoc
pprImpExp name = type_pref <+> pprPrefixOcc name
    where
    occ = occName name
    type_pref | isTcOcc occ && isSymOcc occ = text "type"
              | otherwise                   = empty
