{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


HsImpExp: Abstract syntax: imports, exports, interfaces
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder

module HsImpExp where

import Module           ( ModuleName )
import HsDoc            ( HsDocString )
import OccName          ( HasOccName(..), isTcOcc, isSymOcc )
import BasicTypes       ( SourceText(..), StringLiteral(..), pprWithSourceText )
import FieldLabel       ( FieldLbl(..) )

import Outputable
import FastString
import SrcLoc
import HsExtension

import Data.Data

{-
************************************************************************
*                                                                      *
\subsection{Import and export declaration lists}
*                                                                      *
************************************************************************

One per \tr{import} declaration in a module.
-}

-- | Located Import Declaration
type LImportDecl name = Located (ImportDecl name)
        -- ^ When in a list this may have
        --
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi'

        -- For details on above see note [Api annotations] in ApiAnnotation

-- | Import Declaration
--
-- A single Haskell @import@ declaration.
data ImportDecl name
  = ImportDecl {
      ideclSourceSrc :: SourceText,
                                 -- Note [Pragma source text] in BasicTypes
      ideclName      :: Located ModuleName, -- ^ Module name.
      ideclPkgQual   :: Maybe StringLiteral,  -- ^ Package qualifier.
      ideclSource    :: Bool,          -- ^ True <=> {-\# SOURCE \#-} import
      ideclSafe      :: Bool,          -- ^ True => safe import
      ideclQualified :: Bool,          -- ^ True => qualified
      ideclImplicit  :: Bool,          -- ^ True => implicit import (of Prelude)
      ideclAs        :: Maybe (Located ModuleName),  -- ^ as Module
      ideclHiding    :: Maybe (Bool, Located [LIE name])
                                       -- ^ (True => hiding, names)
    }
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
deriving instance (DataId name) => Data (ImportDecl name)

simpleImportDecl :: ModuleName -> ImportDecl name
simpleImportDecl mn = ImportDecl {
      ideclSourceSrc = NoSourceText,
      ideclName      = noLoc mn,
      ideclPkgQual   = Nothing,
      ideclSource    = False,
      ideclSafe      = False,
      ideclImplicit  = False,
      ideclQualified = False,
      ideclAs        = Nothing,
      ideclHiding    = Nothing
    }

instance (OutputableBndrId pass) => Outputable (ImportDecl pass) where
    ppr (ImportDecl { ideclSourceSrc = mSrcText, ideclName = mod'
                    , ideclPkgQual = pkg
                    , ideclSource = from, ideclSafe = safe
                    , ideclQualified = qual, ideclImplicit = implicit
                    , ideclAs = as, ideclHiding = spec })
      = hang (hsep [text "import", ppr_imp from, pp_implicit implicit, pp_safe safe,
                    pp_qual qual, pp_pkg pkg, ppr mod', pp_as as])
             4 (pp_spec spec)
      where
        pp_implicit False = empty
        pp_implicit True = ptext (sLit ("(implicit)"))

        pp_pkg Nothing                    = empty
        pp_pkg (Just (StringLiteral st p))
          = pprWithSourceText st (doubleQuotes (ftext p))

        pp_qual False   = empty
        pp_qual True    = text "qualified"

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

{-
************************************************************************
*                                                                      *
\subsection{Imported and exported entities}
*                                                                      *
************************************************************************
-}

-- | A name in an import or export specfication which may have adornments. Used
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
type LIE name = Located (IE name)
        -- ^ When in a list this may have
        --
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnComma'

        -- For details on above see note [Api annotations] in ApiAnnotation

-- | Imported or exported entity.
data IE name
  = IEVar       (LIEWrappedName (IdP name))
        -- ^ Imported or Exported Variable

  | IEThingAbs  (LIEWrappedName (IdP name))
        -- ^ Imported or exported Thing with Absent list
        --
        -- The thing is a Class/Type (can't tell)
        --  - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnPattern',
        --             'ApiAnnotation.AnnType','ApiAnnotation.AnnVal'

        -- For details on above see note [Api annotations] in ApiAnnotation
        -- See Note [Located RdrNames] in HsExpr
  | IEThingAll  (LIEWrappedName (IdP name))
        -- ^ Imported or exported Thing with All imported or exported
        --
        -- The thing is a Class/Type and the All refers to methods/constructors
        --
        -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen',
        --       'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnClose',
        --                                 'ApiAnnotation.AnnType'

        -- For details on above see note [Api annotations] in ApiAnnotation
        -- See Note [Located RdrNames] in HsExpr

  | IEThingWith (LIEWrappedName (IdP name))
                IEWildcard
                [LIEWrappedName (IdP name)]
                [Located (FieldLbl (IdP name))]
        -- ^ Imported or exported Thing With given imported or exported
        --
        -- The thing is a Class/Type and the imported or exported things are
        -- methods/constructors and record fields; see Note [IEThingWith]
        -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen',
        --                                   'ApiAnnotation.AnnClose',
        --                                   'ApiAnnotation.AnnComma',
        --                                   'ApiAnnotation.AnnType'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | IEModuleContents  (Located ModuleName)
        -- ^ Imported or exported module contents
        --
        -- (Export Only)
        --
        -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnModule'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | IEGroup             Int HsDocString  -- ^ Doc section heading
  | IEDoc               HsDocString      -- ^ Some documentation
  | IEDocNamed          String           -- ^ Reference to named doc
  -- deriving (Eq, Data)
deriving instance (Eq name, Eq (IdP name)) => Eq (IE name)
deriving instance (DataId name)             => Data (IE name)

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

ieName :: IE pass -> IdP pass
ieName (IEVar (L _ n))              = ieWrappedName n
ieName (IEThingAbs  (L _ n))        = ieWrappedName n
ieName (IEThingWith (L _ n) _ _ _)  = ieWrappedName n
ieName (IEThingAll  (L _ n))        = ieWrappedName n
ieName _ = panic "ieName failed pattern match!"

ieNames :: IE pass -> [IdP pass]
ieNames (IEVar       (L _ n)   )     = [ieWrappedName n]
ieNames (IEThingAbs  (L _ n)   )     = [ieWrappedName n]
ieNames (IEThingAll  (L _ n)   )     = [ieWrappedName n]
ieNames (IEThingWith (L _ n) _ ns _) = ieWrappedName n
                                       : map (ieWrappedName . unLoc) ns
ieNames (IEModuleContents _    )     = []
ieNames (IEGroup          _ _  )     = []
ieNames (IEDoc            _    )     = []
ieNames (IEDocNamed       _    )     = []

ieWrappedName :: IEWrappedName name -> name
ieWrappedName (IEName    (L _ n)) = n
ieWrappedName (IEPattern (L _ n)) = n
ieWrappedName (IEType    (L _ n)) = n

ieLWrappedName :: LIEWrappedName name -> Located name
ieLWrappedName (L l n) = L l (ieWrappedName n)

replaceWrappedName :: IEWrappedName name1 -> name2 -> IEWrappedName name2
replaceWrappedName (IEName    (L l _)) n = IEName    (L l n)
replaceWrappedName (IEPattern (L l _)) n = IEPattern (L l n)
replaceWrappedName (IEType    (L l _)) n = IEType    (L l n)

replaceLWrappedName :: LIEWrappedName name1 -> name2 -> LIEWrappedName name2
replaceLWrappedName (L l n) n' = L l (replaceWrappedName n n')

instance (OutputableBndrId pass) => Outputable (IE pass) where
    ppr (IEVar          var) = ppr (unLoc var)
    ppr (IEThingAbs     thing) = ppr (unLoc thing)
    ppr (IEThingAll     thing) = hcat [ppr (unLoc thing), text "(..)"]
    ppr (IEThingWith thing wc withs flds)
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
    ppr (IEModuleContents mod')
        = text "module" <+> ppr mod'
    ppr (IEGroup n _)           = text ("<IEGroup: " ++ show n ++ ">")
    ppr (IEDoc doc)             = ppr doc
    ppr (IEDocNamed string)     = text ("<IEDocNamed: " ++ string ++ ">")

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
