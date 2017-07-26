{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


HsImpExp: Abstract syntax: imports, exports, interfaces
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder

module HsImpExp
  ( ImportDecl
      , pattern ImportDecl
          , ideclSourceSrc
          , ideclName
          , ideclPkgQual
          , ideclSource
          , ideclSafe
          , ideclQualified
          , ideclImplicit
          , ideclAs
          , ideclHiding
  , LImportDecl
  , IEWrappedName
      , pattern IEName
      , pattern IEPattern
      , pattern IEType
  , LIEWrappedName
  , IE
      , pattern IEVar
      , pattern IEThingAbs
      , pattern IEThingAll
      , pattern IEThingWith
      , pattern IEModuleContents
      , pattern IEGroup
      , pattern IEDoc
      , pattern IEDocNamed
  , LIE
  , IEWildcard
      , pattern NoIEWildcard
      , pattern IEWildcard
  , simpleImportDecl
  , ieName
  , ieNames
  , ieWrappedName
  , ieLWrappedName
  , replaceWrappedName
  , replaceLWrappedName
  , pprImpExp
  ) where

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

import qualified AST

-- -----------------------------------------------------------------------------
-- * Data Declarations
-- -----------------------------------------------------------------------------

-- ** Import and export declaration lists
-- -----------------------------------------------------------------------------

{-
One per \tr{import} declaration in a module.
-}

type
  ImportDecl pass = AST.ImportDecl (GHC pass)
  -- ^ Import Declaration
  --
  -- A single Haskell @import@ declaration.
pattern
  ImportDecl ::
    (SourceText) ->   -- Note [Pragma source text] in BasicTypes
    (Located ModuleName) ->
    -- ^ Module name.
    (Maybe StringLiteral) ->
    -- ^ Package qualifier.
    (Bool) ->
    -- ^ True <=> {-\# SOURCE \#-} import
    (Bool) ->
    -- ^ True => safe import
    (Bool) ->
    -- ^ True => qualified
    (Bool) ->
    -- ^ True => implicit import (of Prelude)
    (Maybe (Located ModuleName)) ->
    -- ^ as Module
    (Maybe (Bool, Located [LIE pass])) ->
    -- ^ (True => hiding, names)
    ImportDecl pass
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

pattern
  ImportDecl
    { ideclSourceSrc,
      ideclName,
      ideclPkgQual,
      ideclSource,
      ideclSafe,
      ideclQualified,
      ideclImplicit,
      ideclAs,
      ideclHiding }
    = AST.ImportDecl NoFieldExt
        ideclSourceSrc
        ideclName
        ideclPkgQual
        ideclSource
        ideclSafe
        ideclQualified
        ideclImplicit
        ideclAs
        ideclHiding

{-#
  COMPLETE
    ImportDecl
  #-}

type instance
  AST.XImportDecl    (GHC pass) = NoFieldExt
type instance
  AST.XNewImportDecl (GHC pass) = NoConExt

type
  LImportDecl pass = AST.LImportDecl (GHC pass)
  -- ^ Located Import Declaration
  --  When in a list this may have
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi'

  -- For details on above see note [Api annotations] in ApiAnnotation

-- ** Imported and exported entities
-- -----------------------------------------------------------------------------

type
  IEWrappedName name = AST.IEWrappedName name
  -- ^ A name in an import or export specification which may have adornments. Used
  -- primarily for accurate pretty printing of ParsedSource, and API Annotation
  -- placement.
pattern
  IEName ::
    (Located name) ->
    IEWrappedName name
  -- ^ no extra
pattern
  IEPattern ::
    (Located name) ->
    IEWrappedName name
  -- ^ pattern X
pattern
  IEType ::
    (Located name) ->
    IEWrappedName name
  -- ^ type (:+:)

pattern
  IEName a
    = AST.IEName a
pattern
  IEPattern a
    = AST.IEPattern a
pattern
  IEType a
    = AST.IEType a

{-#
  COMPLETE
    IEName,
    IEPattern,
    IEType
  #-}


type
  LIEWrappedName name = AST.LIEWrappedName name
  -- ^ Located name with possible adornment
  -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnType',
  --         'ApiAnnotation.AnnPattern'

  -- For details on above see note [Api annotations] in ApiAnnotation

----------------------------------------------------------------------------


type
  IE pass = AST.IE (GHC pass)
  -- ^ Imported or exported entity.
pattern
  IEVar ::
    (LIEWrappedName (IdP pass)) ->
    IE pass
  -- ^ Imported or Exported Variable
pattern
  IEThingAbs ::
    (LIEWrappedName (IdP pass)) ->
    IE pass
  -- ^ Imported or exported Thing with Absent list
  --
  -- The thing is a Class/Type (can't tell)
  --  - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnPattern',
  --             'ApiAnnotation.AnnType','ApiAnnotation.AnnVal'

  -- For details on above see note [Api annotations] in ApiAnnotation
  -- See Note [Located RdrNames] in HsExpr
pattern
  IEThingAll ::
    (LIEWrappedName (IdP pass)) ->
    IE pass
  -- ^ Imported or exported Thing with All imported or exported
  --
  -- The thing is a Class/Type and the All refers to methods/constructors
  --
  -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen',
  --       'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnClose',
  --                                 'ApiAnnotation.AnnType'

  -- For details on above see note [Api annotations] in ApiAnnotation
  -- See Note [Located RdrNames] in HsExpr
pattern
  IEThingWith ::
    (LIEWrappedName (IdP pass)) ->
    IEWildcard ->
    [LIEWrappedName (IdP pass)] ->
    [Located (FieldLbl (IdP pass))] ->
    IE pass
  -- ^ Imported or exported Thing With given imported or exported
  --
  -- The thing is a Class/Type and the imported or exported things are
  -- methods/constructors and record fields; see Note [IEThingWith]
  -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen',
  --                                   'ApiAnnotation.AnnClose',
  --                                   'ApiAnnotation.AnnComma',
  --                                   'ApiAnnotation.AnnType'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  IEModuleContents ::
    (Located ModuleName) ->
    IE pass
  -- ^ Imported or exported module contents
  --
  -- (Export Only)
  --
  -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnModule'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  IEGroup ::
    Int ->
    HsDocString ->
    IE pass
  -- ^ Doc section heading
pattern
  IEDoc ::
    HsDocString ->
    IE pass
  -- ^ Some documentation
pattern
  IEDocNamed ::
    String ->
    IE pass
  -- ^ Reference to named doc

pattern
  IEVar a
    = AST.IEVar NoFieldExt a
pattern
  IEThingAbs a
    = AST.IEThingAbs NoFieldExt a
pattern
  IEThingAll a
    = AST.IEThingAll NoFieldExt a
pattern
  IEThingWith a b c d
    = AST.IEThingWith NoFieldExt a b c d
pattern
  IEModuleContents a
    = AST.IEModuleContents NoFieldExt a
pattern
  IEGroup a b
    = AST.IEGroup NoFieldExt a b
pattern
  IEDoc a
    = AST.IEDoc NoFieldExt a
pattern
  IEDocNamed a
    = AST.IEDocNamed NoFieldExt a

{-#
  COMPLETE
    IEVar,
    IEThingAbs,
    IEThingAll,
    IEThingWith,
    IEModuleContents,
    IEGroup,
    IEDoc,
    IEDocNamed
  #-}

type instance
  AST.XIEVar            (GHC pass) = NoFieldExt
type instance
  AST.XIEThingAbs       (GHC pass) = NoFieldExt
type instance
  AST.XIEThingAll       (GHC pass) = NoFieldExt
type instance
  AST.XIEThingWith      (GHC pass) = NoFieldExt
type instance
  AST.XIEModuleContents (GHC pass) = NoFieldExt
type instance
  AST.XIEGroup          (GHC pass) = NoFieldExt
type instance
  AST.XIEDoc            (GHC pass) = NoFieldExt
type instance
  AST.XIEDocNamed       (GHC pass) = NoFieldExt
type instance
  AST.XNewIE            (GHC pass) = NoConExt

type
  LIE pass = AST.LIE (GHC pass)
  -- ^ Located Import or Export
  --  When in a list this may have
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnComma'

  -- For details on above see note [Api annotations] in ApiAnnotation

-- -----------------------------------------------------------------------------

type
  IEWildcard  = AST.IEWildcard
  -- ^ Imported or Exported Wildcard
pattern
  NoIEWildcard ::
    IEWildcard

pattern
  IEWildcard ::
    Int ->
    IEWildcard

pattern
  NoIEWildcard
    = AST.NoIEWildcard
pattern
  IEWildcard a
    = AST.IEWildcard a

{-#
  COMPLETE
    NoIEWildcard,
    IEWildcard
  #-}

-- -----------------------------------------------------------------------------
-- * Utilities
-- -----------------------------------------------------------------------------

deriving instance
  (DataId name) => Data (ImportDecl name)

deriving instance
  Data name => Data (IEWrappedName name)

deriving instance
  (DataId name) => Data (IE name)

deriving instance
  Data IEWildcard

-- -----------------------------------------------------------------------------

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


deriving instance
  Eq name => Eq (IEWrappedName name)

deriving instance
  (Eq name, Eq (IdP name)) => Eq (IE name)

deriving instance
  Eq IEWildcard

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

-- -----------------------------------------------------------------------------
-- * Pretty Printing
-- -----------------------------------------------------------------------------

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

-- -----------------------------------------------------------------------------
-- Notes
-- -----------------------------------------------------------------------------
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
