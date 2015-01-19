{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


HsImpExp: Abstract syntax: imports, exports, interfaces
-}

{-# LANGUAGE DeriveDataTypeable #-}

module HsImpExp where

import Module           ( ModuleName )
import HsDoc            ( HsDocString )
import OccName          ( HasOccName(..), isTcOcc, isSymOcc )
import BasicTypes       ( SourceText )

import Outputable
import FastString
import SrcLoc

import Data.Data

{-
************************************************************************
*                                                                      *
\subsection{Import and export declaration lists}
*                                                                      *
************************************************************************

One per \tr{import} declaration in a module.
-}

type LImportDecl name = Located (ImportDecl name)
        -- ^ When in a list this may have
        --
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi'

        -- For details on above see note [Api annotations] in ApiAnnotation

-- | A single Haskell @import@ declaration.
data ImportDecl name
  = ImportDecl {
      ideclSourceSrc :: Maybe SourceText,
                                 -- Note [Pragma source text] in BasicTypes
      ideclName      :: Located ModuleName, -- ^ Module name.
      ideclPkgQual   :: Maybe FastString,  -- ^ Package qualifier.
      ideclSource    :: Bool,              -- ^ True <=> {-\# SOURCE \#-} import
      ideclSafe      :: Bool,               -- ^ True => safe import
      ideclQualified :: Bool,               -- ^ True => qualified
      ideclImplicit  :: Bool,               -- ^ True => implicit import (of Prelude)
      ideclAs        :: Maybe ModuleName,   -- ^ as Module
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
       deriving (Data, Typeable)

simpleImportDecl :: ModuleName -> ImportDecl name
simpleImportDecl mn = ImportDecl {
      ideclSourceSrc = Nothing,
      ideclName      = noLoc mn,
      ideclPkgQual   = Nothing,
      ideclSource    = False,
      ideclSafe      = False,
      ideclImplicit  = False,
      ideclQualified = False,
      ideclAs        = Nothing,
      ideclHiding    = Nothing
    }

instance (OutputableBndr name, HasOccName name) => Outputable (ImportDecl name) where
    ppr (ImportDecl { ideclName = mod', ideclPkgQual = pkg
                    , ideclSource = from, ideclSafe = safe
                    , ideclQualified = qual, ideclImplicit = implicit
                    , ideclAs = as, ideclHiding = spec })
      = hang (hsep [ptext (sLit "import"), ppr_imp from, pp_implicit implicit, pp_safe safe,
                    pp_qual qual, pp_pkg pkg, ppr mod', pp_as as])
             4 (pp_spec spec)
      where
        pp_implicit False = empty
        pp_implicit True = ptext (sLit ("(implicit)"))

        pp_pkg Nothing  = empty
        pp_pkg (Just p) = doubleQuotes (ftext p)

        pp_qual False   = empty
        pp_qual True    = ptext (sLit "qualified")

        pp_safe False   = empty
        pp_safe True    = ptext (sLit "safe")

        pp_as Nothing   = empty
        pp_as (Just a)  = ptext (sLit "as") <+> ppr a

        ppr_imp True  = ptext (sLit "{-# SOURCE #-}")
        ppr_imp False = empty

        pp_spec Nothing             = empty
        pp_spec (Just (False, (L _ ies))) = ppr_ies ies
        pp_spec (Just (True, (L _ ies))) = ptext (sLit "hiding") <+> ppr_ies ies

        ppr_ies []  = ptext (sLit "()")
        ppr_ies ies = char '(' <+> interpp'SP ies <+> char ')'

{-
************************************************************************
*                                                                      *
\subsection{Imported and exported entities}
*                                                                      *
************************************************************************
-}

type LIE name = Located (IE name)
        -- ^ When in a list this may have
        --
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnComma'

        -- For details on above see note [Api annotations] in ApiAnnotation

-- | Imported or exported entity.
data IE name
  = IEVar       (Located name)
        -- ^ - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnPattern',
        --             'ApiAnnotation.AnnType'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | IEThingAbs  (Located name)     -- ^ Class/Type (can't tell)
        --  - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnPattern',
        --             'ApiAnnotation.AnnType','ApiAnnotation.AnnVal'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | IEThingAll  (Located name)     -- ^ Class/Type plus all methods/constructors
        --
        -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen',
        --       'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnClose',
        --                                 'ApiAnnotation.AnnType'

        -- For details on above see note [Api annotations] in ApiAnnotation

  | IEThingWith (Located name) [Located name]
                 -- ^ Class/Type plus some methods/constructors
        -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen',
        --                                   'ApiAnnotation.AnnClose',
        --                                   'ApiAnnotation.AnnComma',
        --                                   'ApiAnnotation.AnnType'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | IEModuleContents  (Located ModuleName) -- ^ (Export Only)
        --
        -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnModule'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | IEGroup             Int HsDocString  -- ^ Doc section heading
  | IEDoc               HsDocString      -- ^ Some documentation
  | IEDocNamed          String           -- ^ Reference to named doc
  deriving (Eq, Data, Typeable)

ieName :: IE name -> name
ieName (IEVar (L _ n))         = n
ieName (IEThingAbs  (L _ n))   = n
ieName (IEThingWith (L _ n) _) = n
ieName (IEThingAll  (L _ n))   = n
ieName _ = panic "ieName failed pattern match!"

ieNames :: IE a -> [a]
ieNames (IEVar       (L _ n)   ) = [n]
ieNames (IEThingAbs  (L _ n)   ) = [n]
ieNames (IEThingAll  (L _ n)   ) = [n]
ieNames (IEThingWith (L _ n) ns) = n : map unLoc ns
ieNames (IEModuleContents _    ) = []
ieNames (IEGroup          _ _  ) = []
ieNames (IEDoc            _    ) = []
ieNames (IEDocNamed       _    ) = []

pprImpExp :: (HasOccName name, OutputableBndr name) => name -> SDoc
pprImpExp name = type_pref <+> pprPrefixOcc name
    where
    occ = occName name
    type_pref | isTcOcc occ && isSymOcc occ = ptext (sLit "type")
              | otherwise                   = empty

instance (HasOccName name, OutputableBndr name) => Outputable (IE name) where
    ppr (IEVar          var)    = pprPrefixOcc (unLoc var)
    ppr (IEThingAbs     thing)  = pprImpExp (unLoc thing)
    ppr (IEThingAll      thing) = hcat [pprImpExp (unLoc thing), text "(..)"]
    ppr (IEThingWith thing withs)
        = pprImpExp (unLoc thing) <> parens (fsep (punctuate comma
                                            (map pprImpExp $ map unLoc withs)))
    ppr (IEModuleContents mod')
        = ptext (sLit "module") <+> ppr mod'
    ppr (IEGroup n _)           = text ("<IEGroup: " ++ (show n) ++ ">")
    ppr (IEDoc doc)             = ppr doc
    ppr (IEDocNamed string)     = text ("<IEDocNamed: " ++ string ++ ">")
