%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

HsImpExp: Abstract syntax: imports, exports, interfaces

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}

module HsImpExp where

import Module           ( ModuleName )
import HsDoc            ( HsDocString )
import OccName          ( HasOccName(..), isTcOcc, isSymOcc )
import Avail

import Outputable
import FastString
import SrcLoc

import Data.Data
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Import and export declaration lists}
%*                                                                      *
%************************************************************************

One per \tr{import} declaration in a module.
\begin{code}
type LImportDecl name = Located (ImportDecl name)

-- | A single Haskell @import@ declaration.
data ImportDecl name
  = ImportDecl {
      ideclName      :: Located ModuleName, -- ^ Module name.
      ideclPkgQual   :: Maybe FastString,   -- ^ Package qualifier.
      ideclSource    :: Bool,               -- ^ True <=> {-# SOURCE #-} import
      ideclSafe      :: Bool,               -- ^ True => safe import
      ideclQualified :: Bool,               -- ^ True => qualified
      ideclImplicit  :: Bool,               -- ^ True => implicit import (of Prelude)
      ideclAs        :: Maybe ModuleName,   -- ^ as Module
      ideclHiding    :: Maybe (Bool, [LIE name]) -- ^ (True => hiding, names)
    } deriving (Data, Typeable)

simpleImportDecl :: ModuleName -> ImportDecl name
simpleImportDecl mn = ImportDecl {
      ideclName      = noLoc mn,
      ideclPkgQual   = Nothing,
      ideclSource    = False,
      ideclSafe      = False,
      ideclImplicit  = False,
      ideclQualified = False,
      ideclAs        = Nothing,
      ideclHiding    = Nothing
    }
\end{code}

\begin{code}
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
        pp_spec (Just (False, ies)) = ppr_ies ies
        pp_spec (Just (True,  ies)) = ptext (sLit "hiding") <+> ppr_ies ies

        ppr_ies []  = ptext (sLit "()")
        ppr_ies ies = char '(' <+> interpp'SP ies <+> char ')'
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Imported and exported entities}
%*                                                                      *
%************************************************************************

\begin{code}
type LIE name = Located (IE name)

-- | Imported or exported entity.
data IE name
  = IEVar               name
  | IEThingAbs          name             -- ^ Class/Type (can't tell)
  | IEThingAll          name             -- ^ Class/Type plus all methods/constructors
  | IEThingWith         name [name] (AvailFlds name)  -- ^ Class/Type plus some methods/constructors and record fields
  | IEModuleContents    ModuleName       -- ^ (Export Only)
  | IEGroup             Int HsDocString  -- ^ Doc section heading
  | IEDoc               HsDocString      -- ^ Some documentation
  | IEDocNamed          String           -- ^ Reference to named doc
  deriving (Eq, Data, Typeable)
\end{code}

\begin{code}
ieName :: IE name -> name
ieName (IEVar n)           = n
ieName (IEThingAbs  n)     = n
ieName (IEThingWith n _ _) = n
ieName (IEThingAll  n)     = n
ieName _ = panic "ieName failed pattern match!"

ieNames :: IE a -> [a]
ieNames (IEVar            n     ) = [n]
ieNames (IEThingAbs       n     ) = [n]
ieNames (IEThingAll       n     ) = [n]
ieNames (IEThingWith      n ns fs) = n : ns ++ availFieldsNames fs
ieNames (IEModuleContents _     ) = []
ieNames (IEGroup          _ _   ) = []
ieNames (IEDoc            _     ) = []
ieNames (IEDocNamed       _     ) = []
\end{code}

\begin{code}

pprImpExp :: (HasOccName name, OutputableBndr name) => name -> SDoc
pprImpExp name = type_pref <+> pprPrefixOcc name
    where
    occ = occName name
    type_pref | isTcOcc occ && isSymOcc occ = ptext (sLit "type")
              | otherwise                   = empty

instance (HasOccName name, OutputableBndr name) => Outputable (IE name) where
    ppr (IEVar          var)    = pprPrefixOcc var
    ppr (IEThingAbs     thing)  = pprImpExp thing
    ppr (IEThingAll     thing)  = hcat [pprImpExp thing, text "(..)"]
    ppr (IEThingWith thing withs flds)
        = pprImpExp thing <> parens (fsep (punctuate comma
                                        (map pprImpExp withs ++
                                            map pprAvailField flds)))
    ppr (IEModuleContents mod')
        = ptext (sLit "module") <+> ppr mod'
    ppr (IEGroup n _)           = text ("<IEGroup: " ++ (show n) ++ ">")
    ppr (IEDoc doc)             = ppr doc
    ppr (IEDocNamed string)     = text ("<IEDocNamed: " ++ string ++ ">")
\end{code}


