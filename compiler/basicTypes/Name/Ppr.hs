module Name.Ppr
  ( pprNameDefnLoc
  , pprDefinedAt
  , pprInfixName
  , pprPrefixName
  , pprModulePrefix
  , pprNameUnqualified

  -- ** Predicates on 'Name's
  , isHoleName
  , nameModule
  ) where

import GhcPrelude

import DynFlags
import OccName
import Module
import SrcLoc
import Unique
import Util
import Maybes
import Name
import Name.Internal
import Binary
import FastString
import Outputable

import Control.DeepSeq
import Data.Data

{-
************************************************************************
*                                                                      *
\subsection{Pretty printing}
*                                                                      *
************************************************************************
-}

instance Outputable NameSort where
  ppr (External _)    = text "external"
  ppr (WiredIn _ _ _) = text "wired-in"
  ppr  Internal       = text "internal"
  ppr  System         = text "system"

instance Outputable Name where
    ppr name = pprName name

instance OutputableBndr Name where
    pprBndr _ name = pprName name
    pprInfixOcc  = pprInfixName
    pprPrefixOcc = pprPrefixName

pprName :: Name -> SDoc
pprName (Name {n_sort = sort, n_uniq = uniq, n_occ = occ})
  = getPprStyle $ \ sty ->
    case sort of
      WiredIn mod _ builtin   -> pprExternal sty uniq mod occ True  builtin
      External mod            -> pprExternal sty uniq mod occ False UserSyntax
      System                  -> pprSystem sty uniq occ
      Internal                -> pprInternal sty uniq occ

-- | Print the string of Name unqualifiedly directly.
pprNameUnqualified :: Name -> SDoc
pprNameUnqualified Name { n_occ = occ } = ppr_occ_name occ

pprExternal :: PprStyle -> Unique -> Module -> OccName -> Bool -> BuiltInSyntax -> SDoc
pprExternal sty uniq mod occ is_wired is_builtin
  | codeStyle sty = ppr mod <> char '_' <> ppr_z_occ_name occ
        -- In code style, always qualify
        -- ToDo: maybe we could print all wired-in things unqualified
        --       in code style, to reduce symbol table bloat?
  | debugStyle sty = pp_mod <> ppr_occ_name occ
                     <> braces (hsep [if is_wired then text "(w)" else empty,
                                      pprNameSpaceBrief (occNameSpace occ),
                                      pprUnique uniq])
  | BuiltInSyntax <- is_builtin = ppr_occ_name occ  -- Never qualify builtin syntax
  | otherwise                   =
        if isHoleModule mod
            then case qualName sty mod occ of
                    NameUnqual -> ppr_occ_name occ
                    _ -> braces (ppr (moduleName mod) <> dot <> ppr_occ_name occ)
            else pprModulePrefix sty mod occ <> ppr_occ_name occ
  where
    pp_mod = sdocWithDynFlags $ \dflags ->
             if gopt Opt_SuppressModulePrefixes dflags
             then empty
             else ppr mod <> dot

pprInternal :: PprStyle -> Unique -> OccName -> SDoc
pprInternal sty uniq occ
  | codeStyle sty  = pprUniqueAlways uniq
  | debugStyle sty = ppr_occ_name occ <> braces (hsep [pprNameSpaceBrief (occNameSpace occ),
                                                       pprUnique uniq])
  | dumpStyle sty  = ppr_occ_name occ <> ppr_underscore_unique uniq
                        -- For debug dumps, we're not necessarily dumping
                        -- tidied code, so we need to print the uniques.
  | otherwise      = ppr_occ_name occ   -- User style

-- Like Internal, except that we only omit the unique in Iface style
pprSystem :: PprStyle -> Unique -> OccName -> SDoc
pprSystem sty uniq occ
  | codeStyle sty  = pprUniqueAlways uniq
  | debugStyle sty = ppr_occ_name occ <> ppr_underscore_unique uniq
                     <> braces (pprNameSpaceBrief (occNameSpace occ))
  | otherwise      = ppr_occ_name occ <> ppr_underscore_unique uniq
                                -- If the tidy phase hasn't run, the OccName
                                -- is unlikely to be informative (like 's'),
                                -- so print the unique


pprModulePrefix :: PprStyle -> Module -> OccName -> SDoc
-- Print the "M." part of a name, based on whether it's in scope or not
-- See Note [Printing original names] in HscTypes
pprModulePrefix sty mod occ = sdocWithDynFlags $ \dflags ->
  if gopt Opt_SuppressModulePrefixes dflags
  then empty
  else
    case qualName sty mod occ of              -- See Outputable.QualifyName:
      NameQual modname -> ppr modname <> dot       -- Name is in scope
      NameNotInScope1  -> ppr mod <> dot           -- Not in scope
      NameNotInScope2  -> ppr (moduleUnitId mod) <> colon     -- Module not in
                          <> ppr (moduleName mod) <> dot          -- scope either
      NameUnqual       -> empty                   -- In scope unqualified

pprUnique :: Unique -> SDoc
-- Print a unique unless we are suppressing them
pprUnique uniq
  = sdocWithDynFlags $ \dflags ->
    ppUnless (gopt Opt_SuppressUniques dflags) $
    pprUniqueAlways uniq

ppr_underscore_unique :: Unique -> SDoc
-- Print an underscore separating the name from its unique
-- But suppress it if we aren't printing the uniques anyway
ppr_underscore_unique uniq
  = sdocWithDynFlags $ \dflags ->
    ppUnless (gopt Opt_SuppressUniques dflags) $
    char '_' <> pprUniqueAlways uniq

ppr_occ_name :: OccName -> SDoc
ppr_occ_name occ = ftext (occNameFS occ)
        -- Don't use pprOccName; instead, just print the string of the OccName;
        -- we print the namespace in the debug stuff above

-- In code style, we Z-encode the strings.  The results of Z-encoding each FastString are
-- cached behind the scenes in the FastString implementation.
ppr_z_occ_name :: OccName -> SDoc
ppr_z_occ_name occ = ztext (zEncodeFS (occNameFS occ))

-- Prints (if mod information is available) "Defined at <loc>" or
--  "Defined in <mod>" information for a Name.
pprDefinedAt :: Name -> SDoc
pprDefinedAt name = text "Defined" <+> pprNameDefnLoc name

pprNameDefnLoc :: Name -> SDoc
-- Prints "at <loc>" or
--     or "in <mod>" depending on what info is available
pprNameDefnLoc name
  = case nameSrcLoc name of
         -- nameSrcLoc rather than nameSrcSpan
         -- It seems less cluttered to show a location
         -- rather than a span for the definition point
       RealSrcLoc s -> text "at" <+> ppr s
       UnhelpfulLoc s
         | isInternalName name || isSystemName name
         -> text "at" <+> ftext s
         | otherwise
         -> text "in" <+> quotes (ppr (nameModule name))

pprInfixName :: (Outputable a, NamedThing a) => a -> SDoc
-- See Outputable.pprPrefixVar, pprInfixVar;
-- add parens or back-quotes as appropriate
pprInfixName  n = pprInfixVar (isSymOcc (getOccName n)) (ppr n)

pprPrefixName :: NamedThing a => a -> SDoc
pprPrefixName thing = pprPrefixVar (isSymOcc (nameOccName name)) (ppr name)
 where
   name = getName thing

{-
************************************************************************
*                                                                      *
\subsection{Predicates on names}
*                                                                      *
************************************************************************
-}

isHoleName :: Name -> Bool
isHoleName = isHoleModule . nameModule

nameModule :: HasDebugCallStack => Name -> Module
nameModule name =
  nameModule_maybe name `orElse`
  pprPanic "nameModule" (ppr (n_sort name) <+> ppr name)
