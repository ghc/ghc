%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Name]{@Name@: to transmit name info from renamer to typechecker}

\begin{code}
-- |
-- #name_types#
-- GHC uses several kinds of name internally:
--
-- * 'OccName.OccName': see "OccName#name_types"
--
-- * 'RdrName.RdrName': see "RdrName#name_types"
--
-- *  'Name.Name' is the type of names that have had their scoping and binding resolved. They
--   have an 'OccName.OccName' but also a 'Unique.Unique' that disambiguates Names that have
--   the same 'OccName.OccName' and indeed is used for all 'Name.Name' comparison. Names
--   also contain information about where they originated from, see "Name#name_sorts"
--
-- * 'Id.Id': see "Id#name_types"
--
-- * 'Var.Var': see "Var#name_types"
--
-- #name_sorts#
-- Names are one of:
--
--  * External, if they name things declared in other modules. Some external
--    Names are wired in, i.e. they name primitives defined in the compiler itself
--
--  * Internal, if they name things in the module being compiled. Some internal
--    Names are system names, if they are names manufactured by the compiler

module Name (
        -- * The main types
        Name,                                   -- Abstract
        BuiltInSyntax(..),

        -- ** Creating 'Name's
        mkSystemName, mkSystemNameAt,
        mkInternalName, mkClonedInternalName, mkDerivedInternalName,
        mkSystemVarName, mkSysTvName,
        mkFCallName,
        mkExternalName, mkWiredInName,

        -- ** Manipulating and deconstructing 'Name's
        nameUnique, setNameUnique,
        nameOccName, nameModule, nameModule_maybe,
        tidyNameOcc,
        hashName, localiseName,
        mkLocalisedOccName,

        nameSrcLoc, nameSrcSpan, pprNameDefnLoc, pprDefinedAt,

        -- ** Predicates on 'Name's
        isSystemName, isInternalName, isExternalName,
        isTyVarName, isTyConName, isDataConName,
        isValName, isVarName,
        isWiredInName, isBuiltInSyntax,
        wiredInNameTyThing_maybe,
        nameIsLocalOrFrom, stableNameCmp,

        -- * Class 'NamedThing' and overloaded friends
        NamedThing(..),
        getSrcLoc, getSrcSpan, getOccString,

        pprInfixName, pprPrefixName, pprModulePrefix,

        -- Re-export the OccName stuff
        module OccName
    ) where

import {-# SOURCE #-} TypeRep( TyThing )
import {-# SOURCE #-} PrelNames( liftedTypeKindTyConKey )

import OccName
import Module
import SrcLoc
import Unique
import Util
import Maybes
import Binary
import DynFlags
import FastTypes
import FastString
import Outputable

import Data.Data
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Name-datatype]{The @Name@ datatype, and name construction}
%*                                                                      *
%************************************************************************

\begin{code}
-- | A unique, unambigious name for something, containing information about where
-- that thing originated.
data Name = Name {
                n_sort :: NameSort,     -- What sort of name it is
                n_occ  :: !OccName,     -- Its occurrence name
                n_uniq :: FastInt,      -- UNPACK doesn't work, recursive type
--(note later when changing Int# -> FastInt: is that still true about UNPACK?)
                n_loc  :: !SrcSpan      -- Definition site
            }
    deriving Typeable

-- NOTE: we make the n_loc field strict to eliminate some potential
-- (and real!) space leaks, due to the fact that we don't look at
-- the SrcLoc in a Name all that often.

data NameSort
  = External Module

  | WiredIn Module TyThing BuiltInSyntax
        -- A variant of External, for wired-in things

  | Internal            -- A user-defined Id or TyVar
                        -- defined in the module being compiled

  | System              -- A system-defined Id or TyVar.  Typically the
                        -- OccName is very uninformative (like 's')

-- | BuiltInSyntax is for things like @(:)@, @[]@ and tuples,
-- which have special syntactic forms.  They aren't in scope
-- as such.
data BuiltInSyntax = BuiltInSyntax | UserSyntax
\end{code}

Notes about the NameSorts:

1.  Initially, top-level Ids (including locally-defined ones) get External names,
    and all other local Ids get Internal names

2.  In any invocation of GHC, an External Name for "M.x" has one and only one
    unique.  This unique association is ensured via the Name Cache; 
    see Note [The Name Cache] in IfaceEnv.

3.  Things with a External name are given C static labels, so they finally
    appear in the .o file's symbol table.  They appear in the symbol table
    in the form M.n.  If originally-local things have this property they
    must be made @External@ first.

4.  In the tidy-core phase, a External that is not visible to an importer
    is changed to Internal, and a Internal that is visible is changed to External

5.  A System Name differs in the following ways:
        a) has unique attached when printing dumps
        b) unifier eliminates sys tyvars in favour of user provs where possible

    Before anything gets printed in interface files or output code, it's
    fed through a 'tidy' processor, which zaps the OccNames to have
    unique names; and converts all sys-locals to user locals
    If any desugarer sys-locals have survived that far, they get changed to
    "ds1", "ds2", etc.

Built-in syntax => It's a syntactic form, not "in scope" (e.g. [])

Wired-in thing  => The thing (Id, TyCon) is fully known to the compiler,
                   not read from an interface file.
                   E.g. Bool, True, Int, Float, and many others

All built-in syntax is for wired-in things.

\begin{code}
instance HasOccName Name where
  occName = nameOccName

nameUnique              :: Name -> Unique
nameOccName             :: Name -> OccName
nameModule              :: Name -> Module
nameSrcLoc              :: Name -> SrcLoc
nameSrcSpan             :: Name -> SrcSpan

nameUnique  name = mkUniqueGrimily (iBox (n_uniq name))
nameOccName name = n_occ  name
nameSrcLoc  name = srcSpanStart (n_loc name)
nameSrcSpan name = n_loc  name
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Predicates on names}
%*                                                                      *
%************************************************************************

\begin{code}
nameIsLocalOrFrom :: Module -> Name -> Bool
isInternalName    :: Name -> Bool
isExternalName    :: Name -> Bool
isSystemName      :: Name -> Bool
isWiredInName     :: Name -> Bool

isWiredInName (Name {n_sort = WiredIn _ _ _}) = True
isWiredInName _                               = False

wiredInNameTyThing_maybe :: Name -> Maybe TyThing
wiredInNameTyThing_maybe (Name {n_sort = WiredIn _ thing _}) = Just thing
wiredInNameTyThing_maybe _                                   = Nothing

isBuiltInSyntax :: Name -> Bool
isBuiltInSyntax (Name {n_sort = WiredIn _ _ BuiltInSyntax}) = True
isBuiltInSyntax _                                           = False

isExternalName (Name {n_sort = External _})    = True
isExternalName (Name {n_sort = WiredIn _ _ _}) = True
isExternalName _                               = False

isInternalName name = not (isExternalName name)

nameModule name = nameModule_maybe name `orElse` pprPanic "nameModule" (ppr name)
nameModule_maybe :: Name -> Maybe Module
nameModule_maybe (Name { n_sort = External mod})    = Just mod
nameModule_maybe (Name { n_sort = WiredIn mod _ _}) = Just mod
nameModule_maybe _                                  = Nothing

nameIsLocalOrFrom from name
  | isExternalName name = from == nameModule name
  | otherwise           = True

isTyVarName :: Name -> Bool
isTyVarName name = isTvOcc (nameOccName name)

isTyConName :: Name -> Bool
isTyConName name = isTcOcc (nameOccName name)

isDataConName :: Name -> Bool
isDataConName name = isDataOcc (nameOccName name)

isValName :: Name -> Bool
isValName name = isValOcc (nameOccName name)

isVarName :: Name -> Bool
isVarName = isVarOcc . nameOccName

isSystemName (Name {n_sort = System}) = True
isSystemName _                        = False
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Making names}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Create a name which is (for now at least) local to the current module and hence
-- does not need a 'Module' to disambiguate it from other 'Name's
mkInternalName :: Unique -> OccName -> SrcSpan -> Name
mkInternalName uniq occ loc = Name { n_uniq = getKeyFastInt uniq
                                   , n_sort = Internal
                                   , n_occ = occ
                                   , n_loc = loc }
        -- NB: You might worry that after lots of huffing and
        -- puffing we might end up with two local names with distinct
        -- uniques, but the same OccName.  Indeed we can, but that's ok
        --      * the insides of the compiler don't care: they use the Unique
        --      * when printing for -ddump-xxx you can switch on -dppr-debug to get the
        --        uniques if you get confused
        --      * for interface files we tidyCore first, which makes
        --        the OccNames distinct when they need to be

mkClonedInternalName :: Unique -> Name -> Name
mkClonedInternalName uniq (Name { n_occ = occ, n_loc = loc })
  = Name { n_uniq = getKeyFastInt uniq, n_sort = Internal
         , n_occ = occ, n_loc = loc }

mkDerivedInternalName :: (OccName -> OccName) -> Unique -> Name -> Name
mkDerivedInternalName derive_occ uniq (Name { n_occ = occ, n_loc = loc })
  = Name { n_uniq = getKeyFastInt uniq, n_sort = Internal
         , n_occ = derive_occ occ, n_loc = loc }

-- | Create a name which definitely originates in the given module
mkExternalName :: Unique -> Module -> OccName -> SrcSpan -> Name
-- WATCH OUT! External Names should be in the Name Cache
-- (see Note [The Name Cache] in IfaceEnv), so don't just call mkExternalName
-- with some fresh unique without populating the Name Cache
mkExternalName uniq mod occ loc
  = Name { n_uniq = getKeyFastInt uniq, n_sort = External mod,
           n_occ = occ, n_loc = loc }

-- | Create a name which is actually defined by the compiler itself
mkWiredInName :: Module -> OccName -> Unique -> TyThing -> BuiltInSyntax -> Name
mkWiredInName mod occ uniq thing built_in
  = Name { n_uniq = getKeyFastInt uniq,
           n_sort = WiredIn mod thing built_in,
           n_occ = occ, n_loc = wiredInSrcSpan }

-- | Create a name brought into being by the compiler
mkSystemName :: Unique -> OccName -> Name
mkSystemName uniq occ = mkSystemNameAt uniq occ noSrcSpan

mkSystemNameAt :: Unique -> OccName -> SrcSpan -> Name
mkSystemNameAt uniq occ loc = Name { n_uniq = getKeyFastInt uniq, n_sort = System
                                   , n_occ = occ, n_loc = loc }

mkSystemVarName :: Unique -> FastString -> Name
mkSystemVarName uniq fs = mkSystemName uniq (mkVarOccFS fs)

mkSysTvName :: Unique -> FastString -> Name
mkSysTvName uniq fs = mkSystemName uniq (mkOccNameFS tvName fs)

-- | Make a name for a foreign call
mkFCallName :: Unique -> String -> Name
mkFCallName uniq str = mkInternalName uniq (mkVarOcc str) noSrcSpan
   -- The encoded string completely describes the ccall
\end{code}

\begin{code}
-- When we renumber/rename things, we need to be
-- able to change a Name's Unique to match the cached
-- one in the thing it's the name of.  If you know what I mean.
setNameUnique :: Name -> Unique -> Name
setNameUnique name uniq = name {n_uniq = getKeyFastInt uniq}

tidyNameOcc :: Name -> OccName -> Name
-- We set the OccName of a Name when tidying
-- In doing so, we change System --> Internal, so that when we print
-- it we don't get the unique by default.  It's tidy now!
tidyNameOcc name@(Name { n_sort = System }) occ = name { n_occ = occ, n_sort = Internal}
tidyNameOcc name                            occ = name { n_occ = occ }

-- | Make the 'Name' into an internal name, regardless of what it was to begin with
localiseName :: Name -> Name
localiseName n = n { n_sort = Internal }
\end{code}

\begin{code}
-- |Create a localised variant of a name.
--
-- If the name is external, encode the original's module name to disambiguate.
--
mkLocalisedOccName :: Module -> (Maybe String -> OccName -> OccName) -> Name -> OccName
mkLocalisedOccName this_mod mk_occ name = mk_occ origin (nameOccName name)
  where
    origin
      | nameIsLocalOrFrom this_mod name = Nothing
      | otherwise                       = Just (moduleNameColons . moduleName . nameModule $ name)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Hashing and comparison}
%*                                                                      *
%************************************************************************

\begin{code}
hashName :: Name -> Int         -- ToDo: should really be Word
hashName name = getKey (nameUnique name) + 1
        -- The +1 avoids keys with lots of zeros in the ls bits, which
        -- interacts badly with the cheap and cheerful multiplication in
        -- hashExpr

cmpName :: Name -> Name -> Ordering
cmpName n1 n2 = iBox (n_uniq n1) `compare` iBox (n_uniq n2)

stableNameCmp :: Name -> Name -> Ordering
-- Compare lexicographically
stableNameCmp (Name { n_sort = s1, n_occ = occ1 })
              (Name { n_sort = s2, n_occ = occ2 })
  = (s1 `sort_cmp` s2) `thenCmp` (occ1 `compare` occ2)
    -- The ordinary compare on OccNames is lexicogrpahic
  where
    -- Later constructors are bigger
    sort_cmp (External m1) (External m2)       = m1 `stableModuleCmp` m2
    sort_cmp (External {}) _                   = LT
    sort_cmp (WiredIn {}) (External {})        = GT
    sort_cmp (WiredIn m1 _ _) (WiredIn m2 _ _) = m1 `stableModuleCmp` m2
    sort_cmp (WiredIn {})     _                = LT
    sort_cmp Internal         (External {})    = GT
    sort_cmp Internal         (WiredIn {})     = GT
    sort_cmp Internal         Internal         = EQ
    sort_cmp Internal         System           = LT
    sort_cmp System           System           = EQ
    sort_cmp System           _                = GT
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Name-instances]{Instance declarations}
%*                                                                      *
%************************************************************************

\begin{code}
instance Eq Name where
    a == b = case (a `compare` b) of { EQ -> True;  _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False; _ -> True }

instance Ord Name where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <  b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >  b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = cmpName a b

instance Uniquable Name where
    getUnique = nameUnique

instance NamedThing Name where
    getName n = n

instance Data Name where
  -- don't traverse?
  toConstr _   = abstractConstr "Name"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "Name"
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Binary}
%*                                                                      *
%************************************************************************

\begin{code}
instance Binary Name where
   put_ bh name =
      case getUserData bh of
        UserData{ ud_put_name = put_name } -> put_name bh name

   get bh =
      case getUserData bh of
        UserData { ud_get_name = get_name } -> get_name bh
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Pretty printing}
%*                                                                      *
%************************************************************************

\begin{code}
instance Outputable Name where
    ppr name = pprName name

instance OutputableBndr Name where
    pprBndr _ name = pprName name
    pprInfixOcc  = pprInfixName
    pprPrefixOcc = pprPrefixName


pprName :: Name -> SDoc
pprName n@(Name {n_sort = sort, n_uniq = u, n_occ = occ})
  = getPprStyle $ \ sty ->
    case sort of
      WiredIn mod _ builtin   -> pprExternal sty uniq mod occ n True  builtin
      External mod            -> pprExternal sty uniq mod occ n False UserSyntax
      System                  -> pprSystem sty uniq occ
      Internal                -> pprInternal sty uniq occ
  where uniq = mkUniqueGrimily (iBox u)

pprExternal :: PprStyle -> Unique -> Module -> OccName -> Name -> Bool -> BuiltInSyntax -> SDoc
pprExternal sty uniq mod occ name is_wired is_builtin
  | codeStyle sty = ppr mod <> char '_' <> ppr_z_occ_name occ
        -- In code style, always qualify
        -- ToDo: maybe we could print all wired-in things unqualified
        --       in code style, to reduce symbol table bloat?
  | debugStyle sty = pp_mod <> ppr_occ_name occ
                     <> braces (hsep [if is_wired then ptext (sLit "(w)") else empty,
                                      pprNameSpaceBrief (occNameSpace occ),
                                      pprUnique uniq])
  | BuiltInSyntax <- is_builtin = ppr_occ_name occ  -- Never qualify builtin syntax
  | otherwise                   = pprModulePrefix sty mod name <> ppr_occ_name occ
  where
    pp_mod = sdocWithDynFlags $ \dflags ->
             if gopt Opt_SuppressModulePrefixes dflags
             then empty
             else ppr mod <> dot

pprInternal :: PprStyle -> Unique -> OccName -> SDoc
pprInternal sty uniq occ
  | codeStyle sty  = pprUnique uniq
  | debugStyle sty = ppr_occ_name occ <> braces (hsep [pprNameSpaceBrief (occNameSpace occ),
                                                       pprUnique uniq])
  | dumpStyle sty  = ppr_occ_name occ <> ppr_underscore_unique uniq
                        -- For debug dumps, we're not necessarily dumping
                        -- tidied code, so we need to print the uniques.
  | otherwise      = ppr_occ_name occ   -- User style

-- Like Internal, except that we only omit the unique in Iface style
pprSystem :: PprStyle -> Unique -> OccName -> SDoc
pprSystem sty uniq occ
  | codeStyle sty  = pprUnique uniq
  | debugStyle sty = ppr_occ_name occ <> ppr_underscore_unique uniq
                     <> braces (pprNameSpaceBrief (occNameSpace occ))
  | otherwise      = ppr_occ_name occ <> ppr_underscore_unique uniq
                                -- If the tidy phase hasn't run, the OccName
                                -- is unlikely to be informative (like 's'),
                                -- so print the unique


pprModulePrefix :: PprStyle -> Module -> Name -> SDoc
-- Print the "M." part of a name, based on whether it's in scope or not
-- See Note [Printing original names] in HscTypes
pprModulePrefix sty mod name = sdocWithDynFlags $ \dflags ->
  if gopt Opt_SuppressModulePrefixes dflags
  then empty
  else
    case qualName sty name of              -- See Outputable.QualifyName:
      NameQual modname -> ppr modname <> dot       -- Name is in scope
      NameNotInScope1  -> ppr mod <> dot           -- Not in scope
      NameNotInScope2  -> ppr (modulePackageId mod) <> colon     -- Module not in
                          <> ppr (moduleName mod) <> dot         -- scope either
      _otherwise       -> empty

ppr_underscore_unique :: Unique -> SDoc
-- Print an underscore separating the name from its unique
-- But suppress it if we aren't printing the uniques anyway
ppr_underscore_unique uniq
  = sdocWithDynFlags $ \dflags ->
    if gopt Opt_SuppressUniques dflags
    then empty
    else char '_' <> pprUnique uniq

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
pprDefinedAt name = ptext (sLit "Defined") <+> pprNameDefnLoc name

pprNameDefnLoc :: Name -> SDoc
-- Prints "at <loc>" or
--     or "in <mod>" depending on what info is available
pprNameDefnLoc name
  = case nameSrcLoc name of
         -- nameSrcLoc rather than nameSrcSpan
         -- It seems less cluttered to show a location
         -- rather than a span for the definition point
       RealSrcLoc s -> ptext (sLit "at") <+> ppr s
       UnhelpfulLoc s
         | isInternalName name || isSystemName name
         -> ptext (sLit "at") <+> ftext s
         | otherwise
         -> ptext (sLit "in") <+> quotes (ppr (nameModule name))
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Overloaded functions related to Names}
%*                                                                      *
%************************************************************************

\begin{code}
-- | A class allowing convenient access to the 'Name' of various datatypes
class NamedThing a where
    getOccName :: a -> OccName
    getName    :: a -> Name

    getOccName n = nameOccName (getName n)      -- Default method
\end{code}

\begin{code}
getSrcLoc           :: NamedThing a => a -> SrcLoc
getSrcSpan          :: NamedThing a => a -> SrcSpan
getOccString        :: NamedThing a => a -> String

getSrcLoc           = nameSrcLoc           . getName
getSrcSpan          = nameSrcSpan          . getName
getOccString        = occNameString        . getOccName

pprInfixName, pprPrefixName :: (Outputable a, NamedThing a) => a -> SDoc
-- See Outputable.pprPrefixVar, pprInfixVar;
-- add parens or back-quotes as appropriate
pprInfixName  n = pprInfixVar (isSymOcc (getOccName n)) (ppr n)

pprPrefixName thing 
 |  name `hasKey` liftedTypeKindTyConKey 
 = ppr name   -- See Note [Special treatment for kind *]
 | otherwise
 = pprPrefixVar (isSymOcc (nameOccName name)) (ppr name)
 where
   name = getName thing
\end{code}

Note [Special treatment for kind *]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Do not put parens around the kind '*'.  Even though it looks like
an operator, it is really a special case.

This pprPrefixName stuff is really only used when printing HsSyn,
which has to be polymorphic in the name type, and hence has to go via
the overloaded function pprPrefixOcc.  It's easier where we know the
type being pretty printed; eg the pretty-printing code in TypeRep.

See Trac #7645, which led to this.

