

module GHC.Types.Name.Ppr
   ( mkPrintUnqualified
   , mkQualModule
   , mkQualPackage
   , pkgQual
   )
where

import GHC.Prelude

import GHC.Unit
import GHC.Unit.Env

import GHC.Types.Name
import GHC.Types.Name.Reader

import GHC.Builtin.Types

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Builtin.Types.Prim (tYPETyConName, funTyConName)


{-
Note [Printing original names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Deciding how to print names is pretty tricky.  We are given a name
P:M.T, where P is the package name, M is the defining module, and T is
the occurrence name, and we have to decide in which form to display
the name given a GlobalRdrEnv describing the current scope.

Ideally we want to display the name in the form in which it is in
scope.  However, the name might not be in scope at all, and that's
where it gets tricky.  Here are the cases:

 1. T uniquely maps to  P:M.T      --->  "T"      NameUnqual
 2. There is an X for which X.T
       uniquely maps to  P:M.T     --->  "X.T"    NameQual X
 3. There is no binding for "M.T"  --->  "M.T"    NameNotInScope1
 4. Otherwise                      --->  "P:M.T"  NameNotInScope2

(3) and (4) apply when the entity P:M.T is not in the GlobalRdrEnv at
all. In these cases we still want to refer to the name as "M.T", *but*
"M.T" might mean something else in the current scope (e.g. if there's
an "import X as M"), so to avoid confusion we avoid using "M.T" if
there's already a binding for it.  Instead we write P:M.T.

There's one further subtlety: in case (3), what if there are two
things around, P1:M.T and P2:M.T?  Then we don't want to print both of
them as M.T!  However only one of the modules P1:M and P2:M can be
exposed (say P2), so we use M.T for that, and P1:M.T for the other one.
This is handled by the qual_mod component of PrintUnqualified, inside
the (ppr mod) of case (3), in Name.pprModulePrefix

Note [Printing unit ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the old days, original names were tied to PackageIds, which directly
corresponded to the entities that users wrote in Cabal files, and were perfectly
suitable for printing when we need to disambiguate packages.  However, with
instantiated units, the situation can be different: if the key is instantiated
with some holes, we should try to give the user some more useful information.
-}

-- | Creates some functions that work out the best ways to format
-- names for the user according to a set of heuristics.
mkPrintUnqualified :: UnitEnv -> GlobalRdrEnv -> PrintUnqualified
mkPrintUnqualified unit_env env
 = QueryQualify qual_name
      (mkQualModule unit_state home_unit)
      (mkQualPackage unit_state)
  where
  unit_state = ue_units unit_env
  home_unit  = ue_homeUnit unit_env
  qual_name mod occ
        | [gre] <- unqual_gres
        , right_name gre
        = NameUnqual   -- If there's a unique entity that's in scope
                       -- unqualified with 'occ' AND that entity is
                       -- the right one, then we can use the unqualified name

        | [] <- unqual_gres
        , pretendNameIsInScopeForPpr
        , not (isDerivedOccName occ)
        = NameUnqual   -- See Note [pretendNameIsInScopeForPpr]

        | [gre] <- qual_gres
        = NameQual (greQualModName gre)

        | null qual_gres
        = if null (lookupGRE_RdrName (mkRdrQual (moduleName mod) occ) env)
          then NameNotInScope1
          else NameNotInScope2

        | otherwise
        = NameNotInScope1   -- Can happen if 'f' is bound twice in the module
                            -- Eg  f = True; g = 0; f = False
      where
        is_name :: Name -> Bool
        is_name name = assertPpr (isExternalName name) (ppr name) $
                       nameModule name == mod && nameOccName name == occ

        -- See Note [pretendNameIsInScopeForPpr]
        pretendNameIsInScopeForPpr :: Bool
        pretendNameIsInScopeForPpr =
          any is_name
            [ liftedTypeKindTyConName
            , constraintKindTyConName
            , heqTyConName
            , coercibleTyConName
            , eqTyConName
            , tYPETyConName
            , funTyConName
            , oneDataConName
            , manyDataConName ]

        right_name gre = greDefinitionModule gre == Just mod

        unqual_gres = lookupGRE_RdrName (mkRdrUnqual occ) env
        qual_gres   = filter right_name (lookupGlobalRdrEnv env occ)

    -- we can mention a module P:M without the P: qualifier iff
    -- "import M" would resolve unambiguously to P:M.  (if P is the
    -- current package we can just assume it is unqualified).

{- Note [pretendNameIsInScopeForPpr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Normally, a name is printed unqualified if it's in scope and unambiguous:
  ghci> :t not
  not :: Bool -> Bool

Out of scope names are qualified:
  ghci> import Prelude hiding (Bool)
  ghci> :t not
  not :: GHC.Types.Bool -> GHC.Types.Bool

And so are ambiguous names:
  ghci> data Bool
  ghci> :t not
  not :: Prelude.Bool -> Prelude.Bool

However, these rules alone would lead to excessive qualification:
  ghci> :k Functor
  Functor :: (GHC.Types.Type -> GHC.Types.Type) -> GHC.Types.Constraint

Even if the user has not imported Data.Kind, we would rather print:
  Functor :: (Type -> Type) -> Constraint

So we maintain a list of names for which we only require that they are
unambiguous. It reduces the amount of qualification in GHCi output and error
messages thus improving readability.

One potential problem here is that external tooling that relies on parsing GHCi
output (e.g. Emacs mode for Haskell) requires names to be properly qualified to
make sense of the output (see #11208). So extend this list with care.

Side note (int-index):
  This function is distinct from GHC.Bulitin.Names.pretendNameIsInScope (used
  when filtering out instances), and perhaps we could unify them by taking a
  union, but I have not looked into what that would entail.
-}

-- | Creates a function for formatting modules based on two heuristics:
-- (1) if the module is the current module, don't qualify, and (2) if there
-- is only one exposed package which exports this module, don't qualify.
mkQualModule :: UnitState -> Maybe HomeUnit -> QueryQualifyModule
mkQualModule unit_state mhome_unit mod
     | Just home_unit <- mhome_unit
     , isHomeModule home_unit mod = False

     | [(_, pkgconfig)] <- lookup,
       mkUnit pkgconfig == moduleUnit mod
        -- this says: we are given a module P:M, is there just one exposed package
        -- that exposes a module M, and is it package P?
     = False

     | otherwise = True
     where lookup = lookupModuleInAllUnits unit_state (moduleName mod)

-- | Creates a function for formatting packages based on two heuristics:
-- (1) don't qualify if the package in question is "main", and (2) only qualify
-- with a unit id if the package ID would be ambiguous.
mkQualPackage :: UnitState -> QueryQualifyPackage
mkQualPackage pkgs uid
     | uid == mainUnit || uid == interactiveUnit
        -- Skip the lookup if it's main, since it won't be in the package
        -- database!
     = False
     | Just pkgid <- mb_pkgid
     , searchPackageId pkgs pkgid `lengthIs` 1
        -- this says: we are given a package pkg-0.1@MMM, are there only one
        -- exposed packages whose package ID is pkg-0.1?
     = False
     | otherwise
     = True
     where mb_pkgid = fmap unitPackageId (lookupUnit pkgs uid)

-- | A function which only qualifies package names if necessary; but
-- qualifies all other identifiers.
pkgQual :: UnitState -> PrintUnqualified
pkgQual pkgs = alwaysQualify { queryQualifyPackage = mkQualPackage pkgs }
