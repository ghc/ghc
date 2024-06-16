{-
This module contains code which maintains and manipulates the
fixity environment during renaming.
-}

module GHC.Rename.Fixity
   ( MiniFixityEnv(..)
   , addLocalFixities
   , lookupMiniFixityEnv
   , emptyMiniFixityEnv
   , lookupFixityRn
   , lookupFixityRn_help
   , lookupFieldFixityRn
   , lookupTyFixityRn
   ) where

import GHC.Prelude

import GHC.Iface.Load
import GHC.Hs
import GHC.Tc.Utils.Monad

import GHC.Unit.Module
import GHC.Unit.Module.ModIface

import GHC.Types.Fixity.Env
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Fixity
import GHC.Types.SrcLoc

import GHC.Utils.Outputable

import GHC.Data.Maybe

import GHC.Rename.Unbound

{-
*********************************************************
*                                                      *
                Fixities
*                                                      *
*********************************************************

Note [Fixity signature lookup]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A fixity declaration like

    infixr 2 ?

can refer to a value-level operator, e.g.:

    (?) :: String -> String -> String

or a type-level operator, like:

    data (?) a b = A a | B b

so we extend the lookup of the reader name '?' to the TcClsName namespace, as
well as the original namespace.

The extended lookup is also used in other places, like resolution of
deprecation declarations, and lookup of names in GHCi.
-}

--------------------------------

-- | Mini fixity env for the names we're about
-- to bind, in a single binding group
--
-- It is keyed by the *FastString*, not the *OccName*, because
-- the single fixity decl       @infix 3 T@
-- affects both the data constructor T and the type constructor T
--
-- We keep the location so that if we find
-- a duplicate, we can report it sensibly
--
-- Fixity declarations may influence names in a single namespace by using
-- a type or data specifier, e.g. in:
--
-- >  data a :*: b = a :*: b
-- >  infix 3 type :*:
--
-- To handle that correctly, MiniFixityEnv contains separate
-- fields for type-level and data-level names.
-- If no namespace specifier is provided, the declaration will
-- populate both the type-level and data-level fields.
data MiniFixityEnv = MFE
  { mfe_data_level_names :: FastStringEnv (Located Fixity)
  , mfe_type_level_names :: FastStringEnv (Located Fixity)
  }

--------------------------------
-- Used for nested fixity decls to bind names along with their fixities.
-- the fixities are given as a UFM from an OccName's FastString to a fixity decl

addLocalFixities :: MiniFixityEnv -> [Name] -> RnM a -> RnM a
addLocalFixities env names thing_inside
  = extendFixityEnv (mapMaybe find_fixity names) thing_inside
  where
    find_fixity name = case lookupMiniFixityEnv env name of
          Just lfix -> Just (name, FixItem occ (unLoc lfix))
          Nothing   -> Nothing
      where
        occ = nameOccName name

lookupMiniFixityEnv :: MiniFixityEnv -> Name -> Maybe (Located Fixity)
lookupMiniFixityEnv MFE{mfe_data_level_names, mfe_type_level_names} name
  | isValNameSpace namespace = find_fixity_in_env mfe_data_level_names name
  | otherwise                = find_fixity_in_env mfe_type_level_names name
  where
    namespace = nameNameSpace name

    find_fixity_in_env mini_fix_env name
      = lookupFsEnv mini_fix_env (occNameFS occ)
      where
        occ = nameOccName name

emptyMiniFixityEnv :: MiniFixityEnv
emptyMiniFixityEnv = MFE emptyFsEnv emptyFsEnv

{-
--------------------------------
lookupFixity is a bit strange.

* Nested local fixity decls are put in the local fixity env, which we
  find with getFixtyEnv

* Imported fixities are found in the PIT

* Top-level fixity decls in this module may be for Names that are
    either  Global         (constructors, class operations)
    or      Local/Exported (everything else)
  (See notes with GHC.Rename.Names.getLocalDeclBinders for why we have this split.)
  We put them all in the local fixity environment
-}

lookupFixityRn :: Name -> RnM Fixity
lookupFixityRn = fmap snd . lookupFixityRn_help

-- | 'lookupFixityRn_help' returns @(True, fixity)@ if it finds a 'Fixity'
-- in a local environment or from an interface file. Otherwise, it returns
-- @(False, fixity)@ (e.g., for unbound 'Name's or 'Name's without
-- user-supplied fixity declarations).
lookupFixityRn_help :: Name
                    -> RnM (Bool, Fixity)
lookupFixityRn_help name
  | isUnboundName name
  = return (False, Fixity minPrecedence InfixL)
    -- Minimise errors from unbound names; eg
    --    a>0 `foo` b>0
    -- where 'foo' is not in scope, should not give an error (#7937)

  | otherwise
  = do { local_fix_env <- getFixityEnv
       ; case lookupNameEnv local_fix_env name of {
           Just (FixItem _ fix) -> return (True, fix) ;
           Nothing ->

    do { this_mod <- getModule
       ; if nameIsLocalOrFrom this_mod name
               -- Local (and interactive) names are all in the
               -- fixity env, and don't have entries in the HPT
         then return (False, defaultFixity)
         else lookup_imported } } }
  where
    occ = nameOccName name
    lookup_imported
      -- For imported names, we have to get their fixities by doing a
      -- loadInterfaceForName, and consulting the Ifaces that comes back
      -- from that, because the interface file for the Name might not
      -- have been loaded yet.  Why not?  Suppose you import module A,
      -- which exports a function 'f', thus;
      --        module CurrentModule where
      --          import A( f )
      --        module A( f ) where
      --          import B( f )
      -- Then B isn't loaded right away (after all, it's possible that
      -- nothing from B will be used).  When we come across a use of
      -- 'f', we need to know its fixity, and it's then, and only
      -- then, that we load B.hi.  That is what's happening here.
      --
      -- loadInterfaceForName will find B.hi even if B is a hidden module,
      -- and that's what we want.
      = do { iface <- loadInterfaceForName doc name
           ; let mb_fix = mi_fix_fn (mi_final_exts iface) occ
           ; let msg = case mb_fix of
                            Nothing ->
                                  text "looking up name" <+> ppr name
                              <+> text "in iface, but found no fixity for it."
                              <+> text "Using default fixity instead."
                            Just f ->
                                  text "looking up name in iface and found:"
                              <+> vcat [ppr name, ppr f]
           ; traceRn "lookupFixityRn_either:" msg
           ; return (maybe (False, defaultFixity) (\f -> (True, f)) mb_fix)  }

    doc = text "Checking fixity for" <+> ppr name

---------------
lookupTyFixityRn :: LocatedN Name -> RnM Fixity
lookupTyFixityRn = lookupFixityRn . unLoc

lookupFieldFixityRn :: FieldOcc GhcRn -> RnM Fixity
lookupFieldFixityRn (FieldOcc n _) = lookupFixityRn n
