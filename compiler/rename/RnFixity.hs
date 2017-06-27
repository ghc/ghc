{-

This module contains code which maintains and manipulates the
fixity environment during renaming.

-}
module RnFixity ( MiniFixityEnv,
                  addLocalFixities,
  lookupFixityRn, lookupFixityRn_help,
  lookupFieldFixityRn, lookupTyFixityRn ) where

import LoadIface
import HsSyn
import RdrName
import HscTypes
import TcRnMonad
import Name
import NameEnv
import Module
import BasicTypes       ( Fixity(..), FixityDirection(..), minPrecedence,
                          defaultFixity, SourceText(..) )
import SrcLoc
import Outputable
import Maybes
import Data.List
import Data.Function    ( on )
import RnUnbound

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
type MiniFixityEnv = FastStringEnv (Located Fixity)
        -- Mini fixity env for the names we're about
        -- to bind, in a single binding group
        --
        -- It is keyed by the *FastString*, not the *OccName*, because
        -- the single fixity decl       infix 3 T
        -- affects both the data constructor T and the type constrctor T
        --
        -- We keep the location so that if we find
        -- a duplicate, we can report it sensibly

--------------------------------
-- Used for nested fixity decls to bind names along with their fixities.
-- the fixities are given as a UFM from an OccName's FastString to a fixity decl

addLocalFixities :: MiniFixityEnv -> [Name] -> RnM a -> RnM a
addLocalFixities mini_fix_env names thing_inside
  = extendFixityEnv (mapMaybe find_fixity names) thing_inside
  where
    find_fixity name
      = case lookupFsEnv mini_fix_env (occNameFS occ) of
          Just (L _ fix) -> Just (name, FixItem occ fix)
          Nothing        -> Nothing
      where
        occ = nameOccName name

{-
--------------------------------
lookupFixity is a bit strange.

* Nested local fixity decls are put in the local fixity env, which we
  find with getFixtyEnv

* Imported fixities are found in the PIT

* Top-level fixity decls in this module may be for Names that are
    either  Global         (constructors, class operations)
    or      Local/Exported (everything else)
  (See notes with RnNames.getLocalDeclBinders for why we have this split.)
  We put them all in the local fixity environment
-}

lookupFixityRn :: Name -> RnM Fixity
lookupFixityRn name = lookupFixityRn' name (nameOccName name)

lookupFixityRn' :: Name -> OccName -> RnM Fixity
lookupFixityRn' name = fmap snd . lookupFixityRn_help' name

-- | 'lookupFixityRn_help' returns @(True, fixity)@ if it finds a 'Fixity'
-- in a local environment or from an interface file. Otherwise, it returns
-- @(False, fixity)@ (e.g., for unbound 'Name's or 'Name's without
-- user-supplied fixity declarations).
lookupFixityRn_help :: Name
                    -> RnM (Bool, Fixity)
lookupFixityRn_help name =
    lookupFixityRn_help' name (nameOccName name)

lookupFixityRn_help' :: Name
                     -> OccName
                     -> RnM (Bool, Fixity)
lookupFixityRn_help' name occ
  | isUnboundName name
  = return (False, Fixity NoSourceText minPrecedence InfixL)
    -- Minimise errors from ubound names; eg
    --    a>0 `foo` b>0
    -- where 'foo' is not in scope, should not give an error (Trac #7937)

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
           ; let mb_fix = mi_fix_fn iface occ
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
lookupTyFixityRn :: Located Name -> RnM Fixity
lookupTyFixityRn (L _ n) = lookupFixityRn n

-- | Look up the fixity of a (possibly ambiguous) occurrence of a record field
-- selector.  We use 'lookupFixityRn'' so that we can specifiy the 'OccName' as
-- the field label, which might be different to the 'OccName' of the selector
-- 'Name' if @DuplicateRecordFields@ is in use (Trac #1173). If there are
-- multiple possible selectors with different fixities, generate an error.
lookupFieldFixityRn :: AmbiguousFieldOcc GhcRn -> RnM Fixity
lookupFieldFixityRn (Unambiguous (L _ rdr) n)
  = lookupFixityRn' n (rdrNameOcc rdr)
lookupFieldFixityRn (Ambiguous   (L _ rdr) _) = get_ambiguous_fixity rdr
  where
    get_ambiguous_fixity :: RdrName -> RnM Fixity
    get_ambiguous_fixity rdr_name = do
      traceRn "get_ambiguous_fixity" (ppr rdr_name)
      rdr_env <- getGlobalRdrEnv
      let elts =  lookupGRE_RdrName rdr_name rdr_env

      fixities <- groupBy ((==) `on` snd) . zip elts
                  <$> mapM lookup_gre_fixity elts

      case fixities of
        -- There should always be at least one fixity.
        -- Something's very wrong if there are no fixity candidates, so panic
        [] -> panic "get_ambiguous_fixity: no candidates for a given RdrName"
        [ (_, fix):_ ] -> return fix
        ambigs -> addErr (ambiguous_fixity_err rdr_name ambigs)
                  >> return (Fixity NoSourceText minPrecedence InfixL)

    lookup_gre_fixity gre = lookupFixityRn' (gre_name gre) (greOccName gre)

    ambiguous_fixity_err rn ambigs
      = vcat [ text "Ambiguous fixity for record field" <+> quotes (ppr rn)
             , hang (text "Conflicts: ") 2 . vcat .
               map format_ambig $ concat ambigs ]

    format_ambig (elt, fix) = hang (ppr fix)
                                 2 (pprNameProvenance elt)
