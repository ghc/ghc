{-# LANGUAGE CPP #-}

module GHC.Types.Name.Shape
   ( NameShape(..)
   , emptyNameShape
   , mkNameShape
   , extendNameShape
   , nameShapeExports
   , substNameShape
   , maybeSubstNameShape
   )
where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Driver.Env

import GHC.Unit.Module

import GHC.Types.Unique.FM
import GHC.Types.Avail
import GHC.Types.FieldLabel
import GHC.Types.Name
import GHC.Types.Name.Env

import GHC.Tc.Utils.Monad
import GHC.Iface.Env

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic

import Control.Monad

-- Note [NameShape]
-- ~~~~~~~~~~~~~~~~
-- When we write a declaration in a signature, e.g., data T, we
-- ascribe to it a *name variable*, e.g., {m.T}.  This
-- name variable may be substituted with an actual original
-- name when the signature is implemented (or even if we
-- merge the signature with one which reexports this entity
-- from another module).

-- When we instantiate a signature m with a module M,
-- we also need to substitute over names.  To do so, we must
-- compute the *name substitution* induced by the *exports*
-- of the module in question.  A NameShape represents
-- such a name substitution for a single module instantiation.
-- The "shape" in the name comes from the fact that the computation
-- of a name substitution is essentially the *shaping pass* from
-- Backpack'14, but in a far more restricted form.

-- The name substitution for an export list is easy to explain.  If we are
-- filling the module variable <m>, given an export N of the form
-- M.n or {m'.n} (where n is an OccName), the induced name
-- substitution is from {m.n} to N.  So, for example, if we have
-- A=impl:B, and the exports of impl:B are impl:B.f and
-- impl:C.g, then our name substitution is {A.f} to impl:B.f
-- and {A.g} to impl:C.g




-- The 'NameShape' type is defined in GHC.Tc.Types, because GHC.Tc.Types
-- needs to refer to NameShape, and having GHC.Tc.Types import
-- NameShape (even by SOURCE) would cause a large number of
-- modules to be pulled into the DynFlags cycle.
{-
data NameShape = NameShape {
        ns_mod_name :: ModuleName,
        ns_exports :: [AvailInfo],
        ns_map :: OccEnv Name
    }
-}

-- NB: substitution functions need 'HscEnv' since they need the name cache
-- to allocate new names if we change the 'Module' of a 'Name'

-- | Create an empty 'NameShape' (i.e., the renaming that
-- would occur with an implementing module with no exports)
-- for a specific hole @mod_name@.
emptyNameShape :: ModuleName -> NameShape
emptyNameShape mod_name = NameShape mod_name [] emptyOccEnv

-- | Create a 'NameShape' corresponding to an implementing
-- module for the hole @mod_name@ that exports a list of 'AvailInfo's.
mkNameShape :: ModuleName -> [AvailInfo] -> NameShape
mkNameShape mod_name as =
    NameShape mod_name as $ mkOccEnv $ do
        a <- as
        n <- availName a : availNamesWithSelectors a
        return (occName n, n)

-- | Given an existing 'NameShape', merge it with a list of 'AvailInfo's
-- with Backpack style mix-in linking.  This is used solely when merging
-- signatures together: we successively merge the exports of each
-- signature until we have the final, full exports of the merged signature.
--
-- What makes this operation nontrivial is what we are supposed to do when
-- we want to merge in an export for M.T when we already have an existing
-- export {H.T}.  What should happen in this case is that {H.T} should be
-- unified with @M.T@: we've determined a more *precise* identity for the
-- export at 'OccName' @T@.
--
-- Note that we don't do unrestricted unification: only name holes from
-- @ns_mod_name ns@ are flexible.  This is because we have a much more
-- restricted notion of shaping than in Backpack'14: we do shaping
-- *as* we do type-checking.  Thus, once we shape a signature, its
-- exports are *final* and we're not allowed to refine them further,
extendNameShape :: HscEnv -> NameShape -> [AvailInfo] -> IO (Either SDoc NameShape)
extendNameShape hsc_env ns as =
    case uAvailInfos (ns_mod_name ns) (ns_exports ns) as of
        Left err -> return (Left err)
        Right nsubst -> do
            as1 <- mapM (liftIO . substNameAvailInfo hsc_env nsubst) (ns_exports ns)
            as2 <- mapM (liftIO . substNameAvailInfo hsc_env nsubst) as
            let new_avails = mergeAvails as1 as2
            return . Right $ ns {
                ns_exports = new_avails,
                -- TODO: stop repeatedly rebuilding the OccEnv
                ns_map = mkOccEnv $ do
                            a <- new_avails
                            n <- availName a : availNames a
                            return (occName n, n)
                }

-- | The export list associated with this 'NameShape' (i.e., what
-- the exports of an implementing module which induces this 'NameShape'
-- would be.)
nameShapeExports :: NameShape -> [AvailInfo]
nameShapeExports = ns_exports

-- | Given a 'Name', substitute it according to the 'NameShape' implied
-- substitution, i.e. map @{A.T}@ to @M.T@, if the implementing module
-- exports @M.T@.
substNameShape :: NameShape -> Name -> Name
substNameShape ns n | nameModule n == ns_module ns
                    , Just n' <- lookupOccEnv (ns_map ns) (occName n)
                    = n'
                    | otherwise
                    = n

-- | Like 'substNameShape', but returns @Nothing@ if no substitution
-- works.
maybeSubstNameShape :: NameShape -> Name -> Maybe Name
maybeSubstNameShape ns n
    | nameModule n == ns_module ns
    = lookupOccEnv (ns_map ns) (occName n)
    | otherwise
    = Nothing

-- | The 'Module' of any 'Name's a 'NameShape' has action over.
ns_module :: NameShape -> Module
ns_module = mkHoleModule . ns_mod_name

{-
************************************************************************
*                                                                      *
                        Name substitutions
*                                                                      *
************************************************************************
-}

-- | Substitution on @{A.T}@.  We enforce the invariant that the
-- 'nameModule' of keys of this map have 'moduleUnit' @hole@
-- (meaning that if we have a hole substitution, the keys of the map
-- are never affected.)  Alternatively, this is isomorphic to
-- @Map ('ModuleName', 'OccName') 'Name'@.
type ShNameSubst = NameEnv Name

-- NB: In this module, we actually only ever construct 'ShNameSubst'
-- at a single 'ModuleName'.  But 'ShNameSubst' is more convenient to
-- work with.

-- | Substitute names in a 'Name'.
substName :: ShNameSubst -> Name -> Name
substName env n | Just n' <- lookupNameEnv env n = n'
                | otherwise                      = n

-- | Substitute names in an 'AvailInfo'.  This has special behavior
-- for type constructors, where it is sufficient to substitute the 'availName'
-- to induce a substitution on 'availNames'.
substNameAvailInfo :: HscEnv -> ShNameSubst -> AvailInfo -> IO AvailInfo
substNameAvailInfo _ env (Avail (NormalGreName n)) = return (Avail (NormalGreName (substName env n)))
substNameAvailInfo _ env (Avail (FieldGreName fl)) =
    return (Avail (FieldGreName fl { flSelector = substName env (flSelector fl) }))
substNameAvailInfo hsc_env env (AvailTC n ns) =
    let mb_mod = fmap nameModule (lookupNameEnv env n)
    in AvailTC (substName env n) <$> mapM (setNameGreName hsc_env mb_mod) ns

setNameGreName :: HscEnv -> Maybe Module -> GreName -> IO GreName
setNameGreName hsc_env mb_mod gname = case gname of
    NormalGreName n -> NormalGreName <$> initIfaceLoad hsc_env (setNameModule mb_mod n)
    FieldGreName fl -> FieldGreName  <$> setNameFieldSelector hsc_env mb_mod fl

-- | Set the 'Module' of a 'FieldSelector'
setNameFieldSelector :: HscEnv -> Maybe Module -> FieldLabel -> IO FieldLabel
setNameFieldSelector _ Nothing f = return f
setNameFieldSelector hsc_env mb_mod (FieldLabel l b sel) = do
    sel' <- initIfaceLoad hsc_env $ setNameModule mb_mod sel
    return (FieldLabel l b sel')

{-
************************************************************************
*                                                                      *
                        AvailInfo merging
*                                                                      *
************************************************************************
-}

-- | Merges to 'AvailInfo' lists together, assuming the 'AvailInfo's have
-- already been unified ('uAvailInfos').
mergeAvails :: [AvailInfo] -> [AvailInfo] -> [AvailInfo]
mergeAvails as1 as2 =
    let mkNE as = mkNameEnv [(availName a, a) | a <- as]
    in nameEnvElts (plusNameEnv_C plusAvail (mkNE as1) (mkNE as2))

{-
************************************************************************
*                                                                      *
                        AvailInfo unification
*                                                                      *
************************************************************************
-}

-- | Unify two lists of 'AvailInfo's, given an existing substitution @subst@,
-- with only name holes from @flexi@ unifiable (all other name holes rigid.)
uAvailInfos :: ModuleName -> [AvailInfo] -> [AvailInfo] -> Either SDoc ShNameSubst
uAvailInfos flexi as1 as2 = -- pprTrace "uAvailInfos" (ppr as1 $$ ppr as2) $
    let mkOE as = listToUFM $ do a <- as
                                 n <- availNames a
                                 return (nameOccName n, a)
    in foldM (\subst (a1, a2) -> uAvailInfo flexi subst a1 a2) emptyNameEnv
             (eltsUFM (intersectUFM_C (,) (mkOE as1) (mkOE as2)))
             -- Edward: I have to say, this is pretty clever.

-- | Unify two 'AvailInfo's, given an existing substitution @subst@,
-- with only name holes from @flexi@ unifiable (all other name holes rigid.)
uAvailInfo :: ModuleName -> ShNameSubst -> AvailInfo -> AvailInfo
           -> Either SDoc ShNameSubst
uAvailInfo flexi subst (Avail (NormalGreName n1)) (Avail (NormalGreName n2)) = uName flexi subst n1 n2
uAvailInfo flexi subst (AvailTC n1 _) (AvailTC n2 _) = uName flexi subst n1 n2
uAvailInfo _ _ a1 a2 = Left $ text "While merging export lists, could not combine"
                           <+> ppr a1 <+> text "with" <+> ppr a2
                           <+> parens (text "one is a type, the other is a plain identifier")

-- | Unify two 'Name's, given an existing substitution @subst@,
-- with only name holes from @flexi@ unifiable (all other name holes rigid.)
uName :: ModuleName -> ShNameSubst -> Name -> Name -> Either SDoc ShNameSubst
uName flexi subst n1 n2
    | n1 == n2      = Right subst
    | isFlexi n1    = uHoleName flexi subst n1 n2
    | isFlexi n2    = uHoleName flexi subst n2 n1
    | otherwise     = Left (text "While merging export lists, could not unify"
                         <+> ppr n1 <+> text "with" <+> ppr n2 $$ extra)
  where
    isFlexi n = isHoleName n && moduleName (nameModule n) == flexi
    extra | isHoleName n1 || isHoleName n2
          = text "Neither name variable originates from the current signature."
          | otherwise
          = empty

-- | Unify a name @h@ which 'isHoleName' with another name, given an existing
-- substitution @subst@, with only name holes from @flexi@ unifiable (all
-- other name holes rigid.)
uHoleName :: ModuleName -> ShNameSubst -> Name {- hole name -} -> Name
          -> Either SDoc ShNameSubst
uHoleName flexi subst h n =
    ASSERT( isHoleName h )
    case lookupNameEnv subst h of
        Just n' -> uName flexi subst n' n
                -- Do a quick check if the other name is substituted.
        Nothing | Just n' <- lookupNameEnv subst n ->
                    ASSERT( isHoleName n ) uName flexi subst h n'
                | otherwise ->
                    Right (extendNameEnv subst h n)
