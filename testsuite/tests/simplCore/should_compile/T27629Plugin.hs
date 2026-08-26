-- Injects the example of Note [Weird special case for SpecDict] in
-- GHC.Core.Opt.Specialise, which cannot be written in source Haskell:
-- a call whose dictionary argument mentions a type variable that is
-- free in the dictionary expression but not in its type.
--
-- Runs before the main Core pipeline (in particular before the
-- Specialiser). It adds
--
--   mkD :: forall a. T -> C T    {-# NOINLINE mkD #-}
--   mkD = /\a. \_. $fCT
--
-- and rewrites   poly = /\x. \t. [t]
-- to             poly = /\x. \t. split @T (mkD @x MkT) [t]
{-# LANGUAGE CPP #-}
module T27629Plugin (plugin) where

import GHC.Core.Make (mkCoreApps, mkListExpr)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Plugins
#if __GLASGOW_HASKELL__ >= 1000
import GHC.Types.InlinePragma (neverInlinePragma)
#else
import GHC.Types.Basic (neverInlinePragma)
#endif

plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = \_ todos ->
        return (CoreDoPluginPass "inject-weird-specdict" pass : todos)
    , pluginRecompile = purePlugin
    }

pass :: ModGuts -> CoreM ModGuts
pass guts = do
  let binds = mg_binds guts

      dfun = case [b | b <- bindersOfBinds binds, isDFunId b] of
        (b : _) -> b
        [] -> panic "ReproPlugin: no dfun binding in module"

      dictTy = idType dfun -- C T
      tyT = case getClassPredTys_maybe dictTy of
        Just (_cls, [t]) -> t
        _ -> panic "ReproPlugin: dfun type is not C T"

      mkTCon = case tyConDataCons (tyConAppTyCon tyT) of
        (dc : _) -> dataConWorkId dc
        [] -> panic "ReproPlugin: T has no data constructors"

      findTopId nm =
        case [b | b <- bindersOfBinds binds, occNameString (getOccName b) == nm] of
          (b : _) -> b
          [] -> panic ("ReproPlugin: no top-level binding " ++ nm)

      splitId = findTopId "split"
      polyId = findTopId "poly"

  uMkD <- getUniqueM
  uTv <- getUniqueM
  uWild <- getUniqueM

  let aTv = mkTyVar (mkSystemName uTv (mkTyVarOcc "a")) liftedTypeKind
      mkDTy = mkSpecForAllTys [aTv] (mkFunctionType ManyTy tyT dictTy)
      mkDId =
        setInlinePragma
          (mkExportedVanillaId (mkSystemName uMkD (mkVarOcc "mkD")) mkDTy)
          neverInlinePragma
      wild = mkSysLocal (fsLit "eta") uWild ManyTy tyT
      mkDBind = NonRec mkDId (mkLams [aTv, wild] (Var dfun))

      rewritePoly rhs
        | (bndrs, _body) <- collectBinders rhs
        , (x : _) <- reverse (filter isTyVar bndrs)
        , (t : _) <- filter isId bndrs
        = mkLams bndrs $
            mkCoreApps
              (Var splitId)
              [ Type tyT
              , mkCoreApps (Var mkDId) [Type (mkTyVarTy x), Var mkTCon]
              , mkListExpr tyT [Var t]
              ]
        | otherwise =
            pprPanic "ReproPlugin: unexpected shape of poly" (ppr rhs)

      go (b, rhs)
        | b == polyId = (b, rewritePoly rhs)
        | otherwise = (b, rhs)

      -- One top-level Rec, so bind order is irrelevant.
      binds' = [Rec (map go (flattenBinds binds) ++ flattenBinds [mkDBind])]

  putMsgS "ReproPlugin: injected weird SpecDict call into poly"
  return guts {mg_binds = binds'}
