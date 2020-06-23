{-# LANGUAGE CPP, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2006
--
-- The purpose of this module is to transform an HsExpr into a CoreExpr which
-- when evaluated, returns a (Meta.Q Meta.Exp) computation analogous to the
-- input HsExpr. We do this in the DsM monad, which supplies access to
-- CoreExpr's of the "smart constructors" of the Meta.Exp datatype.
--
-- It also defines a bunch of knownKeyNames, in the same way as is done
-- in prelude/PrelNames.  It's much more convenient to do it here, because
-- otherwise we have to recompile PrelNames whenever we add a Name, which is
-- a Royal Pain (triggers other recompilation).
-----------------------------------------------------------------------------

module GHC.HsToCore.DsMetaTc( dsBracketTc, dsType, repEFPE, repCore, Core(MkC), repCodeCEv ) where

#include "HsVersions.h"

import GhcPrelude

import {-# SOURCE #-}  GHC.HsToCore.Expr ( dsExpr )

import GHC.HsToCore.Monad

import GHC.Hs

import GHC.Core.Class
import GHC.Types.Id
--import Name hiding( isVarOcc, isTcOcc, varName, tcName )
import GHC.Builtin.Names.TH
import GHC.Tc.Utils.TcType
import GHC.Core.TyCon
import GHC.Builtin.Types
import {-# SOURCE #-} GHC.HsToCore.Binds
import GHC.Core
import GHC.Core.Make
import GHC.Core.Utils
import GHC.Core.Type
import GHC.Types.SrcLoc
import GHC.Types.Unique
import Outputable
import GHC.Driver.Session
import MonadUtils
import GHC.HsToCore.Quote (Core(..))

import GHC.CoreToIface
import GHC.Iface.Binary
import Binary
import GHC.Types.Var.Set
import GHC.Core.FVs
import GHC.Core.SimpleOpt
import Data.ByteString.Internal
import qualified Data.ByteString as BS
import GHC.Types.Literal

import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe
import GHC.Tc.Types.Evidence

initBinMemSize :: Int
initBinMemSize = 1024*1024

dsType :: Type -> DsM CoreExpr
dsType t = repType t

-- | CoreExpr is of type m Exp
mkUntypedExpr :: Type -> Maybe CoreExpr -> DsM CoreExpr
mkUntypedExpr mtau mexp = do
  ec <- dsLookupTyCon expTyConName
  let ty = mkAppTy mtau (mkTyConApp ec [])
  return $ case mexp of
    Nothing -> mkNothingExpr ty
    Just ec -> mkJustExpr ty ec

-----------------------------------------------------------------------------
dsBracketTc :: (Type, Type)
            -> Maybe CoreExpr -- This is a CoreExpr representing an Exp
            -> QuoteWrapper
            -> HsBracket GhcTc
            -> [PendingTcTypedSplice]
            -> [PendingZonkSplice]
            -> [PendingZonkSplice]
            -> DsM CoreExpr
-- Returns a CoreExpr of type String which can be deserialised to get an
-- IfaceExpr.

dsBracketTc (rty, ur_ty) exp q@(QuoteWrapper ev_var m_tau) brack splices ev_zs zs
  = newSysLocalDs (mkListTy (mkBoxedTupleTy [intTy, intTy])) >>= \env_var -> dsExtendBindVar env_var $ \env_expr ->
    do
      (bounds_vars, body) <- do_brack brack
      sps <- mapMaybeM (do_one (dVarSetToVarSet bounds_vars)) splices
      zss <- mapM do_one_z zs
      (ev_zss, ev_tss) <- mapAndUnzipM do_one_ev ev_zs

      _q_w <- dsHsWrapper (applyQuoteWrapper q)
      -- thname <- lookupType nameTyConName
      u_ty <- dsLookupTyCon tExpUTyConName
      let ty = mkAppTy m_tau (mkTyConApp u_ty [])
      let tu_ty = mkBoxedTupleTy [intTy, ty]
      ty' <- funResultTy . funResultTy . idType <$> dsLookupGlobalId mkTTExpName
      let tt_ty = mkBoxedTupleTy [intTy, ty']
      th_rep_tc <- dsLookupTyCon thRepTyConName
      let thRepTy = mkBoxedTupleTy [intTy, mkTyConApp th_rep_tc []]

      dflags <- getDynFlags
      let mkKExpr = mkIntExprInt (targetPlatform dflags)
          bids = map (mkKExpr . getKey . getUnique) (dVarSetElems bounds_vars)
          bid_expr = mkListExpr intTy bids
      --pprTraceM "env_var" (ppr env_var $$ ppr bids)
      uco <- dsLookupGlobalId unsafeTExpCoerceName
      bind_vars <- dsLookupGlobalId bindVarsName
      env_var_final <- dsGetBindVar

      untyped_expr <- mkUntypedExpr m_tau exp

      --pprTraceM "dsBracket" (ppr splices $$ ppr sps $$ ppr zss $$ ppr ev_zss $$ ppr ev_tss)
      let u_co_expr = mkCoreApps ((Var uco)) [Type rty, Type ur_ty, Type m_tau, Var ev_var, mkCoreTup [untyped_expr, mkListExpr tt_ty (zss ++ ev_tss), mkListExpr tu_ty sps, mkListExpr thRepTy ev_zss, env_var_final, body]]
      let cont_expr  = mkLams [env_var] u_co_expr
      return $ mkCoreApps (Var bind_vars) [Type rty, Type ur_ty, Type m_tau, Var ev_var, bid_expr, cont_expr]
  where
    do_one_z (PendingZonkSplice n _mt mn e) = do
      let k = getKey (idUnique n)
      dflags <- getDynFlags
      let k_expr = mkIntExprInt (targetPlatform dflags) k
      Just liftTClass <- tyConClass_maybe <$> dsLookupTyCon liftTClassName
      let [lift_id, _] = classMethods liftTClass
      --untype <- dsLookupGlobalId unTypeQName
      --e' <- dsExpr (unLoc e)
      -- The type of the expression has to be Q (TExp r)
--      pprTraceM "do_one_z" (ppr (exprType e))
      {-
      case splitTyConApp (exprType e) of
          (ty, [r]) -> let (ty', [r']) = splitTyConApp r
                       in return $ Just $ mkCoreTup [k_expr, mkCoreApps (Var untype) [Type r', e']]
          _ -> return Nothing
      -}
      let (_, [k, a]) = splitTyConApp (exprType e)
      --pprTraceM "do_one_z" (ppr a $$  ppr k)
      selTT <- dsLookupGlobalId selTTExpName
      let wrap = case mn of
                    Nothing -> id
                    Just n -> \e -> mkCoreApps (Var selTT) [mkIntExprInt (targetPlatform dflags) n, e]
      return $ mkCoreTup [k_expr, wrap (mkCoreApps (Var lift_id) [Type k, Type a, e])]
    do_one_ev (PendingZonkSplice n (Just t) _ e) = do
      let k = getKey (idUnique n)
          kt = getKey (getUnique t)
      dflags <- getDynFlags
      let k_expr = mkIntExprInt (targetPlatform dflags) k
          kt_expr = mkIntExprInt (targetPlatform dflags) kt
      e' <- return e
      -- The type of the expression has to be Q (TExp r)
      --pprTraceM "ty" (ppr $ exprType e')
      --pprTraceM "e" (ppr $ e')
      ccev <- dsLookupGlobalId codeCevidenceName
      Just cc_class <- tyConClass_maybe <$> dsLookupTyCon codeCTyConName
      let cc_sup = classSCSelId cc_class 0

      Just liftTClass <- tyConClass_maybe <$> dsLookupTyCon liftTClassName
      let [lift_id, _] = classMethods liftTClass


      case splitTyConApp (exprType e') of
          (_ty, [r]) -> do
            let final_t_e body = mkCoreApps (Var lift_id) [Type constraintKind, Type r, body]
            return $ ( mkCoreTup [k_expr, mkCoreApps (Var ccev) [Type r, e']]
                     , mkCoreTup [kt_expr, final_t_e $ mkCoreApps (Var cc_sup) [Type r, e']] )
          _ -> pprPanic "split failed" (ppr e')
    do_one bs (PendingTcSplice n e) = do
      let k = getKey (idUnique n)
      dflags <- getDynFlags
      let k_expr = mkIntExprInt (targetPlatform dflags) k
      untype <- dsLookupGlobalId unTypeQTName
      e' <- dsSetBindEnv bs $ dsExpr (unLoc e)
      -- The type of the expression has to be Q (TExp r)
      --pprTraceM "ty" (ppr $ exprType e')
      --pprTraceM "e" (ppr $ e')
      --let e'' = simpleOptExpr dflags e'
      case splitAppTy (exprType e') of
          (_ty, r)  ->
            let (_ty', [rep, r']) = splitAppTys r
            in return $ Just $ mkCoreTup [k_expr, mkCoreApps (Var untype) [Type rep, Type r', Type m_tau, Var ev_var, e']]
          _ -> return Nothing

    do_brack (TExpBr _ e)  = do { (bs, s) <- repLE' e; return (bs, s) }
    do_brack _ = panic "dsBracket: unexpected XBracket"


boundVars :: CoreExpr -> DVarSet
boundVars (Lam x b)       = unitDVarSet x `unionDVarSet` boundVars b
boundVars (App f a)       = boundVars f `unionDVarSet` boundVars a
boundVars (Case s x _ty as) = unitDVarSet x `unionDVarSet` boundVars s `unionDVarSet` unionDVarSets (map boundVarsAlt as)
boundVars (Let b e)       = boundVarsBind b `unionDVarSet` (boundVars e)
boundVars (Cast e _co)     = boundVars e
boundVars (Tick _t e)      = boundVars e
boundVars _ = emptyDVarSet

boundVarsAlt :: Alt CoreBndr -> DVarSet
boundVarsAlt (_, bs, e) = mkDVarSet bs `unionDVarSet` boundVars e



boundVarsBind :: Bind Var -> DVarSet
boundVarsBind (NonRec b e) = unitDVarSet b `unionDVarSet` boundVars e
boundVarsBind (Rec g) = unionDVarSets (map (\(b, e) -> unitDVarSet b `unionDVarSet` boundVars e) g)



-----------------------------------------------------------------------------
--              Expressions
-----------------------------------------------------------------------------

repLE' :: LHsExpr GhcTc -> DsM (DVarSet, CoreExpr)
repLE' (L loc e) = putSrcSpanDs loc (repECore e)

repType :: Type -> DsM CoreExpr
repType ty = do
  let it = toIfaceTypeX (tyCoVarsOfType ty) ty
  --pprTraceM "Writing type" (ppr ty)
  liftIO (writeBracket it) >>= buildTHRep

repECore :: HsExpr GhcTc -> DsM (DVarSet, CoreExpr)
repECore e = do
  {-c_e <- mkCoreLams vs <$> dsExpr e
  dflags <- getDynFlags
  env <- env_top <$> getEnv
  c_e' <- liftIO $ corePrepExpr dflags env c_e
  let c_e'' = tidyExpr emptyTidyEnv c_e'
  pprTraceM "desugaring" (ppr ())
  pprTraceM "desugared" (ppr ())
  -}
  c_e <- dsExpr e
  let bvs = boundVars c_e
  dflags <- getDynFlags
  let _c_e' = simpleOptExpr dflags c_e
  --pprTraceM "c_e'" (ppr c_e $$ ppr c_e' $$ ppr bvs)
  res <- repCore c_e
  return (bvs, res)

repCore :: CoreExpr -> DsM CoreExpr
repCore c_e = do
  bs <- repEFP c_e
  buildTHRep bs

buildTHRep :: ByteString -> DsM CoreExpr
buildTHRep bs@(PS _ _ sz) = do
  th_rep_func <- dsLookupGlobalId mkTHRepName
  dflags <- getDynFlags
  let sz_expr = mkIntExprInt (targetPlatform dflags) sz
  --pprTraceM "buildTHRep" (text (show bs))
  let th_rep = mkCoreApps (Var th_rep_func) [sz_expr, (Lit (LitString bs))]
  return th_rep



repCodeCEv :: CoreExpr -> DsM CoreExpr
repCodeCEv c_e = do
  s <- repCore c_e
  return s

repEFPE :: HsExpr GhcTc -> DsM ByteString
repEFPE e = dsExpr e >>= repEFP

allFree :: CoreExpr -> VarSet
allFree c_e =
  let vvs = exprFreeVars c_e
  in vvs `unionVarSet` nonDetFoldVarSet (\v e -> unionVarSet (allTyFree v) e) mempty vvs

allTyFree :: Var -> VarSet
allTyFree t =  varTypeTyCoVars t

repEFP :: CoreExpr -> DsM ByteString
repEFP c_e = do
  benv <- dsGetBindEnv
  let ie = toIfaceExpr (allFree c_e `minusVarSet` benv) c_e
  --pprTraceM "toIfaceExpr" (ppr ie $$ ppr (allFree c_e) $$ ppr benv)
  liftIO (writeBracket ie)


writeBracket :: (Binary b, Outputable b) => b -> IO ByteString
writeBracket e = do
  bh <- openBinMem initBinMemSize
  putWithUserData (\_ -> return ()) bh e
  --(fp, h) <- openBinaryTempFile "/tmp" "bracket"
  --pprTraceM "Writing" (ppr (fp, e))
  --hClose h
  withBinBuffer bh (\(PS fptr off sz) -> do
    let bs = unsafePerformIO $ withForeignPtr fptr $ \ptr ->
               BS.packCStringLen (ptr `plusPtr` fromIntegral off, fromIntegral sz)
    return $! bs)


{-
lookupType :: Name      -- Name of type constructor (e.g. TH.ExpQ)
           -> DsM Type  -- The type
lookupType tc_name = do { tc <- dsLookupTyCon tc_name ;
                          return (mkTyConApp tc []) }
                          -}

