module Vectorise( vectorise )
where

#include "HsVersions.h"

import DynFlags
import HscTypes

import CoreLint             ( showPass, endPass )
import CoreSyn
import CoreUtils
import CoreFVs
import TyCon
import Type
import TypeRep
import Var
import VarEnv
import Name                 ( mkSysTvName )
import NameEnv
import Id

import DsMonad hiding (mapAndUnzipM)

import PrelNames

import Outputable
import FastString
import Control.Monad        ( liftM, liftM2, mapAndUnzipM )

vectorise :: HscEnv -> ModGuts -> IO ModGuts
vectorise hsc_env guts
  | not (Opt_Vectorise `dopt` dflags) = return guts
  | otherwise
  = do
      showPass dflags "Vectorisation"
      eps <- hscEPS hsc_env
      let info = hptVectInfo hsc_env `plusVectInfo` eps_vect_info eps
      Just guts' <- initDs hsc_env (mg_module guts)
                                   (mg_rdr_env guts)
                                   (mg_types guts)
                                   (vectoriseModule info guts)
      endPass dflags "Vectorisation" Opt_D_dump_vect (mg_binds guts')
      return guts'
  where
    dflags = hsc_dflags hsc_env

-- ----------------------------------------------------------------------------
-- Vectorisation monad

data Builtins = Builtins {
                  parrayTyCon      :: TyCon
                , paTyCon          :: TyCon
                , closureTyCon     :: TyCon
                , mkClosureVar     :: Var
                , applyClosureVar  :: Var
                , mkClosurePVar    :: Var
                , applyClosurePVar :: Var
                , closurePAVar     :: Var
                , lengthPAVar      :: Var
                , replicatePAVar   :: Var
                }

initBuiltins :: DsM Builtins
initBuiltins
  = do
      parrayTyCon  <- dsLookupTyCon parrayTyConName
      paTyCon      <- dsLookupTyCon paTyConName
      closureTyCon <- dsLookupTyCon closureTyConName

      mkClosureVar     <- dsLookupGlobalId mkClosureName
      applyClosureVar  <- dsLookupGlobalId applyClosureName
      mkClosurePVar    <- dsLookupGlobalId mkClosurePName
      applyClosurePVar <- dsLookupGlobalId applyClosurePName
      closurePAVar     <- dsLookupGlobalId closurePAName
      lengthPAVar      <- dsLookupGlobalId lengthPAName
      replicatePAVar   <- dsLookupGlobalId replicatePAName

      return $ Builtins {
                 parrayTyCon      = parrayTyCon
               , paTyCon          = paTyCon
               , closureTyCon     = closureTyCon
               , mkClosureVar     = mkClosureVar
               , applyClosureVar  = applyClosureVar
               , mkClosurePVar    = mkClosurePVar
               , applyClosurePVar = applyClosurePVar
               , closurePAVar     = closurePAVar
               , lengthPAVar      = lengthPAVar
               , replicatePAVar   = replicatePAVar
               }

data GlobalEnv = GlobalEnv {
                  -- Mapping from global variables to their vectorised versions.
                  -- 
                  global_vars :: VarEnv CoreExpr

                  -- Exported variables which have a vectorised version
                  --
                , global_exported_vars :: VarEnv (Var, Var)

                  -- Mapping from TyCons to their vectorised versions.
                  -- TyCons which do not have to be vectorised are mapped to
                  -- themselves.
                  --
                , global_tycons :: NameEnv TyCon

                  -- Mapping from TyCons to their PA dictionaries
                  --
                , global_tycon_pa :: NameEnv CoreExpr
                }

data LocalEnv = LocalEnv {
                 -- Mapping from local variables to their vectorised and
                 -- lifted versions
                 --
                 local_vars :: VarEnv (CoreExpr, CoreExpr)

                 -- Mapping from tyvars to their PA dictionaries
               , local_tyvar_pa :: VarEnv CoreExpr
               }
              

initGlobalEnv :: VectInfo -> GlobalEnv
initGlobalEnv info
  = GlobalEnv {
      global_vars          = mapVarEnv  (Var . snd) $ vectInfoCCVar   info
    , global_exported_vars = emptyVarEnv
    , global_tycons        = mapNameEnv snd $ vectInfoCCTyCon info
    , global_tycon_pa      = emptyNameEnv
    }

emptyLocalEnv = LocalEnv {
                   local_vars     = emptyVarEnv
                 , local_tyvar_pa = emptyVarEnv
                 }

-- FIXME
updVectInfo :: GlobalEnv -> ModGuts -> ModGuts
updVectInfo env guts = guts { mg_vect_info = info' }
  where
    info' = info {
              vectInfoCCVar   = global_exported_vars env
            , vectInfoCCTyCon = tc_env
            }

    info  = mg_vect_info guts
    tyenv = mg_types guts

    tc_env = mkNameEnv [(tc_name, (tc,tc')) | tc <- typeEnvTyCons tyenv
                                            , let tc_name = tyConName tc
                                            , Just tc' <- [lookupNameEnv (global_tycons env) tc_name]]

data VResult a = Yes GlobalEnv LocalEnv a | No

newtype VM a = VM { runVM :: Builtins -> GlobalEnv -> LocalEnv -> DsM (VResult a) }

instance Monad VM where
  return x   = VM $ \bi genv lenv -> return (Yes genv lenv x)
  VM p >>= f = VM $ \bi genv lenv -> do
                                      r <- p bi genv lenv
                                      case r of
                                        Yes genv' lenv' x -> runVM (f x) bi genv' lenv'
                                        No                -> return No

noV :: VM a
noV = VM $ \_ _ _ -> return No

tryV :: VM a -> VM (Maybe a)
tryV (VM p) = VM $ \bi genv lenv ->
  do
    r <- p bi genv lenv
    case r of
      Yes genv' lenv' x -> return (Yes genv' lenv' (Just x))
      No                -> return (Yes genv  lenv  Nothing)

maybeV :: VM (Maybe a) -> VM a
maybeV p = maybe noV return =<< p

orElseV :: VM a -> VM a -> VM a
orElseV p q = maybe q return =<< tryV p

localV :: VM a -> VM a
localV p = do
             env <- readLEnv id
             x <- p
             setLEnv env
             return x

liftDs :: DsM a -> VM a
liftDs p = VM $ \bi genv lenv -> do { x <- p; return (Yes genv lenv x) }

builtin :: (Builtins -> a) -> VM a
builtin f = VM $ \bi genv lenv -> return (Yes genv lenv (f bi))

readGEnv :: (GlobalEnv -> a) -> VM a
readGEnv f = VM $ \bi genv lenv -> return (Yes genv lenv (f genv))

setGEnv :: GlobalEnv -> VM ()
setGEnv genv = VM $ \_ _ lenv -> return (Yes genv lenv ())

updGEnv :: (GlobalEnv -> GlobalEnv) -> VM ()
updGEnv f = VM $ \_ genv lenv -> return (Yes (f genv) lenv ())

readLEnv :: (LocalEnv -> a) -> VM a
readLEnv f = VM $ \bi genv lenv -> return (Yes genv lenv (f lenv))

setLEnv :: LocalEnv -> VM ()
setLEnv lenv = VM $ \_ genv _ -> return (Yes genv lenv ())

updLEnv :: (LocalEnv -> LocalEnv) -> VM ()
updLEnv f = VM $ \_ genv lenv -> return (Yes genv (f lenv) ())

newLocalVar :: FastString -> Type -> VM Var
newLocalVar fs ty
  = do
      u <- liftDs newUnique
      return $ mkSysLocal fs u ty

newTyVar :: FastString -> Kind -> VM Var
newTyVar fs k
  = do
      u <- liftDs newUnique
      return $ mkTyVar (mkSysTvName u fs) k

lookupTyCon :: TyCon -> VM (Maybe TyCon)
lookupTyCon tc = readGEnv $ \env -> lookupNameEnv (global_tycons env) (tyConName tc)


extendTyVarPA :: Var -> CoreExpr -> VM ()
extendTyVarPA tv pa = updLEnv $ \env -> env { local_tyvar_pa = extendVarEnv (local_tyvar_pa env) tv pa }

-- ----------------------------------------------------------------------------
-- Bindings

vectoriseModule :: VectInfo -> ModGuts -> DsM ModGuts
vectoriseModule info guts
  = do
      builtins <- initBuiltins
      r <- runVM (vectModule guts) builtins (initGlobalEnv info) emptyLocalEnv
      case r of
        Yes genv _ guts' -> return $ updVectInfo genv guts'
        No               -> return guts

vectModule :: ModGuts -> VM ModGuts
vectModule guts = return guts



vectBndr :: Var -> VM (Var, Var)
vectBndr v
  = do
      vty <- vectType (idType v)
      lty <- mkPArrayTy vty
      let vv = v `Id.setIdType` vty
          lv = v `Id.setIdType` lty
      updLEnv (mapTo vv lv)
      return (vv, lv)
  where
    mapTo vv lv env = env { local_vars = extendVarEnv (local_vars env) v (Var vv, Var lv) }

vectBndrIn :: Var -> VM a -> VM (Var, Var, a)
vectBndrIn v p
  = localV
  $ do
      (vv, lv) <- vectBndr v
      x <- p
      return (vv, lv, x)

vectBndrsIn :: [Var] -> VM a -> VM ([Var], [Var], a)
vectBndrsIn vs p
  = localV
  $ do
      (vvs, lvs) <- mapAndUnzipM vectBndr vs
      x <- p
      return (vvs, lvs, x)

-- ----------------------------------------------------------------------------
-- Expressions

replicateP :: CoreExpr -> CoreExpr -> VM CoreExpr
replicateP expr len
  = do
      pa  <- paOfType ty
      rep <- builtin replicatePAVar
      return $ mkApps (Var rep) [Type ty, pa, expr, len]
  where
    ty = exprType expr

capply :: (CoreExpr, CoreExpr) -> (CoreExpr, CoreExpr) -> VM (CoreExpr, CoreExpr)
capply (vfn, lfn) (varg, larg)
  = do
      apply  <- builtin applyClosureVar
      applyP <- builtin applyClosurePVar
      return (mkApps (Var apply)  [Type arg_ty, Type res_ty, vfn, varg],
              mkApps (Var applyP) [Type arg_ty, Type res_ty, lfn, larg])
  where
    fn_ty            = exprType vfn
    (arg_ty, res_ty) = splitClosureTy fn_ty

vectVar :: CoreExpr -> Var -> VM (CoreExpr, CoreExpr)
vectVar lc v = local v `orElseV` global v
  where
    local  v = maybeV (readLEnv $ \env -> lookupVarEnv (local_vars env) v)
    global v = do
                 vexpr <- maybeV (readGEnv $ \env -> lookupVarEnv (global_vars env) v)
                 lexpr <- replicateP vexpr lc
                 return (vexpr, lexpr)
                
vectExpr :: CoreExpr -> CoreExprWithFVs -> VM (CoreExpr, CoreExpr)
vectExpr lc (_, AnnType ty)
  = do
      vty <- vectType ty
      return (Type vty, Type vty)
vectExpr lc (_, AnnVar v)   = vectVar lc v
vectExpr lc (_, AnnLit lit)
  = do
      let vexpr = Lit lit
      lexpr <- replicateP vexpr lc
      return (vexpr, lexpr)
vectExpr lc (_, AnnNote note expr)
  = do
      (vexpr, lexpr) <- vectExpr lc expr
      return (Note note vexpr, Note note lexpr)
vectExpr lc (_, AnnApp fn arg)
  = do
      fn'  <- vectExpr lc fn
      arg' <- vectExpr lc arg
      capply fn' arg'
vectExpr lc (_, AnnCase expr bndr ty alts)
  = panic "vectExpr: case"
vectExpr lc (_, AnnLet (AnnNonRec bndr rhs) body)
  = do
      (vrhs, lrhs) <- vectExpr lc rhs
      (vbndr, lbndr, (vbody, lbody)) <- vectBndrIn bndr (vectExpr lc body)
      return (Let (NonRec vbndr vrhs) vbody,
              Let (NonRec lbndr lrhs) lbody)
vectExpr lc (_, AnnLet (AnnRec prs) body)
  = do
      (vbndrs, lbndrs, (vrhss, vbody, lrhss, lbody)) <- vectBndrsIn bndrs vect
      return (Let (Rec (zip vbndrs vrhss)) vbody,
              Let (Rec (zip lbndrs lrhss)) lbody)
  where
    (bndrs, rhss) = unzip prs
    
    vect = do
             (vrhss, lrhss) <- mapAndUnzipM (vectExpr lc) rhss
             (vbody, lbody) <- vectExpr lc body
             return (vrhss, vbody, lrhss, lbody)
vectExpr lc (_, AnnLam bndr body)
  | isTyVar bndr
  = do
      pa_ty          <- paArgType' (TyVarTy bndr) (tyVarKind bndr)
      pa_var         <- newLocalVar FSLIT("dPA") pa_ty
      (vbody, lbody) <- localV
                      $ do
                          extendTyVarPA bndr (Var pa_var)
                          -- FIXME: what about shadowing here (bndr in lc)?
                          vectExpr lc body
      return (mkLams [bndr, pa_var] vbody,
              mkLams [bndr, pa_var] lbody)

-- ----------------------------------------------------------------------------
-- PA dictionaries

paArgType :: Type -> Kind -> VM (Maybe Type)
paArgType ty k
  | Just k' <- kindView k = paArgType ty k'

-- Here, we assume that for a kind (k1 -> k2) to be valid, k1 and k2 can only
-- be made up of * and (->), i.e., they can't be coercion kinds or #.
paArgType ty (FunTy k1 k2)
  = do
      tv  <- newTyVar FSLIT("a") k1
      ty1 <- paArgType' (TyVarTy tv) k1
      ty2 <- paArgType' (AppTy ty (TyVarTy tv)) k2
      return . Just $ ForAllTy tv (FunTy ty1 ty2)

paArgType ty k
  | isLiftedTypeKind k
  = do
      tc <- builtin paTyCon
      return . Just $ TyConApp tc [ty]

  | otherwise
  = return Nothing 

paArgType' :: Type -> Kind -> VM Type
paArgType' ty k
  = do
      r <- paArgType ty k
      case r of
        Just ty' -> return ty'
        Nothing  -> pprPanic "paArgType'" (ppr ty)

paOfTyCon :: TyCon -> VM CoreExpr
-- FIXME: just for now
paOfTyCon tc = maybeV (readGEnv $ \env -> lookupNameEnv (global_tycon_pa env) (tyConName tc))

paOfType :: Type -> VM CoreExpr
paOfType ty | Just ty' <- coreView ty = paOfType ty'

paOfType (TyVarTy tv) = maybeV (readLEnv $ \env -> lookupVarEnv (local_tyvar_pa env) tv)
paOfType (AppTy ty1 ty2)
  = do
      e1 <- paOfType ty1
      e2 <- paOfType ty2
      return $ mkApps e1 [Type ty2, e2]
paOfType (TyConApp tc tys)
  = do
      e  <- paOfTyCon tc
      es <- mapM paOfType tys
      return $ mkApps e [arg | (t,e) <- zip tys es, arg <- [Type t, e]]
paOfType (FunTy ty1 ty2) = paOfType (TyConApp funTyCon [ty1,ty2])
paOfType t@(ForAllTy tv ty) = pprPanic "paOfType:" (ppr t)
paOfType ty = pprPanic "paOfType:" (ppr ty)
        


-- ----------------------------------------------------------------------------
-- Types

vectTyCon :: TyCon -> VM TyCon
vectTyCon tc
  | isFunTyCon tc        = builtin closureTyCon
  | isBoxedTupleTyCon tc = return tc
  | isUnLiftedTyCon tc   = return tc
  | otherwise = do
                  r <- lookupTyCon tc
                  case r of
                    Just tc' -> return tc'

                    -- FIXME: just for now
                    Nothing  -> pprTrace "ccTyCon:" (ppr tc) $ return tc

vectType :: Type -> VM Type
vectType ty | Just ty' <- coreView ty = vectType ty
vectType (TyVarTy tv) = return $ TyVarTy tv
vectType (AppTy ty1 ty2) = liftM2 AppTy (vectType ty1) (vectType ty2)
vectType (TyConApp tc tys) = liftM2 TyConApp (vectTyCon tc) (mapM vectType tys)
vectType (FunTy ty1 ty2)   = liftM2 TyConApp (builtin closureTyCon)
                                             (mapM vectType [ty1,ty2])
vectType (ForAllTy tv ty)
  = do
      r   <- paArgType (TyVarTy tv) (tyVarKind tv)
      ty' <- vectType ty
      return . ForAllTy tv $ case r of { Just paty -> FunTy paty ty'; Nothing -> ty' }

vectType ty = pprPanic "vectType:" (ppr ty)

isClosureTyCon :: TyCon -> Bool
isClosureTyCon tc = tyConUnique tc == closureTyConKey

splitClosureTy :: Type -> (Type, Type)
splitClosureTy ty
  | Just (tc, [arg_ty, res_ty]) <- splitTyConApp_maybe ty
  , isClosureTyCon tc
  = (arg_ty, res_ty)

  | otherwise = pprPanic "splitClosureTy" (ppr ty)

mkPArrayTy :: Type -> VM Type
mkPArrayTy ty = do
                  tc <- builtin parrayTyCon
                  return $ TyConApp tc [ty]

