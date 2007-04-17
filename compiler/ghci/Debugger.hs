-----------------------------------------------------------------------------
--
-- GHCi Interactive debugging commands 
--
-- Pepe Iborra (supported by Google SoC) 2006
--
-----------------------------------------------------------------------------

module Debugger (pprintClosureCommand, instantiateTyVarsToUnknown) where

import Linker
import RtClosureInspect

import PrelNames
import HscTypes
import IdInfo
--import Id
import Var hiding ( varName )
import VarSet
import VarEnv
import Name 
import NameEnv
import RdrName
import UniqSupply
import Type
import TyCon
import DataCon
import TcGadt
import GHC
import GhciMonad

import Outputable
import Pretty                    ( Mode(..), showDocWith )
import FastString
import SrcLoc

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Data.IORef

import System.IO
import GHC.Exts

#include "HsVersions.h"

-------------------------------------
-- | The :print & friends commands
-------------------------------------
pprintClosureCommand :: Bool -> Bool -> String -> GHCi ()
pprintClosureCommand bindThings force str = do 
  cms <- getSession
  newvarsNames <- io$ do 
           uniques <- liftM uniqsFromSupply (mkSplitUniqSupply 'q')
           return$ map (\u-> (mkSysTvName u (mkFastString "a"))) uniques
  mb_ids  <- io$ mapM (cleanUp cms newvarsNames) (words str)
  mb_new_ids <- mapM (io . go cms) (catMaybes mb_ids)
  io$ updateIds cms (catMaybes mb_new_ids)
 where 
   -- Find the Id, clean up 'Unknowns'
   cleanUp :: Session -> [Name] -> String -> IO (Maybe Id)
   cleanUp cms newNames str = do
     tythings <- GHC.parseName cms str >>= mapM (GHC.lookupName cms)
     return$ listToMaybe (map (stripUnknowns newNames) 
                              [ i | Just (AnId i) <- tythings]) 

   -- Do the obtainTerm--bindSuspensions-refineIdType dance
   -- Warning! This function got a good deal of side-effects
   go :: Session -> Id -> IO (Maybe Id)
   go cms id = do
     mb_term <- obtainTerm cms force id
     maybe (return Nothing) `flip` mb_term $ \term -> do
       term'     <- if not bindThings then return term 
                     else bindSuspensions cms term                         
       showterm  <- pprTerm cms term'
       unqual    <- GHC.getPrintUnqual cms
       let showSDocForUserOneLine unqual doc = 
               showDocWith LeftMode (doc (mkErrStyle unqual))
       (putStrLn . showSDocForUserOneLine unqual) (ppr id <+> char '=' <+> showterm)
     -- Before leaving, we compare the type obtained to see if it's more specific
     -- Note how we need the Unknown-clear type returned by obtainTerm
       let Just reconstructedType = termType term  
       new_type  <- instantiateTyVarsToUnknown cms 
                    (mostSpecificType (idType id) reconstructedType)
       return . Just $ setIdType id new_type

   updateIds :: Session -> [Id] -> IO ()
   updateIds (Session ref) new_ids = do
     hsc_env <- readIORef ref
     let ictxt = hsc_IC hsc_env
         type_env = ic_type_env ictxt
         filtered_type_env = delListFromNameEnv type_env (map idName new_ids)
         new_type_env =  extendTypeEnvWithIds filtered_type_env new_ids
         new_ic = ictxt {ic_type_env = new_type_env }
     writeIORef ref (hsc_env {hsc_IC = new_ic })

isMoreSpecificThan :: Type -> Type -> Bool
ty `isMoreSpecificThan` ty1 
      | Just subst    <- tcUnifyTys bindOnlyTy1 [repType' ty] [repType' ty1] 
      , substFiltered <- filter (not.isTyVarTy) . varEnvElts . getTvSubstEnv $ subst
      , not . null $ substFiltered
      , all (flip notElemTvSubst subst) ty_vars
      = True
      | otherwise = False
      where bindOnlyTy1 tyv | tyv `elem` ty_vars = AvoidMe
                            | otherwise = BindMe
            ty_vars = varSetElems$ tyVarsOfType ty

mostSpecificType ty1 ty2 | ty1 `isMoreSpecificThan` ty2 = ty1
                         | otherwise = ty2

-- | Give names, and bind in the interactive environment, to all the suspensions
--   included (inductively) in a term
bindSuspensions :: Session -> Term -> IO Term
bindSuspensions cms@(Session ref) t = do 
      hsc_env <- readIORef ref
      inScope <- GHC.getBindings cms
      let ictxt        = hsc_IC hsc_env
          rn_env       = ic_rn_local_env ictxt
          type_env     = ic_type_env ictxt
          prefix       = "_t"
          alreadyUsedNames = map (occNameString . nameOccName . getName) inScope
          availNames   = map ((prefix++) . show) [1..] \\ alreadyUsedNames 
      availNames_var  <- newIORef availNames
      (t', stuff)     <- foldTerm (nameSuspensionsAndGetInfos availNames_var) t
      let (names, tys, hvals) = unzip3 stuff
      concrete_tys    <- mapM (instantiateTyVarsToUnknown cms) tys
      let ids = [ mkGlobalId VanillaGlobal name ty vanillaIdInfo
                  | (name,ty) <- zip names concrete_tys]
          new_type_env = extendTypeEnvWithIds type_env ids 
          new_rn_env   = extendLocalRdrEnv rn_env names
          new_ic       = ictxt { ic_rn_local_env = new_rn_env, 
                                 ic_type_env     = new_type_env }
      extendLinkEnv (zip names hvals)
      writeIORef ref (hsc_env {hsc_IC = new_ic })
      return t'
     where    

--    Processing suspensions. Give names and recopilate info
        nameSuspensionsAndGetInfos :: IORef [String] -> TermFold (IO (Term, [(Name,Type,HValue)]))
        nameSuspensionsAndGetInfos freeNames = TermFold 
                      {
                        fSuspension = doSuspension freeNames
                      , fTerm = \ty dc v tt -> do 
                                    tt' <- sequence tt 
                                    let (terms,names) = unzip tt' 
                                    return (Term ty dc v terms, concat names)
                      , fPrim    = \ty n ->return (Prim ty n,[])
                      }
        doSuspension freeNames ct mb_ty hval Nothing = do
          name <- atomicModifyIORef freeNames (\x->(tail x, head x))
          n <- newGrimName cms name
          let ty' = fromMaybe (error "unexpected") mb_ty
          return (Suspension ct mb_ty hval (Just n), [(n,ty',hval)])


--  A custom Term printer to enable the use of Show instances
pprTerm cms@(Session ref) = customPrintTerm customPrint
 where
  customPrint = \p-> customPrintShowable : customPrintTermBase p 
  customPrintShowable t@Term{ty=ty, dc=dc, val=val} = do
    let hasType = isEmptyVarSet (tyVarsOfType ty)  -- redundant
        isEvaled = isFullyEvaluatedTerm t
    if not isEvaled -- || not hasType
     then return Nothing
     else do 
        hsc_env <- readIORef ref
        dflags  <- GHC.getSessionDynFlags cms
        do
           (new_env, bname) <- bindToFreshName hsc_env ty "showme"
           writeIORef ref (new_env)
           let noop_log _ _ _ _ = return () 
               expr = "show " ++ showSDoc (ppr bname)
           GHC.setSessionDynFlags cms dflags{log_action=noop_log}
           mb_txt <- withExtendedLinkEnv [(bname, val)] 
                                         (GHC.compileExpr cms expr)
           case mb_txt of 
             Just txt -> return . Just . text . unsafeCoerce# $ txt
             Nothing  -> return Nothing
         `finally` do 
           writeIORef ref hsc_env
           GHC.setSessionDynFlags cms dflags
     
  bindToFreshName hsc_env ty userName = do
    name <- newGrimName cms userName 
    let ictxt    = hsc_IC hsc_env
        rn_env   = ic_rn_local_env ictxt
        type_env = ic_type_env ictxt
        id       = mkGlobalId VanillaGlobal name ty vanillaIdInfo
        new_type_env = extendTypeEnv type_env (AnId id)
        new_rn_env   = extendLocalRdrEnv rn_env [name]
        new_ic       = ictxt { ic_rn_local_env = new_rn_env, 
                               ic_type_env     = new_type_env }
    return (hsc_env {hsc_IC = new_ic }, name)

--    Create new uniques and give them sequentially numbered names
--    newGrimName :: Session -> String -> IO Name
newGrimName cms userName  = do
    us <- mkSplitUniqSupply 'b'
    let unique  = uniqFromSupply us
        occname = mkOccName varName userName
        name    = mkInternalName unique occname noSrcLoc
    return name

----------------------------------------------------------------------------
-- | Replace all the tyvars in a Term with the opaque type GHC.Base.Unknown
----------------------------------------------------------------------------
instantiateTyVarsToUnknown :: Session -> Type -> IO Type
instantiateTyVarsToUnknown cms ty
-- We have a GADT, so just fix its tyvars
    | Just (tycon, args) <- splitTyConApp_maybe ty
    , tycon /= funTyCon
    , isGADT tycon
    = mapM fixTyVars args >>= return . mkTyConApp tycon
-- We have a regular TyCon, so map recursively to its args
    | Just (tycon, args) <- splitTyConApp_maybe ty
    , tycon /= funTyCon
    = do unknownTyVar <- unknownTV
         args' <- mapM (instantiateTyVarsToUnknown cms) args
         return$ mkTyConApp tycon args'
-- we have a tyvar of kind *
    | Just tyvar <- getTyVar_maybe ty
    , ([],_) <- splitKindFunTys (tyVarKind tyvar) 
    = unknownTV
-- we have a higher kind tyvar, so insert an unknown of the appropriate kind
    | Just tyvar <- getTyVar_maybe ty
    , (args,_) <- splitKindFunTys (tyVarKind tyvar)
    = liftM mkTyConTy $ unknownTC !! length args
-- Base case
    | otherwise    = return ty 

 where unknownTV = do 
         Just (ATyCon unknown_tc) <- lookupName cms unknownTyConName
         return$ mkTyConTy unknown_tc
       unknownTC = [undefined, unknownTC1, unknownTC2, unknownTC3]
       unknownTC1 = do 
         Just (ATyCon unknown_tc) <- lookupName cms unknown1TyConName
         return unknown_tc
       unknownTC2 = do 
         Just (ATyCon unknown_tc) <- lookupName cms unknown2TyConName
         return unknown_tc
       unknownTC3 = do 
         Just (ATyCon unknown_tc) <- lookupName cms unknown3TyConName
         return unknown_tc
--       isGADT ty | pprTrace' "isGADT" (ppr ty <> colon <> ppr(isGadtSyntaxTyCon ty)) False = undefined
       isGADT tc | Just dcs <- tyConDataCons_maybe tc = any (not . null . dataConEqSpec) dcs
                 | otherwise = False
       fixTyVars ty 
           | Just (tycon, args) <- splitTyConApp_maybe ty
           = mapM fixTyVars args >>= return . mkTyConApp tycon
-- Fix the tyvar so that the interactive environment doesn't choke on it TODO 
           | Just tv <- getTyVar_maybe ty = return ty --TODO
           | otherwise = return ty

-- | The inverse function. Strip the GHC.Base.Unknowns in the type of the id, they correspond to tyvars. The caller must provide an infinite list of fresh names
stripUnknowns :: [Name] -> Id -> Id
stripUnknowns names id = setIdType id . fst . go names . idType 
                           $ id
 where 
   go tyvarsNames@(v:vv) ty 
    | Just (ty1,ty2) <- splitFunTy_maybe ty = let
               (ty1',vv') = go tyvarsNames ty1
               (ty2',vv'')= go vv' ty2
               in (mkFunTy ty1' ty2', vv'')
    | Just (ty1,ty2) <- splitAppTy_maybe ty = let
               (ty1',vv') = go tyvarsNames ty1
               (ty2',vv'')= go vv' ty2
               in (mkAppTy ty1' ty2', vv'')
    | Just (tycon, args) <- splitTyConApp_maybe ty 
    , Just (tycon', vv') <- (fixTycon tycon tyvarsNames)
    , (args',vv'') <- foldr (\arg (aa,vv) -> let (arg',vv') = go vv' arg 
                                             in (arg':aa,vv'))
                            ([],vv') args
    = (mkAppTys tycon' args',vv'')
    | Just (tycon, args) <- splitTyConApp_maybe ty
    , (args',vv') <- foldr (\arg (aa,vv) -> let (arg',vv') = go vv' arg 
                                            in (arg':aa,vv'))
                           ([],tyvarsNames) args
    = (mkTyConApp tycon args',vv')
    | otherwise = (ty, tyvarsNames)
    where  fixTycon tycon (v:vv) = do
               k <- lookup (tyConName tycon) kinds
               return (mkTyVarTy$ mkTyVar v k, vv)
           kinds = [ (unknownTyConName, liftedTypeKind)
                   , (unknown1TyConName, kind1)
                   , (unknown2TyConName, kind2)
                   , (unknown3TyConName, kind3)]
           kind1 = mkArrowKind liftedTypeKind liftedTypeKind
           kind2 = mkArrowKind kind1 liftedTypeKind
           kind3 = mkArrowKind kind2 liftedTypeKind
