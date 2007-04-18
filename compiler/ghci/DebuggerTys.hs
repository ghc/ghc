-----------------------------------------------------------------------------
--
-- GHCi Interactive debugging commands 
--
-- Pepe Iborra (supported by Google SoC) 2006
--
-----------------------------------------------------------------------------

module DebuggerTys (instantiateTyVarsToUnknown) where

import HscTypes
import Type
import TcRnDriver
import Var
import PrelNames
import TyCon
import DataCon

import Control.Monad

----------------------------------------------------------------------------
-- | Replace all the tyvars in a Term with the opaque type GHC.Base.Unknown
----------------------------------------------------------------------------
instantiateTyVarsToUnknown :: HscEnv -> Type -> IO Type
instantiateTyVarsToUnknown hsc_env ty
-- We have a GADT, so just fix its tyvars
    | Just (tycon, args) <- splitTyConApp_maybe ty
    , tycon /= funTyCon
    , isGADT tycon
    = mapM fixTyVars args >>= return . mkTyConApp tycon
-- We have a regular TyCon, so map recursively to its args
    | Just (tycon, args) <- splitTyConApp_maybe ty
    , tycon /= funTyCon
    = do unknownTyVar <- unknownTV
         args' <- mapM (instantiateTyVarsToUnknown hsc_env) args
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
         Just (ATyCon unknown_tc) <- tcRnLookupName hsc_env unknownTyConName
         return$ mkTyConTy unknown_tc
       unknownTC = [undefined, unknownTC1, unknownTC2, unknownTC3]
       unknownTC1 = do 
         Just (ATyCon unknown_tc) <- tcRnLookupName hsc_env unknown1TyConName
         return unknown_tc
       unknownTC2 = do 
         Just (ATyCon unknown_tc) <- tcRnLookupName hsc_env unknown2TyConName
         return unknown_tc
       unknownTC3 = do 
         Just (ATyCon unknown_tc) <- tcRnLookupName hsc_env unknown3TyConName
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

