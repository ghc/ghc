module GHC.Types.TypeEnv
   ( TypeEnv(..)
   , emptyTypeEnv
   , lookupTypeEnv
   , mkTypeEnv
   , typeEnvFromEntities
   , mkTypeEnvWithImplicits
   , extendTypeEnv
   , extendTypeEnvList
   , extendTypeEnvWithIds
   , plusTypeEnv
   , typeEnvElts
   , typeEnvTyCons
   , typeEnvIds
   , typeEnvPatSyns
   , typeEnvDataCons
   , typeEnvCoAxioms
   , typeEnvClasses
   , elemTypeEnv
   , mapTypeEnv
   )
where

import GHC.Prelude

import GHC.Core.Class
import GHC.Core.Coercion.Axiom
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.FamInstEnv
import GHC.Core.PatSyn
import GHC.Core.TyCon

import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Var
import GHC.Types.TyThing
import GHC.Utils.Outputable

-- | A map from 'Name's to 'TyThing's, constructed by typechecking
-- local declarations or interface files
data TypeEnv = TypeEnv !(NameEnv TyThing)

emptyTypeEnv    :: TypeEnv
typeEnvElts     :: TypeEnv -> [TyThing]
typeEnvTyCons   :: TypeEnv -> [TyCon]
typeEnvCoAxioms :: TypeEnv -> [CoAxiom Branched]
typeEnvIds      :: TypeEnv -> [Id]
typeEnvPatSyns  :: TypeEnv -> [PatSyn]
typeEnvDataCons :: TypeEnv -> [DataCon]
typeEnvClasses  :: TypeEnv -> [Class]
lookupTypeEnv   :: TypeEnv -> Name -> Maybe TyThing

emptyTypeEnv        = TypeEnv emptyNameEnv
typeEnvElts     (TypeEnv env) = nameEnvElts env
typeEnvTyCons   env = [tc | ATyCon tc   <- typeEnvElts env]
typeEnvCoAxioms env = [ax | ACoAxiom ax <- typeEnvElts env]
typeEnvIds      env = [id | AnId id     <- typeEnvElts env]
typeEnvPatSyns  env = [ps | AConLike (PatSynCon ps) <- typeEnvElts env]
typeEnvDataCons env = [dc | AConLike (RealDataCon dc) <- typeEnvElts env]
typeEnvClasses  env = [cl | tc <- typeEnvTyCons env,
                            Just cl <- [tyConClass_maybe tc]]

mkTypeEnv :: [TyThing] -> TypeEnv
mkTypeEnv things = extendTypeEnvList emptyTypeEnv things

elemTypeEnv :: Name -> TypeEnv -> Bool
elemTypeEnv n (TypeEnv env) = elemNameEnv n env

mapTypeEnv :: (TyThing -> TyThing) -> TypeEnv -> TypeEnv
mapTypeEnv f (TypeEnv env) = TypeEnv (mapNameEnv f env)

mkTypeEnvWithImplicits :: [TyThing] -> TypeEnv
mkTypeEnvWithImplicits things =
  mkTypeEnv things
    `plusTypeEnv`
  mkTypeEnv (concatMap implicitTyThings things)

plusTypeEnv :: TypeEnv -> TypeEnv -> TypeEnv
plusTypeEnv (TypeEnv t1) (TypeEnv t2) = TypeEnv (t1 `plusNameEnv` t2)

typeEnvFromEntities :: [Id] -> [TyCon] -> [PatSyn] -> [FamInst] -> TypeEnv
typeEnvFromEntities ids tcs patsyns famInsts =
  mkTypeEnv (   map AnId ids
             ++ map ATyCon all_tcs
             ++ concatMap implicitTyConThings all_tcs
             ++ map (ACoAxiom . toBranchedAxiom . famInstAxiom) famInsts
             ++ map (AConLike . PatSynCon) patsyns
            )
 where
  all_tcs = tcs ++ famInstsRepTyCons famInsts

lookupTypeEnv (TypeEnv t) = lookupNameEnv t

-- Extend the type environment
extendTypeEnv :: TypeEnv -> TyThing -> TypeEnv
extendTypeEnv (TypeEnv env) thing = TypeEnv (extendNameEnv env (getName thing) thing)

extendTypeEnvList :: TypeEnv -> [TyThing] -> TypeEnv
extendTypeEnvList env things = (foldl' extendTypeEnv env things)

extendTypeEnvWithIds :: TypeEnv -> [Id] -> TypeEnv
extendTypeEnvWithIds (TypeEnv env) ids
  = TypeEnv (extendNameEnvList env [(getName id, AnId id) | id <- ids])

instance Outputable TypeEnv where
  ppr (TypeEnv t) = ppr t

