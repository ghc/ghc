module GHC.Types.TypeEnv
   ( TypeEnv
   , emptyTypeEnv
   , lookupTypeEnv
   , restoreSourceTableId
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
import GHC.Types.Id.Info (IdDetails(..), SourceDFun(sdf_owner))
import GHC.Types.TyThing

-- | A map from 'Name's to 'TyThing's, constructed by typechecking
-- local declarations or interface files
type TypeEnv = NameEnv TyThing

emptyTypeEnv    :: TypeEnv
typeEnvElts     :: TypeEnv -> [TyThing]
typeEnvTyCons   :: TypeEnv -> [TyCon]
typeEnvCoAxioms :: TypeEnv -> [CoAxiom Branched]
typeEnvIds      :: TypeEnv -> [Id]
typeEnvPatSyns  :: TypeEnv -> [PatSyn]
typeEnvDataCons :: TypeEnv -> [DataCon]
typeEnvClasses  :: TypeEnv -> [Class]
lookupTypeEnv   :: TypeEnv -> Name -> Maybe TyThing

emptyTypeEnv        = emptyNameEnv
typeEnvElts     env = nonDetNameEnvElts env
typeEnvTyCons   env = [tc | ATyCon tc   <- typeEnvElts env]
typeEnvCoAxioms env = [ax | ACoAxiom ax <- typeEnvElts env]
typeEnvIds      env = [id | AnId id     <- typeEnvElts env]
typeEnvPatSyns  env = [ps | AConLike (PatSynCon ps) <- typeEnvElts env]
typeEnvDataCons env = [dc | AConLike (RealDataCon dc) <- typeEnvElts env]
typeEnvClasses  env = [cl | tc <- typeEnvTyCons env,
                            Just cl <- [tyConClass_maybe tc]]

mkTypeEnv :: [TyThing] -> TypeEnv
mkTypeEnv things = extendTypeEnvList emptyTypeEnv things

mkTypeEnvWithImplicits :: [TyThing] -> TypeEnv
mkTypeEnvWithImplicits things =
  mkTypeEnv things
    `plusNameEnv`
  mkTypeEnv (concatMap implicitTyThings things)

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

lookupTypeEnv = lookupNameEnv

-- | Restore the semantic details of a checked source binder after desugaring.
-- Preserve the occurrence's own type, multiplicity and optimisation information.
-- See Note [Source table Ids] in GHC.Tc.Gen.SourceTable.
restoreSourceTableId :: TypeEnv -> Id -> Id
restoreSourceTableId types fun
  | isLocalId fun, isExternalName (getName fun)
  , Just (AnId owner) <- lookupTypeEnv types (getName fun)
  , let details = idDetails owner
  , case details of
      SourceTableId name _ _     -> name == getName fun
      DFunId _ (Just source)     -> sdf_owner source == getName fun
      _                          -> False
  = setIdDetails fun details
  | otherwise = fun

-- Extend the type environment
extendTypeEnv :: TypeEnv -> TyThing -> TypeEnv
extendTypeEnv env thing = extendNameEnv env (getName thing) thing

extendTypeEnvList :: TypeEnv -> [TyThing] -> TypeEnv
extendTypeEnvList env things = foldl' extendTypeEnv env things

extendTypeEnvWithIds :: TypeEnv -> [Id] -> TypeEnv
extendTypeEnvWithIds env ids
  = extendNameEnvList env [(getName id, AnId id) | id <- ids]

plusTypeEnv :: TypeEnv -> TypeEnv -> TypeEnv
plusTypeEnv env1 env2 = plusNameEnv env1 env2
