
module Vectorise.Type.Classify
	( TyConGroup
	, classifyTyCons
	, tyConGroups)
where
import UniqSet
import UniqFM
import DataCon
import TyCon
import TypeRep
import Type
import Digraph
import Outputable

type TyConGroup = ([TyCon], UniqSet TyCon)

-- | Split the given tycons into two sets depending on whether they have to be
--   converted (first list) or not (second list). The first argument contains
--   information about the conversion status of external tycons:
--
--   * tycons which have converted versions are mapped to True
--   * tycons which are not changed by vectorisation are mapped to False
--   * tycons which can't be converted are not elements of the map
--
classifyTyCons 
	:: UniqFM Bool
	-> [TyConGroup]
	-> ([TyCon], [TyCon])

classifyTyCons = classify [] []
  where
    classify conv keep _  [] = (conv, keep)
    classify conv keep cs ((tcs, ds) : rs)
      | can_convert && must_convert
        = classify (tcs ++ conv) keep (cs `addListToUFM` [(tc,True) | tc <- tcs]) rs
      | can_convert
        = classify conv (tcs ++ keep) (cs `addListToUFM` [(tc,False) | tc <- tcs]) rs
      | otherwise
        = classify conv keep cs rs
      where
        refs = ds `delListFromUniqSet` tcs

        can_convert  = isNullUFM (refs `minusUFM` cs) && all convertable tcs
        must_convert = foldUFM (||) False (intersectUFM_C const cs refs)

        convertable tc = isDataTyCon tc && all isVanillaDataCon (tyConDataCons tc)


-- | Compute mutually recursive groups of tycons in topological order
tyConGroups :: [TyCon] -> [TyConGroup]
tyConGroups tcs = map mk_grp (stronglyConnCompFromEdgedVertices edges)
  where
    edges = [((tc, ds), tc, uniqSetToList ds) | tc <- tcs
                                , let ds = tyConsOfTyCon tc]

    mk_grp (AcyclicSCC (tc, ds)) = ([tc], ds)
    mk_grp (CyclicSCC els)       = (tcs, unionManyUniqSets dss)
      where
        (tcs, dss) = unzip els


-- | Collect the set of TyCons used by the representation of some data type.
tyConsOfTyCon :: TyCon -> UniqSet TyCon
tyConsOfTyCon
  = tyConsOfTypes . concatMap dataConRepArgTys . tyConDataCons


-- | Collect the set of TyCons that occur in these types.
tyConsOfTypes :: [Type] -> UniqSet TyCon
tyConsOfTypes = unionManyUniqSets . map tyConsOfType


-- | Collect the set of TyCons that occur in this type.
tyConsOfType :: Type -> UniqSet TyCon
tyConsOfType ty
  | Just ty' <- coreView ty    = tyConsOfType ty'
tyConsOfType (TyVarTy _)       = emptyUniqSet
tyConsOfType (TyConApp tc tys) = extend (tyConsOfTypes tys)
  where
    extend |  isUnLiftedTyCon tc
           || isTupleTyCon   tc = id

           | otherwise          = (`addOneToUniqSet` tc)

tyConsOfType (AppTy a b)       = tyConsOfType a `unionUniqSets` tyConsOfType b
tyConsOfType (FunTy a b)       = (tyConsOfType a `unionUniqSets` tyConsOfType b)
                                 `addOneToUniqSet` funTyCon
tyConsOfType (ForAllTy _ ty)   = tyConsOfType ty
tyConsOfType other             = pprPanic "ClosureConv.tyConsOfType" $ ppr other

