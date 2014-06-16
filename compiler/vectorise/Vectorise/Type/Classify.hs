-- Extract from a list of type constructors those (1) which need to be vectorised and (2) those
-- that could be, but need not be vectorised (as a scalar representation is sufficient and more
-- efficient).  The type constructors that cannot be vectorised will be dropped.
--
-- A type constructor will only be vectorised if it is
--
-- (1) a data type constructor, with vanilla data constructors (i.e., data constructors admitted by
--     Haskell 98) and
-- (2) at least one of the type constructors that appears in its definition is also vectorised.
--
-- If (1) is met, but not (2), the type constructor may appear in vectorised code, but there is no
-- need to vectorise that type constructor itself.  This holds, for example, for all enumeration
-- types.  As '([::])' is being vectorised, any type constructor whose definition involves
-- '([::])', either directly or indirectly, will be vectorised.

module Vectorise.Type.Classify 
  ( classifyTyCons
  ) 
where

import NameSet
import UniqSet
import UniqFM
import DataCon hiding (tyConsOfTyCon)
import TyCon
import TypeRep
import Type hiding (tyConsOfType)
import PrelNames
import Digraph


-- |From a list of type constructors, extract those that can be vectorised, returning them in two
-- sets, where the first result list /must be/ vectorised and the second result list /need not be/
-- vectorised. The third result list are those type constructors that we cannot convert (either
-- because they use language extensions or because they dependent on type constructors for which
-- no vectorised version is available).
--
-- NB: In order to be able to vectorise a type constructor, we require members of the depending set
--     (i.e., those type constructors that the current one depends on) to be vectorised only if they
--     are also parallel (i.e., appear in the second argument to the function).
--
-- The first argument determines the /conversion status/ of external type constructors as follows:
--
-- * tycons which have converted versions are mapped to 'True'
-- * tycons which are not changed by vectorisation are mapped to 'False'
-- * tycons which haven't been converted (because they can't or weren't vectorised) are not
--   elements of the map
--
classifyTyCons :: UniqFM Bool                  -- ^type constructor vectorisation status
               -> NameSet                      -- ^tycons involving parallel arrays
               -> [TyCon]                      -- ^type constructors that need to be classified
               -> ( [TyCon]                    -- to be converted
                  , [TyCon]                    -- need not be converted (but could be)
                  , [TyCon]                    -- involve parallel arrays (whether converted or not)
                  , [TyCon]                    -- can't be converted
                  )
classifyTyCons convStatus parTyCons tcs = classify [] [] [] [] convStatus parTyCons (tyConGroups tcs)
  where
    classify conv keep par novect _  _   []               = (conv, keep, par, novect)
    classify conv keep par novect cs pts ((tcs, ds) : rs)
      | can_convert && must_convert
      = classify (tcs ++ conv) keep (par ++ tcs_par) novect (cs `addListToUFM` [(tc, True)  | tc <- tcs]) pts' rs
      | can_convert
      = classify conv (tcs ++ keep) (par ++ tcs_par) novect (cs `addListToUFM` [(tc, False) | tc <- tcs]) pts' rs
      | otherwise
      = classify conv keep (par ++ tcs_par) (tcs ++ novect) cs pts' rs
      where
        refs = ds `delListFromUniqSet` tcs
        
          -- the tycons that directly or indirectly depend on parallel arrays
        tcs_par | any ((`elemNameSet` parTyCons) . tyConName) . eltsUFM $ refs = tcs
                | otherwise                                                    = []

        pts' = pts `addListToNameSet` map tyConName tcs_par

        can_convert  = (isNullUFM (filterUniqSet ((`elemNameSet` pts) . tyConName) (refs `minusUFM` cs)) 
                        && all convertable tcs)
                       || isShowClass tcs
        must_convert = foldUFM (||) False (intersectUFM_C const cs refs)
                       && (not . isShowClass $ tcs)

        -- We currently admit Haskell 2011-style data and newtype declarations as well as type
        -- constructors representing classes.
        convertable tc 
          = (isDataTyCon tc || isNewTyCon tc) && all isVanillaDataCon (tyConDataCons tc)
            || isClassTyCon tc
            
        -- !!!FIXME: currently we allow 'Show' in vectorised code without actually providing a
        --   vectorised definition (to be able to vectorise 'Num')
        isShowClass [tc] = tyConName tc == showClassName
        isShowClass _    = False

-- Used to group type constructors into mutually dependent groups.
--
type TyConGroup = ([TyCon], UniqSet TyCon)

-- Compute mutually recursive groups of tycons in topological order.
--
tyConGroups :: [TyCon] -> [TyConGroup]
tyConGroups tcs = map mk_grp (stronglyConnCompFromEdgedVertices edges)
  where
    edges = [((tc, ds), tc, uniqSetToList ds) | tc <- tcs
                                , let ds = tyConsOfTyCon tc]

    mk_grp (AcyclicSCC (tc, ds)) = ([tc], ds)
    mk_grp (CyclicSCC els)       = (tcs, unionManyUniqSets dss)
      where
        (tcs, dss) = unzip els

-- |Collect the set of TyCons used by the representation of some data type.
--
tyConsOfTyCon :: TyCon -> UniqSet TyCon
tyConsOfTyCon = tyConsOfTypes . concatMap dataConRepArgTys . tyConDataCons

-- |Collect the set of TyCons that occur in these types.
--
tyConsOfTypes :: [Type] -> UniqSet TyCon
tyConsOfTypes = unionManyUniqSets . map tyConsOfType

-- |Collect the set of TyCons that occur in this type.
--
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
tyConsOfType (LitTy _)         = emptyUniqSet
tyConsOfType (ForAllTy _ ty)   = tyConsOfType ty
