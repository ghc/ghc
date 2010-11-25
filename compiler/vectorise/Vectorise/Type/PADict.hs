
module Vectorise.Type.PADict
	(buildPADict)
where
import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Type.Repr
import Vectorise.Type.PRepr
import Vectorise.Type.PRDict
import Vectorise.Utils

import BasicTypes
import CoreSyn
import CoreUtils
import CoreUnfold
import TyCon
import Type
import TypeRep
import Id
import Var
import Name
import Class
import Outputable

-- debug		= False
-- dtrace s x	= if debug then pprTrace "Vectoris.Type.PADict" s x else x

-- | Build the PA dictionary for some type and hoist it to top level.
--   The PA dictionary holds fns that convert values to and from their vectorised representations.
buildPADict
	:: TyCon	-- ^ tycon of the type being vectorised.
	-> TyCon	-- ^ tycon of the type used for the vectorised representation.
	-> TyCon	-- 
	-> SumRepr	-- ^ representation used for the type being vectorised.
	-> VM Var	-- ^ name of the top-level dictionary function.

buildPADict vect_tc prepr_tc arr_tc repr
 = polyAbstract tvs $ \args ->
 case args of
  (_:_) -> pprPanic "Vectorise.Type.PADict.buildPADict" (text "why do we need superclass dicts?")
  [] -> do
      -- TODO: I'm forcing args to [] because I'm not sure why we need them.
      --       class PA has superclass (PR (PRepr a)) but we're not using
      --       the superclass dictionary to build the PA dictionary.

      -- Get ids for each of the methods in the dictionary.
      method_ids <- mapM (method args) paMethods

      -- Expression to build the dictionary.
      pa_dc  <- builtin paDataCon
      let dict = mkLams (tvs ++ args)
               $ mkConApp pa_dc
               $ Type inst_ty : map (method_call args) method_ids

      -- Build the type of the dictionary function.
      pa_tc          <- builtin paTyCon
      let pa_opitems = [(id, NoDefMeth) | id <- method_ids]
      let pa_cls     = mkClass 
			(tyConName pa_tc)
			tvs		-- tyvars of class
			[]		-- fundeps
			[]		-- superclass predicates
			0		-- number of equalities
			[]		-- superclass dict selectors
			[]		-- associated type families
			pa_opitems	-- class op items
			pa_tc		-- dictionary type constructor
			
      let dfun_ty = mkForAllTys tvs
                  $ mkFunTys (map varType args) (PredTy $ ClassP pa_cls [inst_ty])

      -- Set the unfolding for the inliner.
      raw_dfun <- newExportedVar dfun_name dfun_ty
      let dfun = raw_dfun `setIdUnfolding`  mkDFunUnfolding dfun_ty (map Var method_ids)
                          `setInlinePragma` dfunInlinePragma

      -- Add the new binding to the top-level environment.
      hoistBinding dfun dict
      return dfun
  where
    tvs       = tyConTyVars vect_tc
    arg_tys   = mkTyVarTys tvs
    inst_ty   = mkTyConApp vect_tc arg_tys

    dfun_name = mkPADFunOcc (getOccName vect_tc)

    method args (name, build)
      = localV
      $ do
          expr     <- build vect_tc prepr_tc arr_tc repr
          let body = mkLams (tvs ++ args) expr
          raw_var  <- newExportedVar (method_name name) (exprType body)
          let var  = raw_var
                      `setIdUnfolding` mkInlineUnfolding (Just (length args)) body
                      `setInlinePragma` alwaysInlinePragma
          hoistBinding var body
          return var

    method_call args id = mkApps (Var id) (map Type arg_tys ++ map Var args)
    method_name name    = mkVarOcc $ occNameString dfun_name ++ ('$' : name)


paMethods :: [(String, TyCon -> TyCon -> TyCon -> SumRepr -> VM CoreExpr)]
paMethods = [("dictPRepr",    buildPRDict),
             ("toPRepr",      buildToPRepr),
             ("fromPRepr",    buildFromPRepr),
             ("toArrPRepr",   buildToArrPRepr),
             ("fromArrPRepr", buildFromArrPRepr)]

