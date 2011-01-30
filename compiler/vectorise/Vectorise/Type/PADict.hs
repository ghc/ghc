
module Vectorise.Type.PADict
	(buildPADict)
where
import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Type.Repr
import Vectorise.Type.PRepr
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
import FastString
-- import Outputable

-- debug		= False
-- dtrace s x	= if debug then pprTrace "Vectoris.Type.PADict" s x else x

-- | Build the PA dictionary for some type and hoist it to top level.
--   The PA dictionary holds fns that convert values to and from their vectorised representations.
buildPADict
	:: TyCon	-- ^ tycon of the type being vectorised.
	-> TyCon	-- ^ tycon of the type used for the vectorised representation.
	-> TyCon	-- ^ PRepr instance tycon
	-> SumRepr	-- ^ representation used for the type being vectorised.
	-> VM Var	-- ^ name of the top-level dictionary function.

buildPADict vect_tc prepr_tc arr_tc repr
 = polyAbstract tvs $ \args ->
   do
      -- The superclass dictionary is an argument if the tycon is polymorphic
      let mk_super_ty = do
                          r <- mkPReprType inst_ty
                          pr_cls <- builtin prClass
                          return $ PredTy $ ClassP pr_cls [r]
      super_tys <- sequence [mk_super_ty | not (null tvs)]
      super_args <- mapM (newLocalVar (fsLit "pr")) super_tys
      let args' = super_args ++ args

      -- it is constant otherwise
      super_consts <- sequence [prDictOfPReprInstTyCon inst_ty prepr_tc []
                                                | null tvs]

      -- Get ids for each of the methods in the dictionary.
      method_ids <- mapM (method args') paMethods

      -- Expression to build the dictionary.
      pa_dc  <- builtin paDataCon
      let dict = mkLams (tvs ++ args')
               $ mkConApp pa_dc
               $ Type inst_ty
               : map Var super_args ++ super_consts
                                   -- the superclass dictionary is
                                   -- either lambda-bound or
                                   -- constant
                 ++ map (method_call args') method_ids

      -- Build the type of the dictionary function.
      pa_cls <- builtin paClass
      let dfun_ty	= mkForAllTys tvs
			$ mkFunTys (map varType args')
                                   (PredTy $ ClassP pa_cls [inst_ty])

      -- Set the unfolding for the inliner.
      raw_dfun <- newExportedVar dfun_name dfun_ty
      let dfun_unf = mkDFunUnfolding dfun_ty
                   $ map (const $ DFunLamArg 0) super_args
                     ++ map DFunConstArg super_consts
                     ++ map (DFunPolyArg . Var) method_ids
          dfun = raw_dfun `setIdUnfolding`  dfun_unf
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
paMethods = [("toPRepr",      buildToPRepr),
             ("fromPRepr",    buildFromPRepr),
             ("toArrPRepr",   buildToArrPRepr),
             ("fromArrPRepr", buildFromArrPRepr)]

