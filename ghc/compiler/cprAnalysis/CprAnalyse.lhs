\section[CprAnalyse]{Identify functions that always return a
constructed product result}

\begin{code}
module CprAnalyse ( cprAnalyse ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_D_verbose_core2core, opt_D_dump_cpranal )
import CoreLint		( beginPass, endPass )
import CoreSyn
import CoreUtils	( coreExprType )
import Var		( Var, Id, TyVar, idType, varName, varType )
import Id               ( setIdCprInfo, getIdCprInfo )
import IdInfo           ( CprInfo(..) )
import VarEnv
import Type             ( Type, splitFunTys, splitForAllTys, splitTyConApp_maybe,
                          splitAlgTyConApp_maybe ) 
import TyCon            ( maybeTyConSingleCon, isProductTyCon, isNewTyCon )
import DataCon          ( dataConTyCon, dataConArgTys )
import Const
import Util		( zipEqual, zipWithEqual )
import Outputable

import UniqFM (ufmToList)

\end{code}

This module performs an analysis of a set of Core Bindings for the
Constructed Product Result (CPR) transformation.  

It detects functions that always explicitly (manifestly?) construct a
result value with a product type.  A product type is a type which has
only one constructor. For example, tuples and boxed primitive values
have product type.

We must also ensure that the function's body starts with sufficient manifest
lambdas otherwise loss of sharing can occur.  See the comment in  
@StrictAnal.lhs@
 
The transformation of bindings to worker/wrapper pairs is done by the
worker-wrapper pass.  The worker-wrapper pass splits bindings on the basis
of both strictness and CPR info.  If an id has both then it can combine
the transformations so that only one pair is produced.

Data types
~~~~~~~~~~

Abstract domains consist of a `no information' value (Top) and
for tuple types, a corresponding length tuple of abstract values.
Bot is not a proper abstract value but a generic bottom is
required for calculating fixpoints.
Since functions abstract to constant functions we can just
represent their result.  It is not necessary to model functions
directly.

\begin{code}
data AbsVal = Top                -- Not a constructed product
            | Tuple [AbsVal]     -- A constructed product of values
            | Bot
     deriving Show

instance Outputable AbsVal where
  ppr Top    	                = ptext SLIT("Top")
  ppr (Tuple la)  	        = ptext SLIT("Tuple ") <> text "[" <> 
                                  (hsep (punctuate comma (map ppr la))) <>
                                  text "]"
  ppr Bot   	                = ptext SLIT("Bot")

lub :: AbsVal -> AbsVal -> AbsVal
lub Bot a = a
lub a Bot = a
lub Top a = Top
lub a Top = Top
lub (Tuple l) (Tuple r) = Tuple (zipWithEqual "CPR: lub" lub l r)
lub l r      = pprPanic "CPR lub:" $ hsep [ppr l, ppr r] 

\end{code}

\begin{code}

type CPREnv = VarEnv AbsVal

initCPREnv = emptyVarEnv

\end{code}

Programs
~~~~~~~~

Take a list of core bindings and return a new list with CPR function
ids decorated with their CprInfo pragmas. 

\begin{code}

cprAnalyse :: [CoreBind] 
	         -> IO [CoreBind]
cprAnalyse binds
  = do {
	beginPass "Constructed Product analysis" ;
	let { binds_plus_cpr = do_prog binds } ;
	endPass "Constructed Product analysis" 
	 	(opt_D_dump_cpranal || opt_D_verbose_core2core)
		binds_plus_cpr
    }
  where
    do_prog :: [CoreBind] -> [CoreBind]
    do_prog binds
	= fin_binds
	where
	(fin_cprenv, fin_binds) 
	    = foldl cprAnalBinds (initCPREnv, []) binds
        
        cprAnalBinds :: (CPREnv, [CoreBind]) -> CoreBind -> (CPREnv, [CoreBind])
	cprAnalBinds (rho,done_binds) bind 
	    = (rho', done_binds ++ [bind'])
	      where
	      bind' = cprAnalBind rho bind
	      -- Need to add CPR info to the environment for the top level
	      -- vars we just processed.  It seems a waste to go back in
	      -- and transform the decoration back to a absval, but maybe its
	      -- not so bad ....
	      rho'  = addTopBindsInfo rho bind'

              addTopBindsInfo :: CPREnv -> CoreBind -> CPREnv
	      addTopBindsInfo rho (NonRec v e)
		  = extendVarEnv rho v $ ( cprInfoToAbs . getIdCprInfo ) v
	      addTopBindsInfo rho (Rec bounders)
		  = extendVarEnvList rho $ map (\(v,e) -> 
		                                   (v, (cprInfoToAbs . getIdCprInfo) v))
	                                       bounders 
\end{code}

The cprAnal functions take binds/expressions and an environment which 
gives CPR info for visible ids and returns a new bind/expression
with ids decorated with their CPR info.
 
\begin{code}
-- Return environment updated with info from this binding 
cprAnalBind :: CPREnv -> CoreBind -> CoreBind
cprAnalBind rho (NonRec v e) 
    = NonRec (addCpr v e_pluscpr e_absval) e_pluscpr 
      where
      (e_pluscpr, e_absval) = cprAnalExpr rho e

-- When analyzing mutually recursive bindings the iterations to find
-- a fixpoint is bounded by the number of bindings in the group.
-- for simplicity we just iterate that number of times.      
cprAnalBind rho (Rec bounders) 
    = Rec (map (addRecBindsInfo fin_rho) fin_bounders)
      where
      init_rho = rho `extendVarEnvList` 
		 (zip (map fst bounders) (repeat Bot))
      (fin_rho, fin_bounders) = ntimes (length bounders) 
				       do_one_pass 
				       (init_rho, bounders)

-- Updates a binder's CprInfo 
addRecBindsInfo :: CPREnv -> (CoreBndr, CoreExpr) -> (CoreBndr, CoreExpr)
addRecBindsInfo rho (b,e)
    = (addCpr b e (lookupVarEnv_NF rho b), e)


cprAnalExpr :: CPREnv -> CoreExpr -> (CoreExpr, AbsVal)

-- Check in rho,  if not there it must be imported, so check the var's idinfo
cprAnalExpr rho e@(Var v) 
    = (e, case lookupVarEnv rho v of
            Just a_val -> a_val
            Nothing    -> (cprInfoToAbs . getIdCprInfo) v)

-- Return constructor with decorated arguments.  If constructor 
-- has product type then this is a manifest constructor (hooray!)
cprAnalExpr rho (Con con args)
    = (Con con args_cpr, 
       -- Don't need to do this here,  since we will filter out later
       -- but it isn't expensive and will reduce returned abs vals.
       if isConProdType con 
         then Tuple args_avals
         else Top)
    where 
    (args_cpr, args_avals) = foldl anal_arg ([], []) args

    anal_arg :: ([CoreExpr], [AbsVal]) -> CoreExpr -> ([CoreExpr], [AbsVal])
    anal_arg (done_args, avs) arg 
	| isValArg arg = cprAnalExpr rho arg `end_cons` (done_args, avs)
	| otherwise = (done_args ++ [arg], avs)
	where
	end_cons :: (a,b) -> ([a],[b]) -> ([a],[b])
	end_cons (x,y) (xs,ys) = (xs ++ [x], ys ++ [y])

-- For apps we ignore the argument.  This app will return a constructed
-- product if the function does (we check that result type is not a fn when
-- we come to decorate a binder).
cprAnalExpr rho (App fun arg) 
    = (App fun_cpr arg_cpr, res_aval)
      where 
      (fun_cpr, res_aval) = cprAnalExpr rho fun 
      (arg_cpr, arg_aval) = cprAnalExpr rho arg

-- Map arguments to Top (we aren't constructing them)
cprAnalExpr rho (Lam b body) 
    = (Lam b body_cpr, body_aval)
      where 
      (body_cpr, body_aval) = cprAnalExpr (extendVarEnv rho b Top) body

cprAnalExpr rho (Let (NonRec binder rhs) body) 
    = (Let (NonRec (addCpr binder rhs_cpr rhs_aval) rhs_cpr) body_cpr, body_aval)
      where 
      (rhs_cpr, rhs_aval) = cprAnalExpr rho rhs
      (body_cpr, body_aval) = cprAnalExpr (extendVarEnv rho binder rhs_aval) body

cprAnalExpr rho (Let (Rec bounders) body) 
    = (Let (Rec $ map (addRecBindsInfo rhs_rho) fin_bounders) body_cpr, body_aval) 
      where 
      (rhs_rho, fin_bounders) = ntimes 
				(length bounders) 
				do_one_pass 
				(init_rho, bounders)
      (body_cpr, body_aval) = cprAnalExpr rhs_rho  body

      init_rho = rho `extendVarEnvList` 
		 zip (map fst bounders) (repeat Bot)

cprAnalExpr rho (Case scrut bndr alts)
    = (Case scrut_cpr (addCpr bndr scrut_cpr scrut_aval) alts_cpr, alts_aval)
      where 
      (scrut_cpr, scrut_aval) = cprAnalExpr rho scrut
      (alts_cpr, alts_aval) = cprAnalCaseAlts (extendVarEnv rho bndr scrut_aval) alts

cprAnalExpr rho (Note n exp) 
    = (Note n exp_cpr, note_aval)
      where
      (exp_cpr, note_aval) = cprAnalExpr rho exp

cprAnalExpr rho (Type t) 
    = (Type t, Top)


cprAnalCaseAlts :: CPREnv -> [CoreAlt] -> ([CoreAlt], AbsVal)
cprAnalCaseAlts rho alts
    = foldl anal_alt ([], Bot) alts
      where 
      anal_alt :: ([CoreAlt], AbsVal) -> CoreAlt -> ([CoreAlt], AbsVal)
      anal_alt (done, aval) (con, binds, exp) 
	  = (done ++ [(con,binds,exp_cpr)], aval `lub` exp_aval)
	    where (exp_cpr, exp_aval) = cprAnalExpr rho' exp
		  rho' = rho `extendVarEnvList` (zip binds (repeat Top))


-- Does one analysis pass through a list of mutually recursive bindings.
do_one_pass :: (CPREnv, [(CoreBndr,CoreExpr)]) -> (CPREnv, [(CoreBndr,CoreExpr)])
do_one_pass  (i_rho,bounders)
    = foldl (\(c_rho,done) (b,e) -> 
	          let (e', e_absval) = cprAnalExpr c_rho e in
                       (modifyVarEnv (const e_absval) c_rho b, done ++ [(b,e')])) 
	    (i_rho, []) bounders

cprDecorate :: Id -> AbsVal -> Id
cprDecorate v aval = setIdCprInfo v $ absToCprInfo aval

-- Decorate var with CPR info only if:
--  . It has a CPR value, and
--  . It is a function with correct number of explicit lambdas
--    at the head of its body (so that laziness isn't lost)
addCpr :: Var -> CoreExpr -> AbsVal -> Var
addCpr v e aval
    | isCprVal aval = case argtys of
		      [] -> v
		      _ -> 
			  if length argtys == length val_binders
			     then cprDecorate v $ cprFilter (aval,resty)
			     else v
    | otherwise = v
    where
      (_, argtys, resty) = splitTypeToFunArgAndRes (varType v)
      -- val_binders are the explicit lambdas at the head of the expression
      (_,val_binders,_) = collectTyAndValBinders e

absToCprInfo :: AbsVal -> CprInfo
absToCprInfo (Tuple args) = CPRInfo $ map absToCprInfo args 
absToCprInfo _ = NoCPRInfo

cprInfoToAbs :: CprInfo -> AbsVal
cprInfoToAbs NoCPRInfo = Top
cprInfoToAbs (CPRInfo args) = Tuple $ map cprInfoToAbs args
  
-- If a CPR component is actually a function then map it to NoCPRInfo
cprFilter :: (AbsVal, Type) -> AbsVal
cprFilter (aval@(Tuple args),ty) 
    = case split_ty of
        Nothing -> Top
        Just (data_con, tycon, tycon_arg_tys, inst_con_arg_tys) ->
          if isNewTyCon tycon then
            ASSERT ( null $ tail inst_con_arg_tys )
            cprFilter (aval, head inst_con_arg_tys)
          else 
            Tuple $ map cprFilter $ zipEqual "cprFilter" args inst_con_arg_tys  
    where
	split_ty = case (splitAlgTyConApp_maybe ty) of
      	      Just (arg_tycon, tycon_arg_tys, [data_con]) ->
		    -- The main event: a single-constructor data type
		  Just (data_con, arg_tycon, tycon_arg_tys, dataConArgTys data_con tycon_arg_tys)

	      Just (_, _, data_cons) ->
		   pprPanic ("cprFilter:") 
			    (text "not one constructor"
			    $$ ppr ty)

	      Nothing		->
		  Nothing

cprFilter (v, _)     = v


-- Returns True iff abstract value shows a constructed product
isCprVal :: AbsVal -> Bool
isCprVal (Tuple _) = True
isCprVal _ = False

\end{code}

\begin{code}

-- Split a function type into forall tyvars, argument types and result type.
-- If the type isn't a function type then tyvars and argument types will be
-- empty lists
splitTypeToFunArgAndRes :: Type -> ([TyVar], [Type], Type) 
splitTypeToFunArgAndRes ty = (tyvars, {- pprTrace "splitTypeToFunArgAndRes" (ppr tyvars <> ppr argtys <> ppr resty) -} argtys, resty)
    where (tyvars, funty) = splitForAllTys ty
          (argtys, resty) = splitFunTys funty

-- Is this the constructor for a product type (i.e. algebraic, single constructor) 
isConProdType :: Con -> Bool
isConProdType (DataCon con) = isProductTyCon (dataConTyCon con)
isConProdType _ = False

\end{code}

\begin{code}
-- Compose a function with itself n times.  This must be in a library
-- somewhere,  but where!
ntimes :: Int -> (a -> a) -> (a -> a)
ntimes 0 f = id
ntimes 1 f = f
ntimes n f = f . ntimes (n-1) f

\end{code}
