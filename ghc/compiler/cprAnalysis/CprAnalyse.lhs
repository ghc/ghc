\section[CprAnalyse]{Identify functions that always return a
constructed product result}

\begin{code}
module CprAnalyse ( cprAnalyse ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_D_verbose_core2core, opt_D_dump_cpranal )
import CoreLint		( beginPass, endPass )
import CoreSyn
import CoreUtils	( coreExprType )
import CoreUnfold	( maybeUnfoldingTemplate )
import Var		( Var, Id, TyVar, idType, varName, varType )
import Id               ( setIdCprInfo, getIdCprInfo, getIdUnfolding )
import IdInfo           ( CprInfo(..) )
import VarEnv
import Type             ( Type(..), splitFunTys, splitForAllTys, splitNewType_maybe ) 
import TyCon            ( isProductTyCon, isNewTyCon, isUnLiftedTyCon )
import DataCon          ( dataConTyCon, splitProductType_maybe )
import Const            ( Con(DataCon), isWHNFCon )
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

We must also ensure that the function's body starts with sufficient
manifest lambdas otherwise loss of sharing can occur.  See the comment
in @StrictAnal.lhs@.

The transformation of bindings to worker/wrapper pairs is done by the
worker-wrapper pass.  The worker-wrapper pass splits bindings on the
basis of both strictness and CPR info.  If an id has both then it can
combine the transformations so that only one pair is produced.

Data types
~~~~~~~~~~

Within this module Id's CPR information is represented by
``AbsVal''. When adding this information to the Id's pragma info field 
we convert the Absval to a ``CprInfo'' value.  The two are almost
isomorphic, CprInfo doesn't have a represenation for Bot.

Abstract domains consist of a `no information' value (Top) and
for tuple types, a corresponding length tuple of abstract values.
Bot is not a proper abstract value but a generic bottom is
useful for calculating fixpoints.

Since functions abstract to constant functions we can just
represent their result.  It is not necessary to model functions
directly.  This is more efficient,  but unfortunately it both
simplifies and pbscures the code in places.

\begin{code}
data AbsVal = Top                -- Not a constructed product
            | Tuple [AbsVal]     -- A constructed product of values
            | Bot                -- Bot'tom included for convenience
                                 -- we could use appropriate Tuple Vals
     deriving Show

-- For pretty debugging
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

\end{code}

The environment maps Ids to their abstract CPR value.

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
	= snd $ foldl analBind (initCPREnv, []) binds
        where
        analBind :: (CPREnv, [CoreBind]) -> CoreBind -> (CPREnv, [CoreBind])
	analBind (rho,done_binds) bind 
	    = (extendVarEnvList rho env, done_binds ++ [bind'])
	      where
	      (env, bind') = cprAnalTopBind rho bind

\end{code}

The cprAnal functions take binds/expressions and an environment which 
gives CPR info for visible ids and returns a new bind/expression
with ids decorated with their CPR info.
 
\begin{code}
-- Return environment updated with info from this binding 
cprAnalTopBind :: CPREnv -> CoreBind -> ([(Var, AbsVal)], CoreBind)
cprAnalTopBind rho (NonRec v e) 
    = ([(v', e_absval')], NonRec v' e_pluscpr)
      where
      (e_pluscpr, e_absval) = cprAnalExpr rho e
      (v', e_absval')       = pinCPR v e e_absval

-- When analyzing mutually recursive bindings the iterations to find
-- a fixpoint is bounded by the number of bindings in the group.
-- for simplicity we just iterate that number of times.      
cprAnalTopBind rho (Rec bounders) 
    = (map (\(b,e) -> (b, lookupVarEnv_NF fin_rho b)) fin_bounders',
       Rec fin_bounders')
      where
      init_rho = rho `extendVarEnvList`  (zip binders (repeat Bot))
      binders = map fst bounders

      (fin_rho, fin_bounders) = ntimes (length bounders) 
				       do_one_pass 
				       (init_rho, bounders)
      fin_bounders' = map (\(b,e) -> (fst $ pinCPR b e (lookupVarEnv_NF fin_rho b), e))
                      fin_bounders

cprAnalExpr :: CPREnv -> CoreExpr -> (CoreExpr, AbsVal)

-- Check in rho,  if not there it must be imported, so check 
-- the var's idinfo. 
cprAnalExpr rho e@(Var v) 
    = (e, case lookupVarEnv rho v of
            Just a_val -> a_val
            Nothing    -> getCprPragInfo v)
    where
    getCprPragInfo v = let ids_inf = (cprInfoToAbs . getIdCprInfo) v in
                         case ids_inf of
                         Top -> -- if we can inline this var,  then
            		      -- analyse the unfolding
                              case (maybeUnfoldingTemplate.getIdUnfolding) v of
                                Just e ->  if isCon e then snd $ cprAnalExpr rho e 
					   else ids_inf
                                zz_other -> ids_inf
                         zz_other -> ids_inf

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
-- Return the abstract value of the body, since functions 
-- are represented by the CPR value of their result.
cprAnalExpr rho (Lam b body) 
    = (Lam b body_cpr, body_aval)
      where 
      (body_cpr, body_aval) = cprAnalExpr (extendVarEnv rho b Top) body

cprAnalExpr rho (Let (NonRec binder rhs) body) 
    = (Let (NonRec binder' rhs_cpr) body_cpr, body_aval)
      where 
      (rhs_cpr, rhs_aval) = cprAnalExpr rho rhs
      (binder', rhs_aval') = pinCPR binder rhs_cpr rhs_aval
      (body_cpr, body_aval) = cprAnalExpr (extendVarEnv rho binder rhs_aval') body

cprAnalExpr rho (Let (Rec bounders) body) 
    = (Let (Rec fin_bounders) body_cpr, body_aval) 
      where 
      (rhs_rho, fin_bounders) = ntimes 
				(length bounders) 
				do_one_pass 
				(init_rho, bounders)

      (body_cpr, body_aval) = cprAnalExpr rhs_rho  body

      init_rho = rho `extendVarEnvList` zip binders (repeat Bot)
      binders = map fst bounders


cprAnalExpr rho (Case scrut bndr alts)
    = (Case scrut_cpr bndr alts_cpr, alts_aval)
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
    = foldl anal_bind (i_rho, []) bounders
       where
         anal_bind (c_rho, done) (b,e) = (modifyVarEnv (const e_absval') c_rho b, 
					  done ++ [(b,e')])
              where (e', e_absval) = cprAnalExpr c_rho e
                    e_absval' = snd (pinCPR b e e_absval)                     


-- take a binding pair and the abs val calculated from the rhs and
-- calculate a new absval taking into account sufficient manifest
-- lambda condition and that product arguments must be non-functional
-- to have CPR property.  
-- Also we pin the var's CPR property to it.  This only has the CPR property if
-- its a function

pinCPR :: Var -> CoreExpr -> AbsVal -> (Var, AbsVal)
pinCPR v e av = case av of
		Tuple _ -> 
                    -- v is function with sufficent lambdas?
                    if v_is_fn then
		       if {- pprTrace "pinCPR:" (ppr v <+> text "type args:" <+>
					      ppr argtys <+> text "lambda bound vars" <+> 
					      ppr val_binders) -} (length argtys == length val_binders) then
			  (addCpr av, av)
		       else (addCpr Top, Top)
                    else
                      -- not a function.
                      -- Pin NoInfo to v. If v appears in the interface file then an 
		      -- importing module will check to see if it has an unfolding
		      -- with a constructor at its head.  If it does it will re-analyse
                      -- the folding.  I could do the check here, but I don't know if
                      -- the current unfolding info is final. 
		      (addCpr Top,
                       -- OK, not a function but retain CPR info if it has a constructor
                       -- at its head, and thus will be inlined and simplified by
                       -- case of a known constructor
		       if isCon e then
		         -- Need to filter out functions from nested results
                         filterAbsTuple (av, v_type)
                       else Top)
		_ -> (addCpr av, av)
    where
    -- func to pin CPR info on a var
    addCpr :: AbsVal -> Var
    addCpr = (setIdCprInfo v).absToCprInfo
    v_type = varType v
    -- Split argument types and result type from v's type
    (_, argtys, zz_result_type) = splitTypeToFunArgAndRes v_type
    v_is_fn = argtys /= []
    -- val_binders are the explicit lambdas at the head of the expression
    (binders,zz_stripped_exp) = collectBinders e
    val_binders = filter (not.isTyVar) binders

filterAbsTuple :: (AbsVal, Type) -> AbsVal
filterAbsTuple (av@(Tuple args), ty) 
  = case splitProductType_maybe ty of
      Nothing -> WARN( True, text "filterAbsTuple" <+> ppr ty)	-- Or should it be a panic?
		 Top		
      Just (tycon, _, data_con, inst_con_arg_tys)
          |  isNewTyCon tycon 
          -> ASSERT ( null $ tail inst_con_arg_tys )
             filterAbsTuple (av, head inst_con_arg_tys)
          |  otherwise
          -> Tuple $ map filterAbsTuple $ zipEqual "cprFilter" args inst_con_arg_tys  

filterAbsTuple (av, _) = av

absToCprInfo :: AbsVal -> CprInfo
absToCprInfo (Tuple args) = CPRInfo $ map absToCprInfo args 
absToCprInfo _ = NoCPRInfo

cprInfoToAbs :: CprInfo -> AbsVal
cprInfoToAbs NoCPRInfo = Top
cprInfoToAbs (CPRInfo args) = Tuple $ map cprInfoToAbs args

\end{code}

%************************************************************************
%*									*
\subsection{Utilities}
%*									*
%************************************************************************


Now we define a couple of functions that split up types, they should
be moved to Type.lhs if it is agreed that they are doing something
that is sensible.

\begin{code}

-- Split a function type into forall tyvars, argument types and result type.
-- If the type isn't a function type then tyvars and argument types will be
-- empty lists.

-- Experimental,  look through new types.  I have given up on this for now,
-- if the target of a function is a new type which is a function (see monadic
-- functions for examples) we could look into these.  However,  it turns out that 
-- the (necessary) coercions in the code stop the beneficial simplifications.
splitTypeToFunArgAndRes :: Type -> ([TyVar], [Type], Type) 
splitTypeToFunArgAndRes ty = (tyvars, argtys, resty)
    where (tyvars, funty) = splitForAllTys ty
          (argtys, resty) = splitFunTysIgnoringNewTypes funty
--          (argtys, resty) = splitFunTys funty

-- Taken from splitFunTys in Type.lhs.  Modified to keep searching through newtypes
-- Should move to Type.lhs if it is doing something sensible.
splitFunTysIgnoringNewTypes :: Type -> ([Type], Type)
splitFunTysIgnoringNewTypes ty = split ty
  where
    split ty = case splitNewType_maybe res of
		 Nothing     -> (args, res)
		 Just rep_ty -> (args ++ args', res')
			     where
				(args', res') = split rep_ty
	     where
		(args, res) = splitFunTys ty

-- Is this the constructor for a product type (i.e. algebraic, single constructor) 
-- NB: isProductTyCon replies 'False' for unboxed tuples
isConProdType :: Con -> Bool
isConProdType (DataCon con) = isProductTyCon tycon
			    where
			      tycon = dataConTyCon con
isConProdType _ = False

-- returns True iff head of expression is a constructor
-- Should I look through notes?
isCon :: CoreExpr -> Bool
isCon (Con c _) = isWHNFCon c  -- is this the right test?
isCon _         = False
\end{code}

\begin{code}
-- Compose a function with itself n times.  This must be in a library
-- somewhere,  but where!
ntimes :: Int -> (a -> a) -> (a -> a)
ntimes 0 f = id
ntimes 1 f = f
ntimes n f = f . ntimes (n-1) f

\end{code}
