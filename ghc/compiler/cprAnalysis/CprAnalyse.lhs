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
import Id               ( setIdCprInfo, getIdCprInfo, getIdUnfolding, getIdArity,
			  isBottomingId )
import IdInfo           ( CprInfo(..), arityLowerBound )
import VarEnv
import Type             ( Type, splitFunTys, splitFunTy_maybe, splitForAllTys, splitNewType_maybe )
import TyCon            ( isProductTyCon, isNewTyCon, isUnLiftedTyCon )
import DataCon          ( dataConTyCon, splitProductType_maybe, dataConRawArgTys )
import Const            ( Con(DataCon), isDataCon, isWHNFCon )
import Util		( zipEqual, zipWithEqual )
import Outputable

import UniqFM (ufmToList)
import Maybe
import PprType( pprType )	-- Only called in debug messages
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

The analysis here detects nested CPR information.  For example, if a
function returns a constructed pair, the first element of which is a
constructed int, then the analysis will detect nested CPR information
for the int as well.  Unfortunately, the current transformations can't
take advantage of the nested CPR information.  They have (broken now,
I think) code which will flatten out nested CPR components and rebuild
them in the wrapper, but enabling this would lose laziness.  It is
possible to make use of the nested info: if we knew that a caller was
strict in that position then we could create a specialized version of
the function which flattened/reconstructed that position.

It is not known whether this optimisation would be worthwhile.

So we generate and carry round nested CPR information, but before
using this info to guide the creation of workers and wrappers we map
all components of a CPRInfo to NoCprInfo.


Data types
~~~~~~~~~~

Within this module Id's CPR information is represented by
``AbsVal''. When adding this information to the Id's pragma info field 
we convert the ``Absval'' to a ``CprInfo'' value.   

Abstract domains consist of a `no information' value (Top), a function
value (Fun) which when applied to an argument returns a new AbsVal
(note the argument is not used in any way), , for product types, a
corresponding length tuple (Tuple) of abstract values.  And finally,
Bot.  Bot is not a proper abstract value but a generic bottom is
useful for calculating fixpoints and representing divergent
computations.  Note that we equate Bot and Fun^n Bot (n > 0), and
likewise for Top.  This saves a lot of delving in types to keep
everything exactly correct.

Since functions abstract to constant functions we could just
represent them by the abstract value of their result.  However,  it
turns out (I know - I tried!) that this requires a lot of type
manipulation and the code is more straightforward if we represent
functions by an abstract constant function. 

\begin{code}
data AbsVal = Top                -- Not a constructed product
	    | Fun AbsVal         -- A function that takes an argument 
				 -- and gives AbsVal as result. 
            | Tuple [AbsVal]     -- A constructed product of values
            | Bot                -- Bot'tom included for convenience
                                 -- we could use appropriate Tuple Vals
     deriving (Eq,Show)

isFun :: AbsVal -> Bool
isFun (Fun _) = True
isFun _       = False

-- For pretty debugging
instance Outputable AbsVal where
  ppr Top    	                = ptext SLIT("Top")
  ppr (Fun r)                   = ptext SLIT("Fun->") <> (parens.ppr) r
  ppr (Tuple la)  	        = ptext SLIT("Tuple ") <> text "[" <> 
                                  (hsep (punctuate comma (map ppr la))) <>
                                  text "]"
  ppr Bot   	                = ptext SLIT("Bot")


-- lub takes the lowest upper bound of two abstract values, standard.
lub :: AbsVal -> AbsVal -> AbsVal
lub Bot a = a
lub a Bot = a
lub Top a = Top
lub a Top = Top
lub (Tuple l) (Tuple r) = Tuple (zipWithEqual "CPR: lub" lub l r)
lub (Fun l) (Fun r)     = Fun (lub l r)
lub l r = panic "CPR Analysis tried to take the lub of a function and a tuple"


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

      (fin_rho, fin_bounders) = nTimes (length bounders) 
				       do_one_pass 
				       (init_rho, bounders)
      fin_bounders' = map (\(b,e) -> (fst $ pinCPR b e (lookupVarEnv_NF fin_rho b), e))
                      fin_bounders

cprAnalExpr :: CPREnv -> CoreExpr -> (CoreExpr, AbsVal)


-- If Id will always diverge when given sufficient arguments then
-- we can just set its abs val to Bot.  Any other CPR info
-- from other paths will then dominate,  which is what we want.
-- Check in rho,  if not there it must be imported, so check 
-- the var's idinfo. 
cprAnalExpr rho e@(Var v) 
    | isBottomingId v = (e, Bot)
    | otherwise       = (e, case lookupVarEnv rho v of
                             Just a_val -> a_val
			     Nothing    -> cpr_prag_a_val)
    where
    ids_inf   = (cprInfoToAbs.getIdCprInfo) v
    ids_arity = (arityLowerBound.getIdArity) v
    cpr_prag_a_val = case ids_inf of
                       Top -> -- if we can inline this var, and its a constructor app
            		      -- then analyse the unfolding
                              case (maybeUnfoldingTemplate.getIdUnfolding) v of
                                Just e | isCon e ->  snd $ cprAnalExpr rho e 
                                zz_other         -> Top
                       zz_other -> -- Unfortunately,  cprinfo doesn't store the # of args
		                   nTimes ids_arity Fun ids_inf

-- Return constructor with decorated arguments.  If constructor 
-- has product type then this is a manifest constructor (hooray!)
cprAnalExpr rho (Con con args)
    = (Con con args_cpr, 
       if isConProdType con
         then Tuple args_aval_filt_funs
         else Top)
    where 
      anal_con_args = map (cprAnalExpr rho) args 
      args_cpr      = map fst anal_con_args

      args_aval_filt_funs = if (not.isDataCon) con then
			       map snd anal_con_args
			    else
			       map (ifApply isFun (const Top)) $ 
			        map snd $ 
				filter (not.isTypeArg.fst) anal_con_args  

-- For apps we don't care about the argument's abs val.  This
-- app will return a constructed product if the function does. We strip
-- a Fun from the functions abs val, unless the argument is a type argument 
-- or it is already Top or Bot.
cprAnalExpr rho (App fun arg@(Type _))
    = (App fun_cpr arg, fun_res)  
      where 
      (fun_cpr, fun_res)  = cprAnalExpr rho fun 

cprAnalExpr rho (App fun arg) 
    = (App fun_cpr arg_cpr, if fun_res==Top || fun_res==Bot 
                            then fun_res 
                            else res_res)
      where 
      (fun_cpr, fun_res)  = cprAnalExpr rho fun 
      (arg_cpr, _)        = cprAnalExpr rho arg
      Fun res_res         = fun_res

-- Map arguments to Top (we aren't constructing them)
-- Return the abstract value of the body, since functions 
-- are represented by the CPR value of their result, and 
-- add a Fun for this lambda..
cprAnalExpr rho (Lam b body) | isTyVar b = (Lam b body_cpr, body_aval)
                             | otherwise = (Lam b body_cpr, Fun body_aval)
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
      (rhs_rho, fin_bounders) = nTimes 
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
    = (Note n exp_cpr, expr_aval)
      where
      (exp_cpr, expr_aval) = cprAnalExpr rho exp

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
-- lambda condition 
-- Also we pin the var's CPR property to it.  A var only has the CPR property if
-- it is a function

pinCPR :: Var -> CoreExpr -> AbsVal -> (Var, AbsVal)
pinCPR v e av = case av of
                    -- is v a function with insufficent lambdas?
                 Fun _ | length argtys /= length val_binders ->  
                      -- argtys must be greater than val_binders.  So stripped_exp
		      -- has a function type.  The head of this expr can't be lambda 
		      -- a note, because we stripped them off before.  It can't be a 
		      -- Con because it has a function type.  It can't be a Type. 
		      -- If its an app, let or case then there is work to get the 
		      -- and we can't do anything because we may lose laziness. *But*
		      -- if its a var (i.e. a function name) then we are fine.  Note 
		      -- that I don't think this case is at all interesting,  but I have
		      -- a test program that generates it.

                      -- UPDATE: 20 Jul 1999
                      -- I've decided not to allow this (useless) optimisation.  It will make
                      -- the w/w split more complex.
		      -- if isVar stripped_exp then
                      --    (addCpr av, av)
		      -- else
			    (addCpr Top, Top)
		 Tuple _ -> 
                      -- not a function.
                      -- Pin NoInfo to v. If v appears in the interface file then an 
		      -- importing module will check to see if it has an unfolding
		      -- with a constructor at its head (WHNF).  If it does it will re-analyse
                      -- the folding.  I could do the check here, but I don't know if
                      -- the current unfolding info is final. 
		      (addCpr Top,
                       -- Retain CPR info if it has a constructor
                       -- at its head, and thus will be inlined and simplified by
                       -- case of a known constructor
		       if isCon e then av else Top)
		 _ -> (addCpr av, av)
    where
    -- func to pin CPR info on a var
    addCpr :: AbsVal -> Var
    addCpr = (setIdCprInfo v).absToCprInfo

    -- Split argument types and result type from v's type
    (_, argtys, _) = (splitTypeToFunArgAndRes.varType) v 

    -- val_binders are the explicit lambdas at the head of the expression
    (_, val_binders, _) = collectTyAndValBinders e -- collectBindersIgnoringNotes e'


absToCprInfo :: AbsVal -> CprInfo
absToCprInfo (Tuple args) = CPRInfo $ map absToCprInfo args 
absToCprInfo (Fun r)      = absToCprInfo r
absToCprInfo _            = NoCPRInfo

-- Cpr Info doesn't store the number of arguments a function has,  so the caller
-- must take care to add the appropriate number of Funs.
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

-- splitFunTys, modified to keep searching through newtypes.
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
isConProdType (DataCon con) = isProductTyCon . dataConTyCon $ con 
isConProdType _ = False

-- returns True iff head of expression is a constructor
-- Should I look through notes? I think so ...
isCon :: CoreExpr -> Bool
isCon (Con c _) = isWHNFCon c  -- is this the right test?
isCon (Note _ e) = isCon e
isCon _         = False

-- Compose a function with itself n times.  (nth rather than twice)
-- This must/should be in a library somewhere,  but where!
nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f

-- Only apply f to argument if it satisfies p
ifApply :: (a -> Bool) -> (a -> a) -> (a -> a)
ifApply p f x = if p x then f x else x

\end{code}
