\section[CprAnalyse]{Identify functions that always return a
constructed product result}

\begin{code}
module CprAnalyse ( cprAnalyse ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_D_verbose_core2core, opt_D_dump_cpranal )
import CoreLint		( beginPass, endPass )
import CoreSyn
import CoreUtils	( exprIsValue )
import CoreUnfold	( maybeUnfoldingTemplate )
import Var		( Var, Id, TyVar, idType, varName, varType )
import Id               ( setIdCprInfo, idCprInfo, idArity,
			  isBottomingId )
import IdInfo           ( CprInfo(..) )
import VarEnv
import Type             ( Type, splitFunTys, splitFunTy_maybe, splitForAllTys )
import TyCon            ( isNewTyCon, isUnLiftedTyCon )
import DataCon          ( dataConTyCon )
import Util		( zipEqual, zipWithEqual, nTimes, mapAccumL )
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

            | Tuple 		 -- A constructed product of values

            | Bot                -- Bot'tom included for convenience
                                 -- we could use appropriate Tuple Vals
     deriving (Eq,Show)

isFun :: AbsVal -> Bool
isFun (Fun _) = True
isFun _       = False

-- For pretty debugging
instance Outputable AbsVal where
  ppr Top    	= ptext SLIT("Top")
  ppr (Fun r)	= ptext SLIT("Fun->") <> (parens.ppr) r
  ppr Tuple     = ptext SLIT("Tuple ")
  ppr Bot       = ptext SLIT("Bot")


-- lub takes the lowest upper bound of two abstract values, standard.
lub :: AbsVal -> AbsVal -> AbsVal
lub Bot a = a
lub a Bot = a
lub Top a = Top
lub a Top = Top
lub Tuple Tuple 	= Tuple
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
    do_prog binds = snd $ mapAccumL cprAnalBind initCPREnv binds
\end{code}

The cprAnal functions take binds/expressions and an environment which 
gives CPR info for visible ids and returns a new bind/expression
with ids decorated with their CPR info.
 
\begin{code}
-- Return environment extended with info from this binding 
cprAnalBind :: CPREnv -> CoreBind -> (CPREnv, CoreBind)
cprAnalBind rho (NonRec b e) 
  = (extendVarEnv rho b absval, NonRec b' e')
  where
    (e', absval) = cprAnalRhs rho e
    b' = setIdCprInfo b (absToCprInfo absval)

cprAnalBind rho (Rec prs)
  = (final_rho, Rec (map do_pr prs))
  where
    do_pr (b,e) = (b', e') 
		where
		  b'           = setIdCprInfo b (absToCprInfo absval)
		  (e', absval) = cprAnalRhs final_rho e

	-- When analyzing mutually recursive bindings the iterations to find
	-- a fixpoint is bounded by the number of bindings in the group.
	-- for simplicity we just iterate that number of times.      
    final_rho = nTimes (length prs) do_one_pass init_rho
    init_rho  = rho `extendVarEnvList` [(b,Bot) | (b,e) <- prs]

    do_one_pass :: CPREnv -> CPREnv
    do_one_pass rho = foldl (\ rho (b,e) -> extendVarEnv rho b (snd (cprAnalRhs rho e)))
			    rho prs

cprAnalRhs :: CPREnv -> CoreExpr -> (CoreExpr, AbsVal)
cprAnalRhs rho e
  = case cprAnalExpr rho e of
	(e_pluscpr, e_absval) -> (e_pluscpr, pinCPR e e_absval)


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
			     Nothing    -> getCprAbsVal v)

-- Literals are unboxed
cprAnalExpr rho (Lit l) = (Lit l, Top)

-- For apps we don't care about the argument's abs val.  This
-- app will return a constructed product if the function does. We strip
-- a Fun from the functions abs val, unless the argument is a type argument 
-- or it is already Top or Bot.
cprAnalExpr rho (App fun arg@(Type _))
    = (App fun_cpr arg, fun_res)  
    where 
      (fun_cpr, fun_res)  = cprAnalExpr rho fun 

cprAnalExpr rho (App fun arg) 
    = (App fun_cpr arg_cpr, res_res)
    where 
      (fun_cpr, fun_res)  = cprAnalExpr rho fun 
      (arg_cpr, _)        = cprAnalExpr rho arg
      res_res		  = case fun_res of
				Fun res_res -> res_res
				Top 	    -> Top
				Bot	    -> Bot
				Tuple	    -> WARN( True, ppr (App fun arg) ) Top
						-- This really should not happen!


-- Map arguments to Top (we aren't constructing them)
-- Return the abstract value of the body, since functions 
-- are represented by the CPR value of their result, and 
-- add a Fun for this lambda..
cprAnalExpr rho (Lam b body) | isTyVar b = (Lam b body_cpr, body_aval)
                             | otherwise = (Lam b body_cpr, Fun body_aval)
      where 
      (body_cpr, body_aval) = cprAnalExpr (extendVarEnv rho b Top) body

cprAnalExpr rho (Let bind body)
    = (Let bind' body', body_aval)
    where 
      (rho', bind') = cprAnalBind rho bind
      (body', body_aval) = cprAnalExpr rho' body

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


-- take a binding pair and the abs val calculated from the rhs and
-- calculate a new absval taking into account sufficient manifest
-- lambda condition 
-- Also we pin the var's CPR property to it.  A var only has the CPR property if
-- it is a function

pinCPR :: CoreExpr -> AbsVal -> AbsVal
pinCPR e av = case av of
                    -- is v a function with insufficent lambdas?
                 Fun _ | n_fun_tys av /= length val_binders ->  
                      -- argtys must be greater than val_binders.  So stripped_exp
		      -- has a function type.  The head of this expr can't be lambda 
		      -- a note, because we stripped them off before.  It can't be a 
		      -- constructor because it has a function type.  It can't be a Type. 
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
			    Top

		 Tuple | exprIsValue e -> av
		       | otherwise     -> Top
			-- If the rhs is a value, and returns a constructed product,
			-- it will be inlined at usage sites, so we give it a Tuple absval
			-- If it isn't a value, we won't inline it (code/work dup worries), so
			-- we discard its absval.

		 _ -> av
    where
      n_fun_tys :: AbsVal -> Int
      n_fun_tys (Fun av) = 1 + n_fun_tys av
      n_fun_tys other    = 0

	-- val_binders are the explicit lambdas at the head of the expression
	-- Don't get confused by inline pragamas
      val_binders = filter isId (fst (collectBindersIgnoringNotes e))

absToCprInfo :: AbsVal -> CprInfo
absToCprInfo Tuple   = ReturnsCPR
absToCprInfo (Fun r) = absToCprInfo r
absToCprInfo _       = NoCPRInfo

-- Cpr Info doesn't store the number of arguments a function has,  so the caller
-- must take care to add the appropriate number of Funs.
getCprAbsVal v = case idCprInfo v of
			NoCPRInfo -> Top
			ReturnsCPR -> nTimes arity Fun Tuple
	       where
		 arity = idArity v
	-- Imported (non-nullary) constructors will have the CPR property
	-- in their IdInfo, so no need to look at their unfolding
\end{code}
