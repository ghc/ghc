%               Filename:  %M%
%               Version :  %I%
%               Date    :  %G%
%
\section[MatchProcessors]{Pattern-matching processors}
\begin{code}
module MatchProc (
    matchProcessor
) where

#include "HsVersions.h"

import AbsSyn		-- the stuff being desugared
import PlainCore	-- the output of desugaring;
			-- importing this module also gets all the
			-- CoreSyn utility functions
import DsMonad		-- the monadery used in the desugarer

import AbsUniType	( mkTyVarTy, splitType, mkProcessorTyCon,
			  TyVar, TyCon, Class, UniType,
			  TauType(..)
			)
import DsUtils		( EquationInfo(..), selectMatchVars )
import Id		( getDataConFamily, getDataConTyCon,
			  getIdUniType, mkProcessorCon
			)
import ListSetOps	( minusList )
import Maybes		( Maybe(..) )
import Match		( match )
import Util
import DsExpr		( dsExpr)
\end{code}

The matching of processors is based upon that of constructors. Given the 
pattern :
\begin{verbatim}
	(|x1,..xn;y|)
\end{verbatim}

The pattern matching compiler converts the above into :
\begin{verbatim}
	case x of
		(|u1,..un;uy|) -> let x1 = fromDomain u_1 of
				  	 ....
				  let xn = fromDomain u_n of
				  let y  = fromDomain uy of
				      PATTERN MATCH REST
\end{verbatim}

\begin{code}
matchProcessor :: [Id]
	       -> [EquationInfo]
	       -> PlainCoreExpr
	       -> DsM PlainCoreExpr

matchProcessor (v:vs) eqnInfo ifFail  
  = selectMatchVars [pat]	             `thenDs` 	(\ [var]             -> 
    selectMatchVars pats	             `thenDs` 	(\ vars              -> 
    match (var:vs) 
	  [(pat:ps,after_fun)]
	  ifFail		             `thenDs` 	(\ body              ->
    create_lets vars pats convs body ifFail  `thenDs` 	(\ rhs	             ->
    returnDs (
      CoCase 
	  (CoVar v)
	  (CoAlgAlts
	      [((mkProcessorCon podSize),vars++[var], rhs)]
	      CoNoDefault))
    )))) 
  where
    podSize = (length pats)
    -- Sanity checking pattern match. Product type of processors ensures
    -- there can be only one result if the equations are properly unmixed.
    ((ProcessorPat pats convs pat):ps,after_fun)
    	| length eqnInfo == 1 = head eqnInfo
        | otherwise           = panic "matchProcessor more than one"

\end{code}

\begin{code}
create_lets::[Id] ->
	     [TypecheckedPat] -> 
	     [TypecheckedExpr] -> 
	     PlainCoreExpr ->
	     PlainCoreExpr ->
	     (DsM PlainCoreExpr)

create_lets [] _ _ body _ = returnDs (body)
create_lets (v:vs) (p:ps) (c:cs) body ifFail
   = selectMatchVars [p]	        	`thenDs`  (\ var   -> 
     create_lets vs ps cs body ifFail		`thenDs`  (\ after ->
     dsExpr c					`thenDs`  (\ c'    ->
     match var 
	   [([p], \x -> after)] 
	   ifFail				`thenDs`  (\ exp  ->
     returnDs ( CoApp (CoLam var exp) (CoApp c' (CoVar v))) ))))
\end{code}

