%               Filename:  %M%
%               Version :  %I%
%               Date    :  %G%
%
\section[TcParQuals]{TcParQuals}

\begin{code}
module TcParQuals ( tcParQuals , tcPidPats , tcPidExprs ) where

#include "HsVersions.h"

import TcMonad		-- typechecking monad machinery
import TcMonadFns		
import AbsSyn		-- the stuff being typechecked

import AbsPrel		( boolTy, mkProcessorTy, mkPodTy , 
			  toDomainId, fromDomainId
			)
import AbsUniType
import Id		( mkInstId )
import Inst		( InstOrigin(..) )
import E		
import LIE		
import TcExpr		( tcExpr , tcExprs )
import TcPat		( tcPat , tcPats )
import Unify
import Util
\end{code}


\begin{code}
tcParQuals :: E -> RenamedParQuals -> TcM (TypecheckedParQuals,LIE)
tcParQuals e (AndParQuals quals1 quals2)
 = (tcParQuals e quals1)		   `thenTc` (\ (quals1',lie1) ->
   (tcParQuals e quals2)  		   `thenTc` (\ (quals2',lie2) ->
   returnTc (AndParQuals quals1' quals2', lie1 `plusLIE` lie2) ))

tcParQuals e (ParFilter expr)
 = (tcExpr e expr)			        `thenTc`  (\ (expr',lie,ty) ->
   (unifyTauTy ty boolTy (ParFilterCtxt expr))  `thenTc_`  
   returnTc (ParFilter expr',lie) )

tcParQuals e (DrawnGenIn pats pat expr)
 = (tcPidPats e pats)       	    `thenTc` (\ (pats',convs,lie1,patsTy) ->
   (tcPat     e pat)	    	    `thenTc` (\ (pat' ,patTy, lie2) ->
   (tcExpr e expr)		    `thenTc` (\ (expr',lie3,exprTy) ->
   (unifyTauTy exprTy 
	       (mkPodTy (mkProcessorTy patsTy patTy)) 
	       (DrawnCtxt pats pat expr))	`thenTc_`	
   returnTc (DrawnGenOut pats' convs pat' expr',
	    plusLIE (plusLIE lie1 lie2) lie3 ) )))

tcParQuals e (IndexGen exprs pat expr)
 = (tcPidExprs e exprs)         	`thenTc` (\ (exprs',lie1,exprsTy) ->
   (tcPat      e pat)			`thenTc` (\ (pat',patTy,  lie2) ->
   (tcExpr e expr)			`thenTc` (\ (expr',lie3,exprTy) ->
   (unifyTauTy exprTy 
	       (mkPodTy (mkProcessorTy exprsTy patTy))
	       (IndexCtxt exprs pat expr))	`thenTc_`
   returnTc (IndexGen exprs' pat' expr',	
	     plusLIE (plusLIE lie1 lie2) lie3) )))

\end{code}

\begin{code}
tcPidExprs:: E -> [RenamedExpr] -> TcM ([TypecheckedExpr],LIE,[TauType])
tcPidExprs e exprs
  = tcExprs e exprs			     `thenTc`     (\ (exprs',lie,tys)->
    getSrcLocTc				     `thenNF_Tc`  (\ loc             ->
    listNF_Tc (map (getFromDomain loc) tys)  `thenNF_Tc`  (\ fromDomains     ->
    returnTc (zipWith mkConversion fromDomains exprs',
	      mkLIE fromDomains `plusLIE` lie,tys) 
    )))
  where
    getFromDomain loc ty
      = newMethod (OccurrenceOf toDomainId loc) fromDomainId [ty]

    mkConversion fromDom expr 
      = App (Var (mkInstId fromDom)) expr  
\end{code}

\begin{code}
tcPidPats ::E ->[RenamedPat]->TcM ([TypecheckedPat],   -- Expression
				   [TypecheckedExpr],  -- Conversion fns
				   LIE,
				   [UniType])
tcPidPats e pats 
  = tcPats e pats		           `thenTc`       (\ (pats',tys,lie)->
    getSrcLocTc				   `thenNF_Tc`    (\ loc            ->
    listNF_Tc (map (getToDomain loc) tys)  `thenNF_Tc`    (\ toDomains      ->
    returnTc (pats',map mkConversion toDomains,
	      mkLIE toDomains `plusLIE` lie,tys) 
    )))
  where
    getToDomain loc ty= newMethod (OccurrenceOf toDomainId loc) toDomainId [ty]
    mkConversion toDom= Var (mkInstId toDom)
\end{code}
