%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[FoldrBuildWW]{Spliting suitable functions into Workers and Wrappers}

\begin{code}
#include "HsVersions.h"

module FoldrBuildWW ( mkFoldrBuildWW ) where

IMPORT_Trace
import Outputable
import Pretty 
import AbsUniType	( alpha_tv, cloneTyVarFromTemplate, mkTyVarTy,
			  splitTypeWithDictsAsArgs, eqTyCon,  mkForallTy,
			  alpha_tyvar, alpha_ty, alpha, TyVarTemplate
			  IF_ATTACK_PRAGMAS(COMMA cmpTyCon)
			)
import UniType		( UniType(..) ) -- **** CAN SEE THE CONSTRUCTORS ****
import PlainCore
import Unique		( runBuiltinUs )
import WwLib            -- share the same monad (is this eticit ?)
import AbsPrel		( listTyCon, mkListTy, nilDataCon, consDataCon,
			  foldrId, mkBuild, mkFoldr, buildId,
			  mkFunTy
			)
import Id               ( getIdFBTypeInfo, mkWorkerId, getIdInfo,
			  replaceIdInfo, mkSysLocal, getIdUniType
			)
import IdInfo           
import Maybes
import SrcLoc		( mkUnknownSrcLoc, SrcLoc )
import Util
\end{code}

\begin{code}
mkFoldrBuildWW 
        :: (GlobalSwitch -> Bool)
        -> SplitUniqSupply 
        -> PlainCoreProgram 
        -> PlainCoreProgram
mkFoldrBuildWW switch us top_binds = 
   (mapWw wwBind top_binds `thenWw` \ top_binds2 ->
   returnWw (concat top_binds2)) us switch
\end{code}

\begin{code}
wwBind :: PlainCoreBinding -> WwM [PlainCoreBinding]
wwBind (CoNonRec bndr expr) 
  = try_split_bind bndr expr    `thenWw` \ re ->
    returnWw [CoNonRec bnds expr | (bnds,expr) <- re]
wwBind (CoRec binds) 
  = mapWw (\ (bndr,expr) -> try_split_bind bndr expr) binds   `thenWw` \ res ->
    returnWw [CoRec (concat res)]

wwExpr :: PlainCoreExpr -> WwM PlainCoreExpr
wwExpr e@(CoVar _) = returnWw e
wwExpr e@(CoLit _) = returnWw e
wwExpr e@(CoCon _ _ _) = returnWw e
wwExpr e@(CoPrim _ _ _) = returnWw e
wwExpr   (CoLam ids e) = 
        wwExpr e                `thenWw` \ e' ->
        returnWw (CoLam ids e')
wwExpr   (CoTyLam tyvar e) = 
        wwExpr e                `thenWw` \ e' ->
        returnWw (CoTyLam tyvar e')
wwExpr   (CoApp f atom) = 
        wwExpr f                `thenWw` \ f' ->
        returnWw (CoApp f atom)
wwExpr   (CoTyApp f ty) = 
        wwExpr f                `thenWw` \ f' ->
        returnWw (CoTyApp f' ty)
wwExpr   (CoSCC lab e) = 
        wwExpr e                `thenWw` \ e' ->
        returnWw (CoSCC lab e')
wwExpr   (CoLet bnds e) = 
        wwExpr e                `thenWw` \ e' ->
        wwBind bnds             `thenWw` \ bnds' ->
        returnWw (foldr CoLet e' bnds')
wwExpr   (CoCase e alts) =
        wwExpr e                `thenWw` \ e' ->
        wwAlts alts             `thenWw` \ alts' ->
        returnWw  (CoCase e' alts')

wwAlts (CoAlgAlts alts deflt) =
        mapWw (\(con,binders,e) -> 
                        wwExpr e        `thenWw` \ e' ->
                        returnWw (con,binders,e')) alts `thenWw` \ alts' ->
        wwDef deflt                                     `thenWw` \ deflt' ->
        returnWw (CoAlgAlts alts' deflt)
wwAlts (CoPrimAlts alts deflt) =
        mapWw (\(lit,e) -> 
                        wwExpr e        `thenWw` \ e' ->
                        returnWw (lit,e')) alts         `thenWw` \ alts' ->
        wwDef deflt                                     `thenWw` \ deflt' ->
        returnWw (CoPrimAlts alts' deflt)

wwDef e@CoNoDefault = returnWw e
wwDef  (CoBindDefault v e) = 
        wwExpr e                                        `thenWw` \ e' ->
        returnWw (CoBindDefault v e')
\end{code}

\begin{code}
try_split_bind :: Id -> PlainCoreExpr -> WwM [(Id,PlainCoreExpr)]
try_split_bind id expr = 
  wwExpr expr                   `thenWw` \ expr' ->
  case getFBType (getIdFBTypeInfo id) of
    Just (FBType consum prod) 
        |  FBGoodProd == prod ->
{-      || any (== FBGoodConsum) consum -}
      let
        (big_args,args,body) = digForLambdas expr'
      in
        if length args /= length consum   -- funny number of arguments
        then returnWw [(id,expr')]
        else 
        -- f /\ t1 .. tn \ v1 .. vn -> e
        -- 	===>
        -- f_wrk /\ t1 .. tn t_new \ v1 .. vn c n -> foldr <exprTy> <nTy> c n e
        -- f /\ t1 .. tn \ v1 .. vn 
	--	-> build exprTy (\ c n -> f_wrk t1 .. tn t_new v1 .. vn c n)
	pprTrace "WW:" (ppr PprDebug id) (returnWw ())
				`thenWw` \ () ->
        getUniqueWw             `thenWw` \ ty_new_uq ->
        getUniqueWw             `thenWw` \ worker_new_uq ->
        getUniqueWw             `thenWw` \ c_new_uq ->
        getUniqueWw             `thenWw` \ n_new_uq ->
      let
	-- The *new* type
	n_ty = alpha_ty
	n_ty_templ = alpha

	(templ,arg_tys,res) = splitTypeWithDictsAsArgs (getIdUniType id)
	expr_ty = getListTy res
   	getListTy res = case res of
		         UniData lty [ty] | lty `eqTyCon` listTyCon -> ty
		         _ -> panic "Trying to split a non List datatype into Worker/Wrapper"

        c_ty       = expr_ty `mkFunTy` (n_ty `mkFunTy` n_ty)
        c_ty_templ = expr_ty `mkFunTy` (n_ty_templ `mkFunTy` n_ty_templ)

	worker_ty = mkForallTy (templ  ++ [alpha_tv])
			(foldr mkFunTy n_ty_templ (arg_tys++[c_ty_templ,n_ty_templ]))
	wrapper_id  = id `replaceIdInfo`
			      (getIdInfo id	`addInfo_UF`
			       iWantToBeINLINEd UnfoldAlways)
	worker_id  = mkWorkerId worker_new_uq id worker_ty
				noIdInfo
		-- TODO : CHECK if mkWorkerId is thr
		-- right function to use ..
	-- Now the bodies
	
	c_id = mkSysLocal SLIT("_fbww") c_new_uq c_ty mkUnknownSrcLoc
	n_id = mkSysLocal SLIT("_fbww") n_new_uq n_ty mkUnknownSrcLoc
	worker_rhs = foldr CoTyLam 
			(mkCoLam (args++[c_id,n_id]) worker_body) 
			(big_args ++ [alpha_tyvar])
	worker_body = runBuiltinUs (
			 mkCoApps (mkCoTyApps (CoVar foldrId) [expr_ty, n_ty])
				  [CoVar c_id,CoVar n_id,body])
	wrapper_rhs = foldr CoTyLam 
			(mkCoLam (args) wrapper_body) 
			big_args
	wrapper_body = runBuiltinUs (
		 mkCoApps (mkCoTyApp (CoVar buildId) expr_ty)
				[CoTyLam alpha_tyvar (mkCoLam [c_id,n_id]
		(foldl CoApp 
			(mkCoTyApps (CoVar worker_id) 
				[mkTyVarTy t | t <- big_args ++ [alpha_tyvar]])
			(map CoVarAtom (args++[c_id,n_id]))))])

      in
	if length args /= length arg_tys ||
	   length big_args /= length templ 
	then panic "LEN PROBLEM"
	else
        returnWw  [(worker_id,worker_rhs),(wrapper_id,wrapper_rhs)]
    _ -> returnWw [(id,expr')]
\end{code}

