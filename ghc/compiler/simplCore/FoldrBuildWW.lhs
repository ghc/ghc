%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[FoldrBuildWW]{Spliting suitable functions into Workers and Wrappers}

\begin{code}
module FoldrBuildWW ( mkFoldrBuildWW ) where

#include "HsVersions.h"

-- Just a stub for now
import CoreSyn		( CoreBind )
import UniqSupply	( UniqSupply )
import Panic		( panic )

--import Type		( cloneTyVarFromTemplate, mkTyVarTy,
--			  splitFunTyExpandingDicts, eqTyCon,  mkForallTy )
--import TysPrim		( alphaTy )
--import TyVar		( alphaTyVar )
--
--import Type		( Type ) -- **** CAN SEE THE CONSTRUCTORS ****
--import UniqSupply	( runBuiltinUs )
--import WwLib            -- share the same monad (is this eticit ?)
--import PrelInfo		( listTyCon, mkListTy, nilDataCon, consDataCon,
--			  foldrId, buildId
--			)
--import Id               ( getIdFBTypeInfo, mkWorkerId, getIdInfo,
--			  mkSysLocal, idType
--			)
--import IdInfo
--import Maybes
--import SrcLoc		( noSrcLoc, SrcLoc )
--import Util
\end{code}

\begin{code}
mkFoldrBuildWW
	:: UniqSupply
	-> [CoreBind]
	-> [CoreBind]

mkFoldrBuildWW = panic "mkFoldrBuildWW (ToDo)"

{- LATER:
mkFoldrBuildWW us top_binds =
   (mapWw wwBind top_binds `thenWw` \ top_binds2 ->
   returnWw (concat top_binds2)) us
\end{code}

\begin{code}
wwBind :: CoreBinding -> WwM [CoreBinding]
wwBind (NonRec bndr expr)
  = try_split_bind bndr expr    `thenWw` \ re ->
    returnWw [NonRec bnds expr | (bnds,expr) <- re]
wwBind (Rec binds)
  = mapWw (\ (bndr,expr) -> try_split_bind bndr expr) binds   `thenWw` \ res ->
    returnWw [Rec (concat res)]

wwExpr :: CoreExpr -> WwM CoreExpr
wwExpr e@(Var _) = returnWw e
wwExpr e@(Lit _) = returnWw e
wwExpr e@(Con _ _ _) = returnWw e
wwExpr e@(Prim _ _ _) = returnWw e
wwExpr   (Lam ids e) =
	wwExpr e                `thenWw` \ e' ->
	returnWw (Lam ids e')
wwExpr   (CoTyLam tyvar e) =
	wwExpr e                `thenWw` \ e' ->
	returnWw (CoTyLam tyvar e')
wwExpr   (App f atom) =
	wwExpr f                `thenWw` \ f' ->
	returnWw (App f atom)
wwExpr   (CoTyApp f ty) =
	wwExpr f                `thenWw` \ f' ->
	returnWw (CoTyApp f' ty)
wwExpr   (Note note e) =
	wwExpr e                `thenWw` \ e' ->
	returnWw (Note note e')
wwExpr   (Let bnds e) =
	wwExpr e                `thenWw` \ e' ->
	wwBind bnds             `thenWw` \ bnds' ->
	returnWw (foldr Let e' bnds')
wwExpr   (Case e alts) =
	wwExpr e                `thenWw` \ e' ->
	wwAlts alts             `thenWw` \ alts' ->
	returnWw  (Case e' alts')

wwAlts (AlgAlts alts deflt) =
	mapWw (\(con,binders,e) ->
			wwExpr e        `thenWw` \ e' ->
			returnWw (con,binders,e')) alts `thenWw` \ alts' ->
	wwDef deflt                                     `thenWw` \ deflt' ->
	returnWw (AlgAlts alts' deflt)
wwAlts (PrimAlts alts deflt) =
	mapWw (\(lit,e) ->
			wwExpr e        `thenWw` \ e' ->
			returnWw (lit,e')) alts         `thenWw` \ alts' ->
	wwDef deflt                                     `thenWw` \ deflt' ->
	returnWw (PrimAlts alts' deflt)

wwDef e@NoDefault = returnWw e
wwDef  (BindDefault v e) =
	wwExpr e                                        `thenWw` \ e' ->
	returnWw (BindDefault v e')
\end{code}

\begin{code}
try_split_bind :: Id -> CoreExpr -> WwM [(Id,CoreExpr)]
try_split_bind id expr =
  wwExpr expr                   `thenWw` \ expr' ->
  case getFBType (getIdFBTypeInfo id) of
    Just (FBType consum prod)
	|  FBGoodProd == prod ->
{-      || any (== FBGoodConsum) consum -}
      let
	(big_args,args,body) = collectBinders expr'
      in
	if length args /= length consum   -- funny number of arguments
	then returnWw [(id,expr')]
	else
	-- f /\ t1 .. tn \ v1 .. vn -> e
	-- 	===>
	-- f_wrk /\ t1 .. tn t_new \ v1 .. vn c n -> foldr <exprTy> <nTy> c n e
	-- f /\ t1 .. tn \ v1 .. vn
	--	-> build exprTy (\ c n -> f_wrk t1 .. tn t_new v1 .. vn c n)
	pprTrace "WW:" (ppr id) (returnWw ())
				`thenWw` \ () ->
	getUniqueWw             `thenWw` \ ty_new_uq ->
	getUniqueWw             `thenWw` \ worker_new_uq ->
	getUniqueWw             `thenWw` \ c_new_uq ->
	getUniqueWw             `thenWw` \ n_new_uq ->
      let
	-- The *new* type
	n_ty = alphaTy
	n_ty_templ = alphaTy

	(templ,arg_tys,res) = splitFunTyExpandingDicts (idType id)
	expr_ty = getListTy res
   	getListTy res = panic "FoldrBuildWW:getListTy:ToDo" {-LATER:case res of
			 UniData lty [ty] | lty `eqTyCon` listTyCon -> ty
			 _ -> panic "Trying to split a non List datatype into Worker/Wrapper"-}

	c_ty       = expr_ty `mkFunTy` (n_ty `mkFunTy` n_ty)
	c_ty_templ = expr_ty `mkFunTy` (n_ty_templ `mkFunTy` n_ty_templ)

	worker_ty = mkForallTy (templ  ++ [alphaTyVar])
			(foldr mkFunTy n_ty_templ (arg_tys++[c_ty_templ,n_ty_templ]))
	wrapper_id  = setInlinePragma id IWantToBeINLINEd
	worker_id  = mkWorkerId worker_new_uq id worker_ty
		-- TODO : CHECK if mkWorkerId is thr
		-- right function to use ..
	-- Now the bodies

	c_id = mkSysLocal SLIT("fbww") c_new_uq c_ty
	n_id = mkSysLocal SLIT("fbww") n_new_uq n_ty
	worker_rhs
	  = mkTyLam [] (big_args ++ [alphaTyVar]) (args++[c_id,n_id]) worker_body
			
	worker_body = runBuiltinUs (
	  mkCoApps
	    (Var foldrId `CoTyApp` expr_ty `CoTyApp` n_ty `App`
	       VarArg c_id `App` VarArg n_id)
	    [body])
	wrapper_rhs = mkLam big_args args wrapper_body

	wrapper_body = runBuiltinUs (
		 mkCoApps (CoTyApp (Var buildId) expr_ty)
				[mkLam [alphaTyVar] [c_id,n_id]
		(foldl App
			(mkCoTyApps (Var worker_id)
				[mkTyVarTy t | t <- big_args ++ [alphaTyVar]])
			(map VarArg (args++[c_id,n_id])))])

      in
	if length args /= length arg_tys ||
	   length big_args /= length templ
	then panic "LEN PROBLEM"
	else
	returnWw  [(worker_id,worker_rhs),(wrapper_id,wrapper_rhs)]
    _ -> returnWw [(id,expr')]
-}
\end{code}
