%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Cyclic]{Knot tying}

>#include "HsVersions.h"
>
> module Cyclic (
> 	mkLoops, fixupFreeVars
> 	) where

> import DefSyn
> import DefUtils
> import Def2Core 	( d2c, defPanic )

> import Type		( glueTyArgs, quantifyTy, mkForallTy, mkTyVarTys,
>			  TyVarTemplate
>			)
> import Digraph	( dfs )
> import Id		( idType, updateIdType,
> 			  getIdInfo, replaceIdInfo, eqId, Id
>			)
> import IdInfo
> import Outputable
> import Pretty
> import UniqSupply
> import Util

-----------------------------------------------------------------------------
A more efficient representation for lists that are extended multiple
times, but only examined once.

> type FList a  = [a] -> [a]
> append 	= (.)
> singleton x	= (x:)
> cons x xs	= \ys -> x:(xs ys)
> list x	= (x++)
> emptylist 	= id

-----------------------------------------------------------------------------
Monad for the knot-tier.

> type Lbl a = UniqSM (
> 	[(Id)],				-- loops used
>	[(Id,DefExpr,[Id],DefExpr)],	-- bindings floating upwards
>	[(Id,DefExpr)],			-- back loops
>	a)				-- computation result
>
> thenLbl :: Lbl a -> (a -> Lbl b) -> Lbl b
> thenLbl a k
> 	= a 	`thenUs` \(ls, bs, bls,  a) ->
>	  k a	`thenUs` \(ls',bs',bls', b) ->
>	  returnUs (ls ++ ls', bs ++ bs', bls ++ bls', b)
>
> returnLbl :: a -> Lbl a
> returnLbl a = returnUs ([],[],[],a)
>
> mapLbl :: (a -> Lbl b) -> [a] -> Lbl [b]
> mapLbl f [] = returnLbl []
> mapLbl f (x:xs)
> 	= f x		`thenLbl` \x ->
>	  mapLbl f xs	`thenLbl` \xs ->
>	  returnLbl (x:xs)

-----------------------------------------------------------------------------

This is terribly inefficient.

> mkLoops :: DefExpr -> UniqSM ([(Id,DefExpr)],DefExpr)
> mkLoops e =
>  error "mkLoops"
>{- LATER:
> 	loop [] e `thenUs` \(ls,bs,bls,e) ->

Throw away all the extracted bindings that can't be reached.  These
can occur as the result of some forward loops being short-circuited by
back-loops.  We find out which bindings can be reached by a
depth-first search of the call graph starting with the free variables
of the expression being returned.

>	let
>		loops_out = filter deforestable (freeVars e)
>		(_,reachable) = dfs (==) r ([],[]) loops_out
>		r f = lookup f bs
>
>		lookup f [] = []
>		lookup f ((g,out,_):xs) | f == g = out
>					| otherwise = lookup f xs
>
>		isReachable (f,_,_) = f `elem` reachable
>	in
> 	returnUs (map (\(f,_,e) -> (f,e)) (filter isReachable bs),e)
>   where

>       loop :: [(Id,DefExpr,[Id],[TyVar])] -> DefExpr -> Lbl DefExpr

>   	loop ls (Var (Label e e1))
>   	    =
>	     d2c e `thenUs` \core_e ->
>--	     trace ("loop:\n" ++ show (ppr PprDebug core_e)) $

>	     mapUs (\(f,e',val_args,ty_args) ->
>	             renameExprs e' e	`thenUs` \r ->
>		     returnUs (f,val_args,ty_args,r)) ls `thenUs` \results ->
>	     let
> 		loops =
>			[ (f,val_args,ty_args,r) |
>			  (f,val_args,ty_args,IsRenaming r) <- results ]
>		inconsistent_renamings =
>			[ (f,r) |
>			  (f,val_args,ty_args,InconsistentRenaming r)
>			  	<- results ]
>	     in
>
>  	     (case loops of
>	      [] ->

Ok, there are no loops (i.e. this expression hasn't occurred before).
Prepare for a possible re-occurrence of *this* expression, by making
up a new function name and type (laziness ensures that this isn't
actually done unless the function is required).

The type of a new function, if one is generated at this point, is
constructed as follows:

    \/ a1 ... \/ an . b1 -> ... -> bn -> t

where a1...an are the free type variables in the expression, b1...bn
are the types of the free variables in the expression, and t is the
type of the expression itself.

>		let
>
> 		   -- Collect the value/type arguments for the function
>		   fvs       = freeVars e
>		   val_args  = filter isArgId fvs
>		   ty_args   = freeTyVars e
>
>		   -- Now to make up the type...
>		   base_type = coreExprType core_e
>		   fun_type  = glueTyArgs (map idType val_args) base_type
>		   (_, type_of_f) = quantifyTy ty_args fun_type
>		in
>
>		newDefId type_of_f	`thenUs` \f' ->
>		let
> 		       f = replaceIdInfo f'
>		       		(addDeforestInfo (getIdInfo f') DoDeforest)
>		in
>		loop ((f,e,val_args,ty_args):ls) e1
>					`thenUs` \res@(ls',bs,bls,e') ->

Key: ls = loops, bs = bindings, bls = back loops, e = expression.

If we are in a back-loop (i.e. we found a label somewhere below which
this expression is a renaming of), then just insert the expression
here.

Comment the next section out to disable back-loops.

(NB. I've seen this panic too - investigate?)

>		let back_loops = reverse [ e | (f',e) <- bls, f' == f ] in
>		if not (null back_loops){- && not (f `elem` ls')-} then
>		   --if length back_loops > 1 then panic "barf!" else
>		   	d2c (head back_loops)	`thenUs` \core_e ->
>		   	pprTrace "Back Loop:\n" (ppr PprDebug core_e) $

If we find a back-loop that also occurs where we would normally make a
new function...

>		   if f `elem` ls' then
>			d2c e'			`thenUs` \core_e' ->
>			trace ("In Forward Loop " ++
>				show (ppr PprDebug f) ++ "\n" ++
>				show (ppr PprDebug core_e')) $
>		   	if f `notElem` (freeVars (head back_loops)) then
>				returnUs (ls', bs, bls, head back_loops)
>			else
>				panic "hello"
>		   else

> 		   returnUs (ls', bs, bls, head back_loops)
>		else

If we are in a forward-loop (i.e. we found a label somewhere below
which is a renaming of this one), then make a new function definition.

>		if f `elem` ls' then
>
>			rebindExpr (mkLam ty_args val_args e')
>							`thenUs` \rhs ->
>			returnUs
>			    (ls',
>			     (f,filter deforestable (freeVars e'),e,rhs) : bs,
>			     bls,
>			     mkLoopFunApp val_args ty_args f)

otherwise, forget about it

>			else returnUs res

This is a loop, just make a call to the function which we
will create on the way back up the tree.

(NB: it appears that sometimes we do get more than one loop matching,
investigate this?)

>	      ((f,val_args,ty_args,r):_) ->
>
>		     returnUs
>		     	([f],		-- found a loop, propagate it back
>			 [],		-- no bindings
>			 [],		-- no back loops
>		         mkLoopFunApp (applyRenaming r val_args) ty_args f)
>
>		) `thenUs` \res@(ls',bs,bls,e') ->

If this expression reoccurs, record the binding and replace the cycle
with a call to the new function.  We also rebind all the free
variables in the new function to avoid name clashes later.

>	   let
>		findBackLoops (g,r) bls
>			| consistent r' = subst s e' `thenUs` \e' ->
>					  returnUs ((g,e') : bls)
>			| otherwise     = returnUs bls
>			where
>			  r' = map swap r
>			  s = map (\(x,y) -> (x, Var (DefArgVar y))) (nub r')
>	   in

We just want the first one (ie. furthest up the tree), so reverse the
list of inconsistent renamings.

>	   foldrSUs findBackLoops [] (reverse inconsistent_renamings)
>						`thenUs` \back_loops ->

Comment out the next block to disable back-loops.  ToDo: trace all of them.

>	   if not (null back_loops) then
>		d2c e'	`thenUs` \core_e ->
>		trace ("Floating back loop:\n"
>			++ show (ppr PprDebug core_e))
>		returnUs (ls', bs, back_loops ++ bls, e')
>	   else
> 		returnUs res

>   	loop ls e@(Var (DefArgVar v))
> 	    = returnLbl e
>   	loop ls e@(Lit l)
>   	    = returnLbl e
>   	loop ls (Con c ts es)
>   	    = mapLbl (loopAtom ls) es       `thenLbl` \es ->
>   	      returnLbl (Con c ts es)
>   	loop ls (Prim op ts es)
>   	    = mapLbl (loopAtom ls) es       `thenLbl` \es ->
>   	      returnLbl (Prim op ts es)
>   	loop ls (Lam vs e)
>   	    = loop ls e                     `thenLbl` \e ->
>   	      returnLbl (Lam vs e)
>   	loop ls (CoTyLam alpha e)
>   	    = loop ls e                     `thenLbl` \e ->
>   	      returnLbl (CoTyLam alpha e)
>   	loop ls (App e v)
>   	    = loop ls e                     `thenLbl` \e ->
>   	      loopAtom ls v                 `thenLbl` \v ->
>   	      returnLbl (App e v)
>   	loop ls (CoTyApp e t)
>   	    = loop ls e                     `thenLbl` \e ->
>   	      returnLbl (CoTyApp e t)
>   	loop ls (Case e ps)
>   	    = loop ls e                     `thenLbl` \e ->
>   	      loopCaseAlts ls ps            `thenLbl` \ps ->
>   	      returnLbl (Case e ps)
>   	loop ls (Let (NonRec v e) e')
>   	    = loop ls e                     `thenLbl` \e ->
>   	      loop ls e'                    `thenLbl` \e' ->
>   	      returnLbl (Let (NonRec v e) e')
>   	loop ls (Let (Rec bs) e)
>   	    = mapLbl loopRecBind bs         `thenLbl` \bs ->
>   	      loop ls e                     `thenLbl` \e ->
>   	      returnLbl (Let (Rec bs) e)
>   	    where
>	      vs = map fst bs
>   	      loopRecBind (v, e)
>   	            = loop ls e             `thenLbl` \e ->
>   	              returnLbl (v, e)
>	loop ls e
>	    = defPanic "Cyclic" "loop" e

> 	loopAtom ls (VarArg (DefArgExpr e))
> 	      = loop ls e                     `thenLbl` \e ->
> 	        returnLbl (VarArg (DefArgExpr e))
> 	loopAtom ls (VarArg e@(DefArgVar v))
> 	      = defPanic "Cyclic" "loopAtom" (Var e)
> 	loopAtom ls (VarArg e@(Label _ _))
> 	      = defPanic "Cyclic" "loopAtom" (Var e)
> 	loopAtom ls e@(LitArg l)
> 	      = returnLbl e
>
> 	loopCaseAlts ls (AlgAlts as def) =
>		mapLbl loopAlgAlt as		`thenLbl` \as ->
> 	        loopDefault ls def		`thenLbl` \def ->
> 	        returnLbl (AlgAlts as def)
>	      where
>	      	loopAlgAlt (c, vs, e) =
>			loop ls e		`thenLbl` \e ->
> 	        	returnLbl (c, vs, e)

> 	loopCaseAlts ls (PrimAlts as def) =
>		mapLbl loopPrimAlt as		`thenLbl` \as ->
> 	        loopDefault ls def		`thenLbl` \def ->
> 	        returnLbl (PrimAlts as def)
>	      where
>	      	loopPrimAlt (l, e) =
>			loop ls e		`thenLbl` \e ->
> 	        	returnLbl (l, e)

> 	loopDefault ls NoDefault =
>		returnLbl NoDefault
> 	loopDefault ls (BindDefault v e) =
>		loop ls e			`thenLbl` \e ->
> 	        returnLbl (BindDefault v e)
> -}

> mkVar v = VarArg (DefArgExpr (Var (DefArgVar v)))

-----------------------------------------------------------------------------
The next function is applied to all deforestable functions which are
placed in the environment.  Given a list of free variables in the
recursive set of which the function is a member, this funciton
abstracts those variables, generates a new Id with the new type, and
returns a substitution element which can be applied to all other
expressions and function right hand sides that call this function.

	(freeVars e) \subseteq (freeVars l)

> fixupFreeVars :: [Id] -> Id -> DefExpr -> ((Id,DefExpr),[(Id,DefExpr)])
> fixupFreeVars total_fvs id e =
> 	case fvs of
>		[] -> ((id,e),[])
>		_  -> let new_type =
>		      		glueTyArgs (map idType fvs)
>					(idType id)
>			  new_id =
>			  	updateIdType id new_type
>		      in
>		      let
>		          t = foldl App (Var (DefArgVar new_id))
>			  			(map mkVar fvs)
> 		      in
>		      trace ("adding " ++ show (length fvs) ++ " args to " ++ show (ppr PprDebug id)) $
>		      ((new_id, mkValLam fvs e), [(id,t)])
>	where
>		fvs = case e of
>			Lam bvs e -> filter (`notElem` bvs) total_fvs
>			_ -> total_fvs

> swap (x,y) = (y,x)

> applyRenaming :: [(Id,Id)] -> [Id] -> [Id]
> applyRenaming r ids = map rename ids
>    where
> 	rename x = case [ y | (x',y) <- r, x' `eqId` x ] of
> 			[] -> panic "Cyclic(rename): no match in rename"
>			(y:_) -> y

> mkLoopFunApp :: [Id] -> [TyVar] -> Id -> DefExpr
> mkLoopFunApp val_args ty_args f =
> 	foldl App
>	  (foldl CoTyApp (Var (DefArgVar f))
>	    (mkTyVarTys ty_args))
>	  	(map mkVar val_args)

-----------------------------------------------------------------------------
Removing duplicates from a list of definitions.

> removeDuplicateDefinitions
> 	:: [(DefExpr,(Id,DefExpr))] 	-- (label,(id,rhs))
> 	-> UniqSM [(Id,DefExpr)]

> removeDuplicateDefinitions defs =
> 	foldrSUs rem ([],[]) defs	`thenUs` \(newdefs,s) ->
>	mapUs (\(l,(f,e)) -> subst s e `thenUs` \e ->
>			      returnUs (f, e)) newdefs
>   where

> 	rem d@(l,(f,e)) (defs,s) =
>		findDup l defs		`thenUs` \maybe ->
> 		case maybe of
>		   Nothing -> returnUs (d:defs,s)
>		   Just g  -> returnUs (defs, (f,(Var.DefArgVar) g):s)

We insist that labels rename in both directions, is this necessary?

> 	findDup l [] = returnUs Nothing
> 	findDup l ((l',(f,e)):defs) =
> 		renameExprs l l' 	`thenUs` \r ->
>		case r of
>		  IsRenaming _ -> renameExprs l' l 	`thenUs` \r ->
>				  case r of
>				  	IsRenaming r -> returnUs (Just f)
>					_ -> findDup l defs
>		  _ -> findDup l defs
