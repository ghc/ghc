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
> import PlainCore
> import DefUtils
> import Def2Core 	( d2c, defPanic )
>#ifdef __HBC__
> import Trace
>#endif

> import AbsUniType	( glueTyArgs, quantifyTy, mkForallTy, mkTyVarTy,
>			  TyVarTemplate
>			)
> import Digraph	( dfs )
> import Id		( getIdUniType, toplevelishId, updateIdType,
> 			  getIdInfo, replaceIdInfo, eqId, Id
>			)
> import IdInfo
> import Maybes		( Maybe(..) )
> import Outputable
> import Pretty
> import SplitUniq
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

> type Lbl a = SUniqSM (
> 	[(Id)],				-- loops used
>	[(Id,DefExpr,[Id],DefExpr)],	-- bindings floating upwards
>	[(Id,DefExpr)],			-- back loops
>	a)				-- computation result
> 
> thenLbl :: Lbl a -> (a -> Lbl b) -> Lbl b
> thenLbl a k
> 	= a 	`thenSUs` \(ls, bs, bls,  a) ->
>	  k a	`thenSUs` \(ls',bs',bls', b) ->
>	  returnSUs (ls ++ ls', bs ++ bs', bls ++ bls', b)
> 
> returnLbl :: a -> Lbl a
> returnLbl a = returnSUs ([],[],[],a)
> 
> mapLbl :: (a -> Lbl b) -> [a] -> Lbl [b]
> mapLbl f [] = returnLbl []
> mapLbl f (x:xs)
> 	= f x		`thenLbl` \x ->
>	  mapLbl f xs	`thenLbl` \xs ->
>	  returnLbl (x:xs)

-----------------------------------------------------------------------------

This is terribly inefficient.

> mkLoops :: DefExpr -> SUniqSM ([(Id,DefExpr)],DefExpr)
> mkLoops e = 
>  error "mkLoops"
>{- LATER:
> 	loop [] e `thenSUs` \(ls,bs,bls,e) ->

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
> 	returnSUs (map (\(f,_,e) -> (f,e)) (filter isReachable bs),e)
>   where

>       loop :: [(Id,DefExpr,[Id],[TyVar])] -> DefExpr -> Lbl DefExpr

>   	loop ls (CoVar (Label e e1))
>   	    = 
>	     d2c e `thenSUs` \core_e ->
>--	     trace ("loop:\n" ++ ppShow 80 (ppr PprDebug core_e)) $

>	     mapSUs (\(f,e',val_args,ty_args) -> 
>	             renameExprs e' e	`thenSUs` \r ->
>		     returnSUs (f,val_args,ty_args,r)) ls `thenSUs` \results ->
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
>		   base_type = typeOfCoreExpr core_e
>		   fun_type  = glueTyArgs (map getIdUniType val_args) base_type
>		   (_, type_of_f) = quantifyTy ty_args fun_type
>		in
>		
>		newDefId type_of_f	`thenSUs` \f' ->
>		let 
> 		       f = replaceIdInfo f' 
>		       		(addInfo (getIdInfo f') DoDeforest)
>		in
>		loop ((f,e,val_args,ty_args):ls) e1
>					`thenSUs` \res@(ls',bs,bls,e') ->

Key: ls = loops, bs = bindings, bls = back loops, e = expression.

If we are in a back-loop (i.e. we found a label somewhere below which
this expression is a renaming of), then just insert the expression
here.

Comment the next section out to disable back-loops.

(NB. I've seen this panic too - investigate?)

>		let back_loops = reverse [ e | (f',e) <- bls, f' == f ] in
>		if not (null back_loops){- && not (f `elem` ls')-} then
>		   --if length back_loops > 1 then panic "barf!" else
>		   	d2c (head back_loops)	`thenSUs` \core_e ->
>		   	trace ("Back Loop:\n" ++ 
>				ppShow 80 (ppr PprDebug core_e)) $

If we find a back-loop that also occurs where we would normally make a
new function...

>		   if f `elem` ls' then
>			d2c e'			`thenSUs` \core_e' ->
>			trace ("In Forward Loop " ++
>				ppShow 80 (ppr PprDebug f) ++ "\n" ++
>				ppShow 80 (ppr PprDebug core_e')) $
>		   	if f `notElem` (freeVars (head back_loops)) then
>				returnSUs (ls', bs, bls, head back_loops)
>			else
>				panic "hello"
>		   else

> 		   returnSUs (ls', bs, bls, head back_loops)
>		else

If we are in a forward-loop (i.e. we found a label somewhere below
which is a renaming of this one), then make a new function definition.

>		if f `elem` ls' then
>		
>			rebindExpr (mkCoTyLam ty_args (mkCoLam val_args e'))
>							`thenSUs` \rhs ->
>			returnSUs
>			    (ls', 
>			     (f,filter deforestable (freeVars e'),e,rhs) : bs, 
>			     bls,
>			     mkLoopFunApp val_args ty_args f)

otherwise, forget about it

>			else returnSUs res

This is a loop, just make a call to the function which we
will create on the way back up the tree.

(NB: it appears that sometimes we do get more than one loop matching,
investigate this?)

>	      ((f,val_args,ty_args,r):_) -> 
>	      
>		     returnSUs 
>		     	([f],		-- found a loop, propagate it back
>			 [],		-- no bindings
>			 [],		-- no back loops
>		         mkLoopFunApp (applyRenaming r val_args) ty_args f)
>			 
>		) `thenSUs` \res@(ls',bs,bls,e') ->

If this expression reoccurs, record the binding and replace the cycle
with a call to the new function.  We also rebind all the free
variables in the new function to avoid name clashes later.

>	   let
>		findBackLoops (g,r) bls 
>			| consistent r' = subst s e' `thenSUs` \e' ->
>					  returnSUs ((g,e') : bls)
>			| otherwise     = returnSUs bls
>			where
>			  r' = map swap r
>			  s = map (\(x,y) -> (x, CoVar (DefArgVar y))) (nub r')
>	   in

We just want the first one (ie. furthest up the tree), so reverse the
list of inconsistent renamings.

>	   foldrSUs findBackLoops [] (reverse inconsistent_renamings)
>						`thenSUs` \back_loops ->

Comment out the next block to disable back-loops.  ToDo: trace all of them.

>	   if not (null back_loops) then
>		d2c e'	`thenSUs` \core_e ->
>		trace ("Floating back loop:\n" 
>			++ ppShow 80 (ppr PprDebug core_e)) 
>		returnSUs (ls', bs, back_loops ++ bls, e')
>	   else
> 		returnSUs res

>   	loop ls e@(CoVar (DefArgVar v))
> 	    = returnLbl e
>   	loop ls e@(CoLit l)
>   	    = returnLbl e
>   	loop ls (CoCon c ts es)
>   	    = mapLbl (loopAtom ls) es       `thenLbl` \es ->
>   	      returnLbl (CoCon c ts es)
>   	loop ls (CoPrim op ts es)
>   	    = mapLbl (loopAtom ls) es       `thenLbl` \es ->
>   	      returnLbl (CoPrim op ts es)
>   	loop ls (CoLam vs e)
>   	    = loop ls e                     `thenLbl` \e ->
>   	      returnLbl (CoLam vs e)
>   	loop ls (CoTyLam alpha e)
>   	    = loop ls e                     `thenLbl` \e ->
>   	      returnLbl (CoTyLam alpha e)
>   	loop ls (CoApp e v)
>   	    = loop ls e                     `thenLbl` \e ->
>   	      loopAtom ls v                 `thenLbl` \v ->
>   	      returnLbl (CoApp e v)
>   	loop ls (CoTyApp e t)
>   	    = loop ls e                     `thenLbl` \e ->
>   	      returnLbl (CoTyApp e t)
>   	loop ls (CoCase e ps)
>   	    = loop ls e                     `thenLbl` \e ->
>   	      loopCaseAlts ls ps            `thenLbl` \ps ->
>   	      returnLbl (CoCase e ps)
>   	loop ls (CoLet (CoNonRec v e) e')
>   	    = loop ls e                     `thenLbl` \e ->
>   	      loop ls e'                    `thenLbl` \e' ->
>   	      returnLbl (CoLet (CoNonRec v e) e')
>   	loop ls (CoLet (CoRec bs) e)
>   	    = mapLbl loopRecBind bs         `thenLbl` \bs ->
>   	      loop ls e                     `thenLbl` \e ->
>   	      returnLbl (CoLet (CoRec bs) e)
>   	    where
>	      vs = map fst bs
>   	      loopRecBind (v, e)
>   	            = loop ls e             `thenLbl` \e ->
>   	              returnLbl (v, e)
>	loop ls e
>	    = defPanic "Cyclic" "loop" e

> 	loopAtom ls (CoVarAtom (DefArgExpr e))
> 	      = loop ls e                     `thenLbl` \e ->
> 	        returnLbl (CoVarAtom (DefArgExpr e))
> 	loopAtom ls (CoVarAtom e@(DefArgVar v))
> 	      = defPanic "Cyclic" "loopAtom" (CoVar e)
> 	loopAtom ls (CoVarAtom e@(Label _ _))
> 	      = defPanic "Cyclic" "loopAtom" (CoVar e)
> 	loopAtom ls e@(CoLitAtom l)
> 	      = returnLbl e
>
> 	loopCaseAlts ls (CoAlgAlts as def) = 
>		mapLbl loopAlgAlt as		`thenLbl` \as ->
> 	        loopDefault ls def		`thenLbl` \def ->
> 	        returnLbl (CoAlgAlts as def)
>	      where
>	      	loopAlgAlt (c, vs, e) =
>			loop ls e		`thenLbl` \e ->
> 	        	returnLbl (c, vs, e)

> 	loopCaseAlts ls (CoPrimAlts as def) = 
>		mapLbl loopPrimAlt as		`thenLbl` \as ->
> 	        loopDefault ls def		`thenLbl` \def ->
> 	        returnLbl (CoPrimAlts as def)
>	      where
>	      	loopPrimAlt (l, e) = 
>			loop ls e		`thenLbl` \e ->
> 	        	returnLbl (l, e)

> 	loopDefault ls CoNoDefault = 
>		returnLbl CoNoDefault
> 	loopDefault ls (CoBindDefault v e) = 
>		loop ls e			`thenLbl` \e ->
> 	        returnLbl (CoBindDefault v e)
> -}

> mkVar v = CoVarAtom (DefArgExpr (CoVar (DefArgVar v)))

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
>		      		glueTyArgs (map getIdUniType fvs) 
>					(getIdUniType id)
>			  new_id =
>			  	updateIdType id new_type
>		      in
>		      let
>		          t = foldl CoApp (CoVar (DefArgVar new_id)) 
>			  			(map mkVar fvs)
> 		      in
>		      trace ("adding " ++ show (length fvs) ++ " args to " ++ ppShow 80 (ppr PprDebug id)) $
>		      ((new_id, mkCoLam fvs e), [(id,t)])
>	where
>		fvs = case e of
>			CoLam bvs e -> filter (`notElem` bvs) total_fvs
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
> 	foldl CoApp 
>	  (foldl CoTyApp (CoVar (DefArgVar f))
>	    (map mkTyVarTy ty_args))
>	  	(map mkVar val_args)

-----------------------------------------------------------------------------
Removing duplicates from a list of definitions.

> removeDuplicateDefinitions
> 	:: [(DefExpr,(Id,DefExpr))] 	-- (label,(id,rhs))
> 	-> SUniqSM [(Id,DefExpr)]

> removeDuplicateDefinitions defs = 
> 	foldrSUs rem ([],[]) defs	`thenSUs` \(newdefs,s) ->
>	mapSUs (\(l,(f,e)) -> subst s e `thenSUs` \e -> 
>			      returnSUs (f, e)) newdefs
>   where 

> 	rem d@(l,(f,e)) (defs,s) =
>		findDup l defs		`thenSUs` \maybe ->
> 		case maybe of
>		   Nothing -> returnSUs (d:defs,s)
>		   Just g  -> returnSUs (defs, (f,(CoVar.DefArgVar) g):s)

We insist that labels rename in both directions, is this necessary?

> 	findDup l [] = returnSUs Nothing
> 	findDup l ((l',(f,e)):defs) =
> 		renameExprs l l' 	`thenSUs` \r ->
>		case r of
>		  IsRenaming _ -> renameExprs l' l 	`thenSUs` \r ->
>				  case r of
>				  	IsRenaming r -> returnSUs (Just f)
>					_ -> findDup l defs
>		  _ -> findDup l defs
