%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[Deforest]{Top level deforestation module}

>#include "HsVersions.h"
>
> module Deforest (
> 	deforestProgram
>	) where

> import Core2Def
> import Def2Core
> import DefUtils
> import DefSyn
> import DefExpr
> import Cyclic
> import TreelessForm
>#ifdef __HBC__
> import Trace
>#endif

> import CmdLineOpts	( GlobalSwitch, SwitchResult )
> import CoreSyn
> import Id		( getIdInfo, Id )
> import IdInfo
> import Outputable
> import SimplEnv	( SwitchChecker(..) )
> import UniqSupply
> import Util

> -- tmp, for traces
> import Pretty

> -- stub (ToDo)
> domIdEnv = panic "Deforest: domIdEnv"

> deforestProgram
> 	:: SwitchChecker GlobalSwitch{-maybe-}
>	-> [CoreBinding]
>	-> UniqSupply
>	-> [CoreBinding]
>
> deforestProgram sw prog uq =
> 	let
>		def_program = core2def sw prog
>		out_program = (
>			defProg sw nullIdEnv def_program  `thenUs` \prog ->
>			def2core prog)
>			uq
>	in
>		out_program

We have to collect all the unfoldings (functions that were annotated
with DEFOREST) and pass them in an environment to subsequent calls of
the transformer.

Recursive functions are first transformed by the deforester.  If the
function is annotated as deforestable, then it is converted to
treeless form for unfolding later on.

Also converting non-recursive functions that are annotated with
{-# DEFOREST #-} now.  Probably don't need to convert these to treeless
form: just the inner recursive bindings they contain.  eg:

repeat = \x -> letrec xs = x:xs in xs

is non-recursive, but we want to unfold it and annotate the binding
for xs as unfoldable, too.

> defProg
> 	:: SwitchChecker GlobalSwitch{-maybe-}
>	-> IdEnv DefExpr
>	-> [DefBinding]
>	-> UniqSM [DefBinding]
>
> defProg sw p [] = returnUs []
>
> defProg sw p (NonRec v e : bs) =
> 	trace ("Processing: `" ++
>		     	ppShow 80 (ppr PprDebug v) ++ "'\n") (
>	tran sw p nullTyVarEnv e []	 	`thenUs` \e ->
>	mkLoops e				`thenUs` \(extracted,e) ->
>	let e' = mkDefLetrec extracted e in
>	(
>	  if deforestable v then
>	  	let (vs,es) = unzip extracted in
>		convertToTreelessForm sw e	`thenUs` \e ->
>		mapUs (convertToTreelessForm sw) es	`thenUs` \es ->
>		defProg sw (growIdEnvList p ((v,e):zip vs es)) bs
>	  else
>		defProg sw p bs
>	)					`thenUs` \bs ->
>	returnUs (NonRec v e' : bs)
>	)
>
> defProg sw p (Rec bs : bs') =
>	mapUs (defRecBind sw p) bs		`thenUs` \res  ->
>	let
>		(resid, unfold) = unzip res
>		p' = growIdEnvList p (concat unfold)
>	in
>	defProg sw p' bs' 			`thenUs` \bs' ->
>	returnUs (Rec resid: bs')


> defRecBind
> 	:: SwitchChecker GlobalSwitch{-maybe-}
>	-> IdEnv DefExpr
>	-> (Id,DefExpr)
>	-> UniqSM ((Id,DefExpr),[(Id,DefExpr)])
>
> defRecBind sw p (v,e) =
> 	trace ("Processing: `" ++
>		     	ppShow 80 (ppr PprDebug v) ++ "'\n") (
> 	tran sw p nullTyVarEnv e []		`thenUs` \e' ->
>	mkLoops e'				`thenUs` \(bs,e') ->
>	let e'' = mkDefLetrec bs e' in
>
>	d2c e'' `thenUs` \core_e ->
>	let showBind (v,e) = ppShow 80 (ppr PprDebug v) ++
>		"=\n" ++ ppShow 80 (ppr PprDebug e) ++ "\n"
>	in
>	trace ("Extracting from `" ++
>		ppShow 80 (ppr PprDebug v) ++ "'\n"
>		++ "{ result:\n" ++ showBind (v,core_e) ++ "}\n") $
>
>	if deforestable v
>		then
>			let (vs,es) = unzip bs in
>		     	convertToTreelessForm sw e'	`thenUs` \e' ->
>			mapUs (convertToTreelessForm sw) es `thenUs` \es ->
>		     	returnUs ((v,e''),(v,e'):zip vs es)
>		else
>			trace (show (length bs)) (
>			returnUs ((v,e''),[])
>			)
>	)
