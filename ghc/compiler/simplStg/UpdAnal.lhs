\section{Update Avoidance Analyser}			-*-haskell-literate-*-

(c) Simon Marlow, Andre Santos 1992-1993
(c) The AQUA Project, Glasgow University, 1995

%-----------------------------------------------------------------------------
\subsection{Module Interface}

\begin{code}
#include  "HsVersions.h"
\end{code}

> module UpdAnal ( updateAnalyse ) where
>
> IMPORT_Trace

> import AbsUniType	( splitTyArgs, splitType, Class, TyVarTemplate, 
> 			  TauType(..)
>			)
> import Id
> import IdEnv
> import IdInfo
> import Outputable	( isExported )
> import Pretty
> import SrcLoc 	( mkUnknownSrcLoc )
> import StgSyn
> import UniqSet
> import Unique 	( getBuiltinUniques )
> import Util

%-----------------------------------------------------------------------------
\subsection{Reverse application}

This is used instead of lazy pattern bindings to avoid space leaks.

> infixr 3 =:
> a =: k = k a

%-----------------------------------------------------------------------------
\subsection{Types}

List of closure references

> type Refs = IdSet
> x `notInRefs` y = not (x `elementOfUniqSet` y)

A closure value: environment of closures that are evaluated on entry,
a list of closures that are referenced from the result, and an
abstract value for the evaluated closure.

An IdEnv is used for the reference counts, as these environments are
combined often. A generic environment is used for the main environment
mapping closure names to values; as a common operation is extension of
this environment, this representation should be efficient.

> -- partain: funny synonyms to cope w/ the fact
> -- that IdEnvs know longer know what their keys are
> -- (94/05)  ToDo: improve
> type IdEnvInt	    = IdEnv (Id, Int)
> type IdEnvClosure = IdEnv (Id, Closure)

> -- backward-compat functions
> null_IdEnv :: IdEnv (Id, a)
> null_IdEnv = nullIdEnv
>
> unit_IdEnv :: Id -> a -> IdEnv (Id, a)
> unit_IdEnv k v = unitIdEnv k (k, v)
>
> mk_IdEnv :: [(Id, a)] -> IdEnv (Id, a)
> mk_IdEnv pairs = mkIdEnv [ (k, (k,v)) | (k,v) <- pairs ]
>
> grow_IdEnv :: IdEnv (Id, a) -> IdEnv (Id, a) -> IdEnv (Id, a)
> grow_IdEnv env1 env2 = growIdEnv env1 env2
>
> addOneTo_IdEnv :: IdEnv (Id, a) -> Id -> a -> IdEnv (Id, a)
> addOneTo_IdEnv env k v = addOneToIdEnv env k (k, v)
>
> combine_IdEnvs :: (a->a->a) -> IdEnv (Id, a) -> IdEnv (Id, a) -> IdEnv (Id, a)
> combine_IdEnvs combiner env1 env2 = combineIdEnvs new_combiner env1 env2
>   where
>     new_combiner (id, x) (_, y) = (id, combiner x y)
>
> dom_IdEnv :: IdEnv (Id, a) -> Refs
> dom_IdEnv env = mkUniqSet [ i | (i,_) <- rngIdEnv env ]
>
> lookup_IdEnv :: IdEnv (Id, a) -> Id -> Maybe a
> lookup_IdEnv env key = case lookupIdEnv env key of
>			   Nothing    -> Nothing
>			   Just (_,a) -> Just a
> -- end backward compat stuff

> type Closure = (IdEnvInt, Refs, AbFun)

> type AbVal = IdEnvClosure -> Closure
> data AbFun = Fun (Closure -> Closure)

> -- partain: speeding-up stuff
>
> type CaseBoundVars = IdSet
> noCaseBound   = emptyUniqSet
> isCaseBound   = elementOfUniqSet
> x `notCaseBound` y = not (isCaseBound x y)
> moreCaseBound :: CaseBoundVars -> [Id] -> CaseBoundVars
> moreCaseBound old new = old `unionUniqSets` mkUniqSet new
>
> -- end speeding-up

%----------------------------------------------------------------------------
\subsection{Environment lookup}

If the requested value is not in the environment, we return an unknown
value.  Lookup is designed to be partially applied to a variable, and
repeatedly applied to different environments after that.

> lookup v
>   | isImportedId v 
>   = const (case updateInfoMaybe (getIdUpdateInfo v) of
>		Nothing   -> unknownClosure
>		Just spec -> convertUpdateSpec spec)
>   | otherwise	 
>   = \p -> case lookup_IdEnv p v of
>		Just b  -> b
>		Nothing -> unknownClosure

%-----------------------------------------------------------------------------
Represent a list of references as an ordered list.

> mkRefs :: [Id] -> Refs
> mkRefs = mkUniqSet

> noRefs :: Refs
> noRefs = emptyUniqSet

> elemRefs = elementOfUniqSet

> merge :: [Refs] -> Refs
> merge xs = foldr merge2 emptyUniqSet xs

> merge2 :: Refs -> Refs -> Refs
> merge2 = unionUniqSets

%-----------------------------------------------------------------------------
\subsection{Some non-interesting values}

bottom will be used for abstract values that are not functions.
Hopefully its value will never be required!

> bottom 		:: AbFun
> bottom 		= panic "Internal: (Update Analyser) bottom"

noClosure is a value that is definitely not a function (i.e. primitive
values and constructor applications).  unknownClosure is a value about
which we have no information at all.  This should occur rarely, but
could happen when an id is imported and the exporting module was not
compiled with the update analyser.

> noClosure, unknownClosure :: Closure
> noClosure 		= (null_IdEnv, noRefs, bottom)
> unknownClosure 	= (null_IdEnv, noRefs, dont_know noRefs)

dont_know is a black hole: it is something we know nothing about.
Applying dont_know to anything will generate a new dont_know that simply
contains more buried references.

> dont_know :: Refs -> AbFun
> dont_know b'
> 	= Fun (\(c,b,f) -> let b'' = dom_IdEnv c `merge2` b `merge2` b'
>                          in (null_IdEnv, b'', dont_know b''))

%-----------------------------------------------------------------------------

> getrefs :: IdEnvClosure -> [AbVal] -> Refs -> Refs
> getrefs p vs rest = foldr merge2 rest  (getrefs' (map ($ p) vs))
>	where
> 		getrefs' []	      = []
> 		getrefs' ((c,b,_):rs) = dom_IdEnv c : b : getrefs' rs

%-----------------------------------------------------------------------------

udData is used when we are putting a list of closure references into a
data structure, or something else that we know nothing about.

> udData :: [PlainStgAtom] -> CaseBoundVars -> AbVal
> udData vs cvs
> 	= \p -> (null_IdEnv, getrefs p local_ids noRefs, bottom)
>	where local_ids = [ lookup v | (StgVarAtom v) <- vs, v `notCaseBound` cvs ]

%-----------------------------------------------------------------------------
\subsection{Analysing an atom}

> udAtom :: CaseBoundVars -> PlainStgAtom -> AbVal
> udAtom cvs (StgVarAtom v) 
>	| v `isCaseBound` cvs = const unknownClosure
>	| otherwise	      = lookup v
>
> udAtom cvs _		      = const noClosure

%-----------------------------------------------------------------------------
\subsection{Analysing an STG expression}

> ud :: PlainStgExpr			-- Expression to be analysed
>    -> CaseBoundVars			-- List of case-bound vars
>    -> IdEnvClosure			-- Current environment
>    -> (PlainStgExpr, AbVal)		-- (New expression, abstract value)
>
> ud e@(StgPrimApp _ vs _) cvs p = (e, udData vs cvs)
> ud e@(StgConApp  _ vs _) cvs p = (e, udData vs cvs)
> ud e@(StgSCC ty lab a)   cvs p = ud a cvs p =: \(a', abval_a) ->
>                                  (StgSCC ty lab a', abval_a)

Here is application. The first thing to do is analyse the head, and
get an abstract function. Multiple applications are performed by using
a foldl with the function doApp. Closures are actually passed to the
abstract function iff the atom is a local variable.

I've left the type signature for doApp in to make things a bit clearer.

> ud e@(StgApp a atoms lvs) cvs p
>   = (e, abval_app)
>   where
>     abval_atoms = map (udAtom cvs) atoms
>     abval_a     = udAtom cvs a
>     abval_app = \p -> 
>	let doApp :: Closure -> AbVal -> Closure
>	    doApp (c, b, Fun f) abval_atom =
>		  abval_atom p		=: \e@(_,_,_)    -> 
> 		  f e			=: \(c', b', f') -> 
>		  (combine_IdEnvs (+) c' c, b', f')
>	in foldl doApp (abval_a p) abval_atoms

> ud (StgCase expr lve lva uniq alts) cvs p
>   = ud expr cvs p			=: \(expr', abval_selector)  ->
>     udAlt alts p			=: \(alts', abval_alts) ->
>     let
>     	abval_case = \p ->
>      	  abval_selector p		=: \(c, b, abfun_selector) ->
>	  abval_alts p			=: \(cs, bs, abfun_alts)   ->
>	  let bs' = b `merge2` bs in
>      	  (combine_IdEnvs (+) c cs, bs', dont_know bs')
>     in
>     (StgCase expr' lve lva uniq alts', abval_case)
>   where
>   
>     udAlt :: PlainStgCaseAlternatives
>           -> IdEnvClosure
>           -> (PlainStgCaseAlternatives, AbVal)
>	    
>     udAlt (StgAlgAlts ty [alt] StgNoDefault) p
>         = udAlgAlt p alt		=: \(alt', abval) ->
>	    (StgAlgAlts ty [alt'] StgNoDefault, abval)
>     udAlt (StgAlgAlts ty [] def) p
>         = udDef def p			=: \(def', abval) ->
>           (StgAlgAlts ty [] def', abval)
>     udAlt (StgAlgAlts ty alts def) p
>         = udManyAlts alts def udAlgAlt (StgAlgAlts ty) p
>     udAlt (StgPrimAlts ty [alt] StgNoDefault) p
>         = udPrimAlt p alt		=: \(alt', abval) ->
>           (StgPrimAlts ty [alt'] StgNoDefault, abval)
>     udAlt (StgPrimAlts ty [] def) p
>         = udDef def p			=: \(def', abval) ->
>           (StgPrimAlts ty [] def', abval)
>     udAlt (StgPrimAlts ty alts def) p
>         = udManyAlts alts def udPrimAlt (StgPrimAlts ty) p
>
>     udPrimAlt p (l, e)
>       = ud e cvs p		=: \(e', v) -> ((l, e'), v)
>
>     udAlgAlt p (id, vs, use_mask, e)
>       = ud e (moreCaseBound cvs vs) p	=: \(e', v) -> ((id, vs, use_mask, e'), v)
>
>     udDef :: PlainStgCaseDefault
>           -> IdEnvClosure
>           -> (PlainStgCaseDefault, AbVal)
> 
>     udDef StgNoDefault p
>       = (StgNoDefault, \p -> (null_IdEnv, noRefs, dont_know noRefs))
>     udDef (StgBindDefault v is_used expr) p
>       = ud expr (moreCaseBound cvs [v]) p 	=: \(expr', abval) ->
>	  (StgBindDefault v is_used expr', abval)
>
>     udManyAlts alts def udalt stgalts p
>	= udDef def p				=: \(def', abval_def) ->
>	  unzip (map (udalt p) alts)	 	=: \(alts', abvals_alts) ->
>	  let
>		abval_alts = \p ->
>		  abval_def p			 =: \(cd, bd, _) ->
>		  unzip3 (map ($ p) abvals_alts) =: \(cs, bs, _) ->
>		  let bs' = merge (bd:bs) in
>		  (foldr (combine_IdEnvs max) cd cs, bs', dont_know bs')
>	  in (stgalts alts' def', abval_alts)

The heart of the analysis: here we decide whether to make a specific
closure updatable or not, based on the results of analysing the body.

> ud (StgLet binds body) cvs p
>  = udBinding binds cvs p		=: \(binds', vs, abval1, abval2) ->
>    abval1 p				=: \(cs, p') ->
>    grow_IdEnv p p'			=: \p ->
>    ud body cvs p			=: \(body', abval_body) ->
>    abval_body	p 			=: \(c, b, abfun) ->
>    tag b (combine_IdEnvs (+) cs c) binds' =: \tagged_binds ->
>    let 
>       abval p
>	  = abval2 p				=: \(c1, p')       ->
>    	    abval_body (grow_IdEnv p p')	=: \(c2, b, abfun) ->
>	    (combine_IdEnvs (+) c1 c2, b, abfun)
>    in
>    (StgLet tagged_binds body', abval)

%-----------------------------------------------------------------------------
\subsection{Analysing bindings}
  
For recursive sets of bindings we perform one iteration of a fixed
point algorithm, using (dont_know fv) as a safe approximation to the
real fixed point, where fv are the (mappings in the environment of
the) free variables of the function.

We'll return two new environments, one with the new closures in and
one without. There's no point in carrying around closures when their
respective bindings have already been analysed.

We don't need to find anything out about closures with arguments,
constructor closures etc.
  
> udBinding :: PlainStgBinding
>	    -> CaseBoundVars
>           -> IdEnvClosure
>	    -> (PlainStgBinding,
>		[Id],
>	    	IdEnvClosure -> (IdEnvInt, IdEnvClosure),
>		IdEnvClosure -> (IdEnvInt, IdEnvClosure))
> 
> udBinding (StgNonRec v rhs) cvs p
>   = udRhs rhs cvs p			=: \(rhs', abval) ->
>     abval p				=: \(c, b, abfun) ->
>     let
>     	abval_rhs a = \p ->
>     	   abval p			=: \(c, b, abfun) ->
>	   (c, unit_IdEnv v (a, b, abfun))
>	a = case rhs of
>		StgRhsClosure _ _ _ Updatable [] _ -> unit_IdEnv v 1
>		_			           -> null_IdEnv
>     in (StgNonRec v rhs', [v],  abval_rhs a, abval_rhs null_IdEnv)
>
> udBinding (StgRec ve) cvs p
>   = (StgRec ve', [], abval_rhs, abval_rhs)
>   where
>     (vs, ve', abvals) = unzip3 (map udBind ve)
>     fv = (map lookup . filter (`notCaseBound` cvs) . concat . map collectfv) ve
>     vs' = mkRefs vs
>     abval_rhs = \p ->
>     	let
>     	  p' = grow_IdEnv (mk_IdEnv (vs `zip` (repeat closure))) p
>	  closure = (null_IdEnv, fv', dont_know fv')
>     	  fv' =  getrefs p fv vs'
>	  (cs, ps) = unzip (doRec vs abvals)
>
>	  doRec [] _ = []
>	  doRec (v:vs) (abval:as) 
>	  	= abval p'	=: \(c,b,abfun) ->
>		  (c, (v,(null_IdEnv, b, abfun))) : doRec vs as
>		
>      	in
>	(foldr (combine_IdEnvs (+)) null_IdEnv cs, mk_IdEnv ps)
>     
>     udBind (v,rhs)
>       = udRhs rhs cvs p		=: \(rhs', abval) ->
>	  (v,(v,rhs'), abval)
>
>     collectfv (_, StgRhsClosure _ _ fv _ _ _) = fv
>     collectfv (_, StgRhsCon _ con args)       = [ v | (StgVarAtom v) <- args ]
  
%-----------------------------------------------------------------------------
\subsection{Analysing Right-Hand Sides}

> udRhs e@(StgRhsCon _ _ vs) cvs p = (e, udData vs cvs)
>
> udRhs (StgRhsClosure cc bi fv u [] body) cvs p
>   = ud body cvs p			=: \(body', abval_body) ->
>     (StgRhsClosure cc bi fv u [] body', abval_body)

Here is the code for closures with arguments.  A closure has a number
of arguments, which correspond to a set of nested lambda expressions.
We build up the analysis using foldr with the function doLam to
analyse each lambda expression.

> udRhs (StgRhsClosure cc bi fv u args body) cvs p
>   = ud body cvs p			=: \(body', abval_body) ->
>     let
>	fv' = map lookup (filter (`notCaseBound` cvs) fv)
>       abval_rhs = \p ->
>	     foldr doLam (\b -> abval_body) args (getrefs p fv' noRefs) p
>     in
>     (StgRhsClosure cc bi fv u args body', abval_rhs)
>     where
>
>       doLam :: Id -> (Refs -> AbVal) -> Refs -> AbVal
>       doLam i f b p
>		= (null_IdEnv, b, 
>		   Fun (\x@(c',b',_) -> 
>		   	let b'' = dom_IdEnv c' `merge2` b' `merge2` b in
>			f b'' (addOneTo_IdEnv p i x)))
  
%-----------------------------------------------------------------------------
\subsection{Adjusting Update flags}

The closure is tagged single entry iff it is used at most once, it is
not referenced from inside a data structure or function, and it has no
arguments (closures with arguments are re-entrant).

> tag :: Refs -> IdEnvInt -> PlainStgBinding -> PlainStgBinding
>
> tag b c r@(StgNonRec v (StgRhsClosure cc bi fv Updatable [] body)) 
>   = if (v `notInRefs` b) && (lookupc c v <= 1)
>     then -- trace "One!" (
>	   StgNonRec v (StgRhsClosure cc bi fv SingleEntry [] body)
>	   -- )
>     else r
> tag b c other = other
>
> lookupc c v = case lookup_IdEnv c v of
>                 Just n -> n
>                 Nothing -> 0

%-----------------------------------------------------------------------------
\subsection{Top Level analysis}

Should we tag top level closures? This could have good implications
for CAFs (i.e. they could be made non-updateable if only used once,
thus preventing a space leak).

> updateAnalyse :: PlainStgProgram -> PlainStgProgram {- Exported -}
> updateAnalyse bs 
>  = udProgram bs null_IdEnv
  
> udProgram :: PlainStgProgram -> IdEnvClosure -> PlainStgProgram
> udProgram [] p = []
> udProgram (d:ds) p
>  = udBinding d noCaseBound p		=: \(d', vs, _, abval_bind) ->
>    abval_bind p			=: \(_, p') ->
>    grow_IdEnv p p'			=: \p'' ->
>    attachUpdateInfoToBinds d' p''	=: \d'' ->
>    d'' : udProgram ds p''

%-----------------------------------------------------------------------------
\subsection{Exporting Update Information}

Convert the exported representation of a function's update function
into a real Closure value.

> convertUpdateSpec :: UpdateSpec -> Closure
> convertUpdateSpec = mkClosure null_IdEnv noRefs noRefs

> mkClosure :: IdEnvInt -> Refs -> Refs -> UpdateSpec -> Closure
>
> mkClosure c b b' []       = (c, b', dont_know b')
> mkClosure c b b' (0 : ns) = (null_IdEnv, b, Fun (\ _ -> mkClosure c b b' ns))
> mkClosure c b b' (1 : ns) = (null_IdEnv, b, Fun (\ (c',b'',f) -> 
>     mkClosure 
>             (combine_IdEnvs (+) c c') 
>             (dom_IdEnv c' `merge2` b'' `merge2` b)
>             (b'' `merge2` b')
>	      ns ))
> mkClosure c b b' (n : ns) = (null_IdEnv, b, Fun (\ (c',b'',f) ->
>     mkClosure c 
>             (dom_IdEnv c' `merge2` b'' `merge2` b)
>             (dom_IdEnv c' `merge2` b'' `merge2` b')
>	      ns ))

Convert a Closure into a representation that can be placed in a .hi file.

> mkUpdateSpec :: Id -> Closure -> UpdateSpec
> mkUpdateSpec v f = {- removeSuperfluous2s -} (map countUses ids)
>	    where 
>		(c,b,_)     = foldl doApp f ids
>	      	ids         = map mkid (getBuiltinUniques arity)
>	      	mkid u      = mkSysLocal SLIT("upd") u noType mkUnknownSrcLoc
>	      	countUses u = if u `elemRefs` b then 2 else min (lookupc c u) 2
>	      	noType      = panic "UpdAnal: no type!"
>
>     		doApp (c,b,Fun f) i
>       		= f (unit_IdEnv i 1, noRefs, dont_know noRefs)  =: \(c',b',f') -> 
>	  		  (combine_IdEnvs (+) c' c, b', f')
>
>		(_,dict_tys,tau_ty) = (splitType . getIdUniType) v
> 	        (reg_arg_tys, _)    = splitTyArgs tau_ty
>		arity               = length dict_tys + length reg_arg_tys

  removeSuperfluous2s = reverse . dropWhile (> 1) . reverse

%-----------------------------------------------------------------------------
\subsection{Attaching the update information to top-level bindings}

This is so that the information can later be retrieved for printing
out in the .hi file.  This is not an ideal solution, however it will
suffice for now.

> attachUpdateInfoToBinds b p
>   = case b of
>	StgNonRec v rhs -> StgNonRec (attachOne v) rhs
>	StgRec bs 	-> StgRec [ (attachOne v, rhs) | (v, rhs) <- bs ]
>	
>   where attachOne v
>		| isExported v 
>			= let c = lookup v p in
>		 		addIdUpdateInfo v 
>					(mkUpdateInfo (mkUpdateSpec v c))
>		| otherwise    = v

%-----------------------------------------------------------------------------
