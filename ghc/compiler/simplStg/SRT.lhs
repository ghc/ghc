%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%

Run through the STG code and compute the Static Reference Table for
each let-binding.  At the same time, we figure out which top-level
bindings have no CAF references, and record the fact in their IdInfo.

\begin{code}
module SRT where

import Id       ( Id, setIdCafInfo, getIdCafInfo, externallyVisibleId,
		  idAppIsBottom
		)
import IdInfo 	( CafInfo(..) )
import StgSyn

import UniqFM
import UniqSet
\end{code}

\begin{code}
computeSRTs :: [StgBinding] -> [(StgBinding,[Id])]
computeSRTs binds = srtBinds emptyUFM binds
\end{code}

\begin{code}
srtBinds :: UniqFM CafInfo -> [StgBinding] -> [(StgBinding,[Id])] 
srtBinds rho [] = []
srtBinds rho (b:bs) = 
	srtTopBind rho b   =: \(b, srt, rho) ->
	(b,srt) : srtBinds rho bs
\end{code}

-----------------------------------------------------------------------------
Circular algorithm for simultaneously figuring out CafInfo and SRT
layout.

Our functions have type

	:: UniqFM CafInfo	-- which top-level ids don't refer to any CAfs
	-> SrtOffset		-- next free offset within the SRT
	-> (UniqSet Id,		-- global refs in the continuation
	    UniqFM (UniqSet Id))-- global refs in let-no-escaped variables
{- * -}	-> StgExpr		-- expression to analyse

	-> (StgExpr,		-- (e) newly annotated expression
	    UniqSet Id,		-- (g) set of *all* global references
	    [Id],		-- (s) SRT required for this expression
	    SrtOffset)		-- (o) new offset

(g) is a set containing all local top-level and imported ids referred
to by the expression (e).

The set of all global references is used to build the environment,
which is passed in again.  The environment is used to build the final
SRT.

We build a single SRT for a recursive binding group, which is why the
SRT building is done at the binding level rather than the
StgRhsClosure level.

Hence, the only argument which we can look at before returning is the
expression (marked with {- * -} above).

The SRT is built up in reverse order, to avoid too many expensive
appends.  We therefore reverse the SRT before returning it, so that
the offsets will be from the beginning of the SRT.

-----------------------------------------------------------------------------
Top-level Bindings

The environment contains a mapping from local top-level bindings to
CafInfo.  The CafInfo is either

	NoCafRefs      - indicating that the id is not a CAF and furthermore
		         that it doesn't refer, even indirectly, to any CAFs.
	
	MayHaveCafRefs - everything else.

A function whose CafInfo is NoCafRefs will have an empty SRT, and its
closure will not appear in the SRT of any other function (unless we're
compiling without optimisation and the CafInfos haven't been emitted
in the interface files).

Top-Level recursive groups

This gets a bit complicated, but the general idea is that we want a
single SRT for the whole group, and we'd rather not have recursive
references in it if at all possible.

We collect all the global references for the group, and filter out
those that are binders in the group and not CAFs themselves.  This set
of references is then used to infer the CafInfo for each of the
binders in the group.  Why is it done this way?

	- if all the bindings in the group just refer to each other,
	  and none of them are CAFs, we'd like to get an empty SRT.

	- if any of the bindings in the group refer to a CAF, this will
	  appear in the SRT.

Hmm, that probably makes no sense.

\begin{code}
srtTopBind 
	:: UniqFM CafInfo
	-> StgBinding
	-> (StgBinding,			-- the new binding
	    [Id],			-- the SRT for this binding
	    UniqFM CafInfo)		-- the new environment

srtTopBind rho (StgNonRec binder rhs) =

   -- no need to use circularity for non-recursive bindings
   srtRhs rho (emptyUniqSet,emptyUFM) 0{-initial offset-} rhs
					=: \(rhs, g, srt, off) ->
   let
	filtered_g = filter (mayHaveCafRefs rho) (uniqSetToList g)
        caf_info   = mk_caf_info rhs filtered_g
	binder'    = setIdCafInfo binder caf_info
        rho'       = addToUFM rho binder' caf_info
        extra_refs = filter (`notElem` srt) filtered_g
	bind_srt   = reverse (extra_refs ++ srt)
   in
   case rhs of
        StgRhsClosure _ _ _ _ _ _ _ ->
	    (StgNonRec binder' (attach_srt_rhs rhs 0 (length bind_srt)), 
	     bind_srt, rho')

	-- don't output an SRT for the constructor, but just remember
	-- whether it had any caf references or not.
	StgRhsCon _ _ _    -> (StgNonRec binder' rhs, [], rho')


srtTopBind rho (StgRec bs) =
    (attach_srt_bind (StgRec (reverse new_bs')) 0 (length bind_srt), 
	bind_srt, rho')
  where
    (binders,rhss) = unzip bs
    
    non_caf_binders = [ b | (b, rhs) <- bs, not (caf_rhs rhs) ]

    -- circular: rho' is calculated from g below
    (new_bs, g, srt, _) = doBinds bs [] emptyUniqSet [] 0

    -- filter out ourselves from the global references: it makes no
    -- sense to refer recursively to our SRT unless the recursive
    -- reference is required by a nested SRT.
    filtered_g = filter (\id -> id `notElem` non_caf_binders && 
				mayHaveCafRefs rho id) (uniqSetToList g)
    extra_refs = filter (`notElem` srt) filtered_g
    bind_srt = reverse (extra_refs ++ srt)
    caf_infos = map (\rhs -> mk_caf_info rhs filtered_g) rhss
    rho' = addListToUFM rho (zip binders caf_infos)
    binders' = zipWith setIdCafInfo binders caf_infos

    new_bs' = zip binders' (map snd new_bs)

    doBinds [] new_binds g srt off = (reverse new_binds, g, srt, off)
    doBinds ((binder,rhs):binds) new_binds g srt off =
	srtRhs rho' (emptyUniqSet,emptyUFM) off rhs 
				=: \(rhs, rhs_g, rhs_srt, off) ->
	let 
	    g'   = unionUniqSets rhs_g g
	    srt' = rhs_srt ++ srt
 	in
        doBinds binds ((binder,rhs):new_binds) g' srt' off

caf_rhs (StgRhsClosure _ _ _ free_vars _ [] body) = True
caf_rhs _ = False
\end{code}

-----------------------------------------------------------------------------
Non-top-level bindings

\begin{code}
srtBind :: UniqFM CafInfo -> (UniqSet Id, UniqFM (UniqSet Id))
	-> Int -> StgBinding -> (StgBinding, UniqSet Id, [Id], Int)

srtBind rho cont_refs off (StgNonRec binder rhs) =
  srtRhs rho cont_refs off rhs   =: \(rhs, g, srt, off) ->
  (StgNonRec binder rhs, g, srt, off)

srtBind rho cont_refs off (StgRec binds) =
    (StgRec new_binds, g, srt, new_off)
  where
    -- process each binding
    (new_binds, g, srt, new_off) = doBinds binds emptyUniqSet [] off []

    doBinds [] g srt off new_binds = (reverse new_binds, g, srt, off)
    doBinds ((binder,rhs):binds) g srt off new_binds =
        srtRhs rho cont_refs off rhs   =: \(rhs, g', srt', off) ->
	doBinds binds (unionUniqSets g g') (srt'++srt) off
		((binder,rhs):new_binds)
\end{code}

-----------------------------------------------------------------------------
Right Hand Sides

\begin{code}
srtRhs 	:: UniqFM CafInfo -> (UniqSet Id, UniqFM (UniqSet Id))
	-> Int -> StgRhs -> (StgRhs, UniqSet Id, [Id], Int)

srtRhs rho cont off (StgRhsClosure cc bi old_srt free_vars u args body) =
    srtExpr rho cont off body	=: \(body, g, srt, off) ->
    (StgRhsClosure cc bi old_srt free_vars u args body, g, srt, off)

srtRhs rho cont off e@(StgRhsCon cc con args) =
    (e, getGlobalRefs rho args, [], off)
\end{code}

-----------------------------------------------------------------------------
Expressions

\begin{code}
srtExpr :: UniqFM CafInfo -> (UniqSet Id, UniqFM (UniqSet Id))
	-> Int -> StgExpr -> (StgExpr, UniqSet Id, [Id], Int)

srtExpr rho (cont,lne) off e@(StgApp f args) = (e, global_refs, [], off)
  where global_refs = 
		cont `unionUniqSets`
		getGlobalRefs rho (StgVarArg f:args) `unionUniqSets`
		lookupPossibleLNE lne f

srtExpr rho (cont,lne) off e@(StgCon con args ty) =
   (e, cont `unionUniqSets` getGlobalRefs rho args, [], off)

srtExpr rho c@(cont,lne) off (StgCase scrut live1 live2 uniq _{-srt-} alts) =
   srtCaseAlts rho c off alts =: \(alts, alts_g, alts_srt, alts_off) ->

	-- construct the SRT for this case
   let (this_srt, scrut_off) = construct_srt rho alts_g alts_srt alts_off in

	-- global refs in the continuation is alts_g.
   srtExpr rho (alts_g,lne) scrut_off scrut
				=: \(scrut, scrut_g, scrut_srt, case_off) ->
   let
	g = unionUniqSets alts_g scrut_g
	srt = scrut_srt ++ this_srt
	srt_info = case length this_srt of
			0   -> NoSRT
			len -> SRT off len
   in
   (StgCase scrut live1 live2 uniq srt_info alts, g, srt, case_off)

srtExpr rho cont off (StgLet bind body) =
   srtLet rho cont off bind body StgLet (\_ cont -> cont)

srtExpr rho cont off (StgLetNoEscape live1 live2 b@(StgNonRec bndr rhs) body)
  = srtLet rho cont off b body (StgLetNoEscape live1 live2) calc_cont
  where calc_cont g (cont,lne) = (cont,addToUFM lne bndr g)

-- for recursive let-no-escapes, we do *two* passes, the first time
-- just to extract the list of global refs, and the second time we actually
-- construct the SRT now that we know what global refs should be in
-- the various let-no-escape continuations.
srtExpr rho conts@(cont,lne) off 
	(StgLetNoEscape live1 live2 bind@(StgRec pairs) body)
  = srtBind rho conts off bind =: \(_, g, _, _) ->
    let 
	lne' = addListToUFM lne [ (bndr,g) | (bndr,_) <- pairs ]
	calc_cont _ conts = conts
    in
    srtLet rho (cont,lne') off bind body (StgLetNoEscape live1 live2) calc_cont


srtExpr rho cont off (StgSCC cc expr) =
   srtExpr rho cont off expr	=: \(expr, g, srt, off) ->
   (StgSCC cc expr, g, srt, off)
\end{code}

-----------------------------------------------------------------------------
Let-expressions

This is quite complicated stuff...

\begin{code}
srtLet rho cont off bind body let_constr calc_cont

 -- If the bindings are all constructors, then we don't need to
 -- buid an SRT at all...
 | all_con_binds bind =
   srtBind rho cont off bind	=: \(bind, bind_g, bind_srt, off) ->
   srtExpr rho cont off body	=: \(body, body_g, body_srt, off) ->
   let
	g   = unionUniqSets bind_g body_g
	srt = body_srt ++ bind_srt
   in
   (let_constr bind body, g, srt, off)

 -- we have some closure bindings...
 | otherwise =

    -- first, find the sub-SRTs in the binding
   srtBind rho cont off bind	=: \(bind, bind_g, bind_srt, bind_off) ->

    -- construct the SRT for this binding
   let (this_srt, body_off) = construct_srt rho bind_g bind_srt bind_off in

    -- get the new continuation information (if a let-no-escape)
   let new_cont = calc_cont bind_g cont in

    -- now find the SRTs in the body
   srtExpr rho new_cont body_off body  =: \(body, body_g, body_srt, let_off) ->

   let
	-- union all the global references together
       let_g   = unionUniqSets bind_g body_g

	-- concatenate the sub-SRTs
       let_srt = body_srt ++ this_srt

	-- attach the SRT info to the binding
       bind' = attach_srt_bind bind off (length this_srt)
   in
   (let_constr bind' body, let_g, let_srt, let_off)
\end{code}

-----------------------------------------------------------------------------
Construct an SRT.

Construct the SRT at this point from its sub-SRTs and any new global
references which aren't already contained in one of the sub-SRTs (and
which are "live").

\begin{code}
construct_srt rho global_refs sub_srt current_offset
   = let
       extra_refs = filter (`notElem` sub_srt) 
		      (filter (mayHaveCafRefs rho) (uniqSetToList global_refs))
       this_srt = extra_refs ++ sub_srt

	-- Add the length of the new entries to the	
        -- current offset to get the next free offset in the global SRT.
       new_offset = current_offset + length extra_refs
   in (this_srt, new_offset)
\end{code}

-----------------------------------------------------------------------------
Case Alternatives

\begin{code}
srtCaseAlts :: UniqFM CafInfo -> (UniqSet Id, UniqFM (UniqSet Id))
	-> Int -> StgCaseAlts -> (StgCaseAlts, UniqSet Id, [Id], Int)

srtCaseAlts rho cont off (StgAlgAlts  t alts dflt) =
   srtAlgAlts rho cont off alts [] emptyUniqSet []  
				  =: \(alts, alts_g, alts_srt, off) ->
   srtDefault rho cont off dflt	  =: \(dflt, dflt_g, dflt_srt, off) ->
   let
	g   = unionUniqSets alts_g dflt_g
	srt = dflt_srt ++ alts_srt
   in
   (StgAlgAlts t alts dflt, g, srt, off)

srtCaseAlts rho cont off (StgPrimAlts t alts dflt) =
   srtPrimAlts rho cont off alts [] emptyUniqSet []  
				   =: \(alts, alts_g, alts_srt, off) ->
   srtDefault rho cont off dflt	   =: \(dflt, dflt_g, dflt_srt, off) ->
   let
	g   = unionUniqSets alts_g dflt_g
	srt = dflt_srt ++ alts_srt
   in
   (StgPrimAlts t alts dflt, g, srt, off)

srtAlgAlts rho cont off [] new_alts g srt = (reverse new_alts, g, srt, off)
srtAlgAlts rho cont off ((con,args,used,rhs):alts) new_alts g srt =
   srtExpr rho cont off rhs	=: \(rhs, rhs_g, rhs_srt, off) ->
   let
	g'   = unionUniqSets rhs_g g
	srt' = rhs_srt ++ srt
   in
   srtAlgAlts rho cont off alts ((con,args,used,rhs) : new_alts) g' srt'

srtPrimAlts rho cont off [] new_alts g srt = (reverse new_alts, g, srt, off)
srtPrimAlts rho cont off ((lit,rhs):alts) new_alts g srt =
   srtExpr rho cont off rhs	=: \(rhs, rhs_g, rhs_srt, off) ->
   let
	g'   = unionUniqSets rhs_g g
	srt' = rhs_srt ++ srt
   in
   srtPrimAlts rho cont off alts ((lit,rhs) : new_alts) g' srt'

srtDefault rho cont off StgNoDefault = (StgNoDefault,emptyUniqSet,[],off)
srtDefault rho cont off (StgBindDefault rhs) =
   srtExpr rho cont off rhs	=: \(rhs, g, srt, off) ->
   (StgBindDefault rhs, g, srt, off)
\end{code}

-----------------------------------------------------------------------------

Decide whether a closure looks like a CAF or not.  In an effort to
keep the number of CAFs (and hence the size of the SRTs) down, we
would also like to look at the expression and decide whether it
requires a small bounded amount of heap, so we can ignore it as a CAF.
In these cases, we need to use an additional CAF list to keep track of
non-collectable CAFs.

We mark real CAFs as `MayHaveCafRefs' because this information is used
to decide whether a particular closure needs to be referenced in an
SRT or not.

\begin{code}
mk_caf_info 
	:: StgRhs			-- right-hand-side of the definition
	-> [Id]				-- static references
	-> CafInfo

-- special case for expressions which are always bottom,
-- such as 'error "..."'.  We don't need to record it as
-- a CAF, since it can only be entered once.
mk_caf_info (StgRhsClosure _ _ _ free_vars _ [] e) srt
        | isBottomingExpr e && null srt = NoCafRefs

mk_caf_info (StgRhsClosure _ _ _ free_vars upd args body) srt 
	| isUpdatable upd = MayHaveCafRefs -- a real live CAF
	| null srt  = NoCafRefs		 -- function w/ no static references
	| otherwise = MayHaveCafRefs	 -- function w/ some static references

mk_caf_info rcon@(StgRhsCon cc con args) srt 
	| null srt   = NoCafRefs 	 -- constructor w/ no static references
	| otherwise  = MayHaveCafRefs	 -- otherwise, treat as a CAF


isBottomingExpr (StgLet bind expr) = isBottomingExpr expr
isBottomingExpr (StgApp f args)    = idAppIsBottom f (length args)
isBottomingExpr _ 		   = False
\end{code}

-----------------------------------------------------------------------------

Here we decide which Id's to place in the static reference table.  An
internal top-level id will be in the environment with the appropriate
CafInfo, so we use that if available.  An imported top-level Id will
have the CafInfo attached.  Otherwise, we just ignore the Id.

\begin{code}
getGlobalRefs :: UniqFM CafInfo -> [StgArg] -> UniqSet Id
getGlobalRefs rho args = mkUniqSet (concat (map (globalRefArg rho) args))

globalRefArg :: UniqFM CafInfo -> StgArg -> [Id]

globalRefArg rho (StgVarArg id)

  | otherwise =
    case lookupUFM rho id of {
	Just _ -> [id];			-- can't look at the caf_info yet...
        Nothing ->

    if externallyVisibleId id 
	then case getIdCafInfo id of
		MayHaveCafRefs -> [id]
		NoCafRefs      -> []
	else []
   }

globalRefArg rho _ = []
\end{code}

\begin{code}
mayHaveCafRefs rho id =
  case lookupUFM rho id of
	Just MayHaveCafRefs -> True
	Just NoCafRefs	    -> False
	Nothing		    -> True
\end{code}

-----------------------------------------------------------------------------
Misc stuff

\begin{code}
attach_srt_bind :: StgBinding -> Int -> Int -> StgBinding
attach_srt_bind (StgNonRec binder rhs) off len = 
	StgNonRec binder (attach_srt_rhs rhs off len)
attach_srt_bind (StgRec binds) off len =
	StgRec [ (v,attach_srt_rhs rhs off len) | (v,rhs) <- binds ]

attach_srt_rhs :: StgRhs -> Int -> Int -> StgRhs
attach_srt_rhs (StgRhsCon cc con args) off length
  = StgRhsCon cc con args
attach_srt_rhs (StgRhsClosure cc bi _ free upd args rhs) off length
  = StgRhsClosure cc bi srt free upd args rhs
  where
 	srt | length == 0 = NoSRT
	    | otherwise   = SRT off length


all_con_binds (StgNonRec x rhs) = con_rhs rhs
all_con_binds (StgRec bs) = all con_rhs (map snd bs)

con_rhs (StgRhsCon _ _ _) = True
con_rhs _ = False


a =: k  = k a
\end{code}

-----------------------------------------------------------------------------
Fix up the SRT's in a let-no-escape.

(for a description of let-no-escapes, see CgLetNoEscape.lhs)

Here's the problem: a let-no-escape isn't represented by an activation
record on the stack.  It seems either very difficult or impossible to
get the liveness bitmap right in the info table, so we don't do it
this way (the liveness mask isn't constant).

So, the question is how does the garbage collector get access to the
SRT for the rhs of the let-no-escape?  It can't see an info table, so
it must get the SRT from somewhere else.  Here's an example:

   let-no-escape x = .... f ....
   in  case blah of
	   p -> .... x ... g ....

(f and g are global).  Suppose we garbage collect while evaluating
'blah'.  The stack will contain an activation record for the case,
which will point to an SRT containing [g] (according to our SRT
algorithm above).  But, since the case continuation can call x, and
hence f, the SRT should really be [f,g].

another example:

   let-no-escape {-rec-} z =  \x -> case blah of
				      p1 ->  .... f ...
				      p2 ->  case blah2 of
						p -> .... (z x') ...
   in ....

if we GC while evaluating blah2, then the case continuation on the
stack needs to refer to [f] in its SRT, because we can reach f by
calling z recursively.

FIX:

The following code fixes up a let-no-escape expression after we've run
the SRT algorithm.  It needs to know the SRT for the *whole*
expression (this is plugged in instead of the SRT for case exprsesions
in the body).  The good news is that we only need to traverse nested
case expressions, since the let-no-escape bound variable can't occur
in the rhs of a let or in a case scrutinee.

For recursive let-no-escapes, the body is processed as for
non-recursive let-no-escapes, but case expressions in the rhs of each
binding have their SRTs replaced with the SRT for the binding group
(*not* the SRT of the whole let-no-escape expression).

\begin{code}
lookupPossibleLNE lne_env f = 
  case lookupUFM lne_env f of
	Nothing   -> emptyUniqSet
	Just refs -> refs
\end{code}
