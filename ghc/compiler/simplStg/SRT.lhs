%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%

Run through the STG code and compute the Static Reference Table for
each let-binding.  At the same time, we figure out which top-level
bindings have no CAF references, and record the fact in their IdInfo.

\begin{code}
module SRT where

#include "HsVersions.h"

import Id        ( Id, idCafInfo )
import IdInfo 	 ( mayHaveCafRefs )
import StgSyn

import UniqFM
import UniqSet
import Panic

#ifdef DEBUG
import Outputable
#endif
\end{code}

\begin{code}
computeSRTs :: [StgBinding] -> [(StgBinding,[Id])]
computeSRTs binds = map srtTopBind binds
\end{code}

-----------------------------------------------------------------------------
Algorithm for figuring out SRT layout.

Our functions have type

	:: SrtOffset		-- next free offset within the SRT
	-> (UniqSet Id,		-- global refs in the continuation
	    UniqFM (UniqSet Id))-- global refs in let-no-escaped variables
{- * -}	-> StgExpr		-- expression to analyse

	-> (StgExpr,		-- (e) newly annotated expression
	    UniqSet Id,		-- (g) global refs from this expression
	    [Id],		-- (s) SRT required for this expression
	    SrtOffset)		-- (o) new offset

(g) is a set containing all local top-level and imported ids referred
to by the expression (e), which have MayHaveCafRefs in their CafInfo.

We build a single SRT for a recursive binding group, which is why the
SRT building is done at the binding level rather than the
StgRhsClosure level.

The SRT is built up in reverse order, to avoid too many expensive
appends.  We therefore reverse the SRT before returning it, so that
the offsets will be from the beginning of the SRT.

-----------------------------------------------------------------------------
Top-level Bindings

A function whose CafInfo is NoCafRefs will have an empty SRT, and its
closure will not appear in the SRT of any other function (unless we're
compiling without optimisation and the CafInfos haven't been emitted
in the interface files).

Top-Level recursive groups

This gets a bit complicated, but the general idea is that we want a
single SRT for the whole group, and we'd rather not have recursive
references in it if at all possible.

We collect all the global references for the group, and filter out
those that are binders in the group and not CAFs themselves.  Why is
it done this way?

	- if all the bindings in the group just refer to each other,
	  and none of them are CAFs, we'd like to get an empty SRT.

	- if any of the bindings in the group refer to a CAF, this will
	  appear in the SRT.

Hmm, that probably makes no sense.

\begin{code}
srtTopBind 
	:: StgBinding
	-> (StgBinding,			-- the new binding
	    [Id])			-- the SRT for this binding

srtTopBind (StgNonRec binder rhs) =

   -- no need to use circularity for non-recursive bindings
   srtRhs (emptyUniqSet,emptyUFM) 0{-initial offset-} rhs
					=: \(rhs, g, srt, off) ->
   let
	filtered_g = uniqSetToList g
        extra_refs = filter (`notElem` srt) filtered_g
	bind_srt   = reverse (extra_refs ++ srt)
   in
   ASSERT2(null bind_srt || idMayHaveCafRefs binder, ppr binder)

   case rhs of
        StgRhsClosure _ _ _ _ _ _ _ ->
	    (StgNonRec binder (attach_srt_rhs rhs 0 (length bind_srt)), 
	     bind_srt)

	-- don't output an SRT for the constructor
	StgRhsCon _ _ _    -> (StgNonRec binder rhs, [])


srtTopBind (StgRec bs) =
    ASSERT(null bind_srt || all idMayHaveCafRefs binders)
    (attach_srt_bind (StgRec new_bs) 0 (length bind_srt), bind_srt)
  where
    (binders,rhss) = unzip bs
    
    non_caf_binders = [ b | (b, rhs) <- bs, not (caf_rhs rhs) ]

    (new_bs, g, srt, _) = doBinds bs [] emptyUniqSet [] 0

    -- filter out ourselves from the global references: it makes no
    -- sense to refer recursively to our SRT unless the recursive
    -- reference is required by a nested SRT.
    filtered_g = filter (\id -> id `notElem` non_caf_binders) (uniqSetToList g)
    extra_refs = filter (`notElem` srt) filtered_g
    bind_srt = reverse (extra_refs ++ srt)

    doBinds [] new_binds g srt off = (reverse new_binds, g, srt, off)
    doBinds ((binder,rhs):binds) new_binds g srt off =
	srtRhs (emptyUniqSet,emptyUFM) off rhs 
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
srtBind :: (UniqSet Id, UniqFM (UniqSet Id))
	-> Int -> StgBinding -> (StgBinding, UniqSet Id, [Id], Int)

srtBind cont_refs off (StgNonRec binder rhs) =
  srtRhs cont_refs off rhs   =: \(rhs, g, srt, off) ->
  (StgNonRec binder rhs, g, srt, off)

srtBind cont_refs off (StgRec binds) =
  (StgRec new_binds, g, srt, new_off)
  where
    -- process each binding
    (new_binds, g, srt, new_off) = doBinds binds emptyUniqSet [] off []

    doBinds [] g srt off new_binds = (reverse new_binds, g, srt, off)
    doBinds ((binder,rhs):binds) g srt off new_binds =
        srtRhs cont_refs off rhs   =: \(rhs, g', srt', off) ->
	doBinds binds (unionUniqSets g g') (srt'++srt) off
		((binder,rhs):new_binds)
\end{code}

-----------------------------------------------------------------------------
Right Hand Sides

\begin{code}
srtRhs 	:: (UniqSet Id, UniqFM (UniqSet Id))
	-> Int -> StgRhs -> (StgRhs, UniqSet Id, [Id], Int)

srtRhs cont off (StgRhsClosure cc bi old_srt free_vars u args body) =
    srtExpr cont off body	=: \(body, g, srt, off) ->
    (StgRhsClosure cc bi old_srt free_vars u args body, g, srt, off)

srtRhs cont off e@(StgRhsCon cc con args) =
    (e, getGlobalRefs args, [], off)
\end{code}

-----------------------------------------------------------------------------
Expressions

\begin{code}
srtExpr :: (UniqSet Id, UniqFM (UniqSet Id))
	-> Int -> StgExpr -> (StgExpr, UniqSet Id, [Id], Int)

srtExpr (cont,lne) off e@(StgApp f args) = (e, global_refs, [], off)
  where global_refs = 
		cont `unionUniqSets`
		getGlobalRefs (StgVarArg f:args) `unionUniqSets`
		lookupPossibleLNE lne f

srtExpr (cont,lne) off e@(StgLit l) = (e, cont, [], off)

srtExpr (cont,lne) off e@(StgConApp con args) =
   (e, cont `unionUniqSets` getGlobalRefs args, [], off)

srtExpr (cont,lne) off e@(StgPrimApp op args ty) =
   (e, cont `unionUniqSets` getGlobalRefs args, [], off)

srtExpr c@(cont,lne) off (StgCase scrut live1 live2 uniq _{-srt-} alts) =
   srtCaseAlts c off alts =: \(alts, alts_g, alts_srt, alts_off) ->

	-- construct the SRT for this case
   let (this_srt, scrut_off) = construct_srt alts_g alts_srt alts_off in

	-- global refs in the continuation is alts_g.
   srtExpr (alts_g,lne) scrut_off scrut
				=: \(scrut, scrut_g, scrut_srt, case_off) ->
   let
	g = unionUniqSets alts_g scrut_g
	srt = scrut_srt ++ this_srt
	srt_info = case length this_srt of
			0   -> NoSRT
			len -> SRT off len
   in
   (StgCase scrut live1 live2 uniq srt_info alts, g, srt, case_off)

srtExpr cont off (StgLet bind body) =
   srtLet cont off bind body StgLet (\_ cont -> cont)

srtExpr cont off (StgLetNoEscape live1 live2 b@(StgNonRec bndr rhs) body)
  = srtLet cont off b body (StgLetNoEscape live1 live2) calc_cont
  where calc_cont g (cont,lne) = (cont,addToUFM lne bndr g)

-- for recursive let-no-escapes, we do *two* passes, the first time
-- just to extract the list of global refs, and the second time we actually
-- construct the SRT now that we know what global refs should be in
-- the various let-no-escape continuations.
srtExpr conts@(cont,lne) off 
	(StgLetNoEscape live1 live2 bind@(StgRec pairs) body)
  = srtBind conts off bind =: \(_, g, _, _) ->
    let 
	lne' = addListToUFM lne [ (bndr,g) | (bndr,_) <- pairs ]
	calc_cont _ conts = conts
    in
    srtLet (cont,lne') off bind body (StgLetNoEscape live1 live2) calc_cont


srtExpr cont off (StgSCC cc expr) =
   srtExpr cont off expr	=: \(expr, g, srt, off) ->
   (StgSCC cc expr, g, srt, off)

#ifdef DEBUG
srtExpr cont off expr = pprPanic "srtExpr" (ppr expr)
#else
srtExpr cont off expr = panic "srtExpr"
#endif
\end{code}

-----------------------------------------------------------------------------
Let-expressions

This is quite complicated stuff...

\begin{code}
srtLet cont off bind body let_constr calc_cont

 -- If the bindings are all constructors, then we don't need to
 -- buid an SRT at all...
 | all_con_binds bind =
   srtBind cont off bind	=: \(bind, bind_g, bind_srt, off) ->
   srtExpr cont off body	=: \(body, body_g, body_srt, off) ->
   let
	g   = unionUniqSets bind_g body_g
	srt = body_srt ++ bind_srt
   in
   (let_constr bind body, g, srt, off)

 -- we have some closure bindings...
 | otherwise =

    -- first, find the sub-SRTs in the binding
   srtBind cont off bind	=: \(bind, bind_g, bind_srt, bind_off) ->

    -- construct the SRT for this binding
   let (this_srt, body_off) = construct_srt bind_g bind_srt bind_off in

    -- get the new continuation information (if a let-no-escape)
   let new_cont = calc_cont bind_g cont in

    -- now find the SRTs in the body
   srtExpr new_cont body_off body  =: \(body, body_g, body_srt, let_off) ->

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
construct_srt global_refs sub_srt current_offset
   = let
       extra_refs = filter (`notElem` sub_srt) (uniqSetToList global_refs)
       this_srt = extra_refs ++ sub_srt

	-- Add the length of the new entries to the	
        -- current offset to get the next free offset in the global SRT.
       new_offset = current_offset + length extra_refs
   in (this_srt, new_offset)
\end{code}

-----------------------------------------------------------------------------
Case Alternatives

\begin{code}
srtCaseAlts :: (UniqSet Id, UniqFM (UniqSet Id))
	-> Int -> StgCaseAlts -> (StgCaseAlts, UniqSet Id, [Id], Int)

srtCaseAlts cont off (StgAlgAlts t alts dflt) =
   srtAlgAlts cont off alts [] emptyUniqSet []  
				  =: \(alts, alts_g, alts_srt, off) ->
   srtDefault cont off dflt	  =: \(dflt, dflt_g, dflt_srt, off) ->
   let
	g   = unionUniqSets alts_g dflt_g
	srt = dflt_srt ++ alts_srt
   in
   (StgAlgAlts t alts dflt, g, srt, off)

srtCaseAlts cont off (StgPrimAlts t alts dflt) =
   srtPrimAlts cont off alts [] emptyUniqSet []  
				   =: \(alts, alts_g, alts_srt, off) ->
   srtDefault cont off dflt	   =: \(dflt, dflt_g, dflt_srt, off) ->
   let
	g   = unionUniqSets alts_g dflt_g
	srt = dflt_srt ++ alts_srt
   in
   (StgPrimAlts t alts dflt, g, srt, off)

srtAlgAlts cont off [] new_alts g srt = (reverse new_alts, g, srt, off)
srtAlgAlts cont off ((con,args,used,rhs):alts) new_alts g srt =
   srtExpr cont off rhs	=: \(rhs, rhs_g, rhs_srt, off) ->
   let
	g'   = unionUniqSets rhs_g g
	srt' = rhs_srt ++ srt
   in
   srtAlgAlts cont off alts ((con,args,used,rhs) : new_alts) g' srt'

srtPrimAlts cont off [] new_alts g srt = (reverse new_alts, g, srt, off)
srtPrimAlts cont off ((lit,rhs):alts) new_alts g srt =
   srtExpr cont off rhs	=: \(rhs, rhs_g, rhs_srt, off) ->
   let
	g'   = unionUniqSets rhs_g g
	srt' = rhs_srt ++ srt
   in
   srtPrimAlts cont off alts ((lit,rhs) : new_alts) g' srt'

srtDefault cont off StgNoDefault = (StgNoDefault,emptyUniqSet,[],off)
srtDefault cont off (StgBindDefault rhs) =
   srtExpr cont off rhs	=: \(rhs, g, srt, off) ->
   (StgBindDefault rhs, g, srt, off)
\end{code}

-----------------------------------------------------------------------------

Here we decide which Id's to place in the static reference table.  An
internal top-level id will be in the environment with the appropriate
CafInfo, so we use that if available.  An imported top-level Id will
have the CafInfo attached.  Otherwise, we just ignore the Id.

\begin{code}
getGlobalRefs :: [StgArg] -> UniqSet Id
getGlobalRefs args = mkUniqSet (concat (map globalRefArg args))

globalRefArg :: StgArg -> [Id]
globalRefArg (StgVarArg id)
  | idMayHaveCafRefs id = [id]
  | otherwise           = []
globalRefArg _ = []

idMayHaveCafRefs id = mayHaveCafRefs (idCafInfo id)
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

We keep track of the global references made by each let-no-escape in
scope, so we can expand them every time the let-no-escape is
referenced.

\begin{code}
lookupPossibleLNE lne_env f = 
  case lookupUFM lne_env f of
	Nothing   -> emptyUniqSet
	Just refs -> refs
\end{code}
