%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%

Run through the STG code and compute the Static Reference Table for
each let-binding.  At the same time, we figure out which top-level
bindings have no CAF references, and record the fact in their IdInfo.

\begin{code}
module SRT( computeSRTs ) where

#include "HsVersions.h"

import StgSyn
import Id        ( Id )
import VarSet	( varSetElems )
import Util	( mapAccumL )

#ifdef DEBUG
import Outputable
#endif
\end{code}

\begin{code}
computeSRTs :: [StgBinding] -> [(StgBinding,[Id])]
  -- The incoming bindingd are filled with SRTEntries in their SRT slots
  -- the outgoing ones have NoSRT/SRT values instead

computeSRTs binds = map srtTopBind binds
\end{code}

-----------------------------------------------------------------------------
Algorithm for figuring out SRT layout.

Our functions have type

srtExpr	:: SrtOffset		-- Next free offset within the SRT
	-> StgExpr		-- Expression to analyse
	-> (StgExpr,		-- (e) newly annotated expression
	    SrtIds,		-- (s) SRT required for this expression (reversed)
	    SrtOffset)		-- (o) new offset

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
type SrtOffset = Int
type SrtIds    = [Id]  -- An *reverse-ordered* list of the Ids needed in the SRT

srtTopBind :: StgBinding -> (StgBinding, SrtIds)

srtTopBind bind
  = srtBind 0 bind	=: \ (bind', srt, off) ->
    (bind', reverse srt)	-- The 'reverse' is because the SRT is 
				-- built up reversed, for efficiency's sake

srtBind :: SrtOffset -> StgBinding -> (StgBinding, SrtIds, SrtOffset)

srtBind off (StgNonRec (SRTEntries rhs_cafs) binder rhs) 
  = (StgNonRec srt_info binder new_rhs, this_srt, body_off)
  where
    (new_rhs,  rhs_srt,  rhs_off)  = srtRhs off rhs
    (srt_info, this_srt, body_off) = constructSRT rhs_cafs rhs_srt off rhs_off
    

srtBind off (StgRec (SRTEntries rhss_cafs) pairs)
  = (StgRec srt_info new_pairs, this_srt, body_off)
  where
    ((rhss_off, rhss_srt), new_pairs) = mapAccumL do_bind (off, []) pairs

    do_bind (off,srt) (bndr,rhs)
	= srtRhs off rhs		=: \(rhs', srt', off') ->
	  ((off', srt'++srt), (bndr, rhs'))

    (srt_info, this_srt, body_off)
	 = constructSRT rhss_cafs rhss_srt off rhss_off
\end{code}

-----------------------------------------------------------------------------
Right Hand Sides

\begin{code}
srtRhs 	:: SrtOffset -> StgRhs -> (StgRhs, SrtIds, SrtOffset)

srtRhs off (StgRhsClosure cc bi free_vars u args body)
  = srtExpr off body			=: \(body, srt, off) ->
    (StgRhsClosure cc bi free_vars u args body, srt, off)

srtRhs off e@(StgRhsCon cc con args) = (e, [], off)
\end{code}

-----------------------------------------------------------------------------
Expressions

\begin{code}
srtExpr :: SrtOffset -> StgExpr -> (StgExpr, SrtIds, SrtOffset)

srtExpr off e@(StgApp f args) 	      = (e, [], off)
srtExpr off e@(StgLit l)      	      = (e, [], off)
srtExpr off e@(StgConApp con args)    = (e, [], off)
srtExpr off e@(StgPrimApp op args ty) = (e, [], off)

srtExpr off (StgSCC cc expr) =
   srtExpr off expr	=: \(expr, srt, off) ->
   (StgSCC cc expr, srt, off)

srtExpr off (StgCase scrut live1 live2 uniq (SRTEntries cafs_in_alts) alts)
 = srtCaseAlts off alts 	=: \(alts, alts_srt, alts_off) ->
   let
	(srt_info, this_srt, scrut_off) 
		= constructSRT cafs_in_alts alts_srt off alts_off
   in
   srtExpr scrut_off scrut	=: \(scrut, scrut_srt, case_off) ->

   (StgCase scrut live1 live2 uniq srt_info alts, 
    scrut_srt ++ this_srt, 
    case_off)

srtExpr off (StgLet bind body)
  = srtBind off bind		=: \ (bind', bind_srt, body_off) ->
    srtExpr body_off body	=: \ (body', expr_srt, let_off) ->
    (StgLet bind' body', expr_srt ++ bind_srt, let_off)
     
srtExpr off (StgLetNoEscape live1 live2 bind body)
  = srtBind off bind		=: \ (bind', bind_srt, body_off) ->
    srtExpr body_off body	=: \ (body', expr_srt, let_off) ->
    (StgLetNoEscape live1 live2 bind' body', expr_srt ++ bind_srt, let_off)

#ifdef DEBUG
srtExpr off expr = pprPanic "srtExpr" (ppr expr)
#endif
\end{code}

-----------------------------------------------------------------------------
Construct an SRT.

Construct the SRT at this point from its sub-SRTs and any new global
references which aren't already contained in one of the sub-SRTs (and
which are "live").

\begin{code}
constructSRT caf_refs sub_srt initial_offset current_offset
   = let
       extra_refs = filter (`notElem` sub_srt) (varSetElems caf_refs)
       this_srt   = extra_refs ++ sub_srt

	-- Add the length of the new entries to the	
        -- current offset to get the next free offset in the global SRT.
       new_offset = current_offset + length extra_refs
       srt_length = new_offset - initial_offset

       srt_info | srt_length == 0 = NoSRT
		| otherwise       = SRT initial_offset srt_length

   in ASSERT( srt_length == length this_srt )
      (srt_info, this_srt, new_offset)
\end{code}

-----------------------------------------------------------------------------
Case Alternatives

\begin{code}
srtCaseAlts :: SrtOffset -> StgCaseAlts -> (StgCaseAlts, SrtIds, SrtOffset)

srtCaseAlts off (StgAlgAlts t alts dflt)
  = srtDefault off dflt	 				=: \ ((dflt_off, dflt_srt), dflt') ->
    mapAccumL srtAlgAlt (dflt_off, dflt_srt) alts	=: \ ((alts_off, alts_srt), alts') ->
    (StgAlgAlts t alts' dflt', alts_srt, alts_off)

srtCaseAlts off (StgPrimAlts t alts dflt)
  = srtDefault off dflt	 				=: \ ((dflt_off, dflt_srt), dflt') ->
    mapAccumL srtPrimAlt (dflt_off, dflt_srt) alts	=: \ ((alts_off, alts_srt), alts') ->
    (StgPrimAlts t alts' dflt', alts_srt, alts_off)

srtAlgAlt (off,srt) (con,args,used,rhs)
  = srtExpr off rhs	=: \(rhs', rhs_srt, rhs_off) ->
    ((rhs_off, rhs_srt ++ srt), (con,args,used,rhs'))

srtPrimAlt (off,srt) (lit,rhs)
  = srtExpr off rhs	=: \(rhs', rhs_srt, rhs_off) ->
    ((rhs_off, rhs_srt ++ srt), (lit, rhs'))

srtDefault off StgNoDefault
  = ((off,[]), StgNoDefault)
srtDefault off (StgBindDefault rhs)
  = srtExpr off rhs	=: \(rhs', srt, off) ->
    ((off,srt), StgBindDefault rhs')
\end{code}

-----------------------------------------------------------------------------
Misc stuff

\begin{code}
a =: k  = k a
\end{code}
