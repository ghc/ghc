%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Rename-aux-funs]{Functions used by both renaming passes}

\begin{code}
#include "HsVersions.h"

module RenameAuxFuns (
	mkGlobalNameFun, mkNameFun,
	GlobalNameFun(..),  GlobalNameFuns(..),
	PreludeNameFun(..), PreludeNameFuns(..),

	-- and for self-containedness...
	Bag, ProtoName, Maybe
    ) where

IMPORT_Trace		-- ToDo: rm (for debugging)
import Outputable
import Pretty

import Bag		( Bag, bagToList )
import FiniteMap
import Maybes
import Name		( Name ) -- for instances
--OLD: import NameEnv
import ProtoName
import Util
\end{code}

\begin{code}
type GlobalNameFun  = ProtoName -> Maybe Name
type GlobalNameFuns = (GlobalNameFun, GlobalNameFun)

type PreludeNameFun = FAST_STRING -> Maybe Name
type PreludeNameFuns = (PreludeNameFun,		-- Values
			PreludeNameFun		-- Types and classes
		       )
\end{code}

\begin{code}
mkGlobalNameFun :: FAST_STRING		-- The module name
	        -> PreludeNameFun 	-- The prelude things
	        -> [(ProtoName, Name)]	-- The local and imported things
		-> GlobalNameFun	-- The global name function

mkGlobalNameFun this_module prel_nf alist
  = the_fun
  where
    the_fun (Prel n)	  = Just n
    the_fun (Unk s) 	  = case (unk_fun s) of
			      Just n  -> Just n
			      Nothing -> prel_nf s
    the_fun (Imp m d _ _) = imp_fun (d, m) -- NB: module-name 2nd!

    -- Things in the domain of the prelude function shouldn't be put
    -- in the unk_fun; because the prel_nf will catch them.
    -- This can arise if, for example, an interface gives a signature
    -- for a prelude thing.
    --
    -- Neither should they be in the domain of the imp_fun, because
    -- prelude things will have been converted to Prel x rather than 
    -- Imp p q r s.
    --
    -- So we strip out prelude things from the alist; this is not just
    -- desirable, it's essential because get_orig and get_local don't handle
    -- prelude things.

    non_prel_alist = filter non_prel alist

    non_prel (Prel _, _) = False
    non_prel other       = True

    -- unk_fun looks up local names (just strings),
    -- imp_fun looks up original names: (string,string) pairs
    unk_fun = lookupFM (listToFM [(get_local pn,n) | (pn,n) <- non_prel_alist])
    imp_fun = lookupFM (listToFM [(get_orig  pn,n) | (pn,n) <- non_prel_alist])

{- OLD:
    unk_fun = mkStringLookupFn  [(get_local pn,n) | (pn,n) <- non_prel_alist] False{-not sorted-}
    imp_fun = mk2StringLookupFn [(get_orig  pn,n) | (pn,n) <- non_prel_alist] False{-not sorted-}
-}
		-- the lists *are* sorted by *some* ordering (by local
		-- names), but not generally, and not in some way we
		-- are going to rely on.

    get_local :: ProtoName -> FAST_STRING
    get_local (Unk s)       = s
    get_local (Imp _ _ _ l) = l
    get_local (Prel n)	    = pprPanic "get_local: " (ppr PprShowAll n)

    get_orig :: ProtoName -> (FAST_STRING, FAST_STRING) -- **NB**! module-name 2nd!
    get_orig (Unk s)       = (s, this_module)
    get_orig (Imp m d _ _) = (d, m)
    get_orig (Prel n)	    = pprPanic "get_orig: " (ppr PprShowAll n)
\end{code}


@mkNameFun@ builds a function from @ProtoName@s to things, where a
``thing'' is either a @ProtoName@ (in the case of values), or a
@(ProtoName, ProtoName -> ProtoName)@ pair in the case of types and
classes.  It takes:

\begin{itemize}
\item	The name of the interface
\item	A bag of new string-to-thing bindings to add,

\item	An extractor function, to get a @ProtoName@ out of a thing,
	for use in error messages.
\end{itemize}
The function it returns only expects to see @Unk@ things.

@mkNameFun@ checks for clashes in the domain of the new bindings.

ToDo: it should check for clashes with the prelude bindings too.

\begin{code}
mkNameFun :: Bag (FAST_STRING, thing)	    -- Value bindings
	  -> (FAST_STRING -> Maybe thing,   -- The function to use
	      [[(FAST_STRING,thing)]])	    -- Duplicates, if any

mkNameFun the_bag
  = case (removeDups cmp (bagToList the_bag)) of { (no_dup_list, dups) ->
    case (lookupFM (listToFM no_dup_list))    of { the_fun -> 
    --OLD :case (mkStringLookupFn no_dup_list True{-list is pre-sorted-}) of the_fun -> 
    (the_fun, dups)
    }}
  where
    cmp :: (FAST_STRING, a) -> (FAST_STRING, a) -> TAG_

    cmp (s1,_) (s2,_) = _CMP_STRING_ s1 s2
\end{code}
