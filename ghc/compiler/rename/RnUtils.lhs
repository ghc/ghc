%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnUtils]{Functions used by both renaming passes}

\begin{code}
#include "HsVersions.h"

module RnUtils (
	mkGlobalNameFun, mkNameFun,
	GlobalNameMapper(..),  GlobalNameMappers(..),
	PreludeNameMapper(..), PreludeNameMappers(..),

	dupNamesErr -- used in various places
    ) where

import Ubiq{-uitous-}

import Bag		( bagToList, Bag )
import FiniteMap	( lookupFM, listToFM )
import Name		( Name{-instances-} )
import Outputable	( pprNonOp )
import PprStyle		( PprStyle(..) )
import Pretty
import ProtoName	( ProtoName(..) )
import Util		( cmpPString, removeDups, pprPanic, panic )
\end{code}

\begin{code}
type GlobalNameMapper  = ProtoName -> Maybe Name
type GlobalNameMappers = (GlobalNameMapper, GlobalNameMapper)

type PreludeNameMapper = FAST_STRING -> Maybe Name
type PreludeNameMappers = (PreludeNameMapper,		-- Values
			PreludeNameMapper		-- Types and classes
		       )
\end{code}

\begin{code}
mkGlobalNameFun :: FAST_STRING		-- The module name
		-> PreludeNameMapper 	-- The prelude things
		-> [(ProtoName, Name)]	-- The local and imported things
		-> GlobalNameMapper	-- The global name function

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
    (the_fun, dups) }}
  where
    cmp :: (FAST_STRING, a) -> (FAST_STRING, a) -> TAG_

    cmp (s1,_) (s2,_) = _CMP_STRING_ s1 s2
\end{code}

\begin{code}
dupNamesErr descriptor ((first_pname,locn1) : dup_things) sty
  = ppAboves (first_item : map dup_item dup_things)
  where
    first_item
      = ppBesides [ ppr PprForUser locn1,
	    ppStr ": multiple declarations of a ", ppStr descriptor, ppStr ": ",
	    pprNonOp sty first_pname ]

    dup_item (pname, locn)
      = ppBesides [ ppr PprForUser locn,
	    ppStr ": here was another declaration of `", pprNonOp sty pname, ppStr "'" ]
\end{code}
