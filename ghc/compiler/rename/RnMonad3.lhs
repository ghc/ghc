%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnMonad3]{The monad used by the third renamer pass}

\begin{code}
#include "HsVersions.h"

module RnMonad3 (
	Rn3M(..),
	initRn3, thenRn3, andRn3, returnRn3, mapRn3, fixRn3,

	putInfoDownM3,

	newFullNameM3, newInvisibleNameM3

	-- for completeness
    ) where

import Ubiq{-uitous-}

import FiniteMap	( emptyFM,  isEmptyFM,  lookupFM,
			  emptySet, isEmptySet, elementOf
			)
import HsSyn		( IE )
import NameTypes	-- lots of stuff
import Outputable	( ExportFlag(..) )
import ProtoName	( ProtoName(..) )
import RdrHsSyn		( getExportees, ExportListInfo(..), ProtoNameIE(..) )
import UniqSupply	( getUnique, splitUniqSupply )
import Util		( panic )

infixr 9 `thenRn3`
\end{code}

%************************************************************************
%*									*
\subsection{Plain @RnPass3@ monadery}
%*									*
%************************************************************************

\begin{code}
type Rn3M result
  =  ExportListInfo -> FAST_STRING{-ModuleName-} -> UniqSupply
  -> result

{-# INLINE andRn3 #-}
{-# INLINE thenRn3 #-}
{-# INLINE returnRn3 #-}

initRn3 :: Rn3M a -> UniqSupply -> a

initRn3 m us = m Nothing{-no export list-} (panic "initRn3: uninitialised module name") us

thenRn3 :: Rn3M a -> (a -> Rn3M b) -> Rn3M b
andRn3  :: (a -> a -> a) -> Rn3M a -> Rn3M a -> Rn3M a

thenRn3 expr continuation exps mod_name uniqs
  = case splitUniqSupply uniqs      of { (s1, s2) ->
    case (expr exps mod_name s1)    of { res1 ->
    continuation res1 exps mod_name s2 }}

andRn3 combiner m1 m2 exps mod_name uniqs
  = case splitUniqSupply uniqs      of { (s1, s2) ->
    case (m1 exps mod_name s1)      of { res1 ->
    case (m2 exps mod_name s2)	    of { res2 ->
    combiner res1 res2 }}}

returnRn3 :: a -> Rn3M a
returnRn3 result exps mod_name uniqs = result

mapRn3 :: (a -> Rn3M b) -> [a] -> Rn3M [b]

mapRn3 f []     = returnRn3 []
mapRn3 f (x:xs)
  = f x		`thenRn3` \ r ->
    mapRn3 f xs	`thenRn3` \ rs ->
    returnRn3 (r:rs)

fixRn3 :: (a -> Rn3M a) -> Rn3M a

fixRn3 m exps mod_name us
  = result
  where
    result = m result exps mod_name us

putInfoDownM3 :: FAST_STRING{-ModuleName-} -> Maybe [ProtoNameIE] -> Rn3M a -> Rn3M a

putInfoDownM3 mod_name exports cont _ _ uniqs
  = cont (getExportees exports) mod_name uniqs
\end{code}

%************************************************************************
%*									*
\subsection[RnMonad3-new-names]{Making new names}
%*									*
%************************************************************************

@newFullNameM3@ makes a new user-visible FullName (the usual);
@newInvisibleNameM3@ is the odd case.  @new_name@ does all the work.

\begin{code}
newFullNameM3, newInvisibleNameM3
	:: ProtoName		-- input
	-> SrcLoc		-- where it started life
	-> Bool			-- if it is "TyCon"ish (rather than "val"ish)
	-> Maybe ExportFlag	-- Just flag => force the use of that exportness
	-> Rn3M (Unique, FullName)

newFullNameM3 pn src_loc is_tycon_ish frcd_exp exps mod_name uniqs
  = new_name pn src_loc is_tycon_ish frcd_exp False{-visible-} exps mod_name uniqs

newInvisibleNameM3 pn src_loc is_tycon_ish frcd_exp exps mod_name uniqs
  = new_name pn src_loc is_tycon_ish frcd_exp True{-invisible-} exps mod_name uniqs
\end{code}

\begin{code}
new_name pn src_loc is_tycon_ish frcd_export_flag want_invisible exps mod_name uniqs
  = (uniq, name)
  where
    uniq = getUnique uniqs

    mk_name = if want_invisible then mkPrivateFullName else mkFullName

    name = case pn of

	Unk s -> mk_name mod_name s
		   (if fromPrelude mod_name
		      && is_tycon_ish then -- & tycon/clas/datacon => Core
		       HereInPreludeCore
		    else
		       ThisModule
		   )
		   (case frcd_export_flag of
		      Just fl -> fl
		      Nothing -> mk_export_flag True [mod_name] s exps)
		   src_loc

	Qunk m s -> mk_name mod_name s
		      (if fromPrelude mod_name
			 && is_tycon_ish then -- & tycon/clas/datacon => Core
			  HereInPreludeCore
		       else
			  ThisModule
		      )
		      (case frcd_export_flag of
			 Just fl -> fl
			 Nothing -> mk_export_flag (_trace "mk_export_flag?" True) [m] s exps)
		      src_loc

	-- note: the assigning of prelude-ness is most dubious (ToDo)

	Imp m d informant_mods l
	  -> mk_name m d
	       (if fromPrelude m then	-- as above
		   if is_tycon_ish then
		       ExportedByPreludeCore
		   else
		       OtherPrelude l
		else if m == mod_name then -- pretty dang weird... (ToDo: anything?)
		   ThisModule
		else
		   OtherModule l informant_mods -- for Other*, we save its occurrence name
	       )
	       (case frcd_export_flag of
		  Just fl -> fl
		  Nothing -> mk_export_flag (m==mod_name) informant_mods l exps)
	       src_loc

	Prel n	  -> panic "RnMonad3.new_name: prelude name"
\end{code}

In deciding the ``exportness'' of something, there are these cases to
consider:
\begin{description}
\item[No explicit export list:]
Everything defined in this module goes out.

\item[Matches a non-\tr{M..} item in the export list:]
Then it's exported as its @name_pr@ item suggests.

\item[Matches a \tr{M..} item in the export list:]

(Note: the module \tr{M} may be {\em this} module!)  It's exported if
we got it from \tr{M}'s interface; {\em most emphatically not} the
same thing as ``it originally came from \tr{M}''.

\item[Otherwise:]
It isn't exported.
\end{description}

\begin{code}
mk_export_flag	:: Bool		-- True <=> originally from the module we're compiling
		-> [FAST_STRING]-- modules that told us about this thing
		-> FAST_STRING	-- name of the thing we're looking at
		-> ExportListInfo
		-> ExportFlag	-- result

mk_export_flag this_module informant_mods thing Nothing{-no export list-}
  = if this_module then ExportAll else NotExported

mk_export_flag this_module informant_mods thing (Just (exports_alist, dotdot_modules))
  | otherwise
  = case (lookupFM exports_alist thing) of
      Just how_to_export -> how_to_export
      Nothing		 -> if (or [ im `elementOf` dotdot_modules | im <- informant_mods ])
			    then ExportAll
			    else NotExported
\end{code}
