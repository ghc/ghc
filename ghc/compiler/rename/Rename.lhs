%
% (c) The GRASP Project, Glasgow University, 1992-1994
%
\section[Rename]{Renaming and dependency analysis passes}

\begin{code}
#include "HsVersions.h"

module Rename (
	renameModule,

	-- for completeness
	Module, Bag, InPat, ProtoNamePat(..), RenamedPat(..), Name,
	ProtoName, SplitUniqSupply, PreludeNameFun(..),
	PreludeNameFuns(..), Maybe, Error(..), Pretty(..), PprStyle,
	PrettyRep, GlobalNameFuns(..), GlobalNameFun(..),
	GlobalSwitch
    ) where

import AbsSyn
import Bag		( isEmptyBag, unionBags, Bag )
import CmdLineOpts	( GlobalSwitch(..) )
import RenameMonad12
import Rename1
import Rename2
import Rename3
import Rename4
import RenameAuxFuns	( PreludeNameFuns(..), GlobalNameFuns(..) )
--import Pretty		-- ToDo: rm debugging
import SplitUniq	( splitUniqSupply, SplitUniqSupply )
import Util
\end{code}

Here's what the renamer does, basically:
\begin{description}
\item[@Rename1@:]
Flattens out the declarations from the interfaces which this module
imports.  The result is a new module with no imports, but with more
declarations.  (Obviously, the imported declarations have ``funny
names'' [@ProtoNames@] to indicate their origin.)  Handles selective
import, renaming, \& such.

%--------------------------------------------------------------------
\item[@Rename2@:]
Removes duplicate declarations.  Duplicates can arise when two
imported interface have a signature (or whatever) for the same
thing. We check that the two are consistent and then drop one.
Considerable huff and puff to pick the one with the ``better''
pragmatic information.

%--------------------------------------------------------------------
\item[@Rename3@:]
Find all the top-level-ish (i.e., global) entities, assign them
@Uniques@, and make a \tr{ProtoName -> Name} mapping for them,
in preparation for...

%--------------------------------------------------------------------
\item[@Rename4@:]
Actually prepare the ``renamed'' module.  In sticking @Names@ on
everything, it will catch out-of-scope errors (and a couple of similar
type-variable-use errors).  We also our initial dependency analysis of
the program (required before typechecking).
\end{description}

\begin{code}
renameModule :: (GlobalSwitch -> Bool)	-- to check cmd-line opts
	     -> PreludeNameFuns		-- lookup funs for deeply wired-in names
	     -> ProtoNameModule		-- input
	     -> SplitUniqSupply
	     -> (RenamedModule,		-- output, after renaming
		 [FAST_STRING],		-- Names of the imported modules
					-- (profiling needs to know this)
		 GlobalNameFuns,	-- final name funs; used later
					-- to rename generated `deriving'
					-- bindings.
		 Bag Error		-- Errors, from passes 1-4
		)

-- Very space-leak sensitive

renameModule sw_chkr gnfs@(val_pnf, tc_pnf)
	     input@(Module mod_name _ _ _ _ _ _ _ _ _ _ _ _)
	     uniqs
  = let
	use_mentioned_vars = sw_chkr UseGetMentionedVars
    in
    BIND (
    BSCC("Rename1")
    initRn12 mod_name (rnModule1 gnfs use_mentioned_vars input)
    ESCC
    )		_TO_ ((mod1, imported_module_names), errs1) ->

    BIND (
    BSCC("Rename2")
    initRn12 mod_name (rnModule2 mod1)
    ESCC
    )		_TO_ (mod2, errs2) ->

--  pprTrace "rename2:" (ppr PprDebug mod2) (

    BIND (splitUniqSupply uniqs) _TO_ (us1, us2) ->

    BIND (
    BSCC("Rename3")
    initRn3 (rnModule3 gnfs imported_module_names mod2) us1
    ESCC
    )		_TO_ (val_space, tc_space, v_gnf, tc_gnf, errs3) ->

    let
	final_name_funs = (v_gnf, tc_gnf)

	errs_so_far = errs1 `unionBags` errs2 `unionBags` errs3
		-- see note below about why we consult errs at this pt
    in
    if not (isEmptyBag errs_so_far) then -- give up now
	( panic "rename", imported_module_names, final_name_funs, errs_so_far )
    else
	BIND (
	BSCC("Rename4")
	initRn4 sw_chkr final_name_funs (rnModule4 mod2) us2
	ESCC
	)		_TO_ (mod4, errs4) ->

	( mod4, imported_module_names, final_name_funs, errs4 )
	BEND
    BEND
--  )
    BEND
    BEND
    BEND
\end{code}

Why stop if errors in the first three passes: Suppose you're compiling
a module with a top-level definition named \tr{scaleFloat}.  Sadly,
this is also a Prelude class-method name.  \tr{rnModule3} will have
detected this error, but: it will also have picked (arbitrarily) one
of the two definitions for its final ``value'' name-function.  If, by
chance, it should have picked the class-method... when it comes to pin
a Unique on the top-level (bogus) \tr{scaleFloat}, it will ask for the
class-method's Unique (!); it doesn't have one, and you will get a
panic.

Another way to handle this would be for the duplicate detector to
clobber duplicates with some ``safe'' value.  Then things would be
fine in \tr{rnModule4}.  Maybe some other time...
