%
% (c) The GRASP Project, Glasgow University, 1992-1996
%
\section[Rename]{Renaming and dependency analysis passes}

\begin{code}
#include "HsVersions.h"

module Rename ( renameModule ) where

import Ubiq{-uitous-}

import HsSyn
import RdrHsSyn		( ProtoNameHsModule(..) )
import RnHsSyn		( RenamedHsModule(..) )

import Bag		( isEmptyBag, unionBags )
import CmdLineOpts	( opt_UseGetMentionedVars )
import ErrUtils		( Error(..) )
import Pretty		( Pretty(..){-ToDo:rm?-} )
import RnMonad12	( initRn12 )
import RnMonad4		( initRn4 )
import RnPass1
import RnPass2
import RnPass3
import RnPass4
import RnUtils		( PreludeNameMappers(..), GlobalNameMappers(..) )
import UniqSupply	( splitUniqSupply )
import Util		( panic )
\end{code}

Here's what the renamer does, basically:
\begin{description}
\item[@RnPass1@:]
Flattens out the declarations from the interfaces which this module
imports.  The result is a new module with no imports, but with more
declarations.  (Obviously, the imported declarations have ``funny
names'' [@ProtoNames@] to indicate their origin.)  Handles selective
import, renaming, \& such.

%--------------------------------------------------------------------
\item[@RnPass2@:]
Removes duplicate declarations.  Duplicates can arise when two
imported interface have a signature (or whatever) for the same
thing. We check that the two are consistent and then drop one.
Considerable huff and puff to pick the one with the ``better''
pragmatic information.

%--------------------------------------------------------------------
\item[@RnPass3@:]
Find all the top-level-ish (i.e., global) entities, assign them
@Uniques@, and make a \tr{ProtoName -> Name} mapping for them,
in preparation for...

%--------------------------------------------------------------------
\item[@RnPass4@:]
Actually prepare the ``renamed'' module.  In sticking @Names@ on
everything, it will catch out-of-scope errors (and a couple of similar
type-variable-use errors).  We also our initial dependency analysis of
the program (required before typechecking).
\end{description}

\begin{code}
renameModule :: PreludeNameMappers	-- lookup funs for deeply wired-in names
	     -> ProtoNameHsModule	-- input
	     -> UniqSupply
	     -> (RenamedHsModule,	-- output, after renaming
		 Bag FAST_STRING,	-- Names of the imported modules
					-- (profiling needs to know this)
		 GlobalNameMappers,	-- final name funs; used later
					-- to rename generated `deriving'
					-- bindings.
		 Bag Error		-- Errors, from passes 1-4
		)

-- Very space-leak sensitive

renameModule gnfs@(val_pnf, tc_pnf)
	     input@(HsModule mod_name _ _ _ _ _ _ _ _ _ _ _ _)
	     uniqs
  = let
	use_mentioned_vars = opt_UseGetMentionedVars
    in
    case (initRn12 mod_name (rnModule1 gnfs use_mentioned_vars input))
      of { ((mod1, imported_module_names), errs1) ->

    case (initRn12 mod_name (rnModule2 mod1)) of { (mod2, errs2) ->

    case (splitUniqSupply uniqs) of { (us1, us2) ->

    case (initRn3 (rnModule3 gnfs imported_module_names mod2) us1)
      of { (val_space, tc_space, v_gnf, tc_gnf, errs3) ->

    let
	final_name_funs = (v_gnf, tc_gnf)

	errs_so_far = errs1 `unionBags` errs2 `unionBags` errs3
		-- see note below about why we consult errs at this pt
    in
    if not (isEmptyBag errs_so_far) then -- give up now
	( panic "rename", imported_module_names, final_name_funs, errs_so_far )
    else
	case (initRn4 final_name_funs (rnModule mod2) us2)
	  of { (mod4, errs4) ->

	( mod4, imported_module_names, final_name_funs, errs4 ) }
    }}}}
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
fine in \tr{rnModule}.  Maybe some other time...
