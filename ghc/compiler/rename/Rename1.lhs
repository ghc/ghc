%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Rename1]{@Rename1@: gather up imported information}

See the @Rename@ module for a basic description of the renamer.

\begin{code}
#include "HsVersions.h"

module Rename1 (
	rnModule1,

	-- for completeness
	Module, Bag, ProtoNamePat(..), InPat, Maybe,
	PprStyle, Pretty(..), PrettyRep, ProtoName, Name,
	PreludeNameFun(..), PreludeNameFuns(..)
    ) where

IMPORT_Trace		-- ToDo: rm
import Pretty		-- these two too
import Outputable

import AbsSyn
import AbsSynFuns	( getMentionedVars ) -- *** not via AbsSyn ***
import Bag		( Bag, emptyBag, unitBag, snocBag, unionBags, bagToList )
import Errors
import HsPragmas
import FiniteMap
import Maybes		( maybeToBool, catMaybes, Maybe(..) )
--OLD: import NameEnv	( mkStringLookupFn )
import ProtoName	( ProtoName(..), mkPreludeProtoName )
import RenameAuxFuns
import RenameMonad12
import Util
\end{code}


%************************************************************************
%*									*
\subsection{Types and things used herein}
%*									*
%************************************************************************

@AllIntDecls@ is the type returned from processing import statement(s)
in the main module.

\begin{code}
type AllIntDecls = ([ProtoNameFixityDecl], [ProtoNameTyDecl],
		    [ProtoNameClassDecl],  [ProtoNameInstDecl],
		    [ProtoNameSig], Bag FAST_STRING)
\end{code}

The selective-import function @SelectiveImporter@ maps a @ProtoName@
to something which indicates how much of the thing, if anything, is
wanted by the importing module.
\begin{code}
type SelectiveImporter = ProtoName -> Wantedness

data Wantedness
  = Wanted
  | NotWanted
  | WantedWith IE
\end{code}

The @ProtoNames@ supplied to these ``name functions'' are always
@Unks@, unless they are fully-qualified names, which occur only in
interface pragmas (and, therefore, never on the {\em definitions} of
things).  That doesn't happen in @Rename1@!
\begin{code}
type IntNameFun	  = ProtoName -> ProtoName
type IntTCNameFun = ProtoName -> (ProtoName, IntNameFun)
\end{code}

%************************************************************************
%*									*
\subsection{First pass over the entire module}
%*									*
%************************************************************************

This pass flattens out the declarations embedded within the interfaces
which this module imports.  The result is a new module with no
imports, but with more declarations.  The declarations which arose
from the imported interfaces will have @ProtoNames@ with @Imp@
constructors; the declarations in the body of this module are
unaffected, so they will still be @Unk@'s.

We import only the declarations from interfaces which are actually {\em
used}.  This saves time later, because we don't need process the
unused ones.

\begin{code}
rnModule1 :: PreludeNameFuns
	  -> Bool		-- see use below
	  -> ProtoNameModule
	  -> Rn12M (ProtoNameModule, [FAST_STRING])

rnModule1 pnf@(v_pnf, tc_pnf)
	use_mentioned_vars_heuristic
	(Module mod_name exports imports fixes
		ty_decls absty_sigs class_decls inst_decls specinst_sigs
		defaults binds _ src_loc)

  =	-- slurp through the *body* of the module, collecting names of
	-- mentioned *variables*, 3+ letters long & not prelude names.
	-- Note: we *do* have to pick up top-level binders,
	-- so we can check for conflicts with imported guys!
    let
{- OLD:MENTIONED-}
	(uses_Mdotdot_in_exports, mentioned_vars)
	  = getMentionedVars v_pnf exports fixes class_decls inst_decls binds

	-- Using the collected "mentioned" variables, create an
	-- "is-mentioned" function (:: FAST_STRING -> Bool), which gives
	-- True if something is mentioned is in the list collected.
	-- For more details, see under @selectAll@, notably the
	-- handling of short (< 3 chars) names.

	-- Note: this "is_mentioned" game doesn't work if the export
	-- list includes any M.. constructs (because that mentions
	-- variables *implicitly*, basically).  getMentionedVars tells
	-- us this, and we act accordingly.

	is_mentioned_maybe
	  = lookupFM {-OLD: mkStringLookupFn-} (listToFM
		[ (x, panic "is_mentioned_fn")
		| x <- mentioned_vars ++ needed_for_deriving ]
		)
		-- OLD: False{-not-sorted-}
	  where
	    needed_for_deriving	-- is this a HACK or what?
	      = [ SLIT("&&"),
		  SLIT("."),
		  SLIT("lex"),
		  SLIT("map"),
		  SLIT("not"),
		  SLIT("readParen"),
		  SLIT("showParen"),
		  SLIT("showSpace__"),
		  SLIT("showString")
		]

	is_mentioned_fn
	  = if use_mentioned_vars_heuristic
	    && not (uses_Mdotdot_in_exports)
	    then \ x -> maybeToBool (is_mentioned_maybe x)
	    else \ x -> True
{- OLD:MENTIONED-}
--O:M	is_mentioned_fn = \ x -> True -- ToDo: delete altogether
    in
	-- OK, now do the business:
    doImportedIfaces pnf is_mentioned_fn imports
		 `thenRn12`  \ (int_fixes, int_ty_decls,
				int_class_decls, int_inst_decls,
				int_sigs, import_names) ->
    let
	inst_decls' = doRevoltingInstDecls tc_nf inst_decls
    in
    returnRn12
	 ((Module mod_name
		exports imports -- passed along mostly for later checking
		(int_fixes	  ++ fixes)
		(int_ty_decls	  ++ ty_decls)
		absty_sigs
		(int_class_decls ++ class_decls)
		(int_inst_decls  ++ inst_decls')
		specinst_sigs
		defaults
		binds
		int_sigs
		src_loc),
	  bagToList import_names)
  where
    -- This function just spots prelude names
    tc_nf pname@(Unk s) = case (tc_pnf s) of
			   Nothing   -> pname
			   Just name -> Prel name

    tc_nf other_pname	= panic "In tc_nf passed to doRevoltingInstDecls"
	-- The only place where Imps occur is on Ids in unfoldings;
	-- this function is only used on type-things.
\end{code}

Instance declarations in the module itself are treated in a horribly
special way.  Because their class name and type constructor will be
compared against imported ones in the second pass (to eliminate
duplicate instance decls) we need to make Prelude classes and tycons
appear as such.  (For class and type decls, the module can't be
declaring a prelude class or tycon, so Prel and Unk things can just
compare non-equal.)  This is a HACK.

\begin{code}
doRevoltingInstDecls :: IntNameFun -> [ProtoNameInstDecl] -> [ProtoNameInstDecl]

doRevoltingInstDecls tc_nf decls
  = map revolt_me decls
  where
    revolt_me (InstDecl context cname ty binds True modname imod uprags pragma src_loc)
      = InstDecl
	    context			-- Context unchanged
	    (tc_nf cname)		-- Look up the class
	    (doIfaceMonoType1 tc_nf ty)	-- Ditto the type
	    binds			-- Binds unchanged
	    True
	    modname
	    imod
	    uprags
	    pragma
	    src_loc
\end{code}

%************************************************************************
%*									*
\subsection{Process a module's imported interfaces}
%*									*
%************************************************************************

@doImportedIfaces@ processes the entire set of interfaces imported by the
module being renamed.

\begin{code}
doImportedIfaces :: PreludeNameFuns
	      -> (FAST_STRING -> Bool)
	      -> [ProtoNameImportedInterface]
	      -> Rn12M AllIntDecls

doImportedIfaces pnfs is_mentioned_fn []
  = returnRn12 ( [{-fixities-}],  [{-tydecls-}], [{-clasdecls-}],
		 [{-instdecls-}], [{-sigs-}], emptyBag )

doImportedIfaces pnfs is_mentioned_fn (iface:ifaces)
  = doOneIface  pnfs is_mentioned_fn iface
			 `thenRn12` \ (ifixes1, itd1, icd1, iid1, isd1, names1) ->

    doImportedIfaces pnfs is_mentioned_fn ifaces
			 `thenRn12` \ (ifixes2, itd2, icd2, iid2, isd2, names2) ->

    returnRn12 (ifixes1 ++ ifixes2,
		itd1 ++ itd2,
		icd1 ++ icd2,
		iid1 ++ iid2,
		isd1 ++ isd2,
		names1 `unionBags` names2)
\end{code}

\begin{code}
doOneIface pnfs is_mentioned_fn (ImportAll int renamings)
  = let
	renaming_fn = mkRenamingFun renamings
	-- if there are any renamings, then we don't use
	-- the "is_mentioned_fn" hack; possibly dangerous (paranoia reigns)
	revised_is_mentioned_fn
	  = if null renamings
	    then is_mentioned_fn
	    else (\ x -> True) -- pretend everything is mentioned
    in
--  pprTrace "ImportAll:mod_rns:" (ppr PprDebug renamings) (
    doIface1 renaming_fn pnfs (selectAll renaming_fn revised_is_mentioned_fn) int
--  )

doOneIface pnfs unused_is_mentioned_fn (ImportSome int ie_list renamings)
  = --pprTrace "ImportSome:mod_rns:" (ppr PprDebug renamings) (
    doIface1 (mkRenamingFun renamings) pnfs si_fun int
    --)
  where
    -- the `selective import' function should not be applied
    -- to the Imps that occur on Ids in unfoldings.

    si_fun (Unk str) = check_ie str ie_list
    si_fun other     = panic "si_fun in doOneIface"

    check_ie name [] = NotWanted
    check_ie name (ie:ies)
      = case ie of
	      IEVar n		  | name == n -> Wanted
	      IEThingAbs n	  | name == n -> WantedWith ie
	      IEThingAll n	  | name == n -> WantedWith ie
	      IEConWithCons n ns  | name == n -> WantedWith ie
	      IEClsWithOps n ns	  | name == n -> WantedWith ie
	      IEModuleContents _	      -> panic "Module.. in import list?"
	      other			      -> check_ie name ies

doOneIface pnfs unused_is_mentioned_fn (ImportButHide int ie_list renamings)
  = --pprTrace "ImportButHide:mod_rns:" (ppr PprDebug renamings) (
    doIface1 (mkRenamingFun renamings) pnfs si_fun int
    --)
  where
    -- see comment above:

    si_fun (Unk str) | str `elemFM` entity_info = NotWanted
		     | otherwise		= Wanted

    entity_info = fst (getIEStrings ie_list)
\end{code}

@selectAll@ ``normally'' creates an @SelectiveImporter@ that declares
everything from an interface to be @Wanted@.  We may, however, pass
in a more discriminating @is_mentioned_fn@ (returns @True@ if the
named entity is mentioned in the body of the module in question), which
can be used to trim off junk from an interface.

For @selectAll@ to say something is @NotWanted@, it must be a
variable, it must not be in the collected-up list of mentioned
variables (checked with @is_mentioned_fn@), and it must be three chars
or longer.

And, of course, we mustn't forget to take account of renaming!

ADR Question: What's so magical about names longer than 3 characters?
Why would we want to keep long names which aren't mentioned when we're
quite happy to throw away short names that aren't mentioned?

\begin{code}
selectAll :: (FAST_STRING -> FAST_STRING) -> (FAST_STRING -> Bool) -> SelectiveImporter

selectAll renaming_fn is_mentioned_fn (Unk str) -- gotta be an Unk
  = let
	rn_str = renaming_fn str
    in
    if (isAvarid rn_str)
    && (not (is_mentioned_fn rn_str))
    && (_UNPK_ rn_str `lengthExceeds` 2)
    then NotWanted
    else Wanted
\end{code}


%************************************************************************
%*									*
\subsection{First pass over a particular interface}
%*									*
%************************************************************************


@doIface1@ handles a specific interface. First it looks at the
interface imports, creating a bag that maps local names back to their
original names, from which it makes a function that does the same. It
then uses this function to create a triple of bags for the interface
type, class and value declarations, in which local names have been
mapped back into original names.

Notice that @mkLocalNameFun@ makes two different functions. The first
is the name function for the interface. This takes a local name and
provides an original name for any name in the interface by using
either of:
\begin{itemize}
\item
the original name produced by the renaming function;
\item
the local name in the interface and the interface name.
\end{itemize}

The function @doIfaceImports1@ receives two association lists which will
be described at its definition.

\begin{code}
doIface1 :: (FAST_STRING -> FAST_STRING)    -- Renamings in import stmt of module
       -> PreludeNameFuns
       -> SelectiveImporter
       -> ProtoNameInterface
       -> Rn12M AllIntDecls

doIface1 mod_rn_fn (v_pnf, tc_pnf) sifun
       (MkInterface i_name import_decls fix_decls ty_decls class_decls
		    inst_decls sig_decls anns)

  = doIfaceImports1 mod_rn_fn i_name import_decls	`thenRn12` \ (v_bag, tc_bag) ->
    do_body (v_bag, tc_bag)
  where
    do_body (v_bag, tc_bag)
      = report_all_errors			`thenRn12` \ _ ->

	doIfaceTyDecls1 sifun full_tc_nf ty_decls	`thenRn12` \ ty_decls' ->

	doIfaceClassDecls1 sifun full_tc_nf class_decls  `thenRn12` \ class_decls' ->

	let sig_decls'	= doIfaceSigs1 sifun v_nf tc_nf sig_decls
	    fix_decls'	= doIfaceFixes1 sifun v_nf fix_decls
	    inst_decls'	= doIfaceInstDecls1 sifun tc_nf inst_decls
	in
	returnRn12 (fix_decls', ty_decls', class_decls', inst_decls', sig_decls', unitBag i_name)
      where
	v_dups  :: [[(FAST_STRING, ProtoName)]]
	tc_dups :: [[(FAST_STRING, (ProtoName, IntNameFun))]]

	(imp_v_nf, v_dups)   = mkNameFun {-OLD:v_pnf-}  v_bag
	(imp_tc_nf, tc_dups) = mkNameFun {-OLD:tc_pnf-} tc_bag

	v_nf :: IntNameFun
	v_nf (Unk s) = case v_pnf s of
			 Just n	 -> mkPreludeProtoName n
			 Nothing -> case imp_v_nf s of
				      Just n  -> n
				      Nothing -> Imp i_name s [i_name] (mod_rn_fn s)

	prel_con_or_op_nf  :: FAST_STRING{-module name-}-> IntNameFun
		 -- Used for (..)'d parts of prelude datatype/class decls;
		 -- OLD:? For `data' types, we happen to know everything;
		 -- OLD:? For class decls, we *don't* know what the class-ops are.
	prel_con_or_op_nf m (Unk s)
	  = case v_pnf s of
	      Just n  -> mkPreludeProtoName n
	      Nothing -> Imp m s [m] (mod_rn_fn s)
			 -- Strictly speaking, should be *no renaming* here, folks

	local_con_or_op_nf :: IntNameFun	
		-- used for non-prelude constructors/ops
	local_con_or_op_nf (Unk s) = Imp i_name s [i_name] (mod_rn_fn s)

	full_tc_nf :: IntTCNameFun
	full_tc_nf (Unk s)
	  = case tc_pnf s of
	      Just n  -> (mkPreludeProtoName n,
			  let
			      mod = fst (getOrigName n)
			  in
			  prel_con_or_op_nf mod)

	      Nothing -> case imp_tc_nf s of
			  Just pair -> pair
			  Nothing   -> (Imp i_name s [i_name] (mod_rn_fn s),
				        local_con_or_op_nf)

	tc_nf = fst . full_tc_nf

        -- ADR: commented out next new lines because I don't believe
        -- ADR: the check is useful or required by the Standard. (It
        -- ADR: also messes up the interpreter.)

	tc_errs = [] -- map (map (fst . snd)) tc_dups
		  -- Ugh! Just keep the dup'd protonames
	v_errs	= [] -- map (map snd) v_dups
		  -- Ditto

	report_all_errors
	  = mapRn12 (addErrRn12 . duplicateImportsInInterfaceErr (_UNPK_ i_name))
		    (tc_errs ++ v_errs)
\end{code}


%************************************************************************
%*									*
\subsection{doIfaceImports1}
%*									*
%************************************************************************

@ImportNameBags@ is a pair of bags (one for values, one for types and
classes) which specify the new names brought into scope by some
import declarations in an interface.

\begin{code}
type ImportNameBags = (Bag (FAST_STRING, ProtoName),
		       Bag (FAST_STRING, (ProtoName, IntNameFun))
		      )
\end{code}

\begin{code}
doIfaceImports1
	:: (FAST_STRING -> FAST_STRING)	-- Renamings in import stmt of module
	-> FAST_STRING			-- name of module whose interface we're doing
	-> [IfaceImportDecl]
	-> Rn12M ImportNameBags

doIfaceImports1 _ _  [] = returnRn12 (emptyBag, emptyBag)

doIfaceImports1 mod_rn_fn int_mod_name (imp_decl1 : rest)
  = do_decl				 imp_decl1  `thenRn12` \ (vb1, tcb1) ->
    doIfaceImports1 mod_rn_fn int_mod_name rest	    `thenRn12` \ (vb2, tcb2) ->
--  pprTrace "vbags/tcbags:" (ppr PprDebug (vb1 `unionBags` vb2, [(s,p) | (s,(p,_)) <- bagToList (tcb1 `unionBags` tcb2)])) (
    returnRn12 (vb1 `unionBags` vb2, tcb1 `unionBags` tcb2)
--  )
  where
    do_decl (IfaceImportDecl orig_mod_name imports renamings src_loc)
      =		-- Look at the renamings to get a suitable renaming function
	doRenamings mod_rn_fn int_mod_name orig_mod_name renamings	
				    `thenRn12` \ (orig_to_pn, local_to_pn) ->

	    -- Now deal with one import at a time, combining results.
	returnRn12 (
	  foldl (doIfaceImport1 orig_to_pn local_to_pn)
		(emptyBag, emptyBag)
		imports
	)
\end{code}

@doIfaceImport1@ takes a list of imports and the pair of renaming functions,
returning a bag which maps local names to original names.

\begin{code}
doIfaceImport1 :: ( FAST_STRING	    -- Original local name
		 -> (FAST_STRING,   -- Local name in this interface
		     ProtoName)	    -- Its full protoname
		)		    
				    
	     -> IntNameFun	    -- Local name to ProtoName; use for
				    --   constructors and class ops
				    
	     -> ImportNameBags	    -- Accumulator
	     -> IE		    -- An item in the import list
	     -> ImportNameBags

doIfaceImport1 orig_to_pn local_to_pn (v_bag, tc_bag) (IEVar orig_name)
  = (v_bag `snocBag` (orig_to_pn orig_name), tc_bag)

doIfaceImport1 orig_to_pn local_to_pn acc (IEThingAbs orig_name)
  = int_import1_help orig_to_pn local_to_pn acc orig_name

doIfaceImport1 orig_to_pn local_to_pn acc (IEThingAll orig_name)
  = int_import1_help orig_to_pn local_to_pn acc orig_name

-- the next ones will go away with 1.3:
doIfaceImport1 orig_to_pn local_to_pn acc (IEConWithCons orig_name _)
  = int_import1_help orig_to_pn local_to_pn acc orig_name

doIfaceImport1 orig_to_pn local_to_pn acc (IEClsWithOps orig_name _)
  = int_import1_help orig_to_pn local_to_pn acc orig_name

doIfaceImport1 orig_to_pn local_to_pn (v_bag, tc_bag) other
  = panic "Rename1: strange import decl"

-- Little help guy...

int_import1_help orig_to_pn local_to_pn (v_bag, tc_bag) orig_name
  = case (orig_to_pn orig_name) of { (str, o_name) ->
    (v_bag, tc_bag `snocBag` (str, (o_name, local_to_pn)))
    }
\end{code}


The renaming-processing code.  It returns two name-functions. The
first maps the {\em original} name for an entity onto a @ProtoName@
--- it is used when running over the list of things to be imported.
The second maps the {\em local} name for a constructor or class op
back to its original name --- it is used when scanning the RHS of
a @data@ or @class@ decl.

It can produce errors, if there is a domain clash on the renamings.

\begin{code}
--pprTrace
--instance Outputable _PackedString where
--    ppr sty s = ppStr (_UNPK_ s)

doRenamings :: (FAST_STRING -> FAST_STRING) -- Renamings in import stmt of module
	    -> FAST_STRING	-- Name of the module whose interface we're working on
	    -> FAST_STRING	-- Original-name module for these renamings
	    -> [Renaming]	-- Renamings
	    -> Rn12M
		((FAST_STRING	     -- Original local name to...
		    -> (FAST_STRING, -- ... Local name in this interface
		        ProtoName)   -- ... Its full protoname
		 ),	
		 IntNameFun)	     -- Use for constructors, class ops

doRenamings mod_rn_fn int_mod orig_mod []
  = returnRn12 (
      \ s ->
	let
	    result = (s, Imp orig_mod s [int_mod] (mod_rn_fn s))
	in
--	pprTrace "name1a:" (ppCat [ppr PprDebug s, ppr PprDebug result]) (
	result
--	)
	,

      \ (Unk s) ->
	let
	    result = Imp orig_mod s [int_mod] (mod_rn_fn s)
	in
--	pprTrace "name2a:" (ppCat [ppr PprDebug s, ppr PprDebug result]) (
	result
--	)
    )

doRenamings mod_rn_fn int_mod orig_mod renamings
  = let
	local_rn_fn = mkRenamingFun renamings
    in
    --pprTrace "local_rns:" (ppr PprDebug renamings) (
    returnRn12 (
      \ s ->
	let
	    local_name = local_rn_fn s
	    result
	      = (local_name, Imp orig_mod s [int_mod] (mod_rn_fn local_name))
	in
--	pprTrace "name1:" (ppCat [ppr PprDebug s, ppr PprDebug result]) (
	result
--	)
	,

      \ (Unk s) ->
	let
	    result
	      = Imp orig_mod s [int_mod] (mod_rn_fn (local_rn_fn s))
	in
--	pprTrace "name2:" (ppCat [ppr PprDebug s, ppr PprDebug result]) (
	result
--	)
    )
    --)
\end{code}

\begin{code}
mkRenamingFun :: [Renaming] -> FAST_STRING -> FAST_STRING

mkRenamingFun []	= \ s -> s
mkRenamingFun renamings 
  = let
	rn_fn = lookupFM (listToFM -- OLD: mkStringLookupFn
		  [ (old, new) | MkRenaming old new <- renamings ]
		  ) -- OLD: False {-not-sorted-}
    in
    \s -> case rn_fn s of
	    Nothing -> s
	    Just s' -> s'
\end{code}


%************************************************************************
%*									*
\subsection{Type declarations}
%*									*
%************************************************************************

@doIfaceTyDecls1@ uses the `name function' to map local tycon names into
original names, calling @doConDecls1@ to do the same for the
constructors. @doTyDecls1@ is used to do both module and interface
type declarations.

\begin{code}
doIfaceTyDecls1 :: SelectiveImporter
	      -> IntTCNameFun
	      -> [ProtoNameTyDecl]
	      -> Rn12M [ProtoNameTyDecl]

doIfaceTyDecls1 sifun full_tc_nf ty_decls
  = mapRn12 do_decl ty_decls `thenRn12` \ decls_maybe ->
    returnRn12 (catMaybes decls_maybe)
  where
    do_decl (TyData context tycon tyvars condecls derivs (DataPragmas hidden_cons specs) src_loc)
      = let
	    full_thing = returnRn12 (Just ty_decl')
	in
		-- GHC doesn't allow derivings in interfaces
	(if null derivs
	 then returnRn12 ()
	 else addErrRn12 (derivingInIfaceErr tycon derivs src_loc)
	) `thenRn12` \ _ ->

	case (sifun tycon) of
	  NotWanted			-> returnRn12 Nothing
	  Wanted			-> full_thing
	  WantedWith (IEThingAll _)	-> full_thing
	  WantedWith (IEThingAbs _)	-> returnRn12 (Just abs_ty_decl')
	  WantedWith ie@(IEConWithCons _ _) -> full_thing

	  WantedWith really_weird_ie -> -- probably a typo in the pgm
	    addErrRn12 (weirdImportExportConstraintErr
			tycon really_weird_ie src_loc) `thenRn12` \ _ ->
	    full_thing
      where
	(tycon_name, constr_nf) = full_tc_nf tycon
	tc_nf	    		= fst . full_tc_nf

	condecls'   = map (do_condecl constr_nf tc_nf) condecls
	hidden_cons' = map (do_condecl constr_nf tc_nf) hidden_cons

	pragmas' invent_hidden
    	  = DataPragmas (if null hidden_cons && invent_hidden
			 then condecls' -- if importing abstractly but condecls were
			                -- exported we add them to the data pragma
			 else hidden_cons')
			specs {- ToDo: do_specs -}

	context'    = doIfaceContext1 tc_nf context
	deriv'	    = map tc_nf derivs -- rename derived classes

	ty_decl'    = TyData context' tycon_name tyvars condecls' deriv' (pragmas' False) src_loc
	abs_ty_decl'= TyData context' tycon_name tyvars []	  deriv' (pragmas' True) src_loc

    do_decl (TySynonym tycon tyvars monoty pragmas src_loc)
      = let
	    full_thing = returnRn12 (Just ty_decl')
	in
	case (sifun tycon) of
	  NotWanted		    -> returnRn12 Nothing
	  Wanted		    -> full_thing
	  WantedWith (IEThingAll _) -> full_thing

	  WantedWith weird_ie	    -> full_thing
      where
	(tycon_name,_) = full_tc_nf tycon
	tc_nf	= fst . full_tc_nf
	monoty'	= doIfaceMonoType1 tc_nf monoty
	ty_decl' = TySynonym tycon_name tyvars monoty' pragmas src_loc

    -- one name fun for the data constructor, another for the type:

    do_condecl c_nf tc_nf (ConDecl name tys src_loc)
      = ConDecl (c_nf name) (doIfaceMonoTypes1 tc_nf tys) src_loc
\end{code}

%************************************************************************
%*									*
\subsection{Class declarations}
%*									*
%************************************************************************

@doIfaceClassDecls1@ uses the `name function' to map local class names into
original names, calling @doIfaceClassOp1@ to do the same for the
class operations. @doClassDecls1@ is used to process both module and
interface class declarations.

\begin{code}
doIfaceClassDecls1 ::  SelectiveImporter
		 -> IntTCNameFun
		 -> [ProtoNameClassDecl]
		 -> Rn12M [ProtoNameClassDecl]

doIfaceClassDecls1 sifun full_tc_nf clas_decls
  = mapRn12 do_decl clas_decls `thenRn12` \ decls_maybe ->
    returnRn12 (catMaybes decls_maybe)
  where
    do_decl (ClassDecl ctxt cname tyvar sigs bs@EmptyMonoBinds prags locn)
				     -- No defaults in interface
      = let
	    full_thing = returnRn12 (Just class_decl')
	in
        case (sifun cname) of
	  NotWanted			-> returnRn12 Nothing
	  Wanted			-> full_thing
	  WantedWith (IEThingAll _)	-> full_thing
--???	  WantedWith (IEThingAbs _)	-> returnRn12 (Just abs_class_decl')
	  WantedWith (IEClsWithOps _ _) -> full_thing
	  -- ToDo: add checking of IEClassWithOps
	  WantedWith really_weird_ie	-> -- probably a typo in the pgm
	    addErrRn12 (weirdImportExportConstraintErr
			cname really_weird_ie locn) `thenRn12` \ _ ->
	    full_thing
      where
	(clas, op_nf) = full_tc_nf cname
	tc_nf = fst . full_tc_nf

	sigs' = map (doIfaceClassOp1 op_nf tc_nf) sigs
	ctxt' = doIfaceContext1 tc_nf ctxt

	class_decl'     = ClassDecl ctxt' clas tyvar sigs' bs prags locn
	abs_class_decl' = ClassDecl ctxt' clas tyvar []    bs prags locn
\end{code}

\begin{code}
doIfaceClassOp1 :: IntNameFun	-- Use this for the class ops
	      -> IntNameFun	-- Use this for the types
	      -> ProtoNameClassOpSig
	      -> ProtoNameClassOpSig

doIfaceClassOp1 op_nf tc_nf (ClassOpSig v ty pragma src_loc)
  = ClassOpSig (op_nf v) (doIfacePolyType1 tc_nf ty) pragma src_loc
\end{code}

%************************************************************************
%*									*
\subsection{Instance declarations}
%*									*
%************************************************************************

We select the instance decl if either the class or the type constructor
are selected.

\begin{code}
doIfaceInstDecls1 :: SelectiveImporter
	        -> IntNameFun
		-> [ProtoNameInstDecl]
		-> [ProtoNameInstDecl]

doIfaceInstDecls1 si tc_nf inst_decls
  = catMaybes (map do_decl inst_decls)
  where
    do_decl (InstDecl context cname ty EmptyMonoBinds False modname imod uprags pragmas src_loc)
      = case (si cname, tycon_reqd) of
	  (NotWanted, NotWanted) -> Nothing
	  _			 -> Just inst_decl'
     where
       context' = doIfaceContext1	 tc_nf context
       ty'	= doIfaceMonoType1 tc_nf ty

       inst_decl' = InstDecl context' (tc_nf cname) ty' EmptyMonoBinds False modname imod uprags pragmas src_loc

       tycon_reqd
	 = case getNonPrelOuterTyCon ty of
	     Nothing -> NotWanted    -- Type doesn't have a user-defined tycon
				     -- at its outermost level
	     Just tycon -> si tycon  -- It does, so look up in the si-fun
\end{code}

%************************************************************************
%*									*
\subsection{Signature declarations}
%*									*
%************************************************************************

@doIfaceSigs1@ uses the name function to create a bag that
maps local names into original names.

NB: Can't have user-pragmas & other weird things in interfaces.

\begin{code}
doIfaceSigs1 :: SelectiveImporter -> IntNameFun -> IntNameFun
	   -> [ProtoNameSig]
	   -> [ProtoNameSig]

doIfaceSigs1 si v_nf tc_nf sigs
  = catMaybes (map do_sig sigs)
  where
    do_sig (Sig v ty pragma src_loc)
      = case (si v) of
	  NotWanted -> Nothing
	  Wanted    -> Just (Sig (v_nf v) (doIfacePolyType1 tc_nf ty) pragma src_loc)
	  -- WantedWith doesn't make sense
\end{code}


%************************************************************************
%*									*
\subsection{Fixity declarations}
%*									*
%************************************************************************

\begin{code}
doIfaceFixes1 :: SelectiveImporter -> IntNameFun
	    -> [ProtoNameFixityDecl]
	    -> [ProtoNameFixityDecl]

doIfaceFixes1 si vnf fixities
  = catMaybes (map do_fixity fixities)
  where
    do_fixity (InfixL name i) = do_one InfixL name i
    do_fixity (InfixR name i) = do_one InfixR name i
    do_fixity (InfixN name i) = do_one InfixN name i

    do_one con name i
      = case si name of
	  Wanted    -> Just (con (vnf name) i)
	  NotWanted -> Nothing
\end{code}


%************************************************************************
%*									*
\subsection{doContext, MonoTypes, MonoType, Polytype}
%*									*
%************************************************************************

\begin{code}
doIfacePolyType1 :: IntNameFun -> ProtoNamePolyType -> ProtoNamePolyType

doIfacePolyType1 tc_nf (UnoverloadedTy ty)
  = UnoverloadedTy (doIfaceMonoType1 tc_nf ty)

doIfacePolyType1 tc_nf (OverloadedTy ctxt ty)
  = OverloadedTy (doIfaceContext1 tc_nf ctxt) (doIfaceMonoType1 tc_nf ty)
\end{code}

\begin{code}
doIfaceContext1 :: IntNameFun -> ProtoNameContext -> ProtoNameContext
doIfaceContext1 tc_nf  context = [(tc_nf clas, tyvar) | (clas,tyvar) <- context]
\end{code}


\begin{code}
doIfaceMonoTypes1 :: IntNameFun -> [ProtoNameMonoType] -> [ProtoNameMonoType]
doIfaceMonoTypes1 tc_nf tys = map (doIfaceMonoType1 tc_nf) tys
\end{code}


\begin{code}
doIfaceMonoType1 :: IntNameFun -> ProtoNameMonoType -> ProtoNameMonoType

doIfaceMonoType1 tc_nf (MonoTyVar tyvar) = MonoTyVar tyvar

doIfaceMonoType1 tc_nf (ListMonoTy ty)
  = ListMonoTy (doIfaceMonoType1 tc_nf ty)

doIfaceMonoType1 tc_nf (FunMonoTy ty1 ty2)
  = FunMonoTy (doIfaceMonoType1 tc_nf ty1) (doIfaceMonoType1 tc_nf ty2)

doIfaceMonoType1 tc_nf (TupleMonoTy tys)
  = TupleMonoTy (map (doIfacePolyType1 tc_nf) tys)

doIfaceMonoType1 tc_nf (MonoTyCon name tys)
  = MonoTyCon (tc_nf name) (doIfaceMonoTypes1 tc_nf tys)

#ifdef DPH
doIfaceMonoType1 tc_nf (MonoTyProc tys ty)
  = MonoTyProc (doIfaceMonoTypes1 tc_nf tys) (doIfaceMonoType1 tc_nf ty)

doIfaceMonoType1 tc_nf (MonoTyPod ty)
  = MonoTyPod (doIfaceMonoType1 tc_nf ty)
#endif {- Data Parallel Haskell -}
\end{code}
