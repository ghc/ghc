%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnPass1]{@RnPass1@: gather up imported information}

See the @Rename@ module for a basic description of the renamer.

\begin{code}
#include "HsVersions.h"

module RnPass1 (
	rnModule1

	-- for completeness
    ) where

import Ubiq{-uitous-}

import HsSyn
import HsPragmas	( DataPragmas(..) )
import RdrHsSyn		-- ProtoName* instantiations...

import Bag		( emptyBag, unitBag, snocBag, unionBags, Bag )
import ErrUtils
import FiniteMap	( lookupFM, listToFM, elementOf )
import Maybes		( catMaybes, maybeToBool )
import Name		( Name{-instances-} )
import Outputable	( isAvarid, getLocalName, interpp'SP )
import PprStyle		( PprStyle(..) )
import Pretty
import ProtoName	( mkPreludeProtoName, ProtoName(..) )
import RnMonad12
import RnUtils
import Util		( lengthExceeds, panic )
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
  | WantedWith (IE ProtoName)
\end{code}

The @ProtoNames@ supplied to these ``name functions'' are always
@Unks@, unless they are fully-qualified names, which occur only in
interface pragmas (and, therefore, never on the {\em definitions} of
things).  That doesn't happen in @RnPass1@!
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
rnModule1 :: PreludeNameMappers
	  -> Bool		-- see use below
	  -> ProtoNameHsModule
	  -> Rn12M (ProtoNameHsModule, Bag FAST_STRING)

rnModule1 pnf@(v_pnf, tc_pnf)
	use_mentioned_vars_heuristic
	(HsModule mod_name exports imports fixes
		  ty_decls absty_sigs class_decls inst_decls specinst_sigs
		  defaults binds _ src_loc)

  =	-- slurp through the *body* of the module, collecting names of
	-- mentioned *variables*, 3+ letters long & not prelude names.
	-- Note: we *do* have to pick up top-level binders,
	-- so we can check for conflicts with imported guys!
    let
	is_mentioned_fn = \ x -> True -- wimp way out
{- OLD:
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
	  = lookupFM (listToFM
		[ (x, panic "is_mentioned_fn")
		| x <- mentioned_vars ++ needed_for_deriving ]
		)
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
-}
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
	 ((HsModule mod_name
		    exports imports -- passed along mostly for later checking
		    (int_fixes ++ fixes)
		    (int_ty_decls ++ ty_decls)
		    absty_sigs
		    (int_class_decls ++ class_decls)
		    (int_inst_decls  ++ inst_decls')
		    specinst_sigs
		    defaults
		    binds
		    int_sigs
		    src_loc),
	  import_names)
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
    revolt_me (InstDecl cname ty binds True modname uprags pragma src_loc)
      = InstDecl
	    (tc_nf cname)		-- Look up the class
	    (doIfacePolyType1 tc_nf ty)	-- Ditto the type
	    binds			-- Binds unchanged
	    True{-yes,defined in this module-}
	    modname
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
doImportedIfaces :: PreludeNameMappers
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
doOneIface :: PreludeNameMappers
	   -> (FAST_STRING -> Bool)
	   -> ProtoNameImportedInterface
	   -> Rn12M AllIntDecls

doOneIface _ _ (ImportMod _ True{-qualified-} _ _)
  = panic "RnPass1.doOneIface:can't grok `qualified'"

doOneIface _ _ (ImportMod _ _ (Just _) _)
  = panic "RnPass1.doOneIface:can't grok `as' module (blech)"

doOneIface pnfs is_mentioned_fn (ImportMod iface qual asmod Nothing{-all-})
  = doIface1 pnfs (selectAll is_mentioned_fn) iface

doOneIface pnfs _ (ImportMod iface qual asmod (Just (False{-unhidden-}, ies)))
  = doIface1 pnfs si_fun iface
  where
    -- the `selective import' function should not be applied
    -- to the Imps that occur on Ids in unfoldings.

    si_fun (Unk    n) = check_ie n ies
    si_fun (Qunk _ n) = check_ie n ies

    check_ie name [] = NotWanted
    check_ie name (ie:ies)
      = case ie of
	  IEVar (Unk n)	     | name == n -> Wanted
	  IEThingAbs (Unk n) | name == n -> WantedWith ie
	  IEThingAll (Unk n) | name == n -> WantedWith ie
	  IEModuleContents _ -> panic "Module.. in import list?"
	  other		     -> check_ie name ies

doOneIface pnfs _ (ImportMod iface qual asmod (Just (True{-hidden-}, ies)))
  = doIface1 pnfs si_fun iface
  where
    -- see comment above:

    si_fun x | n `elementOf` entity_info = NotWanted
	     | otherwise		 = Wanted
      where
	n = case x of { Unk s -> s; Qunk _ s -> s }

    entity_info = getImportees ies
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
selectAll :: (FAST_STRING -> Bool) -> SelectiveImporter

selectAll is_mentioned_fn n
  = let
	rn_str = case n of { Unk s -> s ; Qunk _ s -> s }
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
doIface1 :: PreludeNameMappers
	 -> SelectiveImporter
	 -> ProtoNameInterface
	 -> Rn12M AllIntDecls

doIface1 (v_pnf, tc_pnf) sifun
       (Interface i_name import_decls fix_decls ty_decls class_decls
		    inst_decls sig_decls anns)

  = doIfaceImports1 (panic "i_name"{-i_name-}) import_decls	`thenRn12` \ (v_bag, tc_bag) ->
    do_body (v_bag, tc_bag)
  where
    do_body (v_bag, tc_bag)
      = report_all_errors			`thenRn12` \ _ ->

	doIfaceTyDecls1    sifun full_tc_nf ty_decls	`thenRn12` \ ty_decls' ->

	doIfaceClassDecls1 sifun full_tc_nf class_decls  `thenRn12` \ class_decls' ->

	let sig_decls'	= doIfaceSigs1      sifun v_nf tc_nf sig_decls
	    fix_decls'	= doIfaceFixes1     sifun v_nf       fix_decls
	    inst_decls'	= doIfaceInstDecls1 sifun      tc_nf inst_decls
	in
	returnRn12 (fix_decls', ty_decls', class_decls', inst_decls', sig_decls', unitBag i_name)
      where
	v_dups  :: [[(FAST_STRING, ProtoName)]]
	tc_dups :: [[(FAST_STRING, (ProtoName, IntNameFun))]]

	(imp_v_nf, v_dups)   = mkNameFun v_bag
	(imp_tc_nf, tc_dups) = mkNameFun tc_bag

	v_nf :: IntNameFun
	v_nf (Unk s) = case v_pnf s of
			 Just n	 -> mkPreludeProtoName n
			 Nothing -> case imp_v_nf s of
				      Just n  -> n
				      Nothing -> Imp i_name s [i_name] s

		-- used for (..)'d parts of prelude datatype/class decls
	prel_con_or_op_nf  :: FAST_STRING{-module name-}-> IntNameFun
	prel_con_or_op_nf m (Unk s)
	  = case v_pnf s of
	      Just n  -> mkPreludeProtoName n
	      Nothing -> Imp m s [m] s
			 -- Strictly speaking, should be *no renaming* here, folks

		-- used for non-prelude constructors/ops/fields
	local_con_or_op_nf :: IntNameFun
	local_con_or_op_nf (Unk s) = Imp i_name s [i_name] s

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
			  Nothing   -> (Imp i_name s [i_name] s,
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
	:: FAST_STRING			-- name of module whose interface we're doing
	-> [IfaceImportDecl ProtoName]
	-> Rn12M ImportNameBags

doIfaceImports1 _  [] = returnRn12 (emptyBag, emptyBag)

doIfaceImports1 int_mod_name (imp_decl1 : rest)
  = do_decl			 imp_decl1  `thenRn12` \ (vb1, tcb1) ->
    doIfaceImports1 int_mod_name rest	    `thenRn12` \ (vb2, tcb2) ->
    returnRn12 (vb1 `unionBags` vb2, tcb1 `unionBags` tcb2)
  where
    do_decl (IfaceImportDecl orig_mod_name imports src_loc)
      =		-- Look at the renamings to get a suitable renaming function
	doRenamings{-not really-} int_mod_name orig_mod_name
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
doIfaceImport1 :: ( ProtoName	    -- Original local name
		 -> (FAST_STRING,   -- Local name in this interface
		     ProtoName)	    -- Its full protoname
		)

	     -> IntNameFun	    -- Local name to ProtoName; use for
				    --   constructors and class ops

	     -> ImportNameBags	    -- Accumulator
	     -> (IE ProtoName)	    -- An item in the import list 
	     -> ImportNameBags

doIfaceImport1 orig_to_pn local_to_pn (v_bag, tc_bag) (IEVar orig_name)
  = (v_bag `snocBag` (orig_to_pn orig_name), tc_bag)

doIfaceImport1 orig_to_pn local_to_pn acc (IEThingAbs orig_name)
  = int_import1_help orig_to_pn local_to_pn acc orig_name

doIfaceImport1 orig_to_pn local_to_pn acc (IEThingAll orig_name)
  = int_import1_help orig_to_pn local_to_pn acc orig_name

-- the next ones will go away with 1.3:
{- OLD:
doIfaceImport1 orig_to_pn local_to_pn acc (IEConWithCons orig_name _)
  = int_import1_help orig_to_pn local_to_pn acc orig_name

doIfaceImport1 orig_to_pn local_to_pn acc (IEClsWithOps orig_name _)
  = int_import1_help orig_to_pn local_to_pn acc orig_name
-}

doIfaceImport1 orig_to_pn local_to_pn (v_bag, tc_bag) other
  = panic "RnPass1: strange import decl"

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
doRenamings :: FAST_STRING	-- Name of the module whose interface we're working on
	    -> FAST_STRING	-- Original-name module for these renamings
	    -> Rn12M
		((ProtoName	     -- Original local name to...
		    -> (FAST_STRING, -- ... Local name in this interface
			ProtoName)   -- ... Its full protoname
		 ),
		 IntNameFun)	     -- Use for constructors, class ops

doRenamings int_mod orig_mod
  = returnRn12 (
      \ (Unk s) ->
	let
	    result = (s, Imp orig_mod s [int_mod] s)
	in
	result
	,

      \ (Unk s) ->
	let
	    result = Imp orig_mod s [int_mod] s
	in
	result
    )
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
    do_decl (TySynonym tycon tyvars monoty src_loc)
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
	ty_decl' = TySynonym tycon_name tyvars monoty' src_loc

    do_decl (TyData context tycon tyvars condecls derivs pragmas src_loc)
      = do_data context tycon condecls derivs pragmas src_loc `thenRn12` \ done_data ->
	case done_data of
	  Nothing -> returnRn12 Nothing
	  Just (context', tycon', condecls', derivs', pragmas') ->
	     returnRn12 (Just (TyData context' tycon' tyvars condecls' derivs' pragmas' src_loc))

    do_decl (TyNew context tycon tyvars condecl derivs pragmas src_loc)
      = do_data context tycon condecl derivs pragmas src_loc `thenRn12` \ done_data ->
	case done_data of
	  Nothing -> returnRn12 Nothing
	  Just (context', tycon', condecl', derivs', pragmas') ->
	     returnRn12 (Just (TyNew context' tycon' tyvars condecl' derivs' pragmas' src_loc))

    --------------------------------------------
    do_data context tycon condecls derivs (DataPragmas hidden_cons specs) src_loc
      = let
	    full_thing = Just (context', tycon_name, condecls', deriv', (pragmas' False))
	    abs_thing  = Just (context', tycon_name, [],        deriv', (pragmas' True))
	in
	case (sifun tycon) of
	  NotWanted			-> returnRn12 Nothing
	  Wanted			-> returnRn12 full_thing
	  WantedWith (IEThingAll _)	-> returnRn12 full_thing
	  WantedWith (IEThingAbs _)	-> returnRn12 abs_thing

	  WantedWith really_weird_ie -> -- probably a typo in the pgm
	    addErrRn12 (weirdImportExportConstraintErr
			tycon really_weird_ie src_loc) `thenRn12` \ _ ->
	    returnRn12 full_thing
      where
	(tycon_name, constrfield_nf) = full_tc_nf tycon
	tc_nf	    		     = fst . full_tc_nf

	condecls'    = map (do_condecl constrfield_nf tc_nf) condecls
	hidden_cons' = map (do_condecl constrfield_nf tc_nf) hidden_cons

	pragmas' invent_hidden
    	  = DataPragmas (if null hidden_cons && invent_hidden
			 then condecls'  -- if importing abstractly but condecls were
			                 -- exported we add them to the data pragma
			 else hidden_cons')
			specs {- ToDo: do_specs -}

	context'    = doIfaceContext1 tc_nf context
	deriv'	    = case derivs of
			Nothing -> Nothing
			Just ds -> panic "doIfaceTyDecls1:derivs" -- Just (map tc_nf ds)
								  -- rename derived classes

    --------------------------------------------
    -- one name fun for the data constructor, another for the type:

    do_condecl cf_nf tc_nf (ConDecl name tys src_loc)
      = ConDecl (cf_nf name) (map (do_bang tc_nf) tys) src_loc

    do_condecl cf_nf tc_nf (ConOpDecl ty1 op ty2 src_loc)
      = ConOpDecl (do_bang tc_nf ty1) (cf_nf op) (do_bang tc_nf ty2) src_loc

    do_condecl cf_nf tc_nf (NewConDecl name ty src_loc)
      = NewConDecl (cf_nf name) (doIfaceMonoType1 tc_nf ty) src_loc

    do_condecl cf_nf tc_nf (RecConDecl con fields src_loc)
      = RecConDecl (cf_nf con) (map do_field fields) src_loc
      where
	do_field (vars, ty) = (map cf_nf vars, do_bang tc_nf ty)

    --------------------------------------------
    do_bang tc_nf (Banged   ty) = Banged   (doIfaceMonoType1 tc_nf ty)
    do_bang tc_nf (Unbanged ty) = Unbanged (doIfaceMonoType1 tc_nf ty)
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
    do_decl (InstDecl cname ty EmptyMonoBinds False modname uprags pragmas src_loc)
      = case (si cname, tycon_reqd) of
	  (NotWanted, NotWanted) -> Nothing
	  _			 -> Just inst_decl'
     where
       ty'	= doIfacePolyType1 tc_nf ty

       inst_decl' = InstDecl (tc_nf cname) ty' EmptyMonoBinds False modname uprags pragmas src_loc

       tycon_reqd = _trace "RnPass1.tycon_reqd" NotWanted
{- LATER:
	 = case getNonPrelOuterTyCon ty of
	     Nothing -> NotWanted    -- Type doesn't have a user-defined tycon
				     -- at its outermost level
	     Just tycon -> si tycon  -- It does, so look up in the si-fun
-}
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

doIfacePolyType1 tc_nf (HsPreForAllTy ctxt ty)
  = HsPreForAllTy (doIfaceContext1 tc_nf ctxt) (doIfaceMonoType1 tc_nf ty)

doIfacePolyType1 tc_nf (HsForAllTy tvs ctxt ty)
  = HsForAllTy tvs (doIfaceContext1 tc_nf ctxt) (doIfaceMonoType1 tc_nf ty)
\end{code}

\begin{code}
doIfaceContext1 :: IntNameFun -> ProtoNameContext -> ProtoNameContext
doIfaceContext1 tc_nf  context = [(tc_nf clas, tyvar) | (clas,tyvar) <- context]
\end{code}


\begin{code}
doIfaceMonoType1 :: IntNameFun -> ProtoNameMonoType -> ProtoNameMonoType

doIfaceMonoType1 tc_nf tv@(MonoTyVar _) = tv

doIfaceMonoType1 tc_nf (MonoListTy ty)
  = MonoListTy (doIfaceMonoType1 tc_nf ty)

doIfaceMonoType1 tc_nf (MonoFunTy ty1 ty2)
  = MonoFunTy (doIfaceMonoType1 tc_nf ty1) (doIfaceMonoType1 tc_nf ty2)

doIfaceMonoType1 tc_nf (MonoTupleTy tys)
  = MonoTupleTy (map (doIfaceMonoType1 tc_nf) tys)

doIfaceMonoType1 tc_nf (MonoTyApp name tys)
  = MonoTyApp (tc_nf name) (map (doIfaceMonoType1 tc_nf) tys)
\end{code}

%************************************************************************
%*									*
\subsection{Error messages}
%*									*
%************************************************************************

\begin{code}
duplicateImportsInInterfaceErr iface dups
  = panic "duplicateImportsInInterfaceErr: NOT DONE YET?"

weirdImportExportConstraintErr thing constraint locn
  = addShortErrLocLine locn ( \ sty ->
    ppBesides [ppStr "Illegal import/export constraint on `",
	       ppr sty thing,
	       ppStr "': ", ppr PprForUser constraint])
\end{code}
