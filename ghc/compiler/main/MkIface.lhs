%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[MkIface]{Print an interface for a module}

\begin{code}
#include "HsVersions.h"

module MkIface (
	mkInterface,

	-- and to make the interface self-sufficient...
	Bag, CE(..), GlobalSwitch, FixityDecl, Id,
	Name, PrettyRep, StgBinding, TCE(..), UniqFM, InstInfo
    ) where

IMPORT_Trace		-- ToDo: rm (debugging)

import AbsPrel		( mkLiftTy, pRELUDE_CORE, pRELUDE_BUILTIN )
import AbsSyn		( FixityDecl(..), RenamedFixityDecl(..), MonoBinds,
			  RenamedMonoBinds(..), Name, RenamedPat(..), Sig
			)
import AbsUniType
import Bag
import CE
import CmdLineOpts	-- ( GlobalSwitch(..) )
import FiniteMap
import Id
import IdInfo		-- plenty from here
import Maybes		( catMaybes, Maybe(..) )
import Outputable
import Pretty
import StgSyn
import TCE
import TcInstDcls	( InstInfo(..) )
import Util
\end{code}

%************************************************************************
%*									*
\subsection[main-MkIface]{Main routine for making interfaces}
%*									*
%************************************************************************

Misc points:
\begin{enumerate}
\item
We get the general what-to-export information from the ``environments''
produced by the typechecker (the \tr{[RenamedFixityDecl]} through
\tr{Bag InstInfo} arguments).

\item
{\em However:} Whereas (for example) an \tr{InstInfo} will have
\tr{Ids} in it that identify the constant methods for that instance,
those particular \tr{Ids} {\em do not have} the best @IdInfos@!!!
Those @IdInfos@ were figured out long after the \tr{InstInfo} was
created.

That's why we actually look at the final \tr{PlainStgBindings} that go
into the code-generator: they have the best @IdInfos@ on them.
Whenever, we are about to print info about an @Id@, we look in the
Ids-from-STG-bindings list to see if we have an ``equivalent'' @Id@
with presumably-better @IdInfo@.

\item
We play this same game whether for values, classes (for their
method-selectors and default-methods), or instances (for their
@DictFunIds@ or constant-methods).

Of course, for imported things, what we got from the typechecker is
all we're gonna get.

\item
We {\em sort} things in the interface into some ``canonical'' order;
otherwise, with heavily-recursive modules, you can have (unchanged)
information ``move around'' in the interface file---deeply unfriendly
to \tr{make}.
\end{enumerate}

\begin{code}
mkInterface :: (GlobalSwitch -> Bool)
	    -> FAST_STRING
	    -> (FAST_STRING -> Bool,  -- is something in export list, explicitly?
		FAST_STRING -> Bool)  -- is a module among the "dotdot" exported modules?
	    -> IdEnv UnfoldingDetails
	    -> FiniteMap TyCon [(Bool, [Maybe UniType])]
	    -> ([RenamedFixityDecl],  -- interface info from the typecheck
	        [Id],
	        CE,
	        TCE,
	        Bag InstInfo)
	    -> [PlainStgBinding]
	    -> Pretty

mkInterface sw_chkr modname export_list_fns inline_env tycon_specs
	    (fixity_decls, global_ids, ce, tce, inst_infos)
	    stg_binds
  = let
	-- first, gather up the things we want to export:

	exported_tycons  = [ tc | tc <- rngTCE tce,
			   isExported tc,
			   is_exportable_tycon_or_class sw_chkr export_list_fns tc ]
	exported_classes = [  c |  c <- rngCE  ce,
			   isExported  c,
			   is_exportable_tycon_or_class sw_chkr export_list_fns  c ]
	exported_inst_infos = [ i | i <- bagToList inst_infos,
			   is_exported_inst_info sw_chkr export_list_fns i ]
	exported_vals
    	  = [ v | v <- global_ids,
	      isExported v && not (isDataCon v) && not (isClassOpId v) ]

	-- We also have to worry about TyCons/Classes that are
	-- *mentioned* in exported things (e.g., values' types or
	-- instances), so that we can be sure to do an import decl for
	-- them, for original-naming purposes:

	(mentioned_tycons, mentioned_classes)
	  = foldr ( \ (tcs1, cls1) (tcs2, cls2)
		      -> (tcs1 `unionBags` tcs2, cls1 `unionBags` cls2) )
		  (emptyBag, emptyBag)
		  (map getMentionedTyConsAndClassesFromClass exported_classes  ++ 
		   map getMentionedTyConsAndClassesFromTyCon exported_tycons   ++
		   map getMentionedTyConsAndClassesFromId    exported_vals     ++
		   map getMentionedTyConsAndClassesFromInstInfo exported_inst_infos)

	mentionable_classes
	  = filter (is_mentionable sw_chkr) (bagToList mentioned_classes)
	mentionable_tycons
	  = [ tc | tc <- bagToList mentioned_tycons,
		   is_mentionable sw_chkr tc,
		   not (isPrimTyCon tc) ]

	nondup_mentioned_tycons  = fst (removeDups cmpTyCon mentionable_tycons)
	nondup_mentioned_classes = fst (removeDups cmpClass mentionable_classes)

	-- Next: as discussed in the notes, we want the top-level
	-- Ids straight from the final STG code, so we can use
	-- their IdInfos to print pragmas; we slurp them out here,
	-- then pass them to the printing functions, which may
	-- use them.

	better_ids = collectExportedStgBinders stg_binds

	-- Make a lookup function for convenient access:

	better_id_fn i
	  = if not (isLocallyDefined i)
	    then i  -- can't be among our "better_ids"
	    else
	       let
		   eq_fn = if isTopLevId i -- can't trust uniqs
			   then (\ x y -> getOrigName x == getOrigName y)
			   else eqId
	       in
	       case [ x | x <- better_ids, x `eq_fn` i ] of
		 []  -> pprPanic "better_id_fn:" (ppr PprShowAll i)
			i
		 [x] -> x
		 _   -> panic "better_id_fn"

	-- Finally, we sort everything lexically, so that we always
	-- get the same interface from the same information:

	sorted_mentioned_tycons  = sortLt ltLexical nondup_mentioned_tycons
	sorted_mentioned_classes = sortLt ltLexical nondup_mentioned_classes

	sorted_tycons     = sortLt ltLexical exported_tycons
	sorted_classes    = sortLt ltLexical exported_classes
	sorted_vals       = sortLt ltLexical exported_vals
	sorted_inst_infos = sortLt lt_lexical_inst_info exported_inst_infos
    in
    if (any_purely_local sorted_tycons sorted_classes sorted_vals) then
	-- this will be less of a HACK when we teach
	-- mkInterface to do I/O (WDP 94/10)
	error "Can't produce interface file because of errors!\n"
    else
--  trace ("mkIface:Ids:"++(ppShow 80 (ppr PprDebug global_ids))) (
    ppAboves
       [ppPStr SLIT("{-# GHC_PRAGMA INTERFACE VERSION 6 #-}"),
	ppCat [ppPStr SLIT("interface"), ppPStr modname, ppPStr SLIT("where")],

	do_import_decls sw_chkr modname
		sorted_vals sorted_mentioned_classes sorted_mentioned_tycons,
		-- Mustn't give the data constructors to do_import_decls,
		-- because they aren't explicitly imported; their tycon is.
		-- ToDo: modify if we ever add renaming properly.

	ppAboves (map (do_fixity sw_chkr)		              fixity_decls),
	ppAboves (map (pprIfaceClass sw_chkr better_id_fn inline_env) sorted_classes),
	ppAboves (map (do_tycon    sw_chkr tycon_specs)	              sorted_tycons),
	ppAboves (map (do_value    sw_chkr better_id_fn inline_env)   sorted_vals),
	ppAboves (map (do_instance sw_chkr better_id_fn inline_env)   sorted_inst_infos),

        ppChar '\n'
       ]
--  )
  where
    any_purely_local tycons classes vals
      =  any bad_tc tycons || any bad_cl classes || any bad_id vals
      where
	bad_cl cl
	  = case (maybePurelyLocalClass cl) of
	      Nothing -> False
	      Just xs -> naughty_trace cl xs

	bad_id id
	  = case (maybePurelyLocalType (getIdUniType id)) of
	      Nothing -> False
	      Just xs -> naughty_trace id xs

	bad_tc tc
	  = case (maybePurelyLocalTyCon tc) of
	      Nothing -> False
	      Just xs -> if exported_abs then False else naughty_trace tc xs
	  where
	    exported_abs = case (getExportFlag tc) of { ExportAbs -> True; _ -> False }

	naughty_trace x things
	  = pprTrace "Can't export -- `"
		(ppBesides [ppr PprForUser x, ppStr "' mentions purely local things: ",
			ppInterleave pp'SP things])
		True
\end{code}

%************************************************************************
%*									*
\subsection[imports-MkIface]{Generating `import' declarations in an interface}
%*									*
%************************************************************************

Not handling renaming yet (ToDo)

We gather up lots of (module, name) pairs for which we might print an
import declaration.  We sort them, for the usual canonicalisation
reasons.  NB: We {\em assume} the lists passed in don't have duplicates in
them!  expect).

All rather horribly turgid (WDP).

\begin{code}
do_import_decls
	:: (GlobalSwitch -> Bool)
	-> FAST_STRING
	-> [Id] -> [Class] -> [TyCon]
	-> Pretty

do_import_decls sw_chkr mod_name vals classes tycons
  = let
	-- Conjure up (module, name, maybe_renaming) triples for all
	-- the potentially import-decls things:

	vals_names, classes_names, tycons_names :: [(FAST_STRING, FAST_STRING, [Maybe FAST_STRING])]
	vals_names	= map get_val_triple   vals
	classes_names	= map get_class_triple classes
	tycons_names	= map get_tycon_triple tycons

	-- sort the (module, name, renaming) triples and chop
	-- them into per-module groups:

	ie_list = sortLt lt (tycons_names ++ classes_names ++ vals_names)

	per_module_groups = runs same_module ie_list
    in
    ppAboves (map print_a_decl per_module_groups)
  where
    lt, same_module :: (FAST_STRING, FAST_STRING, [Maybe FAST_STRING])
		    -> (FAST_STRING, FAST_STRING, [Maybe FAST_STRING]) -> Bool 

    lt (m1, ie1, _) (m2, ie2, _)
      = case _CMP_STRING_ m1 m2 of { LT_ -> True; EQ_ -> ie1 < ie2; GT__ -> False }

    same_module (m1, _, _) (m2, _, _) = m1 == m2
   
    compiling_the_prelude = sw_chkr CompilingPrelude

    print_a_decl :: [(FAST_STRING, FAST_STRING, [Maybe FAST_STRING])] -> Pretty
    {-
	Obviously, if the module in question is this one,
	don't print an import declaration.

	If it's a Prelude* module, we don't print the TyCons/
	Classes, because the compiler supposedly knows about
	them already (and they are PreludeCore things anyway).

	But if we are compiling a Prelude module, then we
	try to do it as "normally" as possible.
    -}
    print_a_decl (ielist@((m,_,_) : _))
      |  m == mod_name 
      || (not compiling_the_prelude &&
	  (m == pRELUDE_CORE || m == pRELUDE_BUILTIN))
      = ppNil

      | otherwise
      = ppBesides [ppPStr SLIT("import "), ppPStr m, ppLparen, 
		   ppIntersperse pp'SP{-'-} (map pp_str [n | (_,n,_) <- ielist]),
		   ppRparen,
		   case (grab_non_Nothings [rns | (_,_,rns) <- ielist]) of
		     []	       -> ppNil
		     renamings -> pp_renamings renamings
		  ]
      where
	isnt_tycon_ish :: FAST_STRING -> Bool
	isnt_tycon_ish str = not (isConop str)

	grab_non_Nothings :: [[Maybe FAST_STRING]] -> [FAST_STRING]

	grab_non_Nothings rns = catMaybes (concat rns)

	pp_str :: FAST_STRING -> Pretty
	pp_str pstr
	  = if isAvarop pstr then ppStr ("("++str++")") else ppPStr pstr
	  where
	    str = _UNPK_ pstr

	pp_renamings strs
	  = ppBesides [ ppPStr SLIT(" renaming "), ppLparen, ppIntersperse pp'SP{-'-} (map ppPStr strs), ppRparen ]
\end{code}

Most of the huff and puff here is to ferret out renaming strings.

\begin{code}
get_val_triple   :: Id    -> (FAST_STRING, FAST_STRING, [Maybe FAST_STRING])
get_class_triple :: Class -> (FAST_STRING, FAST_STRING, [Maybe FAST_STRING])
get_tycon_triple :: TyCon -> (FAST_STRING, FAST_STRING, [Maybe FAST_STRING])

get_val_triple id
  = case (generic_triple id) of { (a,b,rn) ->
    (a,b,[rn]) }

get_class_triple clas
  = case (generic_triple clas) of { (orig_mod, orig_nm, clas_rn) ->
    let
	nm_to_print = case (getExportFlag clas) of
			ExportAll   -> orig_nm _APPEND_ SLIT("(..)") -- nothing like a good HACK!
			ExportAbs   -> orig_nm
			NotExported -> orig_nm

-- Ops don't have renaming info (bug) ToDo
--	ops	    = getClassOps clas
--	ops_rns	    = [ rn | (_,_,rn) <- map generic_triple ops ]
    in
    (orig_mod, nm_to_print, [clas_rn]) }

get_tycon_triple tycon
  = case (generic_triple tycon) of { (orig_mod, orig_nm, tycon_rn) ->
    let
	nm_to_print = case (getExportFlag tycon) of
			ExportAll   -> orig_nm _APPEND_ SLIT("(..)") -- nothing like a good HACK!
			ExportAbs   -> orig_nm
			NotExported -> orig_nm

	cons	    = getTyConDataCons tycon
	cons_rns    = [ rn | (_,_,rn) <- map generic_triple cons ]
    in
    (orig_mod, nm_to_print, tycon_rn : cons_rns) }

generic_triple thing
  = case (getOrigName       thing) of { (orig_mod, orig_nm) ->
    case (getOccurrenceName thing) of { occur_name ->
    (orig_mod, orig_nm,
     if orig_nm == occur_name
     then Nothing
     else Just (orig_nm _APPEND_ SLIT(" to ") _APPEND_ occur_name)
    )}}
\end{code}

%************************************************************************
%*									*
\subsection[fixities-MkIface]{Generating fixity declarations in an interface}
%*									*
%************************************************************************


\begin{code}
do_fixity :: (GlobalSwitch -> Bool) -> RenamedFixityDecl -> Pretty

do_fixity sw_chkr fixity_decl
  = case (getExportFlag (get_name fixity_decl)) of
      ExportAll -> ppr (PprInterface sw_chkr) fixity_decl
      _	    	-> ppNil
  where
     get_name (InfixL n _) = n
     get_name (InfixR n _) = n
     get_name (InfixN n _) = n
\end{code}

%************************************************************************
%*									*
\subsection[tycons-MkIface]{Generating tycon declarations in an interface}
%*									*
%************************************************************************

\begin{code}
do_tycon :: (GlobalSwitch -> Bool) -> FiniteMap TyCon [(Bool, [Maybe UniType])] -> TyCon -> Pretty

do_tycon sw_chkr tycon_specs_map tycon
  = pprTyCon (PprInterface sw_chkr) tycon tycon_specs
  where
    tycon_specs = map snd (lookupWithDefaultFM tycon_specs_map [] tycon)
\end{code}

%************************************************************************
%*									*
\subsection[values-MkIface]{Generating a value's signature in an interface}
%*									*
%************************************************************************

\begin{code}
do_value :: (GlobalSwitch -> Bool)
	 -> (Id -> Id)
	 -> IdEnv UnfoldingDetails
	 -> Id
	 -> Pretty

do_value sw_chkr better_id_fn inline_env val
  = let
	sty 	    = PprInterface sw_chkr
	better_val  = better_id_fn val
	name_str    = getOccurrenceName better_val -- NB: not orig name!

	id_info	    = getIdInfo better_val

	val_ty	    = let 
			 orig_ty  = getIdUniType val
			 final_ty = getIdUniType better_val
		      in
--		      ASSERT (orig_ty == final_ty || mkLiftTy orig_ty == final_ty)
		      ASSERT (if (orig_ty == final_ty || mkLiftTy orig_ty == final_ty) then True else pprTrace "do_value:" (ppCat [ppr PprDebug val, ppr PprDebug better_val]) False)
		      orig_ty

	-- Note: We export the type of the original val
	-- The type of an unboxed val will have been *lifted* by the desugarer
	-- In this case we export an unlifted type, but id_info which assumes
	--   a lifted Id i.e. extracted from better_val (above)
	-- The importing module must lift the Id before using the imported id_info

	pp_id_info
	  = if sw_chkr OmitInterfacePragmas
	    || boringIdInfo id_info
	    then ppNil
	    else ppCat [ppPStr SLIT("\t{-# GHC_PRAGMA"),
			ppIdInfo sty better_val True{-yes specs-}
			    better_id_fn inline_env id_info,
			ppPStr SLIT("#-}")]
    in
    ppAbove (ppCat [ppr_non_op name_str,
		    ppPStr SLIT("::"), pprUniType sty val_ty])
	    pp_id_info

-- sadly duplicates Outputable.pprNonOp (ToDo)

ppr_non_op str
  = if isAvarop str -- NOT NEEDED: || isAconop
    then ppBesides [ppLparen, ppPStr str, ppRparen]
    else ppPStr str
\end{code}

%************************************************************************
%*									*
\subsection[instances-MkIface]{Generating instance declarations in an interface}
%*									*
%************************************************************************

The types of ``dictionary functions'' (dfuns) have just the required
info for instance declarations in interfaces.  However, the dfuns that
GHC really uses have {\em extra} dictionaries passed to them (for
efficiency).  When we print interfaces, we want to omit that
dictionary information.  (It can be reconsituted on the other end,
from instance and class decls).

\begin{code}
do_instance :: (GlobalSwitch -> Bool)
	    -> (Id -> Id)
	    -> IdEnv UnfoldingDetails
	    -> InstInfo
	    -> Pretty

do_instance sw_chkr better_id_fn inline_env
    (InstInfo clas tv_tmpls ty inst_decl_theta dfun_theta dfun_id constm_ids _ from_here modname _ _)
  = let
	sty = PprInterface sw_chkr

	better_dfun 	 = better_id_fn dfun_id
	better_dfun_info = getIdInfo better_dfun
	better_constms	 = map better_id_fn constm_ids

	class_op_strs = map getClassOpString (getClassOps clas)

	pragma_begin
	  = ppCat [ppPStr SLIT("\t{-# GHC_PRAGMA"), pp_modname, ppPStr SLIT("{-dfun-}"),
		   ppIdInfo sty better_dfun False{-NO specs-}
		    better_id_fn inline_env better_dfun_info]

    	pragma_end = ppPStr SLIT("#-}")

	pp_modname = if _NULL_ modname
		     then ppNil
		     else ppCat [ppStr "_M_", ppPStr modname]

	name_pragma_pairs
	  = pp_the_list [ ppCat [ppChar '\t', ppr_non_op op, ppEquals,
				 ppChar '{' ,
				 ppIdInfo sty constm True{-YES, specs-}
				  better_id_fn inline_env
				  (getIdInfo constm),
				 ppChar '}' ]
			| (op, constm) <- class_op_strs `zip` better_constms ]

#ifdef DEBUG
	pp_the_list [] = panic "MkIface: no class_ops or better_constms?"
#endif
	pp_the_list [p]    = p
	pp_the_list (p:ps) = ppAbove (ppBeside p ppComma) (pp_the_list ps)

	real_stuff 
	  = ppCat [ppPStr SLIT("instance"),
		   ppr sty (mkSigmaTy tv_tmpls inst_decl_theta (mkDictTy clas ty))]
    in
    if sw_chkr OmitInterfacePragmas
    || boringIdInfo better_dfun_info
    then real_stuff
    else ppAbove real_stuff
	  ({-ppNest 8 -} -- ppNest does nothing
	     if null better_constms
	     then ppCat [pragma_begin, pragma_end]
	     else ppAbove pragma_begin (ppCat [name_pragma_pairs, pragma_end])
	  )
\end{code}

%************************************************************************
%*									*
\subsection[utils-InstInfos]{Utility functions for @InstInfos@}
%*									*
%************************************************************************

ToDo: perhaps move.

Classes/TyCons are ``known,'' more-or-less.  Prelude TyCons are
``completely'' known---they don't need to be mentioned in interfaces.
Classes usually don't need to be mentioned in interfaces, but if we're
compiling the prelude, then we treat them without special favours.
\begin{code}
is_exportable_tycon_or_class sw_chkr export_list_fns tc
  = if not (fromPreludeCore tc) then
	True
    else
	in_export_list_or_among_dotdot_modules
	    (sw_chkr CompilingPrelude) -- ignore M.. stuff if compiling prelude
	    export_list_fns tc

in_export_list_or_among_dotdot_modules ignore_Mdotdots (in_export_list, among_dotdot_modules) tc
  = if in_export_list (getOccurrenceName tc) then
	True
    else
--	pprTrace "in_export:" (ppAbove (ppr PprDebug ignore_Mdotdots) (ppPStr (getOccurrenceName tc))) (
    if ignore_Mdotdots then
	False
    else
	any among_dotdot_modules (getInformingModules tc)
--  )

is_mentionable sw_chkr tc
  = not (from_PreludeCore_or_Builtin tc) || (sw_chkr CompilingPrelude)
  where
    from_PreludeCore_or_Builtin thing
      = let
	    mod_name = fst (getOrigName thing)
	in
	mod_name == pRELUDE_CORE || mod_name == pRELUDE_BUILTIN

is_exported_inst_info sw_chkr export_list_fns
	(InstInfo clas _ ty _ _ _ _ _ from_here _ _ _)
  = let
	is_fun_tycon = isFunType ty

    	seems_exported = instanceIsExported clas ty from_here

	(tycon, _, _) = getUniDataTyCon ty
    in
    if (sw_chkr OmitReexportedInstances && not from_here) then
	False -- Flag says to violate Haskell rules, blatantly

    else if not (sw_chkr CompilingPrelude)
         || not (is_fun_tycon || fromPreludeCore tycon)
         || not (fromPreludeCore clas) then
	seems_exported -- take what we got

    else -- compiling Prelude & tycon/class are Prelude things...
	from_here
	|| in_export_list_or_among_dotdot_modules True{-ignore M..s-} export_list_fns clas
	|| (not is_fun_tycon
	    && in_export_list_or_among_dotdot_modules True{-ignore M..s-} export_list_fns tycon)
\end{code}

\begin{code}
lt_lexical_inst_info (InstInfo _ _ _ _ _ dfun1 _ _ _ _ _ _) (InstInfo _ _ _ _ _ dfun2 _ _ _ _ _ _)
  = ltLexical dfun1 dfun2
\end{code}

\begin{code}
getMentionedTyConsAndClassesFromInstInfo (InstInfo clas _ ty _ dfun_theta _ _ _ _ _ _ _)
  = case (getMentionedTyConsAndClassesFromUniType ty) of { (ts, cs) ->
    case [ c | (c, _) <- dfun_theta ]  	    	      of { theta_classes ->
    (ts, (cs `unionBags` listToBag theta_classes) `snocBag` clas)
    }}
\end{code}
