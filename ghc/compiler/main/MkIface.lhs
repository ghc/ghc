%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[MkIface]{Print an interface for a module}

\begin{code}
#include "HsVersions.h"

module MkIface {-( mkInterface )-} where

import Ubiq{-uitous-}

import Bag		( emptyBag, snocBag, bagToList )
import Class		( GenClass{-instance NamedThing-} )
import CmdLineOpts	( opt_ProduceHi )
import HsSyn
import Id		( GenId{-instance NamedThing/Outputable-} )
import Name		( nameOrigName, origName,
			  exportFlagOn, nameExportFlag, ExportFlag(..),
			  ltLexical, isExported,
			  RdrName{-instance Outputable-}
			)
import PprStyle		( PprStyle(..) )
import PprType		( pprType, TyCon{-instance Outputable-}, GenClass{-ditto-} )
import Pretty		-- quite a bit
import RnHsSyn		( RenamedHsModule(..), RnName{-instance NamedThing-} )
import RnIfaces		( VersionInfo(..) )
import TcModule		( TcIfaceInfo(..) )
import TcInstUtil	( InstInfo(..) )
import TyCon		( TyCon{-instance NamedThing-} )
import Type		( mkSigmaTy, mkDictTy, getAppTyCon )
import Util		( sortLt, assertPanic )

ppSemid x = ppBeside (ppr PprInterface x) ppSemi -- micro util
\end{code}

We have a function @startIface@ to open the output file and put
(something like) ``interface Foo N'' in it.  It gives back a handle
for subsequent additions to the interface file.

We then have one-function-per-block-of-interface-stuff, e.g.,
@ifaceExportList@ produces the @__exports__@ section; it appends
to the handle provided by @startIface@.

\begin{code}
startIface  :: Module
	    -> IO (Maybe Handle) -- Nothing <=> don't do an interface
endIface    :: Maybe Handle -> IO ()
ifaceVersions
	    :: Maybe Handle
	    -> VersionInfo
	    -> IO ()
ifaceExportList
	    :: Maybe Handle
	    -> RenamedHsModule
	    -> IO ()
ifaceFixities
	    :: Maybe Handle
	    -> RenamedHsModule
	    -> IO ()
ifaceInstanceModules
	    :: Maybe Handle
	    -> [Module]
	    -> IO ()
ifaceDecls  :: Maybe Handle
	    -> TcIfaceInfo  -- info produced by typechecker, for interfaces
	    -> IO ()
ifaceInstances
	    :: Maybe Handle
	    -> TcIfaceInfo  -- as above
	    -> IO ()
--ifacePragmas
\end{code}

\begin{code}
startIface mod
  = case opt_ProduceHi of
      Nothing -> return Nothing -- not producing any .hi file
      Just fn ->
	openFile fn WriteMode	>>= \ if_hdl ->
	hPutStr if_hdl ("interface "++ _UNPK_ mod ++" 1\n") >>
	return (Just if_hdl)

endIface Nothing	= return ()
endIface (Just if_hdl)	= hPutStr if_hdl "\n" >> hClose if_hdl
\end{code}

\begin{code}
ifaceVersions Nothing{-no iface handle-} _ = return ()

ifaceVersions (Just if_hdl) version_info
  = hPutStr if_hdl "__versions__\nFoo(1)" -- a stub, obviously
\end{code}

\begin{code}
ifaceInstanceModules Nothing{-no iface handle-} _ = return ()
ifaceInstanceModules (Just _)		       [] = return ()

ifaceInstanceModules (Just if_hdl) imods
  = hPutStr if_hdl "\n__instance_modules__\n" >>
    hPutStr if_hdl (ppShow 100 (ppCat (map ppPStr imods)))
\end{code}

Export list: grab the Names of things that are marked Exported, sort
(so the interface file doesn't ``wobble'' from one compilation to the
next...), and print.  Note that the ``module'' now contains all the
imported things that we are dealing with, thus including any entities
that we are re-exporting from somewhere else.
\begin{code}
ifaceExportList Nothing{-no iface handle-} _ = return ()

ifaceExportList (Just if_hdl)
		(HsModule _ _ _ _ _ typedecls _ classdecls _ _ _ binds sigs _)
  = let
	name_flag_pairs :: Bag (Name, ExportFlag)
	name_flag_pairs
	  = foldr from_ty
	   (foldr from_cls
	   (foldr from_sig
	   (from_binds binds emptyBag{-init accum-})
	     sigs)
	     classdecls)
	     typedecls

	sorted_pairs = sortLt lexical_lt (bagToList name_flag_pairs)

    in
    hPutStr if_hdl "\n__exports__\n" >>
    hPutStr if_hdl (ppShow 100 (ppAboves (map pp_pair sorted_pairs)))
  where
    from_ty (TyData _ n _ _ _ _ _) acc = maybe_add acc n
    from_ty (TyNew  _ n _ _ _ _ _) acc = maybe_add acc n
    from_ty (TySynonym n _ _ _)	   acc = maybe_add acc n

    from_cls (ClassDecl _ n _ _ _ _ _) acc = maybe_add acc n

    from_sig (Sig n _ _ _) acc = maybe_add acc n

    from_binds bs acc = maybe_add_list acc (collectTopLevelBinders bs)

    --------------
    maybe_add :: Bag (Name, ExportFlag) -> RnName -> Bag (Name, ExportFlag)

    maybe_add acc rn
      | exportFlagOn ef = acc `snocBag` (n, ef)
      | otherwise       = acc
      where
	n  = getName rn
	ef = nameExportFlag n

    --------------
    maybe_add_list acc []     = acc
    maybe_add_list acc (n:ns) = maybe_add (maybe_add_list acc ns) n

    --------------
    lexical_lt (n1,_) (n2,_) = nameOrigName n1 < nameOrigName n2

    --------------
    pp_pair (n, ef)
      = ppBeside (ppr PprInterface (nameOrigName n)) (pp_export ef)
      where
	pp_export ExportAll = ppPStr SLIT("(..)")
	pp_export ExportAbs = ppNil
\end{code}

\begin{code}
ifaceFixities Nothing{-no iface handle-} _ = return ()

ifaceFixities (Just if_hdl) (HsModule _ _ _ _ fixities _ _ _ _ _ _ _ _ _)
  = if null fixities then
	return ()
    else 
	hPutStr if_hdl "\n__fixities__\n" >>
	hPutStr if_hdl (ppShow 100 (ppAboves (map ppSemid fixities)))
\end{code}

\begin{code}
ifaceDecls Nothing{-no iface handle-} _ = return ()

ifaceDecls (Just if_hdl) (vals, tycons, classes, _)
  = let
	exported_classes = filter isExported classes
	exported_tycons  = filter isExported tycons
	exported_vals	 = filter isExported vals

	sorted_classes   = sortLt ltLexical exported_classes
	sorted_tycons	 = sortLt ltLexical exported_tycons
	sorted_vals	 = sortLt ltLexical exported_vals
    in
    ASSERT(not (null exported_classes && null exported_tycons && null exported_vals))

    hPutStr if_hdl "\n__declarations__\n" >>
    hPutStr if_hdl (ppShow 100 (ppAboves [
	ppAboves (map ppSemid sorted_classes),
	ppAboves (map ppSemid sorted_tycons),
	ppAboves (map ppSemid sorted_vals)]))
\end{code}

\begin{code}
ifaceInstances Nothing{-no iface handle-} _ = return ()

ifaceInstances (Just if_hdl) (_, _, _, insts)
  = let
	exported_insts	= filter is_exported_inst (bagToList insts)

	sorted_insts	= sortLt lt_inst exported_insts
    in
    if null exported_insts then
	return ()
    else
	hPutStr if_hdl "\n__instances__\n" >>
	hPutStr if_hdl (ppShow 100 (ppAboves (map pp_inst sorted_insts)))
  where
    is_exported_inst (InstInfo clas _ ty _ _ _ _ _ from_here _ _ _)
      = from_here -- && ...

    -------
    lt_inst (InstInfo clas1 _ ty1 _ _ _ _ _ _ _ _ _)
	    (InstInfo clas2 _ ty2 _ _ _ _ _ _ _ _ _)
      = let
	    tycon1 = fst (getAppTyCon ty1)
	    tycon2 = fst (getAppTyCon ty2)
	in
	case (origName clas1 `cmp` origName clas2) of
	  LT_ -> True
	  GT_ -> False
	  EQ_ -> origName tycon1 < origName tycon2

    -------
    pp_inst (InstInfo clas tvs ty theta _ _ _ _ _ _ _ _)
      = ppBeside (ppPStr SLIT("instance "))
	    (pprType PprInterface (mkSigmaTy tvs theta (mkDictTy clas ty)))
\end{code}

=== ALL OLD BELOW HERE ==============

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

That's why we actually look at the final \tr{StgBindings} that go
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
{- OLD: to the end
mkInterface :: FAST_STRING
	    -> (FAST_STRING -> Bool,  -- is something in export list, explicitly?
		FAST_STRING -> Bool)  -- is a module among the "dotdot" exported modules?
	    -> IdEnv UnfoldingDetails
	    -> FiniteMap TyCon [(Bool, [Maybe Type])]
	    -> ([RenamedFixityDecl],  -- interface info from the typecheck
		[Id],
		CE,
		TCE,
		Bag InstInfo)
	    -> [StgBinding]
	    -> Pretty

mkInterface modname export_list_fns inline_env tycon_specs
	    (fixity_decls, global_ids, ce, tce, inst_infos)
	    stg_binds
  = let
	-- first, gather up the things we want to export:

	exported_tycons  = [ tc | tc <- rngTCE tce,
			   isExported tc,
			   is_exportable_tycon_or_class export_list_fns tc ]
	exported_classes = [  c |  c <- rngCE  ce,
			   isExported  c,
			   is_exportable_tycon_or_class export_list_fns  c ]
	exported_inst_infos = [ i | i <- bagToList inst_infos,
			   is_exported_inst_info export_list_fns i ]
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
	  = filter is_mentionable (bagToList mentioned_classes)
	mentionable_tycons
	  = [ tc | tc <- bagToList mentioned_tycons,
		   is_mentionable tc,
		   not (isPrimTyCon tc) ]

	nondup_mentioned_tycons  = fst (removeDups cmp mentionable_tycons)
	nondup_mentioned_classes = fst (removeDups cmp mentionable_classes)

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
			   then (\ x y -> origName x == origName y)
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
    ppAboves
       [ppPStr SLIT("{-# GHC_PRAGMA INTERFACE VERSION 7 #-}"),
	ppCat [ppPStr SLIT("interface"), ppPStr modname, ppPStr SLIT("where")],

	do_import_decls modname
		sorted_vals sorted_mentioned_classes sorted_mentioned_tycons,
		-- Mustn't give the data constructors to do_import_decls,
		-- because they aren't explicitly imported; their tycon is.

	ppAboves (map do_fixity					fixity_decls),
	ppAboves (map (pprIfaceClass better_id_fn inline_env)	sorted_classes),
	ppAboves (map (do_tycon      tycon_specs)		sorted_tycons),
	ppAboves (map (do_value      better_id_fn inline_env)   sorted_vals),
	ppAboves (map (do_instance   better_id_fn inline_env)   sorted_inst_infos),

	ppChar '\n'
       ]
  where
    any_purely_local tycons classes vals
      =  any bad_tc tycons || any bad_cl classes || any bad_id vals
      where
	bad_cl cl
	  = case (maybePurelyLocalClass cl) of
	      Nothing -> False
	      Just xs -> naughty_trace cl xs

	bad_id id
	  = case (maybePurelyLocalType (idType id)) of
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

We gather up lots of (module, name) pairs for which we might print an
import declaration.  We sort them, for the usual canonicalisation
reasons.  NB: We {\em assume} the lists passed in don't have duplicates in
them!  expect).

All rather horribly turgid (WDP).

\begin{code}
do_import_decls
	:: FAST_STRING
	-> [Id] -> [Class] -> [TyCon]
	-> Pretty

do_import_decls mod_name vals classes tycons
  = let
	-- Conjure up (module, name) pairs for all
	-- the potentially import-decls things:

	vals_names, classes_names, tycons_names :: [(FAST_STRING, FAST_STRING, [Maybe FAST_STRING])]
	vals_names	= map get_val_pair   vals
	classes_names	= map get_class_pair classes
	tycons_names	= map get_tycon_pair tycons

	-- sort the (module, name) pairs and chop
	-- them into per-module groups:

	ie_list = sortLt lt (tycons_names ++ classes_names ++ vals_names)

	per_module_groups = runs same_module ie_list
    in
    ppAboves (map print_a_decl per_module_groups)
  where
    lt, same_module :: (FAST_STRING, FAST_STRING)
		    -> (FAST_STRING, FAST_STRING) -> Bool

    lt (m1, ie1, ie2)
      = case (_CMP_STRING_ m1 m2) of { LT_ -> True; EQ_ -> ie1 < ie2; GT__ -> False }

    same_module (m1, _, _) (m2, _, _) = m1 == m2

    compiling_the_prelude = opt_CompilingPrelude

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
	  ({-OLD:m == pRELUDE_CORE ||-} m == pRELUDE_BUILTIN))
      = ppNil

      | otherwise
      = ppBesides [ppPStr SLIT("import "), ppPStr m, ppLparen,
		   ppIntersperse pp'SP{-'-} (map pp_str [n | (_,n,_) <- ielist]),
		   ppRparen
		  ]
      where
	isnt_tycon_ish :: FAST_STRING -> Bool
	isnt_tycon_ish str = not (isLexCon str)

	grab_non_Nothings :: [[Maybe FAST_STRING]] -> [FAST_STRING]

	grab_non_Nothings rns = catMaybes (concat rns)

	pp_str :: FAST_STRING -> Pretty
	pp_str pstr
	  = if isLexVarSym pstr then ppStr ("("++str++")") else ppPStr pstr
	  where
	    str = _UNPK_ pstr
\end{code}

\begin{code}
get_val_pair   :: Id    -> (FAST_STRING, FAST_STRING)
get_class_pair :: Class -> (FAST_STRING, FAST_STRING)
get_tycon_pair :: TyCon -> (FAST_STRING, FAST_STRING)

get_val_pair id
  = generic_pair id

get_class_pair clas
  = case (generic_pair clas) of { (orig_mod, orig_nm) ->
    let
	nm_to_print = case (getExportFlag clas) of
			ExportAll   -> orig_nm _APPEND_ SLIT("(..)") -- nothing like a good HACK!
			ExportAbs   -> orig_nm
			NotExported -> orig_nm
    in
    (orig_mod, nm_to_print) }

get_tycon_pair tycon
  = case (generic_pair tycon) of { (orig_mod, orig_nm) ->
    let
	nm_to_print = case (getExportFlag tycon) of
			ExportAll   -> orig_nm _APPEND_ SLIT("(..)") -- nothing like a good HACK!
			ExportAbs   -> orig_nm
			NotExported -> orig_nm

	cons	    = tyConDataCons tycon
    in
    (orig_mod, nm_to_print) }

generic_pair thing
  = case (moduleNamePair       thing) of { (orig_mod, orig_nm) ->
    case (getOccName thing) of { occur_name ->
    (orig_mod, orig_nm) }}
\end{code}

%************************************************************************
%*									*
\subsection[fixities-MkIface]{Generating fixity declarations in an interface}
%*									*
%************************************************************************


\begin{code}
do_fixity :: -> RenamedFixityDecl -> Pretty

do_fixity fixity_decl
  = case (isLocallyDefined name, getExportFlag name) of
      (True, ExportAll) -> ppr PprInterface fixity_decl
      _	    	        -> ppNil
  where
     name = get_name fixity_decl
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
do_tycon :: FiniteMap TyCon [(Bool, [Maybe Type])] -> TyCon -> Pretty

do_tycon tycon_specs_map tycon
  = pprTyCon PprInterface tycon tycon_specs
  where
    tycon_specs = map snd (lookupWithDefaultFM tycon_specs_map [] tycon)
\end{code}

%************************************************************************
%*									*
\subsection[values-MkIface]{Generating a value's signature in an interface}
%*									*
%************************************************************************

\begin{code}
do_value :: (Id -> Id)
	 -> IdEnv UnfoldingDetails
	 -> Id
	 -> Pretty

do_value better_id_fn inline_env val
  = let
	sty 	    = PprInterface
	better_val  = better_id_fn val
	name_str    = getOccName better_val -- NB: not orig name!

	id_info	    = getIdInfo better_val

	val_ty	    = let
			 orig_ty  = idType val
			 final_ty = idType better_val
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
	  = if opt_OmitInterfacePragmas
	    || boringIdInfo id_info
	    then ppNil
	    else ppCat [ppPStr SLIT("\t{-# GHC_PRAGMA"),
			ppIdInfo sty better_val True{-yes specs-}
			    better_id_fn inline_env id_info,
			ppPStr SLIT("#-}")]
    in
    ppAbove (ppCat [ppr_non_op name_str,
		    ppPStr SLIT("::"), pprGenType sty val_ty])
	    pp_id_info

-- sadly duplicates Name.pprNonSym (ToDo)

ppr_non_op str
  = if isLexVarSym str -- NOT NEEDED: || isAconop
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
do_instance :: (Id -> Id)
	    -> IdEnv UnfoldingDetails
	    -> InstInfo
	    -> Pretty

do_instance better_id_fn inline_env
    (InstInfo clas tv_tmpls ty inst_decl_theta dfun_theta dfun_id constm_ids _ from_here modname _ _)
  = let
	sty = PprInterface

	better_dfun 	 = better_id_fn dfun_id
	better_dfun_info = getIdInfo better_dfun
	better_constms	 = map better_id_fn constm_ids

	class_op_strs = map classOpString (classOps clas)

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
    if opt_OmitInterfacePragmas
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
is_exportable_tycon_or_class export_list_fns tc
  = if not (fromPreludeCore tc) then
	True
    else
	in_export_list_or_among_dotdot_modules
	    opt_CompilingPrelude -- ignore M.. stuff if compiling prelude
	    export_list_fns tc

in_export_list_or_among_dotdot_modules ignore_Mdotdots (in_export_list, among_dotdot_modules) tc
  = if in_export_list (getOccName tc) then
	True
    else
--	pprTrace "in_export:" (ppAbove (ppr PprDebug ignore_Mdotdots) (ppPStr (getOccName  tc))) (
    if ignore_Mdotdots then
	False
    else
	any among_dotdot_modules (getInformingModules tc)
--  )

is_mentionable tc
  = not (from_PreludeCore_or_Builtin tc) || opt_CompilingPrelude
  where
    from_PreludeCore_or_Builtin thing
      = let
	    mod_name = fst (moduleNamePair thing)
	in
	mod_name == pRELUDE_CORE || mod_name == pRELUDE_BUILTIN

is_exported_inst_info export_list_fns
	(InstInfo clas _ ty _ _ _ _ _ from_here _ _ _)
  = let
    	seems_exported = instanceIsExported clas ty from_here
	(tycon, _, _) = getAppTyCon ty
    in
    if (opt_OmitReexportedInstances && not from_here) then
	False -- Flag says to violate Haskell rules, blatantly

    else if not opt_CompilingPrelude
	 || not (isFunTyCon tycon || fromPreludeCore tycon)
	 || not (fromPreludeCore clas) then
	seems_exported -- take what we got

    else -- compiling Prelude & tycon/class are Prelude things...
	from_here
	|| in_export_list_or_among_dotdot_modules True{-ignore M..s-} export_list_fns clas
	|| in_export_list_or_among_dotdot_modules True{-ignore M..s-} export_list_fns tycon
\end{code}

\begin{code}
lt_lexical_inst_info (InstInfo _ _ _ _ _ dfun1 _ _ _ _ _ _) (InstInfo _ _ _ _ _ dfun2 _ _ _ _ _ _)
  = ltLexical dfun1 dfun2
\end{code}

\begin{code}
getMentionedTyConsAndClassesFromInstInfo (InstInfo clas _ ty _ dfun_theta _ _ _ _ _ _ _)
  = case (getMentionedTyConsAndClassesFromType ty) of { (ts, cs) ->
    case [ c | (c, _) <- dfun_theta ]  	    	      of { theta_classes ->
    (ts, (cs `unionBags` listToBag theta_classes) `snocBag` clas)
    }}
OLD from the beginning -}
\end{code}
