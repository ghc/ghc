%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Rename-three]{Third of the renaming passes}

The business of this pass is to:
\begin{itemize}
\item	find all the things declared at top level,
\item	assign uniques to them
\item return an association list mapping their @ProtoName@s to
	freshly-minted @Names@ for them.
\end{itemize}

No attempt is made to discover whether the same thing is declared
twice: that is up to the caller to sort out.

\begin{code}
#include "HsVersions.h"

module Rename3 (
	rnModule3,
	initRn3, Rn3M(..),  -- re-exported from monad

	-- for completeness
	Module, Bag, ProtoNamePat(..), InPat, Maybe, Name,
	ExportFlag, PprStyle, Pretty(..), PrettyRep, ProtoName,
	PreludeNameFun(..), PreludeNameFuns(..), SplitUniqSupply
    ) where

import AbsSyn
import Bag		-- lots of stuff
import Errors		( dupNamesErr, dupPreludeNameErr,
			  badExportNameErr, badImportNameErr,
			  Error(..)
			)
import HsCore		-- ****** NEED TO SEE CONSTRUCTORS ******
import HsPragmas	-- ****** NEED TO SEE CONSTRUCTORS ******
import FiniteMap
import Maybes		( Maybe(..) )
import Name		( Name(..) )
import NameTypes	( fromPrelude, FullName )
import ProtoName
import RenameAuxFuns	( mkGlobalNameFun,
			  GlobalNameFuns(..), GlobalNameFun(..),
			  PreludeNameFuns(..), PreludeNameFun(..)
			)
import RenameMonad3
import SrcLoc		( mkUnknownSrcLoc, SrcLoc )
import Util
\end{code}

*********************************************************
*							*
\subsection{Type declarations}
*							*
*********************************************************

\begin{code}
type BagAssoc 	    = Bag (ProtoName, Name)	-- Bag version
type NameSpaceAssoc = [(ProtoName, Name)]	-- List version
\end{code}


*********************************************************
*							*
\subsection{Main function: @rnModule3@}
*							*
*********************************************************

\begin{code}
rnModule3 :: PreludeNameFuns
	  -> [FAST_STRING]	-- list of imported module names
	  -> ProtoNameModule
	  -> Rn3M ( NameSpaceAssoc, NameSpaceAssoc,
		    GlobalNameFun,  GlobalNameFun,
		    Bag Error )

rnModule3 pnfs@(val_pnf, tc_pnf) imported_mod_names
	  (Module mod_name exports imports _ ty_decls _ class_decls
	    inst_decls _ _ binds sigs _)

  = putInfoDownM3 {- ???pnfs -} mod_name exports (

    doTyDecls3 ty_decls		`thenRn3` \ (constrs, tycons) ->
    doClassDecls3 class_decls	`thenRn3` \ (ops, classes) ->
    doBinds3 binds		`thenRn3` \ val_binds ->
    doIntSigs3 sigs		`thenRn3` \ val_sigs ->

    let val_namespace	= constrs `unionBags` ops `unionBags` val_binds
				  `unionBags` val_sigs
	tc_namespace	= tycons `unionBags` classes

	(var_alist, var_dup_errs) = deal_with_dups "variable" val_pnf (bagToList val_namespace)
	(tc_alist, tc_dup_errs)	  = deal_with_dups "type or class" tc_pnf (bagToList tc_namespace)
	v_gnf  = mkGlobalNameFun mod_name val_pnf var_alist
    	tc_gnf = mkGlobalNameFun mod_name tc_pnf  tc_alist
    in

    verifyExports v_gnf tc_gnf (mod_name : imported_mod_names) exports
					`thenRn3` \ export_errs ->
    verifyImports v_gnf tc_gnf imports	`thenRn3` \ import_errs ->

    returnRn3 ( var_alist, tc_alist,
		v_gnf, tc_gnf,
		var_dup_errs `unionBags` tc_dup_errs `unionBags`
		export_errs  `unionBags` import_errs
    ))
  where
    deal_with_dups :: String -> PreludeNameFun -> NameSpaceAssoc
		   -> (NameSpaceAssoc, Bag Error)

    deal_with_dups kind_str pnf alist
      = (goodies,
	 listToBag (map mk_dup_err dup_lists) `unionBags`
	 listToBag (map mk_prel_dup_err prel_dups) 
	)
      where
	goodies   :: [(ProtoName,Name)]		--NameSpaceAssoc
	dup_lists :: [[(ProtoName, Name)]]

	-- Find all the names which are defined twice.
	-- By "name" here, we mean "string"; that is, we are looking
	-- for places where two strings are bound to different Names
	-- in the top-level scope of this module.

	(singles, dup_lists) = removeDups cmp alist
	-- We want to compare their *local* names; the removeDups thing
	-- is checking for whether two objects have the same local name.
	cmp (a, _) (b, _) = cmpByLocalName a b

	-- Anything in alist with a Unk name is defined right here in
	-- this module; hence, it should not be a prelude name.  We
	-- need to check this separately, because the prelude is
	-- imported only implicitly, via the PrelNameFuns argument

	(goodies, prel_dups) = if fromPrelude mod_name then
				 (singles, [])	-- Compiling the prelude, so ignore this check
			       else	
				 partition local_def_of_prelude_thing singles

	local_def_of_prelude_thing (Unk s, _)
	  = case pnf s of 
	      Just _  -> False		-- Eek!  It's a prelude name
	      Nothing -> True		-- It isn't; all is ok
	local_def_of_prelude_thing other = True

	mk_dup_err :: [(ProtoName, Name)] -> Error
	mk_dup_err dups_of_name
	  = let
		dup_pnames_w_src_loc = [ (pn, getSrcLoc name) | (pn,name) <- dups_of_name ]
	    in
	    dupNamesErr kind_str dup_pnames_w_src_loc

	-- This module defines a prelude thing
	mk_prel_dup_err :: (ProtoName, Name) -> Error
	mk_prel_dup_err (pn, name)
	  = dupPreludeNameErr kind_str (pn, getSrcLoc name)
\end{code}

*********************************************************
*							*
\subsection{Type and class declarations}
*							*
*********************************************************

\begin{code}
doTyDecls3 :: [ProtoNameTyDecl] -> Rn3M (BagAssoc, BagAssoc)

doTyDecls3 [] = returnRn3 (emptyBag, emptyBag)

doTyDecls3 (tyd:tyds)
  = andRn3 combiner (do_decl tyd) (doTyDecls3 tyds)
  where
    combiner (cons1, tycons1) (cons2, tycons2)
      = (cons1 `unionBags` cons2, tycons1 `unionBags` tycons2)

    do_decl (TyData context tycon tyvars condecls deriv pragmas src_loc)
      = newFullNameM3 tycon src_loc True{-tycon-ish-} Nothing
					`thenRn3` \ (uniq, tycon_name) ->
    	let
	    exp_flag = getExportFlag tycon_name
		-- we want to force all data cons to have the very
		-- same export flag as their type constructor
	in
	doConDecls3 False{-not invisibles-} exp_flag condecls `thenRn3` \ data_cons ->
	do_data_pragmas exp_flag pragmas		      `thenRn3` \ pragma_data_cons ->
	returnRn3 (data_cons `unionBags` pragma_data_cons,
		   unitBag (tycon, OtherTyCon uniq tycon_name (length tyvars)
					True -- indicates @data@ tycon
					[ c | (_,c) <- bagToList data_cons ]))
			

    do_decl (TySynonym tycon tyvars monoty pragmas src_loc)
      = newFullNameM3 tycon src_loc True{-tycon-ish-} Nothing
					`thenRn3` \ (uniq, tycon_name) ->
	returnRn3 (emptyBag,
		   unitBag (tycon, OtherTyCon uniq tycon_name (length tyvars) False bottom))
					-- False indicates @type@ tycon
      where
	bottom = panic "do_decl: data cons on synonym?"

    do_data_pragmas exp_flag (DataPragmas con_decls specs)
      = doConDecls3 True{-invisibles-} exp_flag con_decls
\end{code}

\begin{code}
doConDecls3 :: Bool		    -- True <=> mk invisible FullNames
	    -> ExportFlag	    -- Export flag of the TyCon; we want
				    -- to force its use.
	    -> [ProtoNameConDecl]
	    -> Rn3M BagAssoc

doConDecls3 _ _ [] = returnRn3 emptyBag

doConDecls3 want_invisibles exp_flag (cd:cds)
  = andRn3 unionBags (do_decl cd) (doConDecls3 want_invisibles exp_flag cds)
  where
    mk_name = if want_invisibles then newInvisibleNameM3 else newFullNameM3

    do_decl (ConDecl con tys src_loc)
      = mk_name con src_loc True{-tycon-ish-} (Just exp_flag) `thenRn3` \ (uniq, con_name) ->
	returnRn3 (unitBag (con, OtherTopId uniq con_name))
\end{code}


@doClassDecls3@ uses the `name function' to map local class names into
original names, calling @doClassOps3@ to do the same for the
class operations. @doClassDecls3@ is used to process module
class declarations.

\begin{code}
doClassDecls3 :: [ProtoNameClassDecl] -> Rn3M (BagAssoc, BagAssoc)

doClassDecls3 [] = returnRn3 (emptyBag, emptyBag)

doClassDecls3 (cd:cds)
  = andRn3 combiner (do_decl cd) (doClassDecls3 cds)
  where
    combiner (ops1, classes1) (ops2, classes2)
      = (ops1 `unionBags` ops2, classes1 `unionBags` classes2)

    do_decl (ClassDecl context cname@(Prel c) tyvar sigs defaults pragmas src_loc)
      = doClassOps3 c 1 sigs	`thenRn3` \ (_, ops) ->
	returnRn3 (ops, unitBag (cname, c))

    do_decl (ClassDecl context cname tyvar sigs defaults pragmas src_loc)
      = newFullNameM3 cname src_loc True{-tycon-ish-} Nothing
					`thenRn3` \ (uniq, class_name) ->
	fixRn3 ( \ ~(clas_ops,_) ->
	    let
		class_Name = OtherClass uniq class_name
					[ o | (_,o) <- bagToList clas_ops ]
	    in
	    doClassOps3 class_Name 1 sigs   `thenRn3` \ (_, ops) ->
	    returnRn3 (ops, class_Name)
	)				`thenRn3` \ (ops, class_Name) ->

	returnRn3 (ops, unitBag (cname, class_Name))
\end{code}

We stitch on a class-op tag to each class operation.  They are guaranteed
to be done in left-to-right order.

\begin{code}
doClassOps3 :: Name{-class-} -> Int -> [ProtoNameSig] -> Rn3M (Int, BagAssoc)

doClassOps3 clas tag [] = returnRn3 (tag, emptyBag)

doClassOps3 clas tag (sig:rest)
  = do_op		  sig	`thenRn3` \ (tag1, bag1) ->
    doClassOps3 clas tag1 rest	`thenRn3` \ (tagr, bagr) ->
    returnRn3 (tagr, bag1 `unionBags` bagr)
  where
    do_op (ClassOpSig op ty pragma src_loc)
      = newFullNameM3 op src_loc False{-not tyconish-} Nothing `thenRn3` \ (uniq, _) ->
	let
	    op_name = ClassOpName uniq clas (get_str op) tag
	in
	returnRn3 (tag+1, unitBag (op, op_name))
      where
	-- A rather yukky function to get the original name out of a class operation.
	get_str :: ProtoName -> FAST_STRING
	get_str (Unk s)       = s
	get_str (Imp _ d _ _) = d
\end{code}

Remember, interface signatures don't have user-pragmas, etc., in them.
\begin{code}
doIntSigs3 :: [ProtoNameSig] -> Rn3M BagAssoc

doIntSigs3 [] = returnRn3 emptyBag

doIntSigs3 (s:ss)
  = andRn3 unionBags (do_sig s) (doIntSigs3 ss)
  where
    do_sig (Sig v ty pragma src_loc)
      = newFullNameM3 v src_loc False{-distinctly untycon-ish-} Nothing
					     `thenRn3` \ (uniq, v_fname) ->
	returnRn3 (unitBag (v, OtherTopId uniq v_fname))
\end{code}

*********************************************************
*							*
\subsection{Bindings}
*							*
*********************************************************

\begin{code}
doBinds3 :: ProtoNameBinds -> Rn3M BagAssoc

doBinds3 EmptyBinds = returnRn3 emptyBag

doBinds3 (ThenBinds binds1 binds2)
  = andRn3 unionBags (doBinds3 binds1) (doBinds3 binds2)

doBinds3 (SingleBind bind)    = doBind3 bind

doBinds3 (BindWith bind sigs) = doBind3 bind
\end{code}

\begin{code}
doBind3 :: ProtoNameBind -> Rn3M BagAssoc
doBind3 EmptyBind          = returnRn3 emptyBag
doBind3 (NonRecBind mbind) = doMBinds3 mbind
doBind3 (RecBind mbind)    = doMBinds3 mbind

doMBinds3 :: ProtoNameMonoBinds -> Rn3M BagAssoc

doMBinds3 EmptyMonoBinds 			 = returnRn3 emptyBag
doMBinds3 (PatMonoBind pat grhss_and_binds locn) = doPat3 locn pat
doMBinds3 (FunMonoBind p_name _ locn) 		 = doTopLevName locn p_name

doMBinds3 (AndMonoBinds mbinds1 mbinds2)
  = andRn3 unionBags (doMBinds3 mbinds1) (doMBinds3 mbinds2)
\end{code}

Fold over a list of patterns:
\begin{code}
doPats3 locn [] = returnRn3 emptyBag
doPats3 locn (pat:pats)
  = andRn3 unionBags (doPat3 locn pat) (doPats3 locn pats)
\end{code}

\begin{code}
doPat3 :: SrcLoc -> ProtoNamePat -> Rn3M BagAssoc

doPat3 locn WildPatIn       	= returnRn3 emptyBag
doPat3 locn (LitPatIn _) 	= returnRn3 emptyBag
doPat3 locn (LazyPatIn pat) 	= doPat3 locn pat
doPat3 locn (VarPatIn n) 	= doTopLevName locn n
doPat3 locn (ListPatIn pats)	= doPats3 locn pats
doPat3 locn (TuplePatIn pats)	= doPats3 locn pats
doPat3 locn (NPlusKPatIn n _)	= doTopLevName locn n

doPat3 locn (AsPatIn p_name pat)
  = andRn3 unionBags (doTopLevName locn p_name) (doPat3 locn pat)

doPat3 locn (ConPatIn name pats) = doPats3 locn pats

doPat3 locn (ConOpPatIn pat1 name pat2)
  = andRn3 unionBags (doPat3 locn pat1) (doPat3 locn pat2)

#ifdef DPH
doPat3 locn (ProcessorPatIn pats pat)
  = andRn3 unionBags (doPats3 locn pats) (doPat3 locn pat)
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
doTopLevName :: SrcLoc -> ProtoName -> Rn3M BagAssoc

doTopLevName locn pn
  = newFullNameM3 pn locn False{-un-tycon-ish-}	Nothing `thenRn3` \ (uniq, name) ->
    returnRn3 (unitBag (pn, OtherTopId uniq name))
\end{code}

Have to check that export/imports lists aren't too drug-crazed.

\begin{code}
verifyExports :: GlobalNameFun -> GlobalNameFun
	      -> [FAST_STRING]	-- module names that might appear
				-- in an export list; includes the
				-- name of this module
	      -> [IE]		-- export list
	      -> Rn3M (Bag Error)

verifyExports v_gnf tc_gnf imported_mod_names exports
  = mapRn3 verify exports	`thenRn3` \ errs ->
    chk_exp_dups  exports	`thenRn3` \ dup_errs ->
    returnRn3 (unionManyBags (errs ++ dup_errs))
  where
    present nf str = nf (Unk str)

    ok	    	   = returnRn3 emptyBag
    naughty nm msg = returnRn3 (unitBag (badExportNameErr (_UNPK_ nm) msg))
    undef_name nm  = naughty nm "is not defined."
    dup_name (nm:_)= naughty nm "occurs more than once."

    ----------------
    chk_exp_dups exports
      = let
	    export_strs = [ nm | (nm, _) <- fst (getRawIEStrings exports) ]
	    (_, dup_lists) = removeDups _CMP_STRING_ export_strs
	in
	mapRn3 dup_name dup_lists

    ---------------- the more serious checking
    verify (IEVar v)
      = case (present v_gnf v) of { Nothing -> undef_name v; _ -> ok }

    verify (IEModuleContents mod)
      = if not (mod `is_elem` imported_mod_names) then undef_name mod else ok
      where
	is_elem = isIn "verifyExports"

    verify (IEThingAbs tc) 
      = case (present tc_gnf tc) of
	  Nothing -> undef_name tc
	  Just nm -> case nm of
		       PreludeTyCon _ _ _ False{-syn-}
			 -> naughty tc "must be exported with a `(..)' -- it's a Prelude synonym."
		       OtherTyCon _ _ _ False{-syn-} _
			 -> naughty tc "must be exported with a `(..)' -- it's a synonym."

		       PreludeClass _ _
		         -> naughty tc "cannot be exported \"abstractly\" (it's a Prelude class)."
		       OtherClass _ _ _
		         -> naughty tc "cannot be exported \"abstractly\" (it's a class)."
		       _ -> ok

    verify (IEThingAll tc)
      = case (present tc_gnf tc) of
	  Nothing -> undef_name tc
	  Just nm -> case nm of
		       OtherTyCon _ _ _ True{-data-} [{-no cons-}]
			 -> naughty tc "can't be exported with a `(..)' -- it was imported abstractly."
		       _ -> ok

    verify (IEConWithCons tc cs)
      = case (present tc_gnf tc) of
	  Nothing -> undef_name tc
	  Just nm -> mapRn3 verify (map IEVar cs) `thenRn3` \ errs ->
		     returnRn3 (unionManyBags errs)
		     -- ToDo: turgid checking which we don't care about (WDP 94/10)

    verify (IEClsWithOps c ms)
      = case (present tc_gnf c) of
	  Nothing -> undef_name c
	  Just  _ -> mapRn3 verify (map IEVar ms) `thenRn3` \ errs ->
		     returnRn3 (unionManyBags errs)
		     -- ToDo: turgid checking which we don't care about (WDP 94/10)
\end{code}

Note: we're not too particular about whether something mentioned in an
import list is in {\em that} interface... (ToDo? Probably not.)

\begin{code}
verifyImports :: GlobalNameFun -> GlobalNameFun
	      -> [ProtoNameImportedInterface]
	      -> Rn3M (Bag Error)

verifyImports v_gnf tc_gnf imports
  = mapRn3 chk_one (map collect imports) `thenRn3` \ errs ->
    returnRn3 (unionManyBags errs)
  where
    -- collect: name/locn, import list, renamings list

    collect (ImportAll     iff renamings)
      = (iface iff, [],       [],       renamings)
    collect (ImportSome    iff imp_list renamings)
      = (iface iff, imp_list, [],   	 renamings)
    collect (ImportButHide iff hide_list renamings)
      = (iface iff, [],        hide_list, renamings)

    ------------
    iface (MkInterface name _ _ _ _ _ _ locn) = (name, locn)

    ------------
    chk_one :: ((FAST_STRING, SrcLoc), [IE], [IE], [Renaming])
	    -> Rn3M (Bag Error)

    chk_one ((mod_name, locn), import_list, hide_list, renamings)
      = mapRn3 verify import_list   `thenRn3` \ errs1 ->
	chk_imp_dups  import_list   `thenRn3` \ dup_errs ->
	-- ToDo: we could check the hiding list more carefully
	chk_imp_dups  hide_list	    `thenRn3` \ dup_errs2 ->
	mapRn3 chk_rn renamings	    `thenRn3` \ errs2 ->
    	returnRn3 (unionManyBags (errs1 ++ dup_errs ++ dup_errs2 ++ errs2))
      where
	present nf str = nf (Unk (rename_it str))

	rename_it str
	  = case [ too | (MkRenaming from too) <- renamings, str == from ] of
	      []    -> str
	      (x:_) -> x

	ok	          = returnRn3 emptyBag
	naughty nm msg    = returnRn3 (unitBag (badImportNameErr (_UNPK_ mod_name) (_UNPK_ nm) msg locn))
	undef_name nm     = naughty nm "is not defined."
	undef_rn_name n r = naughty n  ("is not defined (renamed to `"++ _UNPK_ r ++"').")
	dup_name (nm:_)   = naughty nm "occurs more than once."

	----------------
	chk_imp_dups imports
	  = let
		import_strs = [ nm | (nm, _) <- fst (getRawIEStrings imports) ]
		(_, dup_lists) = removeDups _CMP_STRING_ import_strs
	    in
	    mapRn3 dup_name dup_lists

	----------------
	chk_rn (MkRenaming from too)	    -- Note: "present" will rename
	  = case (present v_gnf from) of    -- the "from" to the "too"...
	      Just  _ -> ok
	      Nothing -> case (present tc_gnf from) of
			   Just  _ -> ok
			   Nothing -> undef_rn_name from too

	----------------
	verify (IEVar v)
	  = case (present v_gnf v) of { Nothing -> undef_name v; _ -> ok }

	verify (IEThingAbs tc) 
	  = case (present tc_gnf tc) of
	      Nothing -> undef_name tc
	      Just nm -> case nm of
			   PreludeTyCon _ _ _ False{-syn-}
			     -> naughty tc "must be imported with a `(..)' -- it's a Prelude synonym."
			   OtherTyCon _ _ _ False{-syn-} _
			     -> naughty tc "must be imported with a `(..)' -- it's a synonym."
			   PreludeClass _ _
			     -> naughty tc "cannot be imported \"abstractly\" (it's a Prelude class)."
			   OtherClass _ _ _
			     -> naughty tc "cannot be imported \"abstractly\" (it's a class)."
			   _ -> ok

	verify (IEThingAll tc)
	  = case (present tc_gnf tc) of
	      Nothing -> undef_name tc
	      Just nm -> case nm of
			   OtherTyCon _ _ _ True{-data-} [{-no cons-}]
			     -> naughty tc "can't be imported with a `(..)' -- the interface says it's abstract."
			   _ -> ok

	verify (IEConWithCons tc cs)
	  = case (present tc_gnf tc) of
	      Nothing -> undef_name tc
	      Just nm -> mapRn3 verify (map IEVar cs) `thenRn3` \ errs ->
			 returnRn3 (unionManyBags errs)
			 -- One could add a great wad of tedious checking
			 -- here, but I am too lazy to do so.  WDP 94/10

	verify (IEClsWithOps c ms)
	  = case (present tc_gnf c) of
	      Nothing -> undef_name c
	      Just  _ -> mapRn3 verify (map IEVar ms) `thenRn3` \ errs ->
			 returnRn3 (unionManyBags errs)
			 -- Ditto about tedious checking.  WDP 94/10
\end{code}
