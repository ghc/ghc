%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[MkIface]{Print an interface for a module}

\begin{code}
#include "HsVersions.h"

module MkIface (
	startIface, endIface,
	ifaceUsages,
	ifaceVersions,
	ifaceExportList,
	ifaceFixities,
	ifaceInstanceModules,
	ifaceDecls,
	ifaceInstances,
	ifacePragmas
    ) where

IMP_Ubiq(){-uitous-}
IMPORT_1_3(IO(Handle,hPutStr,openFile,hClose,IOMode(..)))

import Bag		( bagToList )
import Class		( GenClass(..){-instance NamedThing-}, GenClassOp(..) )
import CmdLineOpts	( opt_ProduceHi )
import FieldLabel	( FieldLabel{-instance NamedThing-} )
import FiniteMap	( emptyFM, addToFM, lookupFM, fmToList, eltsFM, FiniteMap )
import HsSyn
import Id		( idType, dataConRawArgTys, dataConFieldLabels,
			  dataConStrictMarks, StrictnessMark(..),
			  GenId{-instance NamedThing/Outputable-}
			)
import Maybes		( maybeToBool )
import Name		( origName, nameOf, moduleOf,
			  exportFlagOn, nameExportFlag, ExportFlag(..),
			  isLexSym, isLocallyDefined, isWiredInName,
			  RdrName(..){-instance Outputable-},
			  OrigName(..){-instance Ord-},
			  Name{-instance NamedThing-}
			)
import ParseUtils	( UsagesMap(..), VersionsMap(..) )
import PprEnv		-- not sure how much...
import PprStyle		( PprStyle(..) )
import PprType		-- most of it (??)
--import PrelMods	( modulesWithBuiltins )
import PrelInfo		( builtinNameInfo )
import Pretty		( prettyToUn )
import Unpretty		-- ditto
import RnHsSyn		( isRnConstr, SYN_IE(RenamedHsModule), RnName{-instance NamedThing-} )
import TcModule		( SYN_IE(TcIfaceInfo) )
import TcInstUtil	( InstInfo(..) )
import TyCon		( TyCon(..){-instance NamedThing-}, NewOrData(..) )
import Type		( mkSigmaTy, mkDictTy, getAppTyCon, splitForAllTy )
import Util		( sortLt, removeDups, zipWithEqual, zipWith3Equal, assertPanic, panic{-ToDo:rm-}, pprTrace{-ToDo:rm-} )

uppSemid   x = uppBeside (prettyToUn (ppr PprInterface x)) uppSemi -- micro util
ppr_ty	  ty = prettyToUn (pprType PprInterface ty)
ppr_tyvar tv = prettyToUn (ppr PprInterface tv)
ppr_name   n
  = case (origName "ppr_name" n) of { OrigName m s ->
    uppBesides [uppPStr m, uppChar '.', uppPStr s] }
\end{code}

We have a function @startIface@ to open the output file and put
(something like) ``interface Foo'' in it.  It gives back a handle
for subsequent additions to the interface file.

We then have one-function-per-block-of-interface-stuff, e.g.,
@ifaceExportList@ produces the @__exports__@ section; it appends
to the handle provided by @startIface@.

\begin{code}
startIface  :: Module
	    -> IO (Maybe Handle) -- Nothing <=> don't do an interface
endIface    :: Maybe Handle -> IO ()
ifaceUsages
	    :: Maybe Handle
	    -> UsagesMap
	    -> IO ()
ifaceVersions
	    :: Maybe Handle
	    -> VersionsMap
	    -> IO ()
ifaceExportList
	    :: Maybe Handle
	    -> (Name -> ExportFlag)
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
ifacePragmas
	    :: Maybe Handle
	    -> IO ()
ifacePragmas = panic "ifacePragmas" -- stub
\end{code}

\begin{code}
startIface mod
  = case opt_ProduceHi of
      Nothing -> return Nothing -- not producing any .hi file
      Just fn ->
	openFile fn WriteMode	>>= \ if_hdl ->
	hPutStr if_hdl ("interface "++ _UNPK_ mod) >>
	return (Just if_hdl)

endIface Nothing	= return ()
endIface (Just if_hdl)	= hPutStr if_hdl "\n" >> hClose if_hdl
\end{code}

\begin{code}
ifaceUsages Nothing{-no iface handle-} _ = return ()

ifaceUsages (Just if_hdl) usages
  | null usages_list
  = return ()
  | otherwise
  = hPutStr if_hdl "\n__usages__\n"   >>
    hPutStr if_hdl (uppShow 0 (uppAboves (map upp_uses usages_list)))
  where
    usages_list = fmToList usages -- NO: filter has_no_builtins (...)

--  has_no_builtins (m, _)
--    = m `notElem` modulesWithBuiltins
--    -- Don't *have* to do this; save gratuitous spillage in
--    -- every interface.  Could be flag-controlled...

    upp_uses (m, (mv, versions))
      = uppBesides [uppPStr m, uppSP, uppInt mv, uppPStr SLIT(" :: "),
	       upp_versions (fmToList versions), uppSemi]

    upp_versions nvs
      = uppIntersperse uppSP [ uppCat [uppPStr n, uppInt v] | (n,v) <- nvs ]
\end{code}

\begin{code}
ifaceVersions Nothing{-no iface handle-} _ = return ()

ifaceVersions (Just if_hdl) version_info
  | null version_list
  = return ()
  | otherwise
  = hPutStr if_hdl "\n__versions__\n"	>>
    hPutStr if_hdl (uppShow 0 (upp_versions version_list))
    -- NB: when compiling Prelude.hs, this will spew out
    -- stuff for [], (), (,), etc. [i.e., builtins], which
    -- we'd rather it didn't.  The version-mangling in
    -- the driver will ignore them.
  where
    version_list = fmToList version_info

    upp_versions nvs
      = uppAboves [ uppPStr n | (n,v) <- nvs ]
\end{code}

\begin{code}
ifaceInstanceModules Nothing{-no iface handle-} _ = return ()
ifaceInstanceModules (Just _)		       [] = return ()

ifaceInstanceModules (Just if_hdl) imods
  = hPutStr if_hdl "\n__instance_modules__\n" >>
    hPutStr if_hdl (uppShow 0 (uppCat (map uppPStr imods)))
\end{code}

Export list: grab the Names of things that are marked Exported, sort
(so the interface file doesn't ``wobble'' from one compilation to the
next...), and print.  Note that the ``module'' now contains all the
imported things that we are dealing with, thus including any entities
that we are re-exporting from somewhere else.
\begin{code}
ifaceExportList Nothing{-no iface handle-} _ _ = return ()

ifaceExportList (Just if_hdl)
		export_fn -- sadly, just the HsModule isn't enough,
			  -- because it will have no record of exported
			  -- wired-in names.
		(HsModule _ _ _ _ _ typedecls _ classdecls _ _ _ binds sigs _)
  = let
	(vals_wired, tcs_wired)
	  = case builtinNameInfo of { ((vals_fm,tcs_fm), _, _) ->
	    (eltsFM vals_fm, eltsFM tcs_fm) }

	name_flag_pairs :: FiniteMap OrigName ExportFlag
	name_flag_pairs
	  = foldr (from_wired True{-val-ish-})
	   (foldr (from_wired False{-tycon-ish-})
	   (foldr from_ty
	   (foldr from_cls
	   (foldr from_sig
	   (from_binds binds emptyFM{-init accum-})
	     sigs)
	     classdecls)
	     typedecls)
	     tcs_wired)
	     vals_wired

	sorted_pairs = sortLt lexical_lt (fmToList name_flag_pairs)

    in
    hPutStr if_hdl "\n__exports__\n" >>
    hPutStr if_hdl (uppShow 0 (uppAboves (map upp_pair sorted_pairs)))
  where
    from_ty (TyData _ n _ _ _ _ _) acc = maybe_add acc n
    from_ty (TyNew  _ n _ _ _ _ _) acc = maybe_add acc n
    from_ty (TySynonym n _ _ _)	   acc = maybe_add acc n

    from_cls (ClassDecl _ n _ _ _ _ _) acc = maybe_add acc n

    from_sig (Sig n _ _ _) acc = maybe_add acc n

    from_binds bs acc = maybe_add_list acc (collectTopLevelBinders bs)

    --------------
    from_wired is_val_ish rn acc
      | on_in_acc	= acc -- if already in acc (presumably from real decl),
			      -- don't take the dubious export flag from the
			      -- wired-in chappy
      | is_val_ish && isRnConstr rn
			= acc -- these things don't cause export-ery
      | exportFlagOn ef = addToFM acc on ef
      | otherwise       = acc
      where
	n  = getName rn
	ef = export_fn n
	on = origName "from_wired" n
	(OrigName _ str) = on
	on_in_acc = maybeToBool (lookupFM acc on)

    --------------
    maybe_add :: FiniteMap OrigName ExportFlag -> RnName -> FiniteMap OrigName ExportFlag

    maybe_add acc rn
      | on_in_acc	= trace "maybe_add?" acc -- surprising!
      | exportFlagOn ef = addToFM acc on ef
      | otherwise       = acc
      where
	ef = nameExportFlag n
	n  = getName rn
	on = origName "maybe_add" n
	on_in_acc = maybeToBool (lookupFM acc on)

    --------------
    maybe_add_list acc []     = acc
    maybe_add_list acc (n:ns) = maybe_add (maybe_add_list acc ns) n

    --------------
    lexical_lt (n1,_) (n2,_) = n1 < n2

    --------------
    upp_pair (OrigName m n, ef)
      = uppBesides [uppPStr m, uppSP, uppPStr n, uppSP, upp_export ef]
      where
	upp_export ExportAll = uppPStr SLIT("(..)")
	upp_export ExportAbs = uppNil
\end{code}

\begin{code}
ifaceFixities Nothing{-no iface handle-} _ = return ()

ifaceFixities (Just if_hdl) (HsModule _ _ _ _ fixities _ _ _ _ _ _ _ _ _)
  = let
	pp_fixities = foldr go [] fixities
    in
    if null pp_fixities then
	return ()
    else 
	hPutStr if_hdl "\n__fixities__\n" >>
	hPutStr if_hdl (uppShow 0 (uppAboves pp_fixities))
  where
    go (InfixL v i) acc = (if isLocallyDefined v then (:) (print_fix "l" i v) else id) acc
    go (InfixR v i) acc = (if isLocallyDefined v then (:) (print_fix "r" i v) else id) acc
    go (InfixN v i) acc = (if isLocallyDefined v then (:) (print_fix ""  i v) else id) acc

    print_fix suff prec var
      = uppBesides [uppPStr SLIT("infix"), uppStr suff, uppSP, uppInt prec, uppSP, ppr_name var, uppSemi]
\end{code}

\begin{code}
non_wired x = not (isWiredInName (getName x)) --ToDo:move?

ifaceDecls Nothing{-no iface handle-} _ = return ()

ifaceDecls (Just if_hdl) (vals, tycons, classes, _)
  = ASSERT(all isLocallyDefined vals)
    ASSERT(all isLocallyDefined tycons)
    ASSERT(all isLocallyDefined classes)
    let
	nonwired_classes = filter non_wired classes
	nonwired_tycons  = filter non_wired tycons
	nonwired_vals    = filter non_wired vals

	lt_lexical a b = origName "lt_lexical" a < origName "lt_lexical" b

	sorted_classes = sortLt lt_lexical nonwired_classes
	sorted_tycons  = sortLt lt_lexical nonwired_tycons
	sorted_vals    = sortLt lt_lexical nonwired_vals
    in
    if (null sorted_classes && null sorted_tycons && null sorted_vals) then
	--  You could have a module with just (re-)exports/instances in it
	return ()
    else
    hPutStr if_hdl "\n__declarations__\n" >>
    hPutStr if_hdl (uppShow 0 (uppAboves [
	uppAboves (map ppr_class sorted_classes),
	uppAboves (map ppr_tycon sorted_tycons),
	uppAboves [ppr_val v (idType v) | v <- sorted_vals]]))
\end{code}

\begin{code}
ifaceInstances Nothing{-no iface handle-} _ = return ()

ifaceInstances (Just if_hdl) (_, _, _, insts)
  = let
	togo_insts	= filter is_togo_inst (bagToList insts)

	sorted_insts	= sortLt lt_inst togo_insts
    in
    if null togo_insts then
	return ()
    else
	hPutStr if_hdl "\n__instances__\n" >>
	hPutStr if_hdl (uppShow 0 (uppAboves (map pp_inst sorted_insts)))
  where
    is_togo_inst (InstInfo clas _ ty _ _ _ _ _ from_here _ _ _)
      = from_here -- && ...

    -------
    lt_inst (InstInfo clas1 _ ty1 _ _ _ _ _ _ _ _ _)
	    (InstInfo clas2 _ ty2 _ _ _ _ _ _ _ _ _)
      = let
	    tycon1 = fst (getAppTyCon ty1)
	    tycon2 = fst (getAppTyCon ty2)
	in
	case (origName "lt_inst" clas1 `cmp` origName "lt_inst" clas2) of
	  LT_ -> True
	  GT_ -> False
	  EQ_ -> origName "lt_inst2" tycon1 < origName "lt_inst2" tycon2

    -------
    pp_inst (InstInfo clas tvs ty theta _ _ _ _ _ _ _ _)
      = let
	    forall_ty     = mkSigmaTy tvs theta (mkDictTy clas ty)
	    renumbered_ty = initNmbr (nmbrType forall_ty)
	in
	case (splitForAllTy renumbered_ty) of { (rtvs, rrho_ty) ->
	uppBesides [uppPStr SLIT("instance "), ppr_forall rtvs, ppr_ty rrho_ty, uppSemi] }
\end{code}

%************************************************************************
%*									*
\subsection{Printing tycons, classes, ...}
%*									*
%************************************************************************

\begin{code}
ppr_class :: Class -> Unpretty

ppr_class c
  = --pprTrace "ppr_class:" (ppr PprDebug c) $
    case (initNmbr (nmbrClass c)) of { -- renumber it!
      Class _ n tyvar super_classes sdsels ops sels defms insts links ->

	uppCat [uppPStr SLIT("class"), ppr_context tyvar super_classes,
		ppr_name n, ppr_tyvar tyvar,
		if null ops
		then uppSemi
		else uppCat [uppStr "where {", uppCat (map ppr_op ops), uppStr "};"]]
    }
  where
    ppr_context :: TyVar -> [Class] -> Unpretty

    ppr_context tv []   = uppNil
--  ppr_context tv [sc] = uppBeside (ppr_assert tv sc) (uppPStr SLIT(" =>"))
    ppr_context tv super_classes
      = uppBesides [uppStr "{{",
		    uppIntersperse upp'SP{-'-} (map (ppr_assert tv) super_classes),
		    uppStr "}} =>"]

    ppr_assert tv (Class _ n _ _ _ _ _ _ _ _) = uppCat [ppr_name n, ppr_tyvar tv]

    clas_mod = moduleOf (origName "ppr_class" c)

    ppr_op (ClassOp o _ ty) = pp_sig (Qual clas_mod o) ty
\end{code}

\begin{code}
ppr_val v ty -- renumber the type first!
  = --pprTrace "ppr_val:" (ppr PprDebug v) $
    pp_sig v (initNmbr (nmbrType ty))

pp_sig op ty
  = case (splitForAllTy ty) of { (tvs, rho_ty) ->
    uppBesides [ppr_name op, uppPStr SLIT(" :: "), ppr_forall tvs, ppr_ty rho_ty, uppSemi] }

ppr_forall []  = uppNil
ppr_forall tvs = uppBesides [ uppStr "__forall__ [", uppInterleave uppComma (map ppr_tyvar tvs), uppStr "] " ]
\end{code}

\begin{code}
ppr_tycon tycon
  = --pprTrace "ppr_tycon:" (ppr PprDebug tycon) $
    ppr_tc (initNmbr (nmbrTyCon tycon))

------------------------
ppr_tc (PrimTyCon _ n _ _)
  = uppCat [ uppStr "{- data", ppr_name n, uppStr " *built-in* -}" ]

ppr_tc FunTyCon
  = uppCat [ uppStr "{- data", ppr_name FunTyCon, uppStr " *built-in* -}" ]

ppr_tc (TupleTyCon _ n _)
  = uppCat [ uppStr "{- ", ppr_name n, uppStr "-}" ]

ppr_tc (SynTyCon _ n _ _ tvs expand)
  = let
	pp_tyvars   = map ppr_tyvar tvs
    in
    uppBesides [uppPStr SLIT("type "), ppr_name n, uppSP, uppIntersperse uppSP pp_tyvars,
	   uppPStr SLIT(" = "), ppr_ty expand, uppSemi]

ppr_tc this_tycon@(DataTyCon u n k tvs ctxt cons derivings data_or_new)
  = uppCat [pp_data_or_new,
	   ppr_context ctxt,
	   ppr_name n,
	   uppIntersperse uppSP (map ppr_tyvar tvs),
	   uppEquals, pp_condecls,
	   uppSemi]
	   -- NB: we do not print deriving info in interfaces
  where
    pp_data_or_new = case data_or_new of
		      DataType -> uppPStr SLIT("data")
		      NewType  -> uppPStr SLIT("newtype")

    ppr_context []      = uppNil
--  ppr_context [(c,t)] = uppCat [ppr_name c, ppr_ty t, uppPStr SLIT("=>")]
    ppr_context cs
      = uppBesides[uppStr "{{",
		   uppInterleave uppComma [uppCat [ppr_name c, ppr_ty t] | (c,t) <- cs],
		   uppStr "}}", uppPStr SLIT(" =>")]

    pp_condecls
      = let
	    (c:cs) = cons
	in
	uppCat ((ppr_con c) : (map ppr_next_con cs))

    ppr_next_con con = uppCat [uppChar '|', ppr_con con]

    ppr_con con
      = let
	    con_arg_tys  = dataConRawArgTys   con
	    labels       = dataConFieldLabels con -- none if not a record
	    strict_marks = dataConStrictMarks con
	in
	uppCat [ppr_name con, ppr_fields labels strict_marks con_arg_tys]

    ppr_fields labels strict_marks con_arg_tys
      = if null labels then -- not a record thingy
	    uppIntersperse uppSP (zipWithEqual  "ppr_fields" ppr_bang_ty strict_marks con_arg_tys)
	else
	    uppCat [ uppChar '{',
	    uppInterleave uppComma (zipWith3Equal "ppr_field" ppr_field labels strict_marks con_arg_tys),
	    uppChar '}' ]

    ppr_bang_ty b t
      = uppBeside (case b of { MarkedStrict -> uppChar '!'; _ -> uppNil })
		  (prettyToUn (pprParendType PprInterface t))

    ppr_field l b t
      = uppBesides [ppr_name l, uppPStr SLIT(" :: "),
		   case b of { MarkedStrict -> uppChar '!'; _ -> uppNil },
		   ppr_ty t]
\end{code}
