%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
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

import Ubiq{-uitous-}

import Bag		( emptyBag, snocBag, bagToList )
import Class		( GenClass(..){-instance NamedThing-}, GenClassOp(..) )
import CmdLineOpts	( opt_ProduceHi )
import FieldLabel	( FieldLabel{-instance NamedThing-} )
import HsSyn
import Id		( idType, dataConSig, dataConFieldLabels,
			  dataConStrictMarks, StrictnessMark(..),
			  GenId{-instance NamedThing/Outputable-}
			)
import Name		( nameOrigName, origName, nameOf,
			  exportFlagOn, nameExportFlag, ExportFlag(..),
			  ltLexical, isExported, getExportFlag,
			  isLexSym, isLocallyDefined,
			  RdrName(..){-instance Outputable-},
			  Name{-instance NamedThing-}
			)
import ParseUtils	( UsagesMap(..), VersionsMap(..) )
import PprEnv		-- not sure how much...
import PprStyle		( PprStyle(..) )
import PprType		-- most of it (??)
import Pretty		-- quite a bit
import RnHsSyn		( RenamedHsModule(..), RnName{-instance NamedThing-} )
import TcModule		( TcIfaceInfo(..) )
import TcInstUtil	( InstInfo(..) )
import TyCon		( TyCon(..){-instance NamedThing-}, NewOrData(..) )
import Type		( mkSigmaTy, mkDictTy, getAppTyCon )
import Util		( sortLt, zipWithEqual, zipWith3Equal, assertPanic, panic{-ToDo:rm-}, pprTrace{-ToDo:rm-} )

ppSemid    x = ppBeside (ppr PprInterface x) ppSemi -- micro util
ppr_ty	  ty = pprType PprInterface ty
ppr_tyvar tv = ppr PprInterface tv
ppr_name   n
  = let
	on = origName n
	s  = nameOf  on
	pp = ppr PprInterface on
    in
    (if isLexSym s then ppParens else id) pp
ppr_unq_name n
  = let
	on = origName n
	s  = nameOf  on
	pp = ppPStr   s
    in
    (if isLexSym s then ppParens else id) pp
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
	hPutStr if_hdl ("interface "++ _UNPK_ mod ++" 1\n") >>
	return (Just if_hdl)

endIface Nothing	= return ()
endIface (Just if_hdl)	= hPutStr if_hdl "\n" >> hClose if_hdl
\end{code}

\begin{code}
ifaceUsages Nothing{-no iface handle-} _ = return ()

ifaceUsages (Just if_hdl) version_info
  = hPutStr if_hdl "__usages__\nFoo 1" -- a stub, obviously
\end{code}

\begin{code}
ifaceVersions Nothing{-no iface handle-} _ = return ()

ifaceVersions (Just if_hdl) version_info
  = hPutStr if_hdl "\n__versions__\nFoo 1" -- a stub, obviously
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
      = ppBeside (ppr_name n) (pp_export ef)
      where
	pp_export ExportAll = ppPStr SLIT("(..)")
	pp_export ExportAbs = ppNil
\end{code}

\begin{code}
ifaceFixities Nothing{-no iface handle-} _ = return ()

ifaceFixities (Just if_hdl) (HsModule _ _ _ _ fixities _ _ _ _ _ _ _ _ _)
  = let
	local_fixities = filter from_here fixities
    in
    if null local_fixities then
	return ()
    else 
	hPutStr if_hdl "\n__fixities__\n" >>
	hPutStr if_hdl (ppShow 100 (ppAboves (map ppSemid local_fixities)))
  where
    from_here (InfixL v _) = isLocallyDefined v
    from_here (InfixR v _) = isLocallyDefined v
    from_here (InfixN v _) = isLocallyDefined v
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
	ppAboves (map ppr_class sorted_classes),
	ppAboves (map ppr_tycon sorted_tycons),
	ppAboves [ppr_val v (idType v) | v <- sorted_vals]]))
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
      = let
	    forall_ty     = mkSigmaTy tvs theta (mkDictTy clas ty)
	    renumbered_ty = initNmbr (nmbrType forall_ty)
	in
	ppBesides [ppPStr SLIT("instance "), ppr_ty renumbered_ty, ppSemi]
\end{code}

%************************************************************************
%*									*
\subsection{Printing tycons, classes, ...}
%*									*
%************************************************************************

\begin{code}
ppr_class :: Class -> Pretty

ppr_class c
  = --pprTrace "ppr_class:" (ppr PprDebug c) $
    case (initNmbr (nmbrClass c)) of { -- renumber it!
      Class _ n tyvar super_classes sdsels ops sels defms insts links ->

	ppAbove (ppCat [ppPStr SLIT("class"), ppr_theta tyvar super_classes,
		    ppr_name n, ppr_tyvar tyvar,
		    if null ops then ppSemi else ppStr "where {"])
	    (if (null ops)
	     then ppNil
	     else ppAbove (ppNest 2 (ppAboves (map ppr_op ops)))
			  (ppStr "};")
	    )
    }
  where
    ppr_theta :: TyVar -> [Class] -> Pretty

    ppr_theta tv []   = ppNil
    ppr_theta tv [sc] = ppBeside (ppr_assert tv sc) (ppStr " =>")
    ppr_theta tv super_classes
      = ppBesides [ppLparen,
		   ppIntersperse pp'SP{-'-} (map (ppr_assert tv) super_classes),
		   ppStr ") =>"]

    ppr_assert tv (Class _ n _ _ _ _ _ _ _ _) = ppCat [ppr_name n, ppr_tyvar tv]

    ppr_op (ClassOp o _ ty) = pp_sig (Unqual o) ty
\end{code}

\begin{code}
ppr_val v ty -- renumber the type first!
  = --pprTrace "ppr_val:" (ppr PprDebug v) $
    pp_sig v (initNmbr (nmbrType ty))

pp_sig op ty
  = ppBesides [ppr_name op, ppPStr SLIT(" :: "), ppr_ty ty, ppSemi]
\end{code}

\begin{code}
ppr_tycon tycon
  = --pprTrace "ppr_tycon:" (ppr PprDebug tycon) $
    ppr_tc (initNmbr (nmbrTyCon tycon))

------------------------
ppr_tc (PrimTyCon _ n _)
  = ppCat [ ppStr "{- data", ppr_name n, ppStr " *built-in* -}" ]

ppr_tc FunTyCon
  = ppCat [ ppStr "{- data", ppr_name FunTyCon, ppStr " *built-in* -}" ]

ppr_tc (TupleTyCon _ n _)
  = ppCat [ ppStr "{- ", ppr_name n, ppStr "-}" ]

ppr_tc (SynTyCon _ n _ _ tvs expand)
  = let
	pp_tyvars   = map ppr_tyvar tvs
    in
    ppBesides [ppPStr SLIT("type "), ppr_name n, ppSP, ppIntersperse ppSP pp_tyvars,
	   ppPStr SLIT(" = "), ppr_ty expand, ppSemi]

ppr_tc this_tycon@(DataTyCon u n k tvs ctxt cons derivings data_or_new)
  = ppHang (ppCat [pp_data_or_new,
		   ppr_context ctxt,
		   ppr_name n,
		   ppIntersperse ppSP (map ppr_tyvar tvs)])
	   2
	   (ppBeside pp_unabstract_condecls ppSemi)
	   -- NB: we do not print deriving info in interfaces
  where
    pp_data_or_new = case data_or_new of
		      DataType -> ppPStr SLIT("data")
		      NewType  -> ppPStr SLIT("newtype")

    ppr_context []      = ppNil
    ppr_context [(c,t)] = ppCat [ppr_name c, ppr_ty t, ppStr "=>"]
    ppr_context cs
      = ppBesides[ppLparen,
		  ppInterleave ppComma [ppCat [ppr_name c, ppr_ty t] | (c,t) <- cs],
		  ppRparen, ppStr " =>"]

    yes_we_print_condecls
      = case (getExportFlag n) of
	  ExportAbs -> False
	  other	    -> True

    pp_unabstract_condecls
      = if yes_we_print_condecls
	then ppCat [ppEquals, pp_condecls]
	else ppNil

    pp_condecls
      = let
	    (c:cs) = cons
	in
	ppSep ((ppr_con c) : (map ppr_next_con cs))

    ppr_next_con con = ppCat [ppChar '|', ppr_con con]

    ppr_con con
      = let
	    (_, _, con_arg_tys, _) = dataConSig con
	    labels       = dataConFieldLabels con -- none if not a record
	    strict_marks = dataConStrictMarks con
	in
	ppCat [ppr_unq_name con, ppr_fields labels strict_marks con_arg_tys]

    ppr_fields labels strict_marks con_arg_tys
      = if null labels then -- not a record thingy
	    ppIntersperse ppSP (zipWithEqual  ppr_bang_ty strict_marks con_arg_tys)
	else
	    ppCat [ ppChar '{',
	    ppInterleave ppComma (zipWith3Equal ppr_field labels strict_marks con_arg_tys),
	    ppChar '}' ]

    ppr_bang_ty b t
      = ppBeside (case b of { MarkedStrict -> ppChar '!'; _ -> ppNil })
		 (pprParendType PprInterface t)

    ppr_field l b t
      = ppBesides [ppr_unq_name l, ppPStr SLIT(" :: "),
		   case b of { MarkedStrict -> ppChar '!'; _ -> ppNil },
		   ppr_ty t]
\end{code}
