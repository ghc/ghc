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

import Bag		( emptyBag, snocBag, bagToList )
import Class		( GenClass(..){-instance NamedThing-}, GenClassOp(..) )
import CmdLineOpts	( opt_ProduceHi )
import FieldLabel	( FieldLabel{-instance NamedThing-} )
import FiniteMap	( fmToList )
import HsSyn
import Id		( idType, dataConRawArgTys, dataConFieldLabels,
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
import Pretty		( prettyToUn )
import Unpretty		-- ditto
import RnHsSyn		( RenamedHsModule(..), RnName{-instance NamedThing-} )
import TcModule		( TcIfaceInfo(..) )
import TcInstUtil	( InstInfo(..) )
import TyCon		( TyCon(..){-instance NamedThing-}, NewOrData(..) )
import Type		( mkSigmaTy, mkDictTy, getAppTyCon )
import Util		( sortLt, zipWithEqual, zipWith3Equal, assertPanic, panic{-ToDo:rm-}, pprTrace{-ToDo:rm-} )

uppSemid   x = uppBeside (prettyToUn (ppr PprInterface x)) uppSemi -- micro util
ppr_ty	  ty = prettyToUn (pprType PprInterface ty)
ppr_tyvar tv = prettyToUn (ppr PprInterface tv)
ppr_name   n
  = let
	on = origName n
	s  = nameOf  on
	pp = prettyToUn (ppr PprInterface on)
    in
    (if isLexSym s then uppParens else id) pp
{-OLD:
ppr_unq_name n
  = let
	on = origName n
	s  = nameOf  on
	pp = uppPStr  s
    in
    (if isLexSym s then uppParens else id) pp
-}
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
    usages_list = fmToList usages

    upp_uses (m, (mv, versions))
      = uppBesides [uppPStr m, uppSP, uppInt mv, uppPStr SLIT(" :: "),
	       upp_versions (fmToList versions), uppSemi]

    upp_versions nvs
      = uppIntersperse uppSP [ uppCat [(if isLexSym n then uppParens else id) (uppPStr n), uppInt v] | (n,v) <- nvs ]
\end{code}

\begin{code}
ifaceVersions Nothing{-no iface handle-} _ = return ()

ifaceVersions (Just if_hdl) version_info
  | null version_list
  = return ()
  | otherwise
  = hPutStr if_hdl "\n__versions__\n"	>>
    hPutStr if_hdl (uppShow 0 (upp_versions version_list))
  where
    version_list = fmToList version_info

    upp_versions nvs
      = uppAboves [ (if isLexSym n then uppParens else id) (uppPStr n) | (n,v) <- nvs ]
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
    hPutStr if_hdl (uppShow 0 (uppAboves (map upp_pair sorted_pairs)))
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
    upp_pair (n, ef)
      = uppBeside (ppr_name n) (upp_export ef)
      where
	upp_export ExportAll = uppPStr SLIT("(..)")
	upp_export ExportAbs = uppNil
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
	hPutStr if_hdl (uppShow 0 (uppAboves (map uppSemid local_fixities)))
  where
    from_here (InfixL v _) = isLocallyDefined v
    from_here (InfixR v _) = isLocallyDefined v
    from_here (InfixN v _) = isLocallyDefined v
\end{code}

\begin{code}
ifaceDecls Nothing{-no iface handle-} _ = return ()

ifaceDecls (Just if_hdl) (vals, tycons, classes, _)
  = ASSERT(all isLocallyDefined vals)
    ASSERT(all isLocallyDefined tycons)
    ASSERT(all isLocallyDefined classes)
    let
	sorted_classes   = sortLt ltLexical classes
	sorted_tycons	 = sortLt ltLexical tycons
	sorted_vals	 = sortLt ltLexical vals
    in
    if (null sorted_classes && null sorted_tycons && null sorted_vals) then
	--  You could have a module with just instances in it
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
	uppBesides [uppPStr SLIT("instance "), ppr_ty renumbered_ty, uppSemi]
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

	uppCat [uppPStr SLIT("class"), ppr_theta tyvar super_classes,
		ppr_name n, ppr_tyvar tyvar,
		if null ops
		then uppSemi
		else uppCat [uppStr "where {", uppCat (map ppr_op ops), uppStr "};"]]
    }
  where
    ppr_theta :: TyVar -> [Class] -> Unpretty

    ppr_theta tv []   = uppNil
    ppr_theta tv [sc] = uppBeside (ppr_assert tv sc) (uppPStr SLIT(" =>"))
    ppr_theta tv super_classes
      = uppBesides [uppLparen,
		    uppIntersperse upp'SP{-'-} (map (ppr_assert tv) super_classes),
		    uppStr ") =>"]

    ppr_assert tv (Class _ n _ _ _ _ _ _ _ _) = uppCat [ppr_name n, ppr_tyvar tv]

    ppr_op (ClassOp o _ ty) = pp_sig (Unqual o) ty
\end{code}

\begin{code}
ppr_val v ty -- renumber the type first!
  = --pprTrace "ppr_val:" (ppr PprDebug v) $
    pp_sig v (initNmbr (nmbrType ty))

pp_sig op ty
  = uppBesides [ppr_name op, uppPStr SLIT(" :: "), ppr_ty ty, uppSemi]
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
    ppr_context [(c,t)] = uppCat [ppr_name c, ppr_ty t, uppPStr SLIT("=>")]
    ppr_context cs
      = uppBesides[uppLparen,
		   uppInterleave uppComma [uppCat [ppr_name c, ppr_ty t] | (c,t) <- cs],
		   uppRparen, uppPStr SLIT(" =>")]

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
