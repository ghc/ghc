%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[MkIface]{Print an interface for a module}

\begin{code}
#include "HsVersions.h"

module MkIface (
	startIface, endIface,
	ifaceMain,
	ifaceDecls
    ) where

IMP_Ubiq(){-uitous-}
IMPORT_1_3(IO(Handle,hPutStr,openFile,hClose,IOMode(..)))

import HsSyn
import RdrHsSyn		( RdrName(..) )
import RnHsSyn		( SYN_IE(RenamedHsModule) )
import RnMonad
import RnEnv		( availName )

import TcInstUtil	( InstInfo(..) )

import CmdLineOpts
import Id		( idType, dataConRawArgTys, dataConFieldLabels, isDataCon,
			  getIdInfo, getInlinePragma, omitIfaceSigForId,
			  dataConStrictMarks, StrictnessMark(..), 
			  SYN_IE(IdSet), idSetToList, unionIdSets, unitIdSet, minusIdSet, 
			  isEmptyIdSet, elementOfIdSet, emptyIdSet, mkIdSet,
			  GenId{-instance NamedThing/Outputable-}, SYN_IE(Id)

			)
import IdInfo		( StrictnessInfo, ArityInfo, 
			  arityInfo, ppArityInfo, strictnessInfo, ppStrictnessInfo, 
			  getWorkerId_maybe, bottomIsGuaranteed, IdInfo
			)
import CoreSyn		( SYN_IE(CoreExpr), SYN_IE(CoreBinding), GenCoreExpr, GenCoreBinding(..) )
import CoreUnfold	( calcUnfoldingGuidance, UnfoldingGuidance(..), Unfolding )
import FreeVars		( addExprFVs )
import Name		( isLocallyDefined, isWiredInName, modAndOcc, getName, pprOccName,
			  OccName, occNameString, nameOccName, nameString, isExported,
			  Name {-instance NamedThing-}, Provenance, NamedThing(..)
			)
import TyCon		( TyCon(..) {-instance NamedThing-} )
import Class		( GenClass(..){-instance NamedThing-}, SYN_IE(Class), GenClassOp, 
			  classOpLocalType, classSig )
import FieldLabel	( FieldLabel{-instance NamedThing-}, 
		          fieldLabelName, fieldLabelType )
import Type		( mkSigmaTy, mkDictTy, getAppTyCon, splitForAllTy,
			  mkTyVarTy, SYN_IE(Type)
		        )
import TyVar		( GenTyVar {- instance Eq -} )
import Unique		( Unique {- instance Eq -} )

import PprEnv		-- not sure how much...
import PprStyle		( PprStyle(..) )
import PprType
import PprCore		( pprIfaceUnfolding )
import Pretty
import Outputable	( printDoc )


import Bag		( bagToList, isEmptyBag )
import Maybes		( catMaybes, maybeToBool )
import FiniteMap	( emptyFM, addToFM, addToFM_C, lookupFM, fmToList, eltsFM, FiniteMap )
import UniqFM		( UniqFM, lookupUFM, listToUFM )
import Util		( sortLt, zipWithEqual, zipWith3Equal, mapAccumL,
			  assertPanic, panic{-ToDo:rm-}, pprTrace,
			  pprPanic 
			)
import Outputable       ( Outputable(..) )

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

ifaceMain   :: Maybe Handle
	    -> InterfaceDetails
	    -> IO ()


ifaceDecls :: Maybe Handle
	   -> [TyCon] -> [Class]
	   -> Bag InstInfo 
	   -> [Id]		-- Ids used at code-gen time; they have better pragma info!
	   -> [CoreBinding]	-- In dependency order, later depend on earlier
	   -> IO ()

endIface    :: Maybe Handle -> IO ()
\end{code}

\begin{code}
startIface mod
  = case opt_ProduceHi of
      Nothing -> return Nothing -- not producing any .hi file
      Just fn ->
	openFile fn WriteMode	>>= \ if_hdl ->
	hPutStr if_hdl ("{-# GHC_PRAGMA INTERFACE VERSION 20 #-}\n_interface_ "++ _UNPK_ mod ++ "\n") >>
	return (Just if_hdl)

endIface Nothing	= return ()
endIface (Just if_hdl)	= hPutStr if_hdl "\n" >> hClose if_hdl
\end{code}


\begin{code}
ifaceMain Nothing iface_stuff = return ()
ifaceMain (Just if_hdl)
	  (import_usages, ExportEnv avails fixities, instance_modules)
  =
    ifaceInstanceModules	if_hdl instance_modules		>>
    ifaceUsages			if_hdl import_usages		>>
    ifaceExports		if_hdl avails			>>
    ifaceFixities		if_hdl fixities			>>
    return ()

ifaceDecls Nothing tycons classes inst_info final_ids simplified = return ()
ifaceDecls (Just hdl)
	   tycons classes
	   inst_infos
	   final_ids binds
  | null_decls = return ()		 
	--  You could have a module with just (re-)exports/instances in it
  | otherwise
  = ifaceInstances hdl inst_infos		>>= \ needed_ids ->
    hPutStr hdl "_declarations_\n"		>>
    ifaceClasses hdl classes			>>
    ifaceTyCons hdl tycons			>>
    ifaceBinds hdl needed_ids final_ids binds	>>
    return ()
    where
     null_decls = null binds      && 
		  null tycons     &&
	          null classes    && 
	          isEmptyBag inst_infos
\end{code}

\begin{code}
ifaceUsages if_hdl import_usages
  = hPutStr if_hdl "_usages_\n"   >>
    hPutCol if_hdl upp_uses (sortLt lt_imp_vers import_usages)
  where
    upp_uses (m, mv, versions)
      = hcat [upp_module m, space, int mv, ptext SLIT(" :: "),
		    upp_import_versions (sort_versions versions), semi]

	-- For imported versions we do print the version number
    upp_import_versions nvs
      = hsep [ hsep [ppr_unqual_name n, int v] | (n,v) <- nvs ]


ifaceInstanceModules if_hdl [] = return ()
ifaceInstanceModules if_hdl imods
  = hPutStr if_hdl "_instance_modules_\n" >>
    printDoc OneLineMode if_hdl (hsep (map ptext (sortLt (<) imods))) >>
    hPutStr if_hdl "\n"

ifaceExports if_hdl [] = return ()
ifaceExports if_hdl avails
  = hPutStr if_hdl "_exports_\n"			>>
    hPutCol if_hdl do_one_module (fmToList export_fm)
  where
	-- Sort them into groups by module
    export_fm :: FiniteMap Module [AvailInfo]
    export_fm = foldr insert emptyFM avails

    insert NotAvailable efm = efm
    insert avail efm = addToFM_C (++) efm mod [avail] 
		     where
		       (mod,_) = modAndOcc (availName avail)

	-- Print one module's worth of stuff
    do_one_module (mod_name, avails)
	= hcat [upp_module mod_name, space, 
		      hsep (map upp_avail (sortLt lt_avail avails)),
		      semi]

ifaceFixities if_hdl [] = return ()
ifaceFixities if_hdl fixities 
  = hPutStr if_hdl "_fixities_\n"		>>
    hPutCol if_hdl upp_fixity fixities
\end{code}			 

%************************************************************************
%*				 					*
\subsection{Instance declarations}
%*				 					*
%************************************************************************


\begin{code}			 
ifaceInstances :: Handle -> Bag InstInfo -> IO IdSet		-- The IdSet is the needed dfuns
ifaceInstances if_hdl inst_infos
  | null togo_insts = return emptyIdSet		 
  | otherwise 	    = hPutStr if_hdl "_instances_\n" >>
		      hPutCol if_hdl pp_inst (sortLt lt_inst togo_insts) >>
		      return needed_ids
  where				 
    togo_insts	= filter is_togo_inst (bagToList inst_infos)
    needed_ids  = mkIdSet [dfun_id | InstInfo _ _ _ _ _ dfun_id _ _ _ <- togo_insts]
    is_togo_inst (InstInfo _ _ _ _ _ dfun_id _ _ _) = isLocallyDefined dfun_id
				 
    -------			 
    lt_inst (InstInfo _ _ _ _ _ dfun_id1 _ _ _)
	    (InstInfo _ _ _ _ _ dfun_id2 _ _ _)
      = getOccName dfun_id1 < getOccName dfun_id2
	-- The dfuns are assigned names df1, df2, etc, in order of original textual
	-- occurrence, and this makes as good a sort order as any

    -------			 
    pp_inst (InstInfo clas tvs ty theta _ dfun_id _ _ _)
      = let			 
	    forall_ty     = mkSigmaTy tvs theta (mkDictTy clas ty)
	    renumbered_ty = renumber_ty forall_ty
	in			 
	hcat [ptext SLIT("instance "), ppr_ty renumbered_ty, 
		    ptext SLIT(" = "), ppr_unqual_name dfun_id, semi]
\end{code}


%************************************************************************
%*				 					*
\subsection{Printing values}
%*				 					*
%************************************************************************

\begin{code}
ifaceId :: (Id -> IdInfo)		-- This function "knows" the extra info added
					-- by the STG passes.  Sigh

	    -> IdSet			-- Set of Ids that are needed by earlier interface
					-- file emissions.  If the Id isn't in this set, and isn't
					-- exported, there's no need to emit anything
	    -> Bool			-- True <=> recursive, so don't print unfolding
	    -> Id
	    -> CoreExpr			-- The Id's right hand side
	    -> Maybe (Doc, IdSet)	-- The emitted stuff, plus a possibly-augmented set of needed Ids

ifaceId get_idinfo needed_ids is_rec id rhs
  | not (id `elementOfIdSet` needed_ids ||		-- Needed [no id in needed_ids has omitIfaceSigForId]
	 (isExported id && not (omitIfaceSigForId id)))	-- or exported and not to be omitted
  = Nothing 		-- Well, that was easy!

ifaceId get_idinfo needed_ids is_rec id rhs
  = Just (hsep [sig_pretty, pp_double_semi, prag_pretty], new_needed_ids)
  where
    pp_double_semi = ptext SLIT(";;")
    idinfo         = get_idinfo id
    inline_pragma  = getInlinePragma id 

    ty_pretty  = pprType PprInterface (initNmbr (nmbrType (idType id)))
    sig_pretty = hcat [ppr PprInterface (getOccName id), ptext SLIT(" _:_ "), ty_pretty]

    prag_pretty 
     | opt_OmitInterfacePragmas = empty
     | otherwise		= hsep [arity_pretty, strict_pretty, unfold_pretty, pp_double_semi]

    ------------  Arity  --------------
    arity_pretty  = ppArityInfo PprInterface (arityInfo idinfo)

    ------------  Strictness  --------------
    strict_info   = strictnessInfo idinfo
    maybe_worker  = getWorkerId_maybe strict_info
    strict_pretty = ppStrictnessInfo PprInterface strict_info

    ------------  Unfolding  --------------
    unfold_pretty | show_unfold = hsep [ptext SLIT("_U_"), pprIfaceUnfolding rhs]
		  | otherwise   = empty

    show_unfold = not implicit_unfolding && 		-- Not unnecessary
		  not dodgy_unfolding			-- Not dangerous

    implicit_unfolding = maybeToBool maybe_worker ||
			 bottomIsGuaranteed strict_info

    dodgy_unfolding = case guidance of 			-- True <=> too big to show, or the Inline pragma
			UnfoldNever -> True		-- says it shouldn't be inlined
			other       -> False

    guidance    = calcUnfoldingGuidance inline_pragma
					opt_InterfaceUnfoldThreshold
					rhs

    
    ------------  Extra free Ids  --------------
    new_needed_ids = (needed_ids `minusIdSet` unitIdSet id)	`unionIdSets` 
		     extra_ids

    extra_ids | opt_OmitInterfacePragmas = emptyIdSet
	      | otherwise		 = worker_ids	`unionIdSets`
					   unfold_ids

    worker_ids = case maybe_worker of
			Just wkr -> unitIdSet wkr
			Nothing  -> emptyIdSet

    unfold_ids | show_unfold = free_vars
	       | otherwise   = emptyIdSet
			     where
			       (_,free_vars) = addExprFVs interesting emptyIdSet rhs
			       interesting bound id = isLocallyDefined id &&
						      not (id `elementOfIdSet` bound) &&
						      not (omitIfaceSigForId id)
\end{code}

\begin{code}
ifaceBinds :: Handle
	   -> IdSet		-- These Ids are needed already
	   -> [Id]		-- Ids used at code-gen time; they have better pragma info!
	   -> [CoreBinding]	-- In dependency order, later depend on earlier
	   -> IO ()

ifaceBinds hdl needed_ids final_ids binds
  = mapIO (printDoc OneLineMode hdl) pretties >>
    hPutStr hdl "\n"
  where
    final_id_map  = listToUFM [(id,id) | id <- final_ids]
    get_idinfo id = case lookupUFM final_id_map id of
			Just id' -> getIdInfo id'
			Nothing  -> pprTrace "ifaceBinds not found:" (ppr PprDebug id) $
				    getIdInfo id

    pretties = go needed_ids (reverse binds)	-- Reverse so that later things will 
						-- provoke earlier ones to be emitted
    go needed [] = if not (isEmptyIdSet needed) then
			pprTrace "ifaceBinds: free vars:" 
				  (sep (map (ppr PprDebug) (idSetToList needed))) $
			[]
		   else
			[]

    go needed (NonRec id rhs : binds)
	= case ifaceId get_idinfo needed False id rhs of
		Nothing		       -> go needed binds
		Just (pretty, needed') -> pretty : go needed' binds

	-- Recursive groups are a bit more of a pain.  We may only need one to
	-- start with, but it may call out the next one, and so on.  So we
	-- have to look for a fixed point.
    go needed (Rec pairs : binds)
	= pretties ++ go needed'' binds
	where
	  (needed', pretties) = go_rec needed pairs
	  needed'' = needed' `minusIdSet` mkIdSet (map fst pairs)
		-- Later ones may spuriously cause earlier ones to be "needed" again

    go_rec :: IdSet -> [(Id,CoreExpr)] -> (IdSet, [Doc])
    go_rec needed pairs
	| null pretties = (needed, [])
	| otherwise	= (final_needed, more_pretties ++ pretties)
	where
	  reduced_pairs		 	= [pair | (pair,Nothing) <- pairs `zip` maybes]
	  pretties		 	= catMaybes maybes
	  (needed', maybes)	 	= mapAccumL do_one needed pairs
	  (final_needed, more_pretties) = go_rec needed' reduced_pairs

	  do_one needed (id,rhs) = case ifaceId get_idinfo needed True id rhs of
					Nothing		       -> (needed,  Nothing)
					Just (pretty, needed') -> (needed', Just pretty)
\end{code}


%************************************************************************
%*				 					*
\subsection{Random small things}
%*				 					*
%************************************************************************

\begin{code}
ifaceTyCons hdl tycons   = hPutCol hdl upp_tycon (sortLt (<) (filter (for_iface_name . getName) tycons ))
ifaceClasses hdl classes = hPutCol hdl upp_class (sortLt (<) (filter (for_iface_name . getName) classes))

for_iface_name name = isLocallyDefined name && 
		      not (isWiredInName name)

upp_tycon tycon = ifaceTyCon PprInterface tycon
upp_class clas  = ifaceClass PprInterface clas
\end{code}


\begin{code}
ifaceTyCon :: PprStyle -> TyCon -> Doc	
ifaceTyCon sty tycon
  = case tycon of
	DataTyCon uniq name kind tyvars theta data_cons deriv new_or_data
	   -> hsep [	ptext (keyword new_or_data), 
			ppr_decl_context sty theta,
			ppr sty name,
			hsep (map (pprTyVarBndr sty) tyvars),
			ptext SLIT("="),
			hsep (punctuate (ptext SLIT(" | ")) (map ppr_con data_cons)),
			semi
		    ]

	SynTyCon uniq name kind arity tyvars ty
	   -> hsep [	ptext SLIT("type"),
			ppr sty name,
			hsep (map (pprTyVarBndr sty) tyvars),
			ptext SLIT("="),
			ppr sty ty,
			semi
		    ]
	other -> pprPanic "pprIfaceTyDecl" (ppr PprDebug tycon)
  where
    keyword NewType  = SLIT("newtype")
    keyword DataType = SLIT("data")

    ppr_con data_con 
	| null field_labels
	= hsep [ ppr sty name,
		  hsep (map ppr_arg_ty (strict_marks `zip` arg_tys))
	        ]

	| otherwise
	= hsep [ ppr sty name,
		  braces $ hsep $ punctuate comma (map ppr_field (strict_marks `zip` field_labels))
	 	]
          where
           field_labels   = dataConFieldLabels data_con
	   arg_tys        = dataConRawArgTys   data_con
           strict_marks   = dataConStrictMarks data_con
	   name           = getName            data_con

    ppr_arg_ty (strict_mark, ty) = ppr_strict_mark strict_mark <> pprParendType sty ty

    ppr_strict_mark NotMarkedStrict = empty
    ppr_strict_mark MarkedStrict    = ptext SLIT("! ")
				-- The extra space helps the lexical analyser that lexes
				-- interface files; it doesn't make the rigid operator/identifier
				-- distinction, so "!a" is a valid identifier so far as it is concerned

    ppr_field (strict_mark, field_label)
	= hsep [ ppr sty (fieldLabelName field_label),
		  ptext SLIT("::"),
		  ppr_strict_mark strict_mark <> pprParendType sty (fieldLabelType field_label)
		]

ifaceClass sty clas
  = hsep [ptext SLIT("class"),
	   ppr_decl_context sty theta,
	   ppr sty clas,			-- Print the name
	   pprTyVarBndr sty tyvar,
	   pp_ops,
	   semi
	  ]
   where
     (tyvar, super_classes, ops) = classSig clas
     theta = super_classes `zip` repeat (mkTyVarTy tyvar)

     pp_ops | null ops  = empty
	    | otherwise = hsep [ptext SLIT("where"),
				 braces (hsep (punctuate semi (map ppr_classop ops)))
			  ]

     ppr_classop op = hsep [ppr sty (getOccName op),
			     ptext SLIT("::"),
			     ppr sty (classOpLocalType op)
			    ]

ppr_decl_context :: PprStyle -> [(Class,Type)] -> Doc
ppr_decl_context sty [] = empty
ppr_decl_context sty theta
  = braces (hsep (punctuate comma (map (ppr_dict) theta)))
    <> 
    ptext SLIT(" =>")
  where
    ppr_dict (clas,ty) = hsep [ppr sty clas, ppr sty ty]
\end{code}

%************************************************************************
%*				 					*
\subsection{Random small things}
%*				 					*
%************************************************************************

When printing export lists, we print like this:
	Avail   f		f
	AvailTC C [C, x, y]	C(x,y)
	AvailTC C [x, y]	C!(x,y)		-- Exporting x, y but not C

\begin{code}
upp_avail NotAvailable      = empty
upp_avail (Avail name)      = upp_occname (getOccName name)
upp_avail (AvailTC name []) = empty
upp_avail (AvailTC name ns) = hcat [upp_occname (getOccName name), bang, upp_export ns']
			    where
			      bang | name `elem` ns = empty
				   | otherwise	    = char '!'
			      ns' = filter (/= name) ns

upp_export []    = empty
upp_export names = hcat [char '(', 
			       hsep (map (upp_occname . getOccName) names), 
			       char ')']

upp_fixity (occ, (Fixity prec dir, prov)) = hcat [upp_dir dir, space, 
						        int prec, space, 
					       	        upp_occname occ, semi]
upp_dir InfixR = ptext SLIT("infixr")
upp_dir InfixL = ptext SLIT("infixl")
upp_dir InfixN = ptext SLIT("infix")

ppr_unqual_name :: NamedThing a => a -> Doc		-- Just its occurrence name
ppr_unqual_name name = upp_occname (getOccName name)

ppr_name :: NamedThing a => a -> Doc		-- Its full name
ppr_name   n = ptext (nameString (getName n))

upp_occname :: OccName -> Doc
upp_occname occ = ptext (occNameString occ)

upp_module :: Module -> Doc
upp_module mod = ptext mod

uppSemid   x = ppr PprInterface x <> semi -- micro util

ppr_ty	  ty = pprType PprInterface ty
ppr_tyvar tv = ppr PprInterface tv
ppr_tyvar_bndr tv = pprTyVarBndr PprInterface tv

ppr_decl decl = ppr PprInterface decl <> semi

renumber_ty ty = initNmbr (nmbrType ty)
\end{code}


%************************************************************************
%*				 					*
\subsection{Comparisons
%*				 					*
%************************************************************************
				 

The various sorts above simply prevent unnecessary "wobbling" when
things change that don't have to.  We therefore compare lexically, not
by unique

\begin{code}
lt_avail :: AvailInfo -> AvailInfo -> Bool

a1 `lt_avail` a2 = availName a1 `lt_name` availName a2

lt_name :: Name -> Name -> Bool
n1 `lt_name` n2 = modAndOcc n1 < modAndOcc n2

lt_lexical :: NamedThing a => a -> a -> Bool
lt_lexical a1 a2 = getName a1 `lt_name` getName a2

lt_imp_vers :: ImportVersion a -> ImportVersion a -> Bool
lt_imp_vers (m1,_,_) (m2,_,_) = m1 < m2

sort_versions vs = sortLt lt_vers vs

lt_vers :: LocalVersion Name -> LocalVersion Name -> Bool
lt_vers (n1,v1) (n2,v2) = n1 `lt_name` n2
\end{code}


\begin{code}
hPutCol :: Handle 
	-> (a -> Doc)
	-> [a]
	-> IO ()
hPutCol hdl fmt xs = mapIO (printDoc OneLineMode hdl . fmt) xs

mapIO :: (a -> IO b) -> [a] -> IO ()
mapIO f []     = return ()
mapIO f (x:xs) = f x >> mapIO f xs
\end{code}
