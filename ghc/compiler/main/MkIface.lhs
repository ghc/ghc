%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[MkIface]{Print an interface for a module}

\begin{code}
module MkIface (
	startIface, endIface,
	ifaceMain,
	ifaceDecls
    ) where

#include "HsVersions.h"

import IO		( Handle, hPutStr, openFile, 
			  hClose, hPutStrLn, IOMode(..) )

import HsSyn
import RdrHsSyn		( RdrName(..) )
import BasicTypes	( Fixity(..), FixityDirection(..), NewOrData(..), IfaceFlavour(..),
  			  StrictnessMark(..) 
			)
import RnMonad
import RnEnv		( availName, ifaceFlavour )

import TcInstUtil	( InstInfo(..) )
import WorkWrap		( getWorkerIdAndCons )

import CmdLineOpts
import Id		( Id, idType, idInfo, omitIfaceSigForId,
			  getIdSpecialisation
			)
import Var		( isId )
import VarSet
import DataCon		( dataConSig, dataConFieldLabels, dataConStrictMarks )
import IdInfo		( IdInfo, StrictnessInfo, ArityInfo, InlinePragInfo(..), inlinePragInfo,
			  arityInfo, ppArityInfo, 
			  strictnessInfo, ppStrictnessInfo, 
			  cafInfo, ppCafInfo,
			  workerExists, isBottomingStrictness
			)
import CoreSyn		( CoreExpr, CoreBind, Bind(..) )
import CoreUtils	( exprSomeFreeVars )
import CoreUnfold	( calcUnfoldingGuidance, UnfoldingGuidance(..), 
			  Unfolding, okToUnfoldInHiFile )
import Name		( isLocallyDefined, isWiredInName, modAndOcc, nameModule,
			  OccName, pprOccName, pprModule, isExported, moduleString,
			  Name, NamedThing(..)
			)
import TyCon		( TyCon, getSynTyConDefn, isSynTyCon, isNewTyCon, isAlgTyCon,
			  tyConTheta, tyConTyVars, tyConDataCons
			)
import Class		( Class, classBigSig )
import SpecEnv		( specEnvToList )
import FieldLabel	( fieldLabelName, fieldLabelType )
import Type		( mkSigmaTy, splitSigmaTy, mkDictTy, tidyTopType,
			  Type, ThetaType
		        )

import PprType
import PprCore		( pprIfaceUnfolding )

import Bag		( bagToList, isEmptyBag )
import Maybes		( catMaybes, maybeToBool )
import FiniteMap	( emptyFM, addToFM, addToFM_C, fmToList, FiniteMap )
import UniqFM		( lookupUFM, listToUFM )
import UniqSet		( uniqSetToList )
import Util		( sortLt, mapAccumL )
import Outputable
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
	   -> [CoreBind]	-- In dependency order, later depend on earlier
	   -> IO ()

endIface    :: Maybe Handle -> IO ()
\end{code}

\begin{code}
startIface mod
  = case opt_ProduceHi of
      Nothing -> return Nothing -- not producing any .hi file
      Just fn -> do
	if_hdl <- openFile fn WriteMode
	hPutStr if_hdl ("__interface " ++ moduleString mod ++ ' ':show (opt_HiVersion :: Int))
	hPutStrLn if_hdl " where"
	return (Just if_hdl)

endIface Nothing	= return ()
endIface (Just if_hdl)	= hPutStr if_hdl "\n" >> hClose if_hdl
\end{code}


\begin{code}
ifaceMain Nothing iface_stuff = return ()
ifaceMain (Just if_hdl)
	  (import_usages, ExportEnv avails fixities, instance_modules)
  = do
    ifaceImports		if_hdl import_usages
    ifaceInstanceModules	if_hdl instance_modules
    ifaceExports		if_hdl avails
    ifaceFixities		if_hdl fixities
    return ()

ifaceDecls Nothing tycons classes inst_info final_ids simplified = return ()
ifaceDecls (Just hdl)
	   tycons classes
	   inst_infos
	   final_ids binds
  | null_decls = return ()		 
	--  You could have a module with just (re-)exports/instances in it
  | otherwise
  = ifaceClasses hdl classes			>>
    ifaceInstances hdl inst_infos		>>= \ needed_ids ->
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
ifaceImports if_hdl import_usages
  = hPutCol if_hdl upp_uses (sortLt lt_imp_vers import_usages)
  where
    upp_uses (m, hif, mv, whats_imported)
      = ptext SLIT("import ") <>
	hsep [pprModule m, pp_hif hif, int mv, dcolon,
	      upp_import_versions whats_imported
	] <> semi

	-- Importing the whole module is indicated by an empty list
    upp_import_versions Everything = empty

	-- For imported versions we do print the version number
    upp_import_versions (Specifically nvs)
      = hsep [ hsep [ppr_unqual_name n, int v] | (n,v) <- sort_versions nvs ]

ifaceInstanceModules if_hdl [] = return ()
ifaceInstanceModules if_hdl imods
  = let sorted = sortLt (<) imods
	lines = map (\m -> ptext SLIT("__instimport ") <> pprModule m <>
			   ptext SLIT(" ;")) sorted
    in 
    printForIface if_hdl (vcat lines) >>
    hPutStr if_hdl "\n"

ifaceExports if_hdl [] = return ()
ifaceExports if_hdl avails
  = hPutCol if_hdl do_one_module (fmToList export_fm)
  where
	-- Sort them into groups by module
    export_fm :: FiniteMap Module [AvailInfo]
    export_fm = foldr insert emptyFM avails

    insert NotAvailable efm = efm
    insert avail efm = addToFM_C (++) efm mod [avail] 
		     where
		       mod = nameModule (availName avail)

	-- Print one module's worth of stuff
    do_one_module :: (Module, [AvailInfo]) -> SDoc
    do_one_module (mod_name, avails@(avail1:_))
	= ptext SLIT("__export ") <>
	  hsep [pp_hif (ifaceFlavour (availName avail1)), 
		pprModule mod_name,
		hsep (map upp_avail (sortLt lt_avail avails))
	  ] <> semi

-- The "!" indicates that the exported things came from a hi-boot interface 
pp_hif HiFile     = empty
pp_hif HiBootFile = char '!'

ifaceFixities if_hdl [] = return ()
ifaceFixities if_hdl fixities 
  = hPutCol if_hdl upp_fixity fixities
\end{code}			 

%************************************************************************
%*				 					*
\subsection{Instance declarations}
%*				 					*
%************************************************************************


\begin{code}			 
ifaceInstances :: Handle -> Bag InstInfo -> IO IdSet		-- The IdSet is the needed dfuns
ifaceInstances if_hdl inst_infos
  | null togo_insts = return emptyVarSet		 
  | otherwise 	    = hPutCol if_hdl pp_inst (sortLt lt_inst togo_insts) >>
		      return needed_ids
  where				 
    togo_insts	= filter is_togo_inst (bagToList inst_infos)
    needed_ids  = mkVarSet [dfun_id | InstInfo _ _ _ _ dfun_id _ _ _ <- togo_insts]
    is_togo_inst (InstInfo _ _ _ _ dfun_id _ _ _) = isLocallyDefined dfun_id
				 
    -------			 
    lt_inst (InstInfo _ _ _ _ dfun_id1 _ _ _)
	    (InstInfo _ _ _ _ dfun_id2 _ _ _)
      = getOccName dfun_id1 < getOccName dfun_id2
	-- The dfuns are assigned names df1, df2, etc, in order of original textual
	-- occurrence, and this makes as good a sort order as any

    -------			 
    pp_inst (InstInfo clas tvs tys theta dfun_id _ _ _)
      = let			 
	    forall_ty     = mkSigmaTy tvs theta (mkDictTy clas tys)
	    renumbered_ty = tidyTopType forall_ty
	in			 
	hcat [ptext SLIT("instance "), pprType renumbered_ty, 
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
	    -> Maybe (SDoc, IdSet)	-- The emitted stuff, plus a possibly-augmented set of needed Ids

ifaceId get_idinfo needed_ids is_rec id rhs
  | not (id `elemVarSet` needed_ids ||		-- Needed [no id in needed_ids has omitIfaceSigForId]
	 (isExported id && not (omitIfaceSigForId id)))	-- or exported and not to be omitted
  = Nothing 		-- Well, that was easy!

ifaceId get_idinfo needed_ids is_rec id rhs
  = Just (hsep [sig_pretty, prag_pretty, char ';'], new_needed_ids)
  where
    idinfo         = get_idinfo id
    inline_pragma  = inlinePragInfo idinfo

    ty_pretty  = pprType (idType id)
    sig_pretty = hsep [ppr (getOccName id), dcolon, ty_pretty]

    prag_pretty 
     | opt_OmitInterfacePragmas = empty
     | otherwise		= hsep [ptext SLIT("{-##"),
					arity_pretty, 
					caf_pretty,
					strict_pretty, 
					unfold_pretty, 
					spec_pretty,
					ptext SLIT("##-}")]

    ------------  Arity  --------------
    arity_pretty  = ppArityInfo (arityInfo idinfo)

    ------------ Caf Info --------------
    caf_pretty = ppCafInfo (cafInfo idinfo)

    ------------  Strictness  --------------
    strict_info   = strictnessInfo idinfo
    has_worker    = workerExists strict_info
    bottoming_fn  = isBottomingStrictness strict_info
    strict_pretty = ppStrictnessInfo strict_info <+> wrkr_pretty

    wrkr_pretty | not has_worker = empty
		| null con_list  = ppr work_id
		| otherwise      = ppr work_id <+> 
				   braces (hsep (map ppr con_list))

    (work_id, wrapper_cons) = getWorkerIdAndCons id rhs
    con_list 		    = uniqSetToList wrapper_cons

    ------------  Unfolding  --------------
    unfold_pretty | show_unfold = unfold_herald <+> pprIfaceUnfolding rhs
		  | otherwise   = empty

    show_unfold = not has_worker	&&	-- Not unnecessary
		  not bottoming_fn	&&	-- Not necessary
		  unfolding_needed		-- Not dangerous

    unfolding_needed =  case inline_pragma of
			      IMustBeINLINEd    -> definitely_ok_to_unfold
			      IWantToBeINLINEd  -> definitely_ok_to_unfold
			      NoInlinePragInfo  -> rhs_is_small
			      other	        -> False


    unfold_herald = case inline_pragma of
			NoInlinePragInfo -> ptext SLIT("__u")
			other		 -> ppr inline_pragma

    rhs_is_small = case calcUnfoldingGuidance opt_InterfaceUnfoldThreshold rhs of
			UnfoldNever -> False	-- Too big
			other	    ->  definitely_ok_to_unfold -- Small enough

    definitely_ok_to_unfold =  okToUnfoldInHiFile rhs

    ------------  Specialisations --------------
    spec_list = specEnvToList (getIdSpecialisation id)
    spec_pretty = hsep (map pp_spec spec_list)
    pp_spec (tyvars, tys, rhs) = hsep [ptext SLIT("__P"),
				       if null tyvars then ptext SLIT("[ ]")
						      else brackets (interppSP tyvars),
					-- The lexer interprets "[]" as a CONID.  Sigh.
				       hsep (map pprParendType tys),
				       ptext SLIT("="),
				       pprIfaceUnfolding rhs
				 ]
    
    ------------  Extra free Ids  --------------
    new_needed_ids = (needed_ids `minusVarSet` unitVarSet id)	`unionVarSet` 
		     extra_ids

    extra_ids | opt_OmitInterfacePragmas = emptyVarSet
	      | otherwise		 = worker_ids	`unionVarSet`
					   unfold_ids	`unionVarSet`
					   spec_ids

    worker_ids | has_worker = unitVarSet work_id
	       | otherwise  = emptyVarSet

    spec_ids = foldr add emptyVarSet spec_list
	     where
	       add (_, _, rhs) = unionVarSet (find_fvs rhs)

    unfold_ids | show_unfold = find_fvs rhs
	       | otherwise   = emptyVarSet

    find_fvs expr = free_vars
		  where
		    free_vars = exprSomeFreeVars interesting expr
		    interesting id = isId id && isLocallyDefined id &&
				     not (omitIfaceSigForId id)
\end{code}

\begin{code}
ifaceBinds :: Handle
	   -> IdSet		-- These Ids are needed already
	   -> [Id]		-- Ids used at code-gen time; they have better pragma info!
	   -> [CoreBind]	-- In dependency order, later depend on earlier
	   -> IO ()

ifaceBinds hdl needed_ids final_ids binds
  = mapIO (printForIface hdl) pretties >>
    hPutStr hdl "\n"
  where
    final_id_map  = listToUFM [(id,id) | id <- final_ids]
    get_idinfo id = case lookupUFM final_id_map id of
			Just id' -> idInfo id'
			Nothing  -> pprTrace "ifaceBinds not found:" (ppr id) $
				    idInfo id

    pretties = go needed_ids (reverse binds)	-- Reverse so that later things will 
						-- provoke earlier ones to be emitted
    go needed [] = if not (isEmptyVarSet needed) then
			pprTrace "ifaceBinds: free vars:" 
				  (sep (map ppr (varSetElems needed))) $
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
	  needed'' = needed' `minusVarSet` mkVarSet (map fst pairs)
		-- Later ones may spuriously cause earlier ones to be "needed" again

    go_rec :: IdSet -> [(Id,CoreExpr)] -> (IdSet, [SDoc])
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

upp_tycon tycon = ifaceTyCon tycon
upp_class clas  = ifaceClass clas
\end{code}


\begin{code}
ifaceTyCon :: TyCon -> SDoc
ifaceTyCon tycon
  | isSynTyCon tycon
  = hsep [ ptext SLIT("type"),
	   ppr (getName tycon),
	   pprTyVarBndrs tyvars,
	   ptext SLIT("="),
	   ppr ty,
	   semi
    ]
  where
    (tyvars, ty) = getSynTyConDefn tycon

ifaceTyCon tycon
  | isAlgTyCon tycon
  = hsep [ ptext keyword,
	   ppr_decl_context (tyConTheta tycon),
	   ppr (getName tycon),
	   pprTyVarBndrs (tyConTyVars tycon),
	   ptext SLIT("="),
	   hsep (punctuate (ptext SLIT(" | ")) (map ppr_con (tyConDataCons tycon))),
	   semi
    ]
  where
    keyword | isNewTyCon tycon = SLIT("newtype")
	    | otherwise	       = SLIT("data")

    tyvars = tyConTyVars tycon

    ppr_con data_con 
	| null field_labels
	= ASSERT( tycon == tycon1 && tyvars == tyvars1 )
	  hsep [  ppr_ex ex_tyvars ex_theta,
		  ppr name,
		  hsep (map ppr_arg_ty (strict_marks `zip` arg_tys))
	        ]

	| otherwise
	= hsep [  ppr_ex ex_tyvars ex_theta,
		  ppr name,
		  braces $ hsep $ punctuate comma (map ppr_field (strict_marks `zip` field_labels))
	 	]
          where
	   (tyvars1, theta1, ex_tyvars, ex_theta, arg_tys, tycon1) = dataConSig data_con
           field_labels   = dataConFieldLabels data_con
           strict_marks   = dataConStrictMarks data_con
	   name           = getName            data_con

    ppr_ex [] ex_theta = ASSERT( null ex_theta ) empty
    ppr_ex ex_tvs ex_theta = ptext SLIT("__forall") <+> brackets (pprTyVarBndrs ex_tvs)
			     <+> pprIfaceTheta ex_theta <+> ptext SLIT("=>")

    ppr_arg_ty (strict_mark, ty) = ppr_strict_mark strict_mark <> pprParendType ty

    ppr_strict_mark NotMarkedStrict = empty
    ppr_strict_mark MarkedStrict    = ptext SLIT("! ")
				-- The extra space helps the lexical analyser that lexes
				-- interface files; it doesn't make the rigid operator/identifier
				-- distinction, so "!a" is a valid identifier so far as it is concerned

    ppr_field (strict_mark, field_label)
	= hsep [ ppr (fieldLabelName field_label),
		  dcolon,
		  ppr_strict_mark strict_mark <> pprParendType (fieldLabelType field_label)
		]

ifaceTyCon tycon
  = pprPanic "pprIfaceTyDecl" (ppr tycon)

ifaceClass clas
  = hsep [ptext SLIT("class"),
	   ppr_decl_context sc_theta,
	   ppr clas,			-- Print the name
	   pprTyVarBndrs clas_tyvars,
	   pp_ops,
	   semi
	  ]
   where
     (clas_tyvars, sc_theta, _, sel_ids, defms) = classBigSig clas

     pp_ops | null sel_ids  = empty
	    | otherwise = hsep [ptext SLIT("where"),
				 braces (hsep (punctuate semi (zipWith ppr_classop sel_ids defms)))
			  ]

     ppr_classop sel_id maybe_defm
	= ASSERT( sel_tyvars == clas_tyvars)
	  hsep [ppr (getOccName sel_id),
		if maybeToBool maybe_defm then equals else empty,
	        dcolon,
		ppr op_ty
	  ]
	where
	  (sel_tyvars, _, op_ty) = splitSigmaTy (idType sel_id)

ppr_decl_context :: ThetaType -> SDoc
ppr_decl_context []    = empty
ppr_decl_context theta = pprIfaceTheta theta <+> ptext SLIT(" =>")

pprIfaceTheta :: ThetaType -> SDoc	-- Use braces rather than parens in interface files
pprIfaceTheta theta =  braces (hsep (punctuate comma [pprConstraint c tys | (c,tys) <- theta]))
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
upp_avail :: AvailInfo -> SDoc
upp_avail NotAvailable      = empty
upp_avail (Avail name)      = pprOccName (getOccName name)
upp_avail (AvailTC name []) = empty
upp_avail (AvailTC name ns) = hcat [pprOccName (getOccName name), bang, upp_export ns']
			    where
			      bang | name `elem` ns = empty
				   | otherwise	    = char '|'
			      ns' = filter (/= name) ns

upp_export :: [Name] -> SDoc
upp_export []    = empty
upp_export names = braces (hsep (map (pprOccName . getOccName) names)) 

upp_fixity :: (Name, Fixity) -> SDoc
upp_fixity (name, fixity) = hsep [ptext SLIT("0"), ppr fixity, ppr name, semi]
	-- Dummy version number!

ppr_unqual_name :: NamedThing a => a -> SDoc		-- Just its occurrence name
ppr_unqual_name name = pprOccName (getOccName name)
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
lt_imp_vers (m1,_,_,_) (m2,_,_,_) = m1 < m2

sort_versions vs = sortLt lt_vers vs

lt_vers :: LocalVersion Name -> LocalVersion Name -> Bool
lt_vers (n1,v1) (n2,v2) = n1 `lt_name` n2
\end{code}


\begin{code}
hPutCol :: Handle 
	-> (a -> SDoc)
	-> [a]
	-> IO ()
hPutCol hdl fmt xs = mapIO (printForIface hdl . fmt) xs

mapIO :: (a -> IO b) -> [a] -> IO ()
mapIO f []     = return ()
mapIO f (x:xs) = f x >> mapIO f xs
\end{code}
