%
% (c) The GRASP Project, Glasgow University, 1992-1995
%
\section[Rename2]{Second renaming pass: boil down to non-duplicated info}

\begin{code}
#include "HsVersions.h"

module Rename2 (
	rnModule2,

	-- for completeness
	Module, Bag, ProtoNamePat(..), InPat,
	PprStyle, Pretty(..), PrettyRep, ProtoName
    ) where

IMPORT_Trace		-- ToDo: rm (debugging)
import Pretty
import Outputable

import AbsSyn
import Errors		( dupNamesErr, Error(..) )
import HsCore		-- ****** NEED TO SEE CONSTRUCTORS ******
import HsPragmas	-- ****** NEED TO SEE CONSTRUCTORS ******
import HsTypes		( cmpMonoType, pprParendMonoType )
import IdInfo		( DeforestInfo(..) )
import Maybes		( Maybe(..) )
import ProtoName
import RenameMonad12
import SrcLoc		( mkUnknownSrcLoc, SrcLoc )
import Util
\end{code}

This pass removes duplicate declarations.  Duplicates can arise when
two imported interface have a signature (or whatever) for the same
thing.	We check that the two are consistent and then drop one.

For preference, if one is declared in this module and the other is
imported, we keep the former; in the case of an instance decl or type
decl, the local version has a lot more information which we must not
lose!

Similarly, if one has interesting pragmas and one has not, we keep the
former.

The notion of ``duplicate'' includes an imported signature and a
binding in this module.	 In this case, the signature is discarded.
See note below about how this should be improved.

ToDo: There are still known cases in which we blithely consider two
declarations to be ``duplicates'' and we then select one of them, {\em
without} actually checking that they contain the same information!
[WDP 93/8/16] [Improved, at least WDP 93/08/26]

\begin{code}
rnModule2  :: ProtoNameModule -> Rn12M ProtoNameModule

rnModule2 (Module mod_name exports imports fixes
	    ty_decls absty_sigs class_decls inst_decls specinst_sigs
	    defaults binds int_sigs src_loc)

  = uniquefy mod_name cmpFix selFix fixes
				`thenRn12` \ fixes ->

    uniquefy mod_name cmpTys selTys ty_decls
				`thenRn12` \ ty_decls ->

    uniquefy mod_name cmpTySigs selTySigs absty_sigs
				`thenRn12` \ absty_sigs ->

    uniquefy mod_name cmpClassDecl selClass class_decls
				`thenRn12` \ class_decls ->

    uniquefy mod_name cmpInst selInst inst_decls
				`thenRn12` \ inst_decls ->

    uniquefy mod_name cmpSpecInstSigs selSpecInstSigs specinst_sigs
				`thenRn12` \ specinst_sigs ->

	-- From the imported signatures discard any which are for
	-- variables bound in this module.
	-- But, be wary of those that *clash* with those for this
	-- module...
	-- Note that we want to do this properly later (ToDo) because imported
	-- signatures may differ from those declared in the module itself.

    rm_sigs_for_here mod_name int_sigs
				`thenRn12` \ non_here_int_sigs ->

    uniquefy mod_name cmpSig selSig non_here_int_sigs 
				 `thenRn12` \ int_sigs ->
    returnRn12
	(Module mod_name
		exports	-- export and import lists are passed along
		imports	-- for checking in Rename3; no other reason
		fixes
		ty_decls
		absty_sigs
		class_decls
		inst_decls
		specinst_sigs
		defaults
		binds
		int_sigs
		src_loc)
  where
    top_level_binders = collectTopLevelBinders binds

    rm_sigs_for_here :: FAST_STRING -> [ProtoNameSig] -> Rn12M [ProtoNameSig]
	-- NB: operates only on interface signatures, so don't
	-- need to worry about user-pragmas, etc.

    rm_sigs_for_here mod_name [] = returnRn12 []

    rm_sigs_for_here mod_name (sig@(Sig name _ _ src_loc) : more_sigs)
      = rm_sigs_for_here mod_name more_sigs `thenRn12` \ rest_sigs ->

	if  not (name `elemByLocalNames` top_level_binders) then -- no name clash...
	    returnRn12 (sig : rest_sigs)

	else -- name clash...
	    if	name `elemProtoNames` top_level_binders
	     && name_for_this_module name then
		-- the very same thing; just drop it
		returnRn12 rest_sigs
	    else
		-- a different thing with the same name (due to renaming?)
		-- ToDo: locations need improving
		report_dup "(renamed?) variable"
			name src_loc name mkUnknownSrcLoc
			rest_sigs
      where
	 name_for_this_module (Imp m _ _ _) = m == mod_name
	 name_for_this_module other	    = True
\end{code}

%************************************************************************
%*									*
\subsection[FixityDecls-Rename2]{Functions for @FixityDecls@}
%*									*
%************************************************************************

\begin{code}
cmpFix :: ProtoNameFixityDecl -> ProtoNameFixityDecl -> TAG_

cmpFix (InfixL n1 i1) (InfixL n2 i2) = n1 `cmpProtoName` n2
cmpFix (InfixL n1 i1) other	     = LT_
cmpFix (InfixR n1 i1) (InfixR n2 i2) = n1 `cmpProtoName` n2
cmpFix (InfixR n1 i1) (InfixN n2 i2) = LT_
cmpFix (InfixN n1 i1) (InfixN n2 i2) = n1 `cmpProtoName` n2
cmpFix a	      b		     = GT_
\end{code}

We are pretty un-fussy about which FixityDecl we keep.

\begin{code}
selFix :: ProtoNameFixityDecl -> ProtoNameFixityDecl -> Rn12M ProtoNameFixityDecl
selFix f1 f2 = returnRn12 f1
\end{code}

%************************************************************************
%*									*
\subsection[TyDecls-Rename2]{Functions for @TyDecls@}
%*									*
%************************************************************************

\begin{code}
cmpTys :: ProtoNameTyDecl -> ProtoNameTyDecl -> TAG_

cmpTys (TyData _ n1 _ _ _ _ _) (TyData	_ n2 _ _ _ _ _) = cmpProtoName n1 n2
cmpTys (TyData _ n1 _ _ _ _ _) other			= LT_
cmpTys (TySynonym n1 _ _ _ _)  (TySynonym n2 _ _ _ _)	= cmpProtoName n1 n2
cmpTys a		       b			= GT_
\end{code}

\begin{code}
selTys :: ProtoNameTyDecl -> ProtoNameTyDecl
       -> Rn12M ProtoNameTyDecl

-- Note: we could check these more closely.
-- NB: It would be a mistake to cross-check derivings,
-- because we don't preserve those in interfaces.

selTys td1@(TyData c name1 tvs cons1 ds pragmas1 locn1)
       td2@(TyData _ name2 _   cons2 _	pragmas2 locn2)
  = selByBetterName "algebraic datatype"
       name1 pragmas1 locn1 td1
       name2 pragmas2 locn2 td2
       (\ p -> TyData c name1 tvs cons1 ds p locn1)
       chooser_TyData

selTys ts1@(TySynonym name1 tvs expand1 pragmas1 locn1)
       ts2@(TySynonym name2 _	expand2 pragmas2 locn2)
  = selByBetterName "type synonym"
	name1 pragmas1 locn1 ts1
	name2 pragmas2 locn2 ts2
	(\ p -> TySynonym name1 tvs expand1 p locn1)
	chooser_TySynonym
\end{code}

If only one is ``abstract'' (no condecls), we take the other.

Next, we check that they don't have differing lists of data
constructors (what a disaster if those get through...); then we do a
similar thing using pragmatic info.

\begin{code}
chooser_TyData wout pragmas1 locn1 td1@(TyData _ name1 _ cons1 _ _ _)
		    pragmas2 locn2 td2@(TyData _ name2 _ cons2 _ _ _)
  = let
	td1_abstract = null cons1
	td2_abstract = null cons2

	choose_by_pragmas = sub_chooser pragmas1 pragmas2
    in
    if td1_abstract && td2_abstract then
	choose_by_pragmas

    else if td1_abstract then
	returnRn12 td2

    else if td2_abstract then
	returnRn12 td1

    else if not (eqConDecls cons1 cons2) then
    	report_dup "algebraic datatype (mismatched data constuctors)"
    		    name1 locn1 name2 locn2 td1
    else
	sub_chooser pragmas1 pragmas2
  where
    sub_chooser (DataPragmas [] []) b = returnRn12 (wout b)
    sub_chooser a (DataPragmas [] []) = returnRn12 (wout a)
    sub_chooser a@(DataPragmas cons1 specs1) (DataPragmas cons2 specs2)
      = if not (eqConDecls cons1 cons2) then
	    pprTrace "Mismatched info in DATA pragmas:\n"
		     (ppAbove (ppr PprDebug cons1) (ppr PprDebug cons2)) (
	    returnRn12 (wout (DataPragmas [] []))
	    )
	else if not (eq_data_specs specs1 specs2) then
	    pprTrace "Mismatched specialisation info in DATA pragmas:\n"
		     (ppAbove (ppr_data_specs specs1) (ppr_data_specs specs2)) (
	    returnRn12 (wout (DataPragmas [] []))
	    )
	else
	    returnRn12 (wout a)  -- same, pick one

    -- ToDo: Should we use selByBetterName ???
    -- ToDo: Report errors properly and recover quietly ???

    -- ToDo: Should we merge specialisations ???

    eq_data_specs [] [] = True
    eq_data_specs (spec1:specs1) (spec2:specs2)
      = eq_spec spec1 spec2 && eq_data_specs specs1 specs2
    eq_data_specs _  _  = False

    eq_spec spec1 spec2 = case cmp_spec spec1 spec2 of { EQ_ -> True; _ -> False}

    ppr_data_specs specs
      = ppBesides [ppStr "_SPECIALISE_ ", pp_the_list [
	  ppCat [ppLbrack, ppInterleave ppComma (map pp_maybe ty_maybes), ppRbrack]
	  | ty_maybes <- specs ]]
         
    pp_the_list [p]    = p
    pp_the_list (p:ps) = ppAbove (ppBeside p ppComma) (pp_the_list ps)

    pp_maybe Nothing   = pp_NONE
    pp_maybe (Just ty) = pprParendMonoType PprDebug ty

    pp_NONE = ppStr "_N_"
\end{code}

Sort of similar deal on synonyms: this is the time to check that the
expansions are really the same; otherwise, we use the pragmas.

\begin{code}
chooser_TySynonym wout pragmas1 locn1 ts1@(TySynonym name1 _ expand1 _ _)
		       pragmas2 locn2 ts2@(TySynonym name2 _ expand2 _ _)
  = if not (eqMonoType expand1 expand2) then
    	report_dup "type synonym" name1 locn1 name2 locn2 ts1
    else
	sub_chooser pragmas1 pragmas2
  where
    sub_chooser NoTypePragmas b = returnRn12 (wout b)
    sub_chooser a NoTypePragmas = returnRn12 (wout a)
    sub_chooser a _ 	    	= returnRn12 (wout a) -- same, just pick one
\end{code}

%************************************************************************
%*									*
\subsection[DataTypeSigs-Rename2]{Functions for @DataTypeSigs@}
%*									*
%************************************************************************

\begin{code}
cmpTySigs :: ProtoNameDataTypeSig -> ProtoNameDataTypeSig -> TAG_

cmpTySigs (AbstractTypeSig n1 _) (AbstractTypeSig n2 _)
  = cmpProtoName n1 n2
cmpTySigs (SpecDataSig n1 ty1 _) (SpecDataSig n2 ty2 _)
  = case cmpProtoName n1 n2 of
	EQ_   -> LT_   -- multiple SPECIALIZE data pragmas allowed
	other -> other
cmpTySigs (AbstractTypeSig n1 _) (SpecDataSig n2 _ _)
  = LT_
cmpTySigs (SpecDataSig n1 _ _) (AbstractTypeSig n2 _)
  = GT_

selTySigs :: ProtoNameDataTypeSig
	  -> ProtoNameDataTypeSig
	  -> Rn12M ProtoNameDataTypeSig

selTySigs s1@(AbstractTypeSig n1 locn1) s2@(AbstractTypeSig n2 locn2)
  = selByBetterName "ABSTRACT user-pragma"
	n1 bottom locn1 s1
	n2 bottom locn2 s2
	bottom bottom
  where
    bottom = panic "Rename2:selTySigs:AbstractTypeSig"

selTySigs s1@(SpecDataSig n1 ty1 locn1) s2@(SpecDataSig n2 ty2 locn2)
  = selByBetterName "ABSTRACT user-pragma"
	n1 bottom locn1 s1
	n2 bottom locn2 s2
	bottom bottom
  where
    bottom = panic "Rename2:selTySigs:SpecDataSig"
\end{code}

%************************************************************************
%*									*
\subsection[ClassDecl-Rename2]{Functions for @ClassDecls@}
%*									*
%************************************************************************

\begin{code}
cmpClassDecl :: ProtoNameClassDecl -> ProtoNameClassDecl -> TAG_

cmpClassDecl (ClassDecl _ n1 _ _ _ _ _) (ClassDecl _ n2 _ _ _ _ _)
  = cmpProtoName n1 n2

selClass  :: ProtoNameClassDecl -> ProtoNameClassDecl
	  -> Rn12M ProtoNameClassDecl

selClass cd1@(ClassDecl ctxt n1 tv sigs bs pragmas1 locn1)
	 cd2@(ClassDecl _    n2 _  _	_  pragmas2 locn2)
  = selByBetterName "class"
	n1 pragmas1 locn1 cd1
	n2 pragmas2 locn2 cd2
	(\ p -> ClassDecl ctxt n1 tv sigs bs p locn1)
	chooser_Class
\end{code}

\begin{code}
chooser_Class wout NoClassPragmas   _ _ b		_ _ = returnRn12 (wout b)
chooser_Class wout a		    _ _ NoClassPragmas	_ _ = returnRn12 (wout a)

chooser_Class wout sd1@(SuperDictPragmas gs1) l1 _ sd2@(SuperDictPragmas gs2) l2 _
  = if length gs1 /= length gs2 then	-- urgh
       returnRn12 (wout NoClassPragmas)
    else
	recoverQuietlyRn12 [{-no gen prags-}] (
	    zipWithRn12 choose_prag gs1 gs2
	)			`thenRn12` \ new_gprags ->
	returnRn12 (wout (
	    if null new_gprags then
		pprTrace "tossed all SuperDictPragmas (rename2):"
			 (ppAbove (ppr PprDebug sd1) (ppr PprDebug sd2))
		NoClassPragmas
	    else
		SuperDictPragmas new_gprags
	))
  where
    choose_prag g1 g2 = selGenPragmas g1 l1 g2 l2
\end{code}

%************************************************************************
%*									*
\subsection[InstDecls-Rename2]{Functions for @InstDecls@}
%*									*
%************************************************************************

\begin{code}
cmpInst :: ProtoNameInstDecl -> ProtoNameInstDecl -> TAG_

cmpInst (InstDecl _ c1 ty1 _ _ _ _ _ _ _) (InstDecl _ c2 ty2 _ _ _ _ _ _ _)
  = case cmpProtoName c1 c2 of
      EQ_   -> cmpInstanceTypes ty1 ty2
      other -> other
\end{code}

Select the instance declaration from the module (rather than an
interface), if it exists.

\begin{code}
selInst :: ProtoNameInstDecl -> ProtoNameInstDecl
	-> Rn12M ProtoNameInstDecl

selInst i1@(InstDecl ctxt c ty bs from_here1 orig_mod1 infor_mod1 uprags pragmas1 locn1)
	i2@(InstDecl _	  _ _  _  from_here2 orig_mod2 infor_mod2 _      pragmas2 locn2)
  = let
	have_orig_mod1 = not (_NULL_ orig_mod1)
	have_orig_mod2 = not (_NULL_ orig_mod2)

	choose_no1 = returnRn12 i1
	choose_no2 = returnRn12 i2
    in
	-- generally: try to keep the locally-defined instance decl

    if from_here1 && from_here2 then
	-- If they are both from this module, don't throw either away,
	-- otherwise we silently discard erroneous duplicates
	trace ("selInst: duplicate instance in this module (ToDo: msg!)")
	choose_no1

    else if from_here1 then
	if ( have_orig_mod1 && have_orig_mod2 && orig_mod1 /= orig_mod2 ) then
	    trace ("selInst: instance in this module also defined somewhere else! (ToDo: msg!)")
	    choose_no1
	else
	    choose_no1

    else if from_here2 then
	if ( have_orig_mod1 && have_orig_mod2 && orig_mod1 /= orig_mod2 ) then
	    trace ("selInst: instance in this module also defined somewhere else! (ToDo: msg!)")
	    choose_no2
	else
	    choose_no2

    else -- it's definitely an imported instance;
	 -- first, a quick sanity check...
	if ( have_orig_mod1 && have_orig_mod2 && orig_mod1 /= orig_mod2 ) then
	    trace ("selInst: `same' instances coming in from two modules! (ToDo: msg!)")
	    choose_no2 -- arbitrary
	else
	    -- now we *cheat*: so we can use the "informing module" stuff
	    -- in "selByBetterName", we *make up* some ProtoNames for
	    -- these instance decls
	    let
		ii = SLIT("!*INSTANCE*!")
		n1 = Imp orig_mod1 ii [infor_mod1] ii
		n2 = Imp orig_mod2 ii [infor_mod2] ii
	    in
	    selByBetterName "instance"
		n1 pragmas1 locn1 i1
		n2 pragmas2 locn2 i2
	        (\ p -> InstDecl ctxt c ty bs from_here1 orig_mod1 infor_mod1
			[{-none-}] p locn1)
		chooser_Inst
\end{code}

\begin{code}
chooser_Inst wout iprags1 loc1 i1 iprags2 loc2 i2
  = chk_pragmas iprags1 iprags2
  where
	-- easy cases:
    chk_pragmas NoInstancePragmas b = returnRn12 (wout b)
    chk_pragmas a NoInstancePragmas = returnRn12 (wout a)

	-- SimpleInstance pragmas meet: choose by GenPragmas
    chk_pragmas (SimpleInstancePragma gprags1) (SimpleInstancePragma gprags2)
      = recoverQuietlyRn12 NoGenPragmas (
	    selGenPragmas gprags1 loc1 gprags2 loc2
	)				`thenRn12` \ new_prags ->
	returnRn12 (wout (
	    case new_prags of
	      NoGenPragmas -> NoInstancePragmas	-- bottled out
	      _ -> SimpleInstancePragma new_prags
	))

	-- SimpleInstance pragma meets anything else... take the "else"
    chk_pragmas (SimpleInstancePragma _) b = returnRn12 (wout b)
    chk_pragmas a (SimpleInstancePragma _) = returnRn12 (wout a)

    chk_pragmas (ConstantInstancePragma gp1 prs1) (ConstantInstancePragma gp2 prs2)
      = recoverQuietlyRn12 NoGenPragmas (
	    selGenPragmas gp1 loc1 gp2 loc2
	)			`thenRn12` \ dfun_prags ->

	recoverQuietlyRn12 [] (
	    selNamePragmaPairs prs1 loc1 prs2 loc2
	)			`thenRn12` \ new_pairs ->

	returnRn12 (wout (
	    if null new_pairs then -- bottled out
		case dfun_prags of
		  NoGenPragmas -> NoInstancePragmas -- doubly bottled out
		  _ -> SimpleInstancePragma dfun_prags
	    else
		ConstantInstancePragma dfun_prags new_pairs
	))

	-- SpecialisedInstancePragmas: choose by gens, then specialisations
    chk_pragmas a@(SpecialisedInstancePragma _ _) (SpecialisedInstancePragma _ _)
      = trace "not checking two SpecialisedInstancePragma pragmas!" (returnRn12 (wout a))

    chk_pragmas other1 other2  -- oops, bad mismatch
      = pRAGMA_ERROR "instance pragmas" (wout other1) -- ToDo: msg
\end{code}

%************************************************************************
%*									*
\subsection[SpecInstSigs-Rename2]{Functions for @AbstractTypeSigs@}
%*									*
%************************************************************************

We don't make any effort to look for duplicate ``SPECIALIZE instance''
pragmas. (Later??)

We do this by make \tr{cmp*} always return \tr{LT_}---then there's
nothing for \tr{sel*} to do!

\begin{code}
cmpSpecInstSigs
	:: ProtoNameSpecialisedInstanceSig -> ProtoNameSpecialisedInstanceSig -> TAG_
selSpecInstSigs :: ProtoNameSpecialisedInstanceSig
		-> ProtoNameSpecialisedInstanceSig
		-> Rn12M ProtoNameSpecialisedInstanceSig

cmpSpecInstSigs	a b = LT_
selSpecInstSigs a b = panic "Rename2:selSpecInstSigs"
\end{code}

%************************************************************************
%*									*
\subsection{Functions for SigDecls}
%*									*
%************************************************************************

These \tr{*Sig} functions only operate on things from interfaces, so
we don't have to worry about user-pragmas and other such junk.

\begin{code}
cmpSig :: ProtoNameSig -> ProtoNameSig -> TAG_

cmpSig (Sig n1 _ _ _) (Sig n2 _ _ _) = cmpProtoName n1 n2

-- avoid BUG (ToDo)
cmpSig _ _ = case (panic "cmpSig (rename2)") of { s -> -- should never happen
	     cmpSig s s }

selSig :: ProtoNameSig -> ProtoNameSig -> Rn12M ProtoNameSig

selSig s1@(Sig n1 ty pragmas1 locn1) s2@(Sig n2 _ pragmas2 locn2)
  = selByBetterName "type signature"
	n1 pragmas1 locn1 s1
	n2 pragmas2 locn2 s2
	(\ p -> Sig n1 ty p locn1) -- w/out its pragmas
	chooser_Sig
\end{code}

\begin{code}
chooser_Sig wout_prags g1 l1 s1@(Sig n1 ty1 _ _) g2 l2 s2@(Sig n2 ty2 _ _)
  = case (cmpPolyType cmpProtoName ty1 ty2) of
      EQ_ ->
	recoverQuietlyRn12 NoGenPragmas (
	    selGenPragmas g1 l1 g2 l2
	)			`thenRn12` \ new_prags ->
	returnRn12 (wout_prags new_prags)
      _ -> report_dup "signature" n1 l1 n2 l2 s1
\end{code}

%************************************************************************
%*									*
\subsection{Help functions: selecting based on pragmas}
%*									*
%************************************************************************

\begin{code}
selGenPragmas
	:: ProtoNameGenPragmas -> SrcLoc
	-> ProtoNameGenPragmas -> SrcLoc
	-> Rn12M ProtoNameGenPragmas

selGenPragmas NoGenPragmas _ b	          _ = returnRn12 b
selGenPragmas a		   _ NoGenPragmas _ = returnRn12 a

selGenPragmas g1@(GenPragmas arity1 upd1 def1 strict1 unfold1 specs1) locn1
	      g2@(GenPragmas arity2 upd2 def2 strict2 unfold2 specs2) locn2

  = sel_arity  arity1  arity2	`thenRn12` \ arity  ->
    sel_upd    upd1    upd2	`thenRn12` \ upd    ->
    sel_def    def1    def2     `thenRn12` \ def    ->
    sel_strict strict1 strict2	`thenRn12` \ strict ->
    sel_unfold unfold1 unfold2	`thenRn12` \ unfold ->
    sel_specs  specs1  specs2	`thenRn12` \ specs  ->
    returnRn12 (GenPragmas arity upd def strict unfold specs)
  where
    sel_arity Nothing     Nothing   = returnRn12 Nothing
    sel_arity a@(Just a1) (Just a2) = if a1 == a2
				      then returnRn12 a
				      else pRAGMA_ERROR "arity pragmas" a
    sel_arity a		  _	    = pRAGMA_ERROR "arity pragmas" a

    -------
    sel_upd Nothing   	Nothing   = returnRn12 Nothing
    sel_upd a@(Just u1) (Just u2) = if u1 == u2
				    then returnRn12 a
				    else pRAGMA_ERROR "update pragmas" a
    sel_upd a	    	_	  = pRAGMA_ERROR "update pragmas" a

    -------
    sel_def Don'tDeforest Don'tDeforest = returnRn12 Don'tDeforest
    sel_def DoDeforest    DoDeforest    = returnRn12 DoDeforest
    sel_def a             _             = pRAGMA_ERROR "deforest pragmas" a

    ----------
    sel_unfold NoImpUnfolding b	    	     = returnRn12 b
    sel_unfold a	      NoImpUnfolding = returnRn12 a

    sel_unfold a@(ImpUnfolding _ c1) (ImpUnfolding _ c2)
      = if c1 `eqUfExpr` c2 -- very paranoid (and rightly so)
	then returnRn12 a
	else pprTrace "mismatched unfoldings:\n" (ppAbove (ppr PprDebug c1) (ppr PprDebug c2)) (
	     returnRn12 NoImpUnfolding
	     )

    sel_unfold a@(ImpMagicUnfolding b) (ImpMagicUnfolding c)
      = if b == c then returnRn12 a else pRAGMA_ERROR "magic unfolding" a

    sel_unfold a _ = pRAGMA_ERROR "unfolding pragmas" a

    ----------
    sel_strict NoImpStrictness NoImpStrictness = returnRn12 NoImpStrictness

    sel_strict a@(ImpStrictness b1 i1 g1) (ImpStrictness b2 i2 g2)
      = if b1 /= b2 || i1 /= i2
        then pRAGMA_ERROR "strictness pragmas" a
	else recoverQuietlyRn12 NoGenPragmas (
		selGenPragmas g1 locn1 g2 locn2
	     )	`thenRn12` \ wrkr_prags ->
	     returnRn12 (ImpStrictness b1 i1 wrkr_prags)

    sel_strict a _ = pRAGMA_ERROR "strictness pragmas" a

    ---------
    sel_specs specs1 specs2
      = selSpecialisations specs1 locn1 specs2 locn2
\end{code}

\begin{code}
selNamePragmaPairs
	:: [(ProtoName, ProtoNameGenPragmas)] -> SrcLoc
	-> [(ProtoName, ProtoNameGenPragmas)] -> SrcLoc
	-> Rn12M [(ProtoName, ProtoNameGenPragmas)]

selNamePragmaPairs [] _ [] _ = returnRn12 []
selNamePragmaPairs [] _ bs _ = returnRn12 bs
selNamePragmaPairs as _ [] _ = returnRn12 as

selNamePragmaPairs ((name1, prags1) : pairs1) loc1
		   ((name2, prags2) : pairs2) loc2

  = if not (name1 `eqProtoName` name2) then
	-- msg of any kind??? ToDo
	pRAGMA_ERROR "named pragmas" pairs1
    else
    	selGenPragmas prags1 loc1 prags2 loc2	    `thenRn12` \ new_prags ->
    	selNamePragmaPairs pairs1 loc1 pairs2 loc2  `thenRn12` \ rest ->
	returnRn12 ( (name1, new_prags) : rest )
\end{code}

For specialisations we merge the lists from each Sig. This allows the user to
declare specialised prelude functions in their own PreludeSpec module.

\begin{code}
selSpecialisations
	:: [([Maybe ProtoNameMonoType], Int, ProtoNameGenPragmas)] -> SrcLoc
	-> [([Maybe ProtoNameMonoType], Int, ProtoNameGenPragmas)] -> SrcLoc
	-> Rn12M [([Maybe ProtoNameMonoType], Int, ProtoNameGenPragmas)]

selSpecialisations [] _ [] _ = returnRn12 []
selSpecialisations [] _ bs _ = returnRn12 bs -- arguable ... ToDo?
selSpecialisations as _ [] _ = returnRn12 as -- ditto

selSpecialisations all_specs1@((spec1, dicts1, prags1) : rest_specs1) loc1
		   all_specs2@((spec2, dicts2, prags2) : rest_specs2) loc2

  = case (cmp_spec spec1 spec2) of
	 LT_ -> selSpecialisations rest_specs1 loc1 all_specs2 loc2
					`thenRn12` \ rest ->
    		returnRn12 ( (spec1, dicts1, prags1) : rest )

	 EQ_ -> ASSERT(dicts1 == dicts2)
	       	recoverQuietlyRn12 NoGenPragmas (
	    	    selGenPragmas prags1 loc1 prags2 loc2
	        )			`thenRn12` \ new_prags ->
    	        selSpecialisations rest_specs1 loc1 rest_specs2 loc2
					`thenRn12` \ rest ->
    		returnRn12 ( (spec1, dicts1, new_prags) : rest )

	 GT_ -> selSpecialisations all_specs1 loc1 rest_specs2 loc2
					`thenRn12` \ rest ->
    		returnRn12 ( (spec2, dicts2, prags2) : rest )

cmp_spec [] []			   = EQ_
cmp_spec (Nothing:xs) (Nothing:ys) = cmp_spec xs ys
cmp_spec (Just t1:xs) (Just t2:ys) = case cmpMonoType cmpProtoName t1 t2 of
					EQ_ -> cmp_spec xs ys
					xxx -> xxx
cmp_spec (Nothing:xs) (Just t2:ys) = LT_
cmp_spec (Just t1:xs) (Nothing:ys) = GT_
\end{code}

%************************************************************************
%*									*
\subsection{Help functions: @uniquefy@ and @selByBetterName@}
%*									*
%************************************************************************

\begin{code}
uniquefy :: FAST_STRING			-- Module name
	 -> (a -> a -> TAG_)		-- Comparison function
	 -> (a -> a -> Rn12M a)		-- Selection function
	 -> [a]				-- Things to be processed
	 -> Rn12M [a]			-- Processed things

uniquefy mod cmp sel things
  = mapRn12 (check_group_consistency sel) grouped_things
  where
    grouped_things = equivClasses cmp things

    check_group_consistency :: (a -> a -> Rn12M a)	-- Selection function
			    -> [a]			-- things to be compared
			    -> Rn12M a

    check_group_consistency sel []	       = panic "Rename2: runs produced an empty list"
    check_group_consistency sel (thing:things) = foldrRn12 sel thing things
\end{code}

@selByBetterName@: There are two ways one thing can have a ``better
name'' than another.

First: Something with an @Unk@ name is declared in this module, so we
keep that, rather than something from an interface (with an @Imp@
name, probably).

Second: If we have two non-@Unk@ names, but one ``informant module''
is also the {\em original} module for the entity, then we choose that
one.  I.e., if one interface says, ``I am the module that created this
thing'' then we believe it and take that one.

If we can't figure out which one to choose by the names, we use the
info provided to select based on the pragmas.

LATER: but surely we have to worry about different-by-original-name
things which are same-by-local-name things---these should be reported
as errors.

\begin{code}
selByBetterName :: String   -- class/datatype/synonym (for error msg)

		-- 1st/2nd comparee name/pragmas + their things
		-> ProtoName -> pragmas -> SrcLoc -> thing
		-> ProtoName -> pragmas -> SrcLoc -> thing

		-- a thing without its pragmas
		-> (pragmas -> thing)

		-- choose-by-pragma function
		-> ((pragmas -> thing)		    -- thing minus its pragmas
		    -> pragmas -> SrcLoc -> thing   -- comparee 1
		    -> pragmas -> SrcLoc -> thing   -- comparee 2
		    -> Rn12M thing )	    	    -- thing w/ its new pragmas

		-> Rn12M thing		-- selected thing

selByBetterName dup_msg
		pn1 pragmas1 locn1 thing1
		pn2 pragmas2 locn2 thing2
		thing_wout_pragmas
		chooser
  = getModuleNameRn12	`thenRn12` \ mod_name ->
    let
	choose_thing1	= chk_eq (returnRn12 thing1)
	choose_thing2   = chk_eq (returnRn12 thing2)
	check_n_choose  = chk_eq (chooser thing_wout_pragmas
					  pragmas1 locn1 thing1
					  pragmas2 locn2 thing2)

	dup_error = report_dup dup_msg pn1 locn1 pn2 locn2 thing1
    in
    case pn1 of
      Unk _  -> case pn2 of
		 Unk _	-> dup_error
		 _	-> if orig_modules_clash mod_name pn2
			    then dup_error
			    else choose_thing1

      Prel _ -> case pn2 of
		 Unk _	-> if orig_modules_clash mod_name pn1
			   then dup_error
			   else choose_thing2
		 _	-> check_n_choose

      Imp om1 _ im1 _ -> -- we're gonna check `informant module' info...
	case pn2 of
	  Unk _		  -> if orig_modules_clash mod_name pn1
			     then dup_error
			     else choose_thing2
	  Prel _	  -> check_n_choose
	  Imp om2 _ im2 _
	    -> let
		   is_elem = isIn "selByBetterName"

		   name1_claims_orig = om1 `is_elem` im1 && not (_NULL_ om1)
		   name2_claims_orig = om2 `is_elem` im2 && not (_NULL_ om2)
	       in
	       if name1_claims_orig
	       then if name2_claims_orig then check_n_choose else choose_thing1
	       else if name2_claims_orig then choose_thing2  else check_n_choose
  where
    chk_eq if_OK
      = if not (eqProtoName pn1 pn2) && eqByLocalName pn1 pn2
	then report_dup dup_msg pn1 locn1 pn2 locn2 thing1
	else if_OK

    orig_modules_clash this_module pn
      = case (getOrigName pn) of { (that_module, _) ->
	not (this_module == that_module) }

report_dup dup_msg pn1 locn1 pn2 locn2 thing
  = addErrRn12 err_msg `thenRn12` \ _ ->
    returnRn12 thing
  where
    err_msg = dupNamesErr dup_msg [(pn1,locn1), (pn2,locn2)]

pRAGMA_ERROR :: String -> a -> Rn12M a
pRAGMA_ERROR msg x
  = addErrRn12 (\ sty -> ppStr ("PRAGMA ERROR:"++msg)) `thenRn12` \ _ ->
    returnRn12 x
\end{code}
