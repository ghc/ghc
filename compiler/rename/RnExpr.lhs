%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnExpr]{Renaming of expressions}

Basically dependency analysis.

Handles @Match@, @GRHSs@, @HsExpr@, and @Qualifier@ datatypes.  In
general, all of these functions return a renamed thing, and a set of
free variables.

\begin{code}
module RnExpr (
	rnLExpr, rnExpr, rnStmts
   ) where

#include "HsVersions.h"

import RnSource  ( rnSrcDecls, rnSplice, checkTH ) 
import RnBinds	 ( rnLocalBindsAndThen, rnValBinds,
		   rnMatchGroup, trimWith ) 
import HsSyn
import RnHsSyn
import TcRnMonad
import RnEnv
import OccName		( plusOccEnv )
import RnNames		( getLocalDeclBinders, extendRdrEnvRn )
import RnTypes		( rnHsTypeFVs, rnLPat, rnOverLit, rnPatsAndThen, rnLit,
			  mkOpFormRn, mkOpAppRn, mkNegAppRn, checkSectionPrec, 
			  dupFieldErr, checkTupSize )
import DynFlags		( DynFlag(..) )
import BasicTypes	( FixityDirection(..) )
import SrcLoc           ( SrcSpan )
import PrelNames	( thFAKE, hasKey, assertIdKey, assertErrorName,
			  loopAName, choiceAName, appAName, arrAName, composeAName, firstAName,
			  negateName, thenMName, bindMName, failMName )
#if defined(GHCI) && defined(BREAKPOINT)
import PrelNames        ( breakpointJumpName, breakpointCondJumpName
                        , undefined_RDR, breakpointIdKey, breakpointCondIdKey )
import UniqFM           ( eltsUFM )
import DynFlags         ( GhcMode(..) )
import Name             ( isTyVarName )
#endif
import Name		( Name, nameOccName, nameIsLocalOrFrom )
import NameSet
import RdrName		( RdrName, emptyGlobalRdrEnv, extendLocalRdrEnv, lookupLocalRdrEnv )
import LoadIface	( loadInterfaceForName )
import UniqFM		( isNullUFM )
import UniqSet		( emptyUniqSet )
import List		( nub )
import Util		( isSingleton )
import ListSetOps	( removeDups )
import Maybes		( expectJust )
import Outputable
import SrcLoc		( Located(..), unLoc, getLoc, cmpLocated )
import FastString

import List		( unzip4 )
\end{code}


%************************************************************************
%*									*
\subsubsection{Expressions}
%*									*
%************************************************************************

\begin{code}
rnExprs :: [LHsExpr RdrName] -> RnM ([LHsExpr Name], FreeVars)
rnExprs ls = rnExprs' ls emptyUniqSet
 where
  rnExprs' [] acc = returnM ([], acc)
  rnExprs' (expr:exprs) acc
   = rnLExpr expr 	        `thenM` \ (expr', fvExpr) ->

	-- Now we do a "seq" on the free vars because typically it's small
	-- or empty, especially in very long lists of constants
    let
	acc' = acc `plusFV` fvExpr
    in
    (grubby_seqNameSet acc' rnExprs') exprs acc'	`thenM` \ (exprs', fvExprs) ->
    returnM (expr':exprs', fvExprs)

-- Grubby little function to do "seq" on namesets; replace by proper seq when GHC can do seq
grubby_seqNameSet ns result | isNullUFM ns = result
			    | otherwise    = result
\end{code}

Variables. We look up the variable and return the resulting name. 

\begin{code}
rnLExpr :: LHsExpr RdrName -> RnM (LHsExpr Name, FreeVars)
rnLExpr = wrapLocFstM rnExpr

rnExpr :: HsExpr RdrName -> RnM (HsExpr Name, FreeVars)

rnExpr (HsVar v)
  = do name           <- lookupOccRn v
       localRdrEnv    <- getLocalRdrEnv
       lclEnv         <- getLclEnv
       ignore_asserts <- doptM Opt_IgnoreAsserts
       ignore_breakpoints <- doptM Opt_IgnoreBreakpoints
       ghcMode        <- getGhcMode
       let conds = [ (name `hasKey` assertIdKey
                      && not ignore_asserts,
                      do (e, fvs) <- mkAssertErrorExpr
                         return (e, fvs `addOneFV` name))
#if defined(GHCI) && defined(BREAKPOINT)
                   , (name `hasKey` breakpointIdKey
                      && not ignore_breakpoints
                      && ghcMode == Interactive,
                         do let isWantedName = not.isTyVarName
                            (e, fvs) <- mkBreakpointExpr (filter isWantedName (eltsUFM localRdrEnv))
                            return (e, fvs `addOneFV` name)
                     )
                   , (name `hasKey` breakpointCondIdKey
                      && not ignore_breakpoints
                      && ghcMode == Interactive,
                         do let isWantedName = not.isTyVarName
                            (e, fvs) <- mkBreakpointCondExpr (filter isWantedName (eltsUFM localRdrEnv))
                            return (e, fvs `addOneFV` name)
                     )
#endif
                   ]
       case lookup True conds of
         Just action -> action
         Nothing     -> return (HsVar name, unitFV name)

rnExpr (HsIPVar v)
  = newIPNameRn v		`thenM` \ name ->
    returnM (HsIPVar name, emptyFVs)

rnExpr (HsLit lit) 
  = rnLit lit		`thenM_`
    returnM (HsLit lit, emptyFVs)

rnExpr (HsOverLit lit) 
  = rnOverLit lit		`thenM` \ (lit', fvs) ->
    returnM (HsOverLit lit', fvs)

rnExpr (HsApp fun arg)
  = rnLExpr fun		`thenM` \ (fun',fvFun) ->
    rnLExpr arg		`thenM` \ (arg',fvArg) ->
    returnM (HsApp fun' arg', fvFun `plusFV` fvArg)

rnExpr (OpApp e1 op _ e2) 
  = rnLExpr e1				`thenM` \ (e1', fv_e1) ->
    rnLExpr e2				`thenM` \ (e2', fv_e2) ->
    rnLExpr op				`thenM` \ (op'@(L _ (HsVar op_name)), fv_op) ->

	-- Deal with fixity
	-- When renaming code synthesised from "deriving" declarations
	-- we used to avoid fixity stuff, but we can't easily tell any
	-- more, so I've removed the test.  Adding HsPars in TcGenDeriv
	-- should prevent bad things happening.
    lookupFixityRn op_name		`thenM` \ fixity ->
    mkOpAppRn e1' op' fixity e2'	`thenM` \ final_e -> 

    returnM (final_e,
	      fv_e1 `plusFV` fv_op `plusFV` fv_e2)

rnExpr (NegApp e _)
  = rnLExpr e			`thenM` \ (e', fv_e) ->
    lookupSyntaxName negateName	`thenM` \ (neg_name, fv_neg) ->
    mkNegAppRn e' neg_name	`thenM` \ final_e ->
    returnM (final_e, fv_e `plusFV` fv_neg)

rnExpr (HsPar e)
  = rnLExpr e 		`thenM` \ (e', fvs_e) ->
    returnM (HsPar e', fvs_e)

-- Template Haskell extensions
-- Don't ifdef-GHCI them because we want to fail gracefully
-- (not with an rnExpr crash) in a stage-1 compiler.
rnExpr e@(HsBracket br_body)
  = checkTH e "bracket"		`thenM_`
    rnBracket br_body		`thenM` \ (body', fvs_e) ->
    returnM (HsBracket body', fvs_e)

rnExpr e@(HsSpliceE splice)
  = rnSplice splice 		`thenM` \ (splice', fvs) ->
    returnM (HsSpliceE splice', fvs)

rnExpr section@(SectionL expr op)
  = rnLExpr expr	 	`thenM` \ (expr', fvs_expr) ->
    rnLExpr op	 		`thenM` \ (op', fvs_op) ->
    checkSectionPrec InfixL section op' expr' `thenM_`
    returnM (SectionL expr' op', fvs_op `plusFV` fvs_expr)

rnExpr section@(SectionR op expr)
  = rnLExpr op	 				`thenM` \ (op',   fvs_op) ->
    rnLExpr expr	 				`thenM` \ (expr', fvs_expr) ->
    checkSectionPrec InfixR section op' expr'	`thenM_`
    returnM (SectionR op' expr', fvs_op `plusFV` fvs_expr)

rnExpr (HsCoreAnn ann expr)
  = rnLExpr expr `thenM` \ (expr', fvs_expr) ->
    returnM (HsCoreAnn ann expr', fvs_expr)

rnExpr (HsSCC lbl expr)
  = rnLExpr expr	 	`thenM` \ (expr', fvs_expr) ->
    returnM (HsSCC lbl expr', fvs_expr)

rnExpr (HsLam matches)
  = rnMatchGroup LambdaExpr matches	`thenM` \ (matches', fvMatch) ->
    returnM (HsLam matches', fvMatch)

rnExpr (HsCase expr matches)
  = rnLExpr expr		 	`thenM` \ (new_expr, e_fvs) ->
    rnMatchGroup CaseAlt matches	`thenM` \ (new_matches, ms_fvs) ->
    returnM (HsCase new_expr new_matches, e_fvs `plusFV` ms_fvs)

rnExpr (HsLet binds expr)
  = rnLocalBindsAndThen binds		$ \ binds' ->
    rnLExpr expr			 `thenM` \ (expr',fvExpr) ->
    returnM (HsLet binds' expr', fvExpr)

rnExpr e@(HsDo do_or_lc stmts body _)
  = do 	{ ((stmts', body'), fvs) <- rnStmts do_or_lc stmts $
				    rnLExpr body
	; return (HsDo do_or_lc stmts' body' placeHolderType, fvs) }

rnExpr (ExplicitList _ exps)
  = rnExprs exps		 	`thenM` \ (exps', fvs) ->
    returnM  (ExplicitList placeHolderType exps', fvs `addOneFV` listTyCon_name)

rnExpr (ExplicitPArr _ exps)
  = rnExprs exps		 	`thenM` \ (exps', fvs) ->
    returnM  (ExplicitPArr placeHolderType exps', fvs)

rnExpr e@(ExplicitTuple exps boxity)
  = checkTupSize tup_size			`thenM_`
    rnExprs exps	 			`thenM` \ (exps', fvs) ->
    returnM (ExplicitTuple exps' boxity, fvs `addOneFV` tycon_name)
  where
    tup_size   = length exps
    tycon_name = tupleTyCon_name boxity tup_size

rnExpr (RecordCon con_id _ rbinds)
  = lookupLocatedOccRn con_id		`thenM` \ conname ->
    rnRbinds "construction" rbinds	`thenM` \ (rbinds', fvRbinds) ->
    returnM (RecordCon conname noPostTcExpr rbinds', 
	     fvRbinds `addOneFV` unLoc conname)

rnExpr (RecordUpd expr rbinds _ _)
  = rnLExpr expr		`thenM` \ (expr', fvExpr) ->
    rnRbinds "update" rbinds	`thenM` \ (rbinds', fvRbinds) ->
    returnM (RecordUpd expr' rbinds' placeHolderType placeHolderType, 
	     fvExpr `plusFV` fvRbinds)

rnExpr (ExprWithTySig expr pty)
  = rnLExpr expr		`thenM` \ (expr', fvExpr) ->
    rnHsTypeFVs doc pty		`thenM` \ (pty', fvTy) ->
    returnM (ExprWithTySig expr' pty', fvExpr `plusFV` fvTy)
  where 
    doc = text "In an expression type signature"

rnExpr (HsIf p b1 b2)
  = rnLExpr p		`thenM` \ (p', fvP) ->
    rnLExpr b1		`thenM` \ (b1', fvB1) ->
    rnLExpr b2		`thenM` \ (b2', fvB2) ->
    returnM (HsIf p' b1' b2', plusFVs [fvP, fvB1, fvB2])

rnExpr (HsType a)
  = rnHsTypeFVs doc a	`thenM` \ (t, fvT) -> 
    returnM (HsType t, fvT)
  where 
    doc = text "In a type argument"

rnExpr (ArithSeq _ seq)
  = rnArithSeq seq	 `thenM` \ (new_seq, fvs) ->
    returnM (ArithSeq noPostTcExpr new_seq, fvs)

rnExpr (PArrSeq _ seq)
  = rnArithSeq seq	 `thenM` \ (new_seq, fvs) ->
    returnM (PArrSeq noPostTcExpr new_seq, fvs)
\end{code}

These three are pattern syntax appearing in expressions.
Since all the symbols are reservedops we can simply reject them.
We return a (bogus) EWildPat in each case.

\begin{code}
rnExpr e@EWildPat      = patSynErr e
rnExpr e@(EAsPat {})   = patSynErr e
rnExpr e@(ELazyPat {}) = patSynErr e
\end{code}

%************************************************************************
%*									*
	Arrow notation
%*									*
%************************************************************************

\begin{code}
rnExpr (HsProc pat body)
  = newArrowScope $
    rnPatsAndThen ProcExpr [pat] $ \ [pat'] ->
    rnCmdTop body	         `thenM` \ (body',fvBody) ->
    returnM (HsProc pat' body', fvBody)

rnExpr (HsArrApp arrow arg _ ho rtl)
  = select_arrow_scope (rnLExpr arrow)	`thenM` \ (arrow',fvArrow) ->
    rnLExpr arg				`thenM` \ (arg',fvArg) ->
    returnM (HsArrApp arrow' arg' placeHolderType ho rtl,
	     fvArrow `plusFV` fvArg)
  where
    select_arrow_scope tc = case ho of
        HsHigherOrderApp -> tc
        HsFirstOrderApp  -> escapeArrowScope tc

-- infix form
rnExpr (HsArrForm op (Just _) [arg1, arg2])
  = escapeArrowScope (rnLExpr op)
			`thenM` \ (op'@(L _ (HsVar op_name)),fv_op) ->
    rnCmdTop arg1	`thenM` \ (arg1',fv_arg1) ->
    rnCmdTop arg2	`thenM` \ (arg2',fv_arg2) ->

	-- Deal with fixity

    lookupFixityRn op_name		`thenM` \ fixity ->
    mkOpFormRn arg1' op' fixity arg2'	`thenM` \ final_e -> 

    returnM (final_e,
	      fv_arg1 `plusFV` fv_op `plusFV` fv_arg2)

rnExpr (HsArrForm op fixity cmds)
  = escapeArrowScope (rnLExpr op)	`thenM` \ (op',fvOp) ->
    rnCmdArgs cmds			`thenM` \ (cmds',fvCmds) ->
    returnM (HsArrForm op' fixity cmds', fvOp `plusFV` fvCmds)

rnExpr other = pprPanic "rnExpr: unexpected expression" (ppr other)
	-- DictApp, DictLam, TyApp, TyLam
\end{code}


%************************************************************************
%*									*
	Arrow commands
%*									*
%************************************************************************

\begin{code}
rnCmdArgs [] = returnM ([], emptyFVs)
rnCmdArgs (arg:args)
  = rnCmdTop arg	`thenM` \ (arg',fvArg) ->
    rnCmdArgs args	`thenM` \ (args',fvArgs) ->
    returnM (arg':args', fvArg `plusFV` fvArgs)


rnCmdTop = wrapLocFstM rnCmdTop'
 where
  rnCmdTop' (HsCmdTop cmd _ _ _) 
   = rnLExpr (convertOpFormsLCmd cmd) `thenM` \ (cmd', fvCmd) ->
     let 
	cmd_names = [arrAName, composeAName, firstAName] ++
		    nameSetToList (methodNamesCmd (unLoc cmd'))
     in
	-- Generate the rebindable syntax for the monad
     lookupSyntaxTable cmd_names	`thenM` \ (cmd_names', cmd_fvs) ->

     returnM (HsCmdTop cmd' [] placeHolderType cmd_names', 
	     fvCmd `plusFV` cmd_fvs)

---------------------------------------------------
-- convert OpApp's in a command context to HsArrForm's

convertOpFormsLCmd :: LHsCmd id -> LHsCmd id
convertOpFormsLCmd = fmap convertOpFormsCmd

convertOpFormsCmd :: HsCmd id -> HsCmd id

convertOpFormsCmd (HsApp c e) = HsApp (convertOpFormsLCmd c) e
convertOpFormsCmd (HsLam match) = HsLam (convertOpFormsMatch match)
convertOpFormsCmd (OpApp c1 op fixity c2)
  = let
	arg1 = L (getLoc c1) $ HsCmdTop (convertOpFormsLCmd c1) [] placeHolderType []
	arg2 = L (getLoc c2) $ HsCmdTop (convertOpFormsLCmd c2) [] placeHolderType []
    in
    HsArrForm op (Just fixity) [arg1, arg2]

convertOpFormsCmd (HsPar c) = HsPar (convertOpFormsLCmd c)

-- gaw 2004
convertOpFormsCmd (HsCase exp matches)
  = HsCase exp (convertOpFormsMatch matches)

convertOpFormsCmd (HsIf exp c1 c2)
  = HsIf exp (convertOpFormsLCmd c1) (convertOpFormsLCmd c2)

convertOpFormsCmd (HsLet binds cmd)
  = HsLet binds (convertOpFormsLCmd cmd)

convertOpFormsCmd (HsDo ctxt stmts body ty)
  = HsDo ctxt (map (fmap convertOpFormsStmt) stmts)
	      (convertOpFormsLCmd body) ty

-- Anything else is unchanged.  This includes HsArrForm (already done),
-- things with no sub-commands, and illegal commands (which will be
-- caught by the type checker)
convertOpFormsCmd c = c

convertOpFormsStmt (BindStmt pat cmd _ _)
  = BindStmt pat (convertOpFormsLCmd cmd) noSyntaxExpr noSyntaxExpr
convertOpFormsStmt (ExprStmt cmd _ _)
  = ExprStmt (convertOpFormsLCmd cmd) noSyntaxExpr placeHolderType
convertOpFormsStmt (RecStmt stmts lvs rvs es binds)
  = RecStmt (map (fmap convertOpFormsStmt) stmts) lvs rvs es binds
convertOpFormsStmt stmt = stmt

convertOpFormsMatch (MatchGroup ms ty)
  = MatchGroup (map (fmap convert) ms) ty
 where convert (Match pat mty grhss)
	  = Match pat mty (convertOpFormsGRHSs grhss)

convertOpFormsGRHSs (GRHSs grhss binds)
  = GRHSs (map convertOpFormsGRHS grhss) binds

convertOpFormsGRHS = fmap convert
 where 
   convert (GRHS stmts cmd) = GRHS stmts (convertOpFormsLCmd cmd)

---------------------------------------------------
type CmdNeeds = FreeVars	-- Only inhabitants are 
				-- 	appAName, choiceAName, loopAName

-- find what methods the Cmd needs (loop, choice, apply)
methodNamesLCmd :: LHsCmd Name -> CmdNeeds
methodNamesLCmd = methodNamesCmd . unLoc

methodNamesCmd :: HsCmd Name -> CmdNeeds

methodNamesCmd cmd@(HsArrApp _arrow _arg _ HsFirstOrderApp _rtl)
  = emptyFVs
methodNamesCmd cmd@(HsArrApp _arrow _arg _ HsHigherOrderApp _rtl)
  = unitFV appAName
methodNamesCmd cmd@(HsArrForm {}) = emptyFVs

methodNamesCmd (HsPar c) = methodNamesLCmd c

methodNamesCmd (HsIf p c1 c2)
  = methodNamesLCmd c1 `plusFV` methodNamesLCmd c2 `addOneFV` choiceAName

methodNamesCmd (HsLet b c) = methodNamesLCmd c

methodNamesCmd (HsDo sc stmts body ty) 
  = methodNamesStmts stmts `plusFV` methodNamesLCmd body

methodNamesCmd (HsApp c e) = methodNamesLCmd c

methodNamesCmd (HsLam match) = methodNamesMatch match

methodNamesCmd (HsCase scrut matches)
  = methodNamesMatch matches `addOneFV` choiceAName

methodNamesCmd other = emptyFVs
   -- Other forms can't occur in commands, but it's not convenient 
   -- to error here so we just do what's convenient.
   -- The type checker will complain later

---------------------------------------------------
methodNamesMatch (MatchGroup ms _)
  = plusFVs (map do_one ms)
 where 
    do_one (L _ (Match pats sig_ty grhss)) = methodNamesGRHSs grhss

-------------------------------------------------
-- gaw 2004
methodNamesGRHSs (GRHSs grhss binds) = plusFVs (map methodNamesGRHS grhss)

-------------------------------------------------
methodNamesGRHS (L _ (GRHS stmts rhs)) = methodNamesLCmd rhs

---------------------------------------------------
methodNamesStmts stmts = plusFVs (map methodNamesLStmt stmts)

---------------------------------------------------
methodNamesLStmt = methodNamesStmt . unLoc

methodNamesStmt (ExprStmt cmd _ _)     = methodNamesLCmd cmd
methodNamesStmt (BindStmt pat cmd _ _) = methodNamesLCmd cmd
methodNamesStmt (RecStmt stmts _ _ _ _)
  = methodNamesStmts stmts `addOneFV` loopAName
methodNamesStmt (LetStmt b)  = emptyFVs
methodNamesStmt (ParStmt ss) = emptyFVs
   -- ParStmt can't occur in commands, but it's not convenient to error 
   -- here so we just do what's convenient
\end{code}


%************************************************************************
%*									*
	Arithmetic sequences
%*									*
%************************************************************************

\begin{code}
rnArithSeq (From expr)
 = rnLExpr expr 	`thenM` \ (expr', fvExpr) ->
   returnM (From expr', fvExpr)

rnArithSeq (FromThen expr1 expr2)
 = rnLExpr expr1 	`thenM` \ (expr1', fvExpr1) ->
   rnLExpr expr2	`thenM` \ (expr2', fvExpr2) ->
   returnM (FromThen expr1' expr2', fvExpr1 `plusFV` fvExpr2)

rnArithSeq (FromTo expr1 expr2)
 = rnLExpr expr1	`thenM` \ (expr1', fvExpr1) ->
   rnLExpr expr2	`thenM` \ (expr2', fvExpr2) ->
   returnM (FromTo expr1' expr2', fvExpr1 `plusFV` fvExpr2)

rnArithSeq (FromThenTo expr1 expr2 expr3)
 = rnLExpr expr1	`thenM` \ (expr1', fvExpr1) ->
   rnLExpr expr2	`thenM` \ (expr2', fvExpr2) ->
   rnLExpr expr3	`thenM` \ (expr3', fvExpr3) ->
   returnM (FromThenTo expr1' expr2' expr3',
	    plusFVs [fvExpr1, fvExpr2, fvExpr3])
\end{code}


%************************************************************************
%*									*
\subsubsection{@Rbinds@s and @Rpats@s: in record expressions}
%*									*
%************************************************************************

\begin{code}
rnRbinds str rbinds 
  = mappM_ field_dup_err dup_fields	`thenM_`
    mapFvRn rn_rbind rbinds		`thenM` \ (rbinds', fvRbind) ->
    returnM (rbinds', fvRbind)
  where
    (_, dup_fields) = removeDups cmpLocated [ f | (f,_) <- rbinds ]

    field_dup_err dups = mappM_ (\f -> addLocErr f (dupFieldErr str)) dups

    rn_rbind (field, expr)
      = lookupLocatedGlobalOccRn field	`thenM` \ fieldname ->
	rnLExpr expr			`thenM` \ (expr', fvExpr) ->
	returnM ((fieldname, expr'), fvExpr `addOneFV` unLoc fieldname)
\end{code}

%************************************************************************
%*									*
	Template Haskell brackets
%*									*
%************************************************************************

\begin{code}
rnBracket (VarBr n) = do { name <- lookupOccRn n
			 ; this_mod <- getModule
			 ; checkM (nameIsLocalOrFrom this_mod name) $	-- Reason: deprecation checking asumes the
			   do { loadInterfaceForName msg name		-- home interface is loaded, and this is the
			      ; return () }				-- only way that is going to happen
			 ; returnM (VarBr name, unitFV name) }
		    where
		      msg = ptext SLIT("Need interface for Template Haskell quoted Name")

rnBracket (ExpBr e) = do { (e', fvs) <- rnLExpr e
			 ; return (ExpBr e', fvs) }
rnBracket (PatBr p) = do { (p', fvs) <- rnLPat p
			 ; return (PatBr p', fvs) }
rnBracket (TypBr t) = do { (t', fvs) <- rnHsTypeFVs doc t
			 ; return (TypBr t', fvs) }
		    where
		      doc = ptext SLIT("In a Template-Haskell quoted type")
rnBracket (DecBr group) 
  = do 	{ gbl_env  <- getGblEnv

	; let gbl_env1 = gbl_env { tcg_mod = thFAKE }
	-- Note the thFAKE.  The top-level names from the bracketed 
	-- declarations will go into the name cache, and we don't want them to 
	-- confuse the Names for the current module.  
	-- By using a pretend module, thFAKE, we keep them safely out of the way.

	; names    <- getLocalDeclBinders gbl_env1 group
	; rdr_env' <- extendRdrEnvRn emptyGlobalRdrEnv names
	-- Furthermore, the names in the bracket shouldn't conflict with
	-- existing top-level names E.g.
	--	foo = 1
	--	bar = [d| foo = 1|]
	-- But both 'foo's get a LocalDef provenance, so we'd get a complaint unless
	-- we start with an emptyGlobalRdrEnv

	; setGblEnv (gbl_env { tcg_rdr_env = tcg_rdr_env gbl_env1 `plusOccEnv` rdr_env',
			       tcg_dus = emptyDUs }) $ do
		-- Notice plusOccEnv, not plusGlobalRdrEnv.  In this situation we want
		-- to *shadow* top-level bindings.  (See the 'foo' example above.)
		-- If we don't shadow, we'll get an ambiguity complaint when we do 
		-- a lookupTopBndrRn (which uses lookupGreLocalRn) on the binder of the 'foo'
		--
		-- Furthermore, arguably if the splice does define foo, that should hide
		-- any foo's further out
		--
		-- The emptyDUs is so that we just collect uses for this group alone

	{ (tcg_env, group') <- rnSrcDecls group
		-- Discard the tcg_env; it contains only extra info about fixity
	; return (DecBr group', allUses (tcg_dus tcg_env)) } }
\end{code}

%************************************************************************
%*									*
\subsubsection{@Stmt@s: in @do@ expressions}
%*									*
%************************************************************************

\begin{code}
rnStmts :: HsStmtContext Name -> [LStmt RdrName] 
	-> RnM (thing, FreeVars)
	-> RnM (([LStmt Name], thing), FreeVars)

rnStmts (MDoExpr _) = rnMDoStmts
rnStmts ctxt        = rnNormalStmts ctxt

rnNormalStmts :: HsStmtContext Name -> [LStmt RdrName]
	      -> RnM (thing, FreeVars)
	      -> RnM (([LStmt Name], thing), FreeVars)	
-- Used for cases *other* than recursive mdo
-- Implements nested scopes

rnNormalStmts ctxt [] thing_inside 
  = do	{ (thing, fvs) <- thing_inside
	; return (([],thing), fvs) } 

rnNormalStmts ctxt (L loc stmt : stmts) thing_inside
  = do	{ ((stmt', (stmts', thing)), fvs) 
		<- rnStmt ctxt stmt 	$
		   rnNormalStmts ctxt stmts thing_inside
	; return (((L loc stmt' : stmts'), thing), fvs) }
    
rnStmt :: HsStmtContext Name -> Stmt RdrName
       -> RnM (thing, FreeVars)
       -> RnM ((Stmt Name, thing), FreeVars)

rnStmt ctxt (ExprStmt expr _ _) thing_inside
  = do	{ (expr', fv_expr) <- rnLExpr expr
	; (then_op, fvs1)  <- lookupSyntaxName thenMName
	; (thing, fvs2)    <- thing_inside
	; return ((ExprStmt expr' then_op placeHolderType, thing),
		  fv_expr `plusFV` fvs1 `plusFV` fvs2) }

rnStmt ctxt (BindStmt pat expr _ _) thing_inside
  = do	{ (expr', fv_expr) <- rnLExpr expr
		-- The binders do not scope over the expression
	; (bind_op, fvs1) <- lookupSyntaxName bindMName
	; (fail_op, fvs2) <- lookupSyntaxName failMName
	; rnPatsAndThen (StmtCtxt ctxt) [pat] $ \ [pat'] -> do
	{ (thing, fvs3) <- thing_inside
	; return ((BindStmt pat' expr' bind_op fail_op, thing),
		  fv_expr `plusFV` fvs1 `plusFV` fvs2 `plusFV` fvs3) }}
	-- fv_expr shouldn't really be filtered by the rnPatsAndThen
	-- but it does not matter because the names are unique

rnStmt ctxt (LetStmt binds) thing_inside
  = do	{ checkErr (ok ctxt binds) 
		   (badIpBinds (ptext SLIT("a parallel list comprehension:")) binds)
	; rnLocalBindsAndThen binds		$ \ binds' -> do
	{ (thing, fvs) <- thing_inside
	; return ((LetStmt binds', thing), fvs) }}
  where
	-- We do not allow implicit-parameter bindings in a parallel
	-- list comprehension.  I'm not sure what it might mean.
    ok (ParStmtCtxt _) (HsIPBinds _) = False
    ok _	       _	     = True

rnStmt ctxt (RecStmt rec_stmts _ _ _ _) thing_inside
  = bindLocatedLocalsRn doc (collectLStmtsBinders rec_stmts)	$ \ bndrs ->
    rn_rec_stmts bndrs rec_stmts	`thenM` \ segs ->
    thing_inside 			`thenM` \ (thing, fvs) ->
    let
	segs_w_fwd_refs     	 = addFwdRefs segs
	(ds, us, fs, rec_stmts') = unzip4 segs_w_fwd_refs
	later_vars = nameSetToList (plusFVs ds `intersectNameSet` fvs)
	fwd_vars   = nameSetToList (plusFVs fs)
	uses	   = plusFVs us
	rec_stmt   = RecStmt rec_stmts' later_vars fwd_vars [] emptyLHsBinds
    in	
    returnM ((rec_stmt, thing), uses `plusFV` fvs)
  where
    doc = text "In a recursive do statement"

rnStmt ctxt (ParStmt segs) thing_inside
  = do	{ opt_GlasgowExts <- doptM Opt_GlasgowExts
	; checkM opt_GlasgowExts parStmtErr
 	; orig_lcl_env <- getLocalRdrEnv
	; ((segs',thing), fvs) <- go orig_lcl_env [] segs
	; return ((ParStmt segs', thing), fvs) }
  where
--  type ParSeg id = [([LStmt id], [id])]
--  go :: NameSet -> [ParSeg RdrName]
--       -> RnM (([ParSeg Name], thing), FreeVars)

    go orig_lcl_env bndrs [] 
	= do { let { (bndrs', dups) = removeDups cmpByOcc bndrs
		   ; inner_env = extendLocalRdrEnv orig_lcl_env bndrs' }
	     ; mappM dupErr dups
	     ; (thing, fvs) <- setLocalRdrEnv inner_env thing_inside
	     ; return (([], thing), fvs) }

    go orig_lcl_env bndrs_so_far ((stmts, _) : segs)
	= do { ((stmts', (bndrs, segs', thing)), fvs)
		  <- rnNormalStmts par_ctxt stmts $ do
		     { 	-- Find the Names that are bound by stmts
		       lcl_env <- getLocalRdrEnv
		     ; let { rdr_bndrs = collectLStmtsBinders stmts
		     	   ; bndrs = map ( expectJust "rnStmt"
		     		         . lookupLocalRdrEnv lcl_env
		     		         . unLoc) rdr_bndrs
		           ; new_bndrs = nub bndrs ++ bndrs_so_far 
				-- The nub is because there might be shadowing
				--	x <- e1; x <- e2
				-- So we'll look up (Unqual x) twice, getting
				-- the second binding both times, which is the
			}	-- one we want

			-- Typecheck the thing inside, passing on all
			-- the Names bound, but separately; revert the envt
		     ; ((segs', thing), fvs) <- setLocalRdrEnv orig_lcl_env $
						go orig_lcl_env new_bndrs segs

			-- Figure out which of the bound names are used
		     ; let used_bndrs = filter (`elemNameSet` fvs) bndrs
		     ; return ((used_bndrs, segs', thing), fvs) }

	     ; let seg' = (stmts', bndrs)
	     ; return (((seg':segs'), thing), 
		       delListFromNameSet fvs bndrs) }

    par_ctxt = ParStmtCtxt ctxt

    cmpByOcc n1 n2 = nameOccName n1 `compare` nameOccName n2
    dupErr vs = addErr (ptext SLIT("Duplicate binding in parallel list comprehension for:")
		        <+> quotes (ppr (head vs)))
\end{code}


%************************************************************************
%*									*
\subsubsection{mdo expressions}
%*									*
%************************************************************************

\begin{code}
type FwdRefs = NameSet
type Segment stmts = (Defs,
		      Uses, 	-- May include defs
		      FwdRefs,	-- A subset of uses that are 
				--   (a) used before they are bound in this segment, or 
				--   (b) used here, and bound in subsequent segments
		      stmts)	-- Either Stmt or [Stmt]


----------------------------------------------------
rnMDoStmts :: [LStmt RdrName]
	   -> RnM (thing, FreeVars)
	   -> RnM (([LStmt Name], thing), FreeVars)	
rnMDoStmts stmts thing_inside
  = 	-- Step1: bring all the binders of the mdo into scope
	-- Remember that this also removes the binders from the
	-- finally-returned free-vars
    bindLocatedLocalsRn doc (collectLStmtsBinders stmts)	$ \ bndrs ->
    do	{ 
	-- Step 2: Rename each individual stmt, making a
	--	   singleton segment.  At this stage the FwdRefs field
	--	   isn't finished: it's empty for all except a BindStmt
	--	   for which it's the fwd refs within the bind itself
	-- 	   (This set may not be empty, because we're in a recursive 
	--	    context.)
	  segs <- rn_rec_stmts bndrs stmts

	; (thing, fvs_later) <- thing_inside

	; let
	-- Step 3: Fill in the fwd refs.
	-- 	   The segments are all singletons, but their fwd-ref
	--	   field mentions all the things used by the segment
	--	   that are bound after their use
	    segs_w_fwd_refs = addFwdRefs segs

	-- Step 4: Group together the segments to make bigger segments
	--	   Invariant: in the result, no segment uses a variable
	--	   	      bound in a later segment
	    grouped_segs = glomSegments segs_w_fwd_refs

	-- Step 5: Turn the segments into Stmts
	--	   Use RecStmt when and only when there are fwd refs
	--	   Also gather up the uses from the end towards the
	--	   start, so we can tell the RecStmt which things are
	--	   used 'after' the RecStmt
	    (stmts', fvs) = segsToStmts grouped_segs fvs_later

	; return ((stmts', thing), fvs) }
  where
    doc = text "In a recursive mdo-expression"

---------------------------------------------
rn_rec_stmts :: [Name] -> [LStmt RdrName] -> RnM [Segment (LStmt Name)]
rn_rec_stmts bndrs stmts = mappM (rn_rec_stmt bndrs) stmts	`thenM` \ segs_s ->
		   	   returnM (concat segs_s)

----------------------------------------------------
rn_rec_stmt :: [Name] -> LStmt RdrName -> RnM [Segment (LStmt Name)]
	-- Rename a Stmt that is inside a RecStmt (or mdo)
	-- Assumes all binders are already in scope
	-- Turns each stmt into a singleton Stmt

rn_rec_stmt all_bndrs (L loc (ExprStmt expr _ _))
  = rnLExpr expr 		`thenM` \ (expr', fvs) ->
    lookupSyntaxName thenMName	`thenM` \ (then_op, fvs1) ->
    returnM [(emptyNameSet, fvs `plusFV` fvs1, emptyNameSet,
	      L loc (ExprStmt expr' then_op placeHolderType))]

rn_rec_stmt all_bndrs (L loc (BindStmt pat expr _ _))
  = rnLExpr expr		`thenM` \ (expr', fv_expr) ->
    rnLPat pat			`thenM` \ (pat', fv_pat) ->
    lookupSyntaxName bindMName	`thenM` \ (bind_op, fvs1) ->
    lookupSyntaxName failMName	`thenM` \ (fail_op, fvs2) ->
    let
	bndrs = mkNameSet (collectPatBinders pat')
	fvs   = fv_expr `plusFV` fv_pat `plusFV` fvs1 `plusFV` fvs2
    in
    returnM [(bndrs, fvs, bndrs `intersectNameSet` fvs,
	      L loc (BindStmt pat' expr' bind_op fail_op))]

rn_rec_stmt all_bndrs (L loc (LetStmt binds@(HsIPBinds _)))
  = do	{ addErr (badIpBinds (ptext SLIT("an mdo expression")) binds)
	; failM }

rn_rec_stmt all_bndrs (L loc (LetStmt (HsValBinds binds)))
  = rnValBinds (trimWith all_bndrs) binds	`thenM` \ (binds', du_binds) ->
    returnM [(duDefs du_binds, duUses du_binds, 
	      emptyNameSet, L loc (LetStmt (HsValBinds binds')))]

rn_rec_stmt all_bndrs (L loc (RecStmt stmts _ _ _ _))	-- Flatten Rec inside Rec
  = rn_rec_stmts all_bndrs stmts

rn_rec_stmt all_bndrs stmt@(L _ (ParStmt _))	-- Syntactically illegal in mdo
  = pprPanic "rn_rec_stmt" (ppr stmt)

---------------------------------------------
addFwdRefs :: [Segment a] -> [Segment a]
-- So far the segments only have forward refs *within* the Stmt
-- 	(which happens for bind:  x <- ...x...)
-- This function adds the cross-seg fwd ref info

addFwdRefs pairs 
  = fst (foldr mk_seg ([], emptyNameSet) pairs)
  where
    mk_seg (defs, uses, fwds, stmts) (segs, later_defs)
	= (new_seg : segs, all_defs)
	where
	  new_seg = (defs, uses, new_fwds, stmts)
	  all_defs = later_defs `unionNameSets` defs
	  new_fwds = fwds `unionNameSets` (uses `intersectNameSet` later_defs)
		-- Add the downstream fwd refs here

----------------------------------------------------
-- 	Glomming the singleton segments of an mdo into 
--	minimal recursive groups.
--
-- At first I thought this was just strongly connected components, but
-- there's an important constraint: the order of the stmts must not change.
--
-- Consider
--	mdo { x <- ...y...
--	      p <- z
--	      y <- ...x...
--	      q <- x
--	      z <- y
--	      r <- x }
--
-- Here, the first stmt mention 'y', which is bound in the third.  
-- But that means that the innocent second stmt (p <- z) gets caught
-- up in the recursion.  And that in turn means that the binding for
-- 'z' has to be included... and so on.
--
-- Start at the tail { r <- x }
-- Now add the next one { z <- y ; r <- x }
-- Now add one more     { q <- x ; z <- y ; r <- x }
-- Now one more... but this time we have to group a bunch into rec
--	{ rec { y <- ...x... ; q <- x ; z <- y } ; r <- x }
-- Now one more, which we can add on without a rec
--	{ p <- z ; 
--	  rec { y <- ...x... ; q <- x ; z <- y } ; 
-- 	  r <- x }
-- Finally we add the last one; since it mentions y we have to
-- glom it togeher with the first two groups
--	{ rec { x <- ...y...; p <- z ; y <- ...x... ; 
--		q <- x ; z <- y } ; 
-- 	  r <- x }

glomSegments :: [Segment (LStmt Name)] -> [Segment [LStmt Name]]

glomSegments [] = []
glomSegments ((defs,uses,fwds,stmt) : segs)
	-- Actually stmts will always be a singleton
  = (seg_defs, seg_uses, seg_fwds, seg_stmts)  : others
  where
    segs'	     = glomSegments segs
    (extras, others) = grab uses segs'
    (ds, us, fs, ss) = unzip4 extras
    
    seg_defs  = plusFVs ds `plusFV` defs
    seg_uses  = plusFVs us `plusFV` uses
    seg_fwds  = plusFVs fs `plusFV` fwds
    seg_stmts = stmt : concat ss

    grab :: NameSet	 	-- The client
	 -> [Segment a]
	 -> ([Segment a],	-- Needed by the 'client'
	     [Segment a])	-- Not needed by the client
	-- The result is simply a split of the input
    grab uses dus 
	= (reverse yeses, reverse noes)
	where
	  (noes, yeses) 	  = span not_needed (reverse dus)
	  not_needed (defs,_,_,_) = not (intersectsNameSet defs uses)


----------------------------------------------------
segsToStmts :: [Segment [LStmt Name]] 
	    -> FreeVars			-- Free vars used 'later'
	    -> ([LStmt Name], FreeVars)

segsToStmts [] fvs_later = ([], fvs_later)
segsToStmts ((defs, uses, fwds, ss) : segs) fvs_later
  = ASSERT( not (null ss) )
    (new_stmt : later_stmts, later_uses `plusFV` uses)
  where
    (later_stmts, later_uses) = segsToStmts segs fvs_later
    new_stmt | non_rec	 = head ss
	     | otherwise = L (getLoc (head ss)) $ 
			   RecStmt ss (nameSetToList used_later) (nameSetToList fwds) 
				      [] emptyLHsBinds
	     where
	       non_rec    = isSingleton ss && isEmptyNameSet fwds
	       used_later = defs `intersectNameSet` later_uses
				-- The ones needed after the RecStmt
\end{code}

%************************************************************************
%*									*
\subsubsection{breakpoint utils}
%*									*
%************************************************************************

\begin{code}
#if defined(GHCI) && defined(BREAKPOINT)
mkBreakpointExpr :: [Name] -> RnM (HsExpr Name, FreeVars)
mkBreakpointExpr = mkBreakpointExpr' breakpointJumpName

mkBreakpointCondExpr :: [Name] -> RnM (HsExpr Name, FreeVars)
mkBreakpointCondExpr = mkBreakpointExpr' breakpointCondJumpName

mkBreakpointExpr' :: Name -> [Name] -> RnM (HsExpr Name, FreeVars)
mkBreakpointExpr' breakpointFunc scope
    = do sloc <- getSrcSpanM
         undef <- lookupOccRn undefined_RDR
         let inLoc = L sloc
             lHsApp x y = inLoc (HsApp x y)
             mkExpr fnName args = mkExpr' fnName (reverse args)
             mkExpr' fnName [] = inLoc (HsVar fnName)
             mkExpr' fnName (arg:args)
                 = lHsApp (mkExpr' fnName args) (inLoc arg)
             expr = unLoc $ mkExpr breakpointFunc [mkScopeArg scope, HsVar undef, msg]
             mkScopeArg args = unLoc $ mkExpr undef (map HsVar args)
             msg = srcSpanLit sloc
         return (expr, emptyFVs)
#endif

srcSpanLit :: SrcSpan -> HsExpr Name
srcSpanLit span = HsLit (HsString (mkFastString (showSDoc (ppr span))))

srcSpanPrimLit :: SrcSpan -> HsExpr Name
srcSpanPrimLit span = HsLit (HsStringPrim (mkFastString (showSDoc (ppr span))))
\end{code}

%************************************************************************
%*									*
\subsubsection{Assertion utils}
%*									*
%************************************************************************

\begin{code}
mkAssertErrorExpr :: RnM (HsExpr Name, FreeVars)
-- Return an expression for (assertError "Foo.hs:27")
mkAssertErrorExpr
  = getSrcSpanM    			`thenM` \ sloc ->
    let
	expr = HsApp (L sloc (HsVar assertErrorName)) 
		     (L sloc (srcSpanPrimLit sloc))
    in
    returnM (expr, emptyFVs)
\end{code}

%************************************************************************
%*									*
\subsubsection{Errors}
%*									*
%************************************************************************

\begin{code}
patSynErr e = do { addErr (sep [ptext SLIT("Pattern syntax in expression context:"),
			 	nest 4 (ppr e)])
		 ; return (EWildPat, emptyFVs) }

parStmtErr = addErr (ptext SLIT("Illegal parallel list comprehension: use -fglasgow-exts"))

badIpBinds what binds
  = hang (ptext SLIT("Implicit-parameter bindings illegal in") <+> what)
	 2 (ppr binds)
\end{code}
