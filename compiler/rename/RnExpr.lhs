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

#ifdef GHCI
import {-# SOURCE #-} TcSplice( runQuasiQuoteExpr )
#endif 	/* GHCI */

import RnSource  ( rnSrcDecls )
import RnBinds   ( rnLocalBindsAndThen, rnValBindsLHS, rnValBindsRHS,
                   rnMatchGroup, makeMiniFixityEnv) 
import HsSyn
import TcRnMonad
import TcEnv		( thRnBrack )
import RnEnv
import RnTypes		( rnHsTypeFVs, rnSplice, checkTH,
			  mkOpFormRn, mkOpAppRn, mkNegAppRn, checkSectionPrec)
import RnPat
import DynFlags		( DynFlag(..) )
import BasicTypes	( FixityDirection(..) )
import PrelNames

import Name
import NameSet
import RdrName
import LoadIface	( loadInterfaceForName )
import UniqSet
import Data.List
import Util		( isSingleton )
import ListSetOps	( removeDups )
import Maybes		( expectJust )
import Outputable
import SrcLoc
import FastString
import Control.Monad
\end{code}


\begin{code}
-- XXX
thenM :: Monad a => a b -> (b -> a c) -> a c
thenM = (>>=)

thenM_ :: Monad a => a b -> a c -> a c
thenM_ = (>>)
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
  rnExprs' [] acc = return ([], acc)
  rnExprs' (expr:exprs) acc
   = rnLExpr expr 	        `thenM` \ (expr', fvExpr) ->

	-- Now we do a "seq" on the free vars because typically it's small
	-- or empty, especially in very long lists of constants
    let
	acc' = acc `plusFV` fvExpr
    in
    acc' `seq` rnExprs' exprs acc' `thenM` \ (exprs', fvExprs) ->
    return (expr':exprs', fvExprs)
\end{code}

Variables. We look up the variable and return the resulting name. 

\begin{code}
rnLExpr :: LHsExpr RdrName -> RnM (LHsExpr Name, FreeVars)
rnLExpr = wrapLocFstM rnExpr

rnExpr :: HsExpr RdrName -> RnM (HsExpr Name, FreeVars)

finishHsVar :: Name -> RnM (HsExpr Name, FreeVars)
-- Separated from rnExpr because it's also used
-- when renaming infix expressions
-- See Note [Adding the implicit parameter to 'assert']
finishHsVar name 
 = do { ignore_asserts <- doptM Opt_IgnoreAsserts
      ; if ignore_asserts || not (name `hasKey` assertIdKey)
	then return (HsVar name, unitFV name)
	else do { e <- mkAssertErrorExpr
		; return (e, unitFV name) } }

rnExpr (HsVar v)
  = do name <- lookupOccRn v
       finishHsVar name

rnExpr (HsIPVar v)
  = newIPNameRn v		`thenM` \ name ->
    return (HsIPVar name, emptyFVs)

rnExpr (HsLit lit@(HsString s))
  = do {
         opt_OverloadedStrings <- doptM Opt_OverloadedStrings
       ; if opt_OverloadedStrings then
            rnExpr (HsOverLit (mkHsIsString s placeHolderType))
	 else -- Same as below
	    rnLit lit		`thenM_`
            return (HsLit lit, emptyFVs)
       }

rnExpr (HsLit lit) 
  = rnLit lit		`thenM_`
    return (HsLit lit, emptyFVs)

rnExpr (HsOverLit lit) 
  = rnOverLit lit		`thenM` \ (lit', fvs) ->
    return (HsOverLit lit', fvs)

rnExpr (HsApp fun arg)
  = rnLExpr fun		`thenM` \ (fun',fvFun) ->
    rnLExpr arg		`thenM` \ (arg',fvArg) ->
    return (HsApp fun' arg', fvFun `plusFV` fvArg)

rnExpr (OpApp e1 (L op_loc (HsVar op_rdr)) _ e2) 
  = do	{ (e1', fv_e1) <- rnLExpr e1
	; (e2', fv_e2) <- rnLExpr e2
	; op_name <- setSrcSpan op_loc (lookupOccRn op_rdr)
  	; (op', fv_op) <- finishHsVar op_name
		-- NB: op' is usually just a variable, but might be
		--     an applicatoin (assert "Foo.hs:47")
	-- Deal with fixity
	-- When renaming code synthesised from "deriving" declarations
	-- we used to avoid fixity stuff, but we can't easily tell any
	-- more, so I've removed the test.  Adding HsPars in TcGenDeriv
	-- should prevent bad things happening.
	; fixity <- lookupFixityRn op_name
	; final_e <- mkOpAppRn e1' (L op_loc op') fixity e2'
	; return (final_e, fv_e1 `plusFV` fv_op `plusFV` fv_e2) }

rnExpr (NegApp e _)
  = rnLExpr e			`thenM` \ (e', fv_e) ->
    lookupSyntaxName negateName	`thenM` \ (neg_name, fv_neg) ->
    mkNegAppRn e' neg_name	`thenM` \ final_e ->
    return (final_e, fv_e `plusFV` fv_neg)

------------------------------------------
-- Template Haskell extensions
-- Don't ifdef-GHCI them because we want to fail gracefully
-- (not with an rnExpr crash) in a stage-1 compiler.
rnExpr e@(HsBracket br_body)
  = checkTH e "bracket"		`thenM_`
    rnBracket br_body		`thenM` \ (body', fvs_e) ->
    return (HsBracket body', fvs_e)

rnExpr (HsSpliceE splice)
  = rnSplice splice 		`thenM` \ (splice', fvs) ->
    return (HsSpliceE splice', fvs)

#ifndef GHCI
rnExpr e@(HsQuasiQuoteE _) = pprPanic "Cant do quasiquotation without GHCi" (ppr e)
#else
rnExpr (HsQuasiQuoteE qq)
  = rnQuasiQuote qq 		`thenM` \ (qq', fvs_qq) ->
    runQuasiQuoteExpr qq'	`thenM` \ (L _ expr') ->
    rnExpr expr'		`thenM` \ (expr'', fvs_expr) ->
    return (expr'', fvs_qq `plusFV` fvs_expr)
#endif 	/* GHCI */

---------------------------------------------
--	Sections
-- See Note [Parsing sections] in Parser.y.pp
rnExpr (HsPar (L loc (section@(SectionL {}))))
  = do	{ (section', fvs) <- rnSection section
	; return (HsPar (L loc section'), fvs) }

rnExpr (HsPar (L loc (section@(SectionR {}))))
  = do	{ (section', fvs) <- rnSection section
	; return (HsPar (L loc section'), fvs) }

rnExpr (HsPar e)
  = do	{ (e', fvs_e) <- rnLExpr e
	; return (HsPar e', fvs_e) }

rnExpr expr@(SectionL {})
  = do	{ addErr (sectionErr expr); rnSection expr }
rnExpr expr@(SectionR {})
  = do	{ addErr (sectionErr expr); rnSection expr }

---------------------------------------------
rnExpr (HsCoreAnn ann expr)
  = rnLExpr expr `thenM` \ (expr', fvs_expr) ->
    return (HsCoreAnn ann expr', fvs_expr)

rnExpr (HsSCC lbl expr)
  = rnLExpr expr	 	`thenM` \ (expr', fvs_expr) ->
    return (HsSCC lbl expr', fvs_expr)
rnExpr (HsTickPragma info expr)
  = rnLExpr expr	 	`thenM` \ (expr', fvs_expr) ->
    return (HsTickPragma info expr', fvs_expr)

rnExpr (HsLam matches)
  = rnMatchGroup LambdaExpr matches	`thenM` \ (matches', fvMatch) ->
    return (HsLam matches', fvMatch)

rnExpr (HsCase expr matches)
  = rnLExpr expr		 	`thenM` \ (new_expr, e_fvs) ->
    rnMatchGroup CaseAlt matches	`thenM` \ (new_matches, ms_fvs) ->
    return (HsCase new_expr new_matches, e_fvs `plusFV` ms_fvs)

rnExpr (HsLet binds expr)
  = rnLocalBindsAndThen binds		$ \ binds' ->
    rnLExpr expr			 `thenM` \ (expr',fvExpr) ->
    return (HsLet binds' expr', fvExpr)

rnExpr (HsDo do_or_lc stmts body _)
  = do 	{ ((stmts', body'), fvs) <- rnStmts do_or_lc stmts $
				    rnLExpr body
	; return (HsDo do_or_lc stmts' body' placeHolderType, fvs) }

rnExpr (ExplicitList _ exps)
  = rnExprs exps		 	`thenM` \ (exps', fvs) ->
    return  (ExplicitList placeHolderType exps', fvs)

rnExpr (ExplicitPArr _ exps)
  = rnExprs exps		 	`thenM` \ (exps', fvs) ->
    return  (ExplicitPArr placeHolderType exps', fvs)

rnExpr (ExplicitTuple tup_args boxity)
  = do { checkTupleSection tup_args
       ; checkTupSize (length tup_args)
       ; (tup_args', fvs) <- mapAndUnzipM rnTupArg tup_args
       ; return (ExplicitTuple tup_args' boxity, plusFVs fvs) }
  where
    rnTupArg (Present e) = do { (e',fvs) <- rnLExpr e; return (Present e', fvs) }
    rnTupArg (Missing _) = return (Missing placeHolderType, emptyFVs)

rnExpr (RecordCon con_id _ rbinds)
  = do	{ conname <- lookupLocatedOccRn con_id
	; (rbinds', fvRbinds) <- rnHsRecBinds (HsRecFieldCon (unLoc conname)) rbinds
	; return (RecordCon conname noPostTcExpr rbinds', 
		  fvRbinds `addOneFV` unLoc conname) }

rnExpr (RecordUpd expr rbinds _ _ _)
  = do	{ (expr', fvExpr) <- rnLExpr expr
	; (rbinds', fvRbinds) <- rnHsRecBinds HsRecFieldUpd rbinds
	; return (RecordUpd expr' rbinds' [] [] [], 
		  fvExpr `plusFV` fvRbinds) }

rnExpr (ExprWithTySig expr pty)
  = do	{ (pty', fvTy) <- rnHsTypeFVs doc pty
	; (expr', fvExpr) <- bindSigTyVarsFV (hsExplicitTvs pty') $
		  	     rnLExpr expr
	; return (ExprWithTySig expr' pty', fvExpr `plusFV` fvTy) }
  where 
    doc = text "In an expression type signature"

rnExpr (HsIf p b1 b2)
  = rnLExpr p		`thenM` \ (p', fvP) ->
    rnLExpr b1		`thenM` \ (b1', fvB1) ->
    rnLExpr b2		`thenM` \ (b2', fvB2) ->
    return (HsIf p' b1' b2', plusFVs [fvP, fvB1, fvB2])

rnExpr (HsType a)
  = rnHsTypeFVs doc a	`thenM` \ (t, fvT) -> 
    return (HsType t, fvT)
  where 
    doc = text "In a type argument"

rnExpr (ArithSeq _ seq)
  = rnArithSeq seq	 `thenM` \ (new_seq, fvs) ->
    return (ArithSeq noPostTcExpr new_seq, fvs)

rnExpr (PArrSeq _ seq)
  = rnArithSeq seq	 `thenM` \ (new_seq, fvs) ->
    return (PArrSeq noPostTcExpr new_seq, fvs)
\end{code}

These three are pattern syntax appearing in expressions.
Since all the symbols are reservedops we can simply reject them.
We return a (bogus) EWildPat in each case.

\begin{code}
rnExpr e@EWildPat      = patSynErr e
rnExpr e@(EAsPat {})   = patSynErr e
rnExpr e@(EViewPat {}) = patSynErr e
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
    rnPats ProcExpr [pat] $ \ [pat'] ->
    rnCmdTop body	         `thenM` \ (body',fvBody) ->
    return (HsProc pat' body', fvBody)

rnExpr (HsArrApp arrow arg _ ho rtl)
  = select_arrow_scope (rnLExpr arrow)	`thenM` \ (arrow',fvArrow) ->
    rnLExpr arg				`thenM` \ (arg',fvArg) ->
    return (HsArrApp arrow' arg' placeHolderType ho rtl,
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

    return (final_e,
	      fv_arg1 `plusFV` fv_op `plusFV` fv_arg2)

rnExpr (HsArrForm op fixity cmds)
  = escapeArrowScope (rnLExpr op)	`thenM` \ (op',fvOp) ->
    rnCmdArgs cmds			`thenM` \ (cmds',fvCmds) ->
    return (HsArrForm op' fixity cmds', fvOp `plusFV` fvCmds)

rnExpr other = pprPanic "rnExpr: unexpected expression" (ppr other)
	-- HsWrap

----------------------
-- See Note [Parsing sections] in Parser.y.pp
rnSection :: HsExpr RdrName -> RnM (HsExpr Name, FreeVars)
rnSection section@(SectionR op expr)
  = do	{ (op', fvs_op)     <- rnLExpr op
	; (expr', fvs_expr) <- rnLExpr expr
	; checkSectionPrec InfixR section op' expr'
	; return (SectionR op' expr', fvs_op `plusFV` fvs_expr) }

rnSection section@(SectionL expr op)
  = do	{ (expr', fvs_expr) <- rnLExpr expr
	; (op', fvs_op)     <- rnLExpr op
	; checkSectionPrec InfixL section op' expr'
	; return (SectionL expr' op', fvs_op `plusFV` fvs_expr) }

rnSection other = pprPanic "rnSection" (ppr other)
\end{code}

%************************************************************************
%*									*
	Records
%*									*
%************************************************************************

\begin{code}
rnHsRecBinds :: HsRecFieldContext -> HsRecordBinds RdrName
             -> RnM (HsRecordBinds Name, FreeVars)
rnHsRecBinds ctxt rec_binds@(HsRecFields { rec_dotdot = dd })
  = do { (flds, fvs) <- rnHsRecFields1 ctxt HsVar rec_binds
       ; (flds', fvss) <- mapAndUnzipM rn_field flds
       ; return (HsRecFields { rec_flds = flds', rec_dotdot = dd }, 
                 fvs `plusFV` plusFVs fvss) }
  where 
    rn_field fld = do { (arg', fvs) <- rnLExpr (hsRecFieldArg fld)
                      ; return (fld { hsRecFieldArg = arg' }, fvs) }
\end{code}


%************************************************************************
%*									*
	Arrow commands
%*									*
%************************************************************************

\begin{code}
rnCmdArgs :: [LHsCmdTop RdrName] -> RnM ([LHsCmdTop Name], FreeVars)
rnCmdArgs [] = return ([], emptyFVs)
rnCmdArgs (arg:args)
  = rnCmdTop arg	`thenM` \ (arg',fvArg) ->
    rnCmdArgs args	`thenM` \ (args',fvArgs) ->
    return (arg':args', fvArg `plusFV` fvArgs)

rnCmdTop :: LHsCmdTop RdrName -> RnM (LHsCmdTop Name, FreeVars)
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

     return (HsCmdTop cmd' [] placeHolderType cmd_names', 
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

convertOpFormsStmt :: StmtLR id id -> StmtLR id id
convertOpFormsStmt (BindStmt pat cmd _ _)
  = BindStmt pat (convertOpFormsLCmd cmd) noSyntaxExpr noSyntaxExpr
convertOpFormsStmt (ExprStmt cmd _ _)
  = ExprStmt (convertOpFormsLCmd cmd) noSyntaxExpr placeHolderType
convertOpFormsStmt stmt@(RecStmt { recS_stmts = stmts })
  = stmt { recS_stmts = map (fmap convertOpFormsStmt) stmts }
convertOpFormsStmt stmt = stmt

convertOpFormsMatch :: MatchGroup id -> MatchGroup id
convertOpFormsMatch (MatchGroup ms ty)
  = MatchGroup (map (fmap convert) ms) ty
 where convert (Match pat mty grhss)
	  = Match pat mty (convertOpFormsGRHSs grhss)

convertOpFormsGRHSs :: GRHSs id -> GRHSs id
convertOpFormsGRHSs (GRHSs grhss binds)
  = GRHSs (map convertOpFormsGRHS grhss) binds

convertOpFormsGRHS :: Located (GRHS id) -> Located (GRHS id)
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

methodNamesCmd (HsArrApp _arrow _arg _ HsFirstOrderApp _rtl)
  = emptyFVs
methodNamesCmd (HsArrApp _arrow _arg _ HsHigherOrderApp _rtl)
  = unitFV appAName
methodNamesCmd (HsArrForm {}) = emptyFVs

methodNamesCmd (HsPar c) = methodNamesLCmd c

methodNamesCmd (HsIf _ c1 c2)
  = methodNamesLCmd c1 `plusFV` methodNamesLCmd c2 `addOneFV` choiceAName

methodNamesCmd (HsLet _ c) = methodNamesLCmd c

methodNamesCmd (HsDo _ stmts body _) 
  = methodNamesStmts stmts `plusFV` methodNamesLCmd body

methodNamesCmd (HsApp c _) = methodNamesLCmd c

methodNamesCmd (HsLam match) = methodNamesMatch match

methodNamesCmd (HsCase _ matches)
  = methodNamesMatch matches `addOneFV` choiceAName

methodNamesCmd _ = emptyFVs
   -- Other forms can't occur in commands, but it's not convenient 
   -- to error here so we just do what's convenient.
   -- The type checker will complain later

---------------------------------------------------
methodNamesMatch :: MatchGroup Name -> FreeVars
methodNamesMatch (MatchGroup ms _)
  = plusFVs (map do_one ms)
 where 
    do_one (L _ (Match _ _ grhss)) = methodNamesGRHSs grhss

-------------------------------------------------
-- gaw 2004
methodNamesGRHSs :: GRHSs Name -> FreeVars
methodNamesGRHSs (GRHSs grhss _) = plusFVs (map methodNamesGRHS grhss)

-------------------------------------------------

methodNamesGRHS :: Located (GRHS Name) -> CmdNeeds
methodNamesGRHS (L _ (GRHS _ rhs)) = methodNamesLCmd rhs

---------------------------------------------------
methodNamesStmts :: [Located (StmtLR Name Name)] -> FreeVars
methodNamesStmts stmts = plusFVs (map methodNamesLStmt stmts)

---------------------------------------------------
methodNamesLStmt :: Located (StmtLR Name Name) -> FreeVars
methodNamesLStmt = methodNamesStmt . unLoc

methodNamesStmt :: StmtLR Name Name -> FreeVars
methodNamesStmt (ExprStmt cmd _ _)               = methodNamesLCmd cmd
methodNamesStmt (BindStmt _ cmd _ _)             = methodNamesLCmd cmd
methodNamesStmt (RecStmt { recS_stmts = stmts }) = methodNamesStmts stmts `addOneFV` loopAName
methodNamesStmt (LetStmt _)                      = emptyFVs
methodNamesStmt (ParStmt _)                      = emptyFVs
methodNamesStmt (TransformStmt _ _ _)            = emptyFVs
methodNamesStmt (GroupStmt _ _)                  = emptyFVs
   -- ParStmt, TransformStmt and GroupStmt can't occur in commands, but it's not convenient to error 
   -- here so we just do what's convenient
\end{code}


%************************************************************************
%*									*
	Arithmetic sequences
%*									*
%************************************************************************

\begin{code}
rnArithSeq :: ArithSeqInfo RdrName -> RnM (ArithSeqInfo Name, FreeVars)
rnArithSeq (From expr)
 = rnLExpr expr 	`thenM` \ (expr', fvExpr) ->
   return (From expr', fvExpr)

rnArithSeq (FromThen expr1 expr2)
 = rnLExpr expr1 	`thenM` \ (expr1', fvExpr1) ->
   rnLExpr expr2	`thenM` \ (expr2', fvExpr2) ->
   return (FromThen expr1' expr2', fvExpr1 `plusFV` fvExpr2)

rnArithSeq (FromTo expr1 expr2)
 = rnLExpr expr1	`thenM` \ (expr1', fvExpr1) ->
   rnLExpr expr2	`thenM` \ (expr2', fvExpr2) ->
   return (FromTo expr1' expr2', fvExpr1 `plusFV` fvExpr2)

rnArithSeq (FromThenTo expr1 expr2 expr3)
 = rnLExpr expr1	`thenM` \ (expr1', fvExpr1) ->
   rnLExpr expr2	`thenM` \ (expr2', fvExpr2) ->
   rnLExpr expr3	`thenM` \ (expr3', fvExpr3) ->
   return (FromThenTo expr1' expr2' expr3',
	    plusFVs [fvExpr1, fvExpr2, fvExpr3])
\end{code}

%************************************************************************
%*									*
	Template Haskell brackets
%*									*
%************************************************************************

\begin{code}
rnBracket :: HsBracket RdrName -> RnM (HsBracket Name, FreeVars)
rnBracket (VarBr n) = do { name <- lookupOccRn n
			 ; this_mod <- getModule
			 ; unless (nameIsLocalOrFrom this_mod name) $	-- Reason: deprecation checking asumes the
			   do { _ <- loadInterfaceForName msg name	-- home interface is loaded, and this is the
			      ; return () }				-- only way that is going to happen
			 ; return (VarBr name, unitFV name) }
		    where
		      msg = ptext (sLit "Need interface for Template Haskell quoted Name")

rnBracket (ExpBr e) = do { (e', fvs) <- rnLExpr e
			 ; return (ExpBr e', fvs) }

rnBracket (PatBr _) = failWith (ptext (sLit "Tempate Haskell pattern brackets are not supported yet"))
rnBracket (TypBr t) = do { (t', fvs) <- rnHsTypeFVs doc t
			 ; return (TypBr t', fvs) }
		    where
		      doc = ptext (sLit "In a Template-Haskell quoted type")
rnBracket (DecBr group) 
  = do { gbl_env  <- getGblEnv

	; let new_gbl_env = gbl_env { tcg_dus = emptyDUs }
	      		  -- The emptyDUs is so that we just collect uses for this
                          -- group alone in the call to rnSrcDecls below
       ; (tcg_env, group') <- setGblEnv new_gbl_env $ 
       	 	   	      setStage thRnBrack $
			      rnSrcDecls group      

       -- Discard the tcg_env; it contains only extra info about fixity
	; return (DecBr group', allUses (tcg_dus tcg_env)) }
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
rnNormalStmts _ [] thing_inside 
  = do { (thing, fvs) <- thing_inside
	; return (([],thing), fvs) } 

rnNormalStmts ctxt (stmt@(L loc _) : stmts) thing_inside
  = do { ((stmts1, (stmts2, thing)), fvs) 
            <- setSrcSpan loc $
               rnStmt ctxt stmt $
               rnNormalStmts ctxt stmts thing_inside
	; return (((stmts1 ++ stmts2), thing), fvs) }


rnStmt :: HsStmtContext Name -> LStmt RdrName
       -> RnM (thing, FreeVars)
       -> RnM (([LStmt Name], thing), FreeVars)

rnStmt _ (L loc (ExprStmt expr _ _)) thing_inside
  = do	{ (expr', fv_expr) <- rnLExpr expr
	; (then_op, fvs1)  <- lookupSyntaxName thenMName
	; (thing, fvs2)    <- thing_inside
	; return (([L loc (ExprStmt expr' then_op placeHolderType)], thing),
		  fv_expr `plusFV` fvs1 `plusFV` fvs2) }

rnStmt ctxt (L loc (BindStmt pat expr _ _)) thing_inside
  = do	{ (expr', fv_expr) <- rnLExpr expr
		-- The binders do not scope over the expression
	; (bind_op, fvs1) <- lookupSyntaxName bindMName
	; (fail_op, fvs2) <- lookupSyntaxName failMName
	; rnPats (StmtCtxt ctxt) [pat] $ \ [pat'] -> do
	{ (thing, fvs3) <- thing_inside
	; return (([L loc (BindStmt pat' expr' bind_op fail_op)], thing),
		  fv_expr `plusFV` fvs1 `plusFV` fvs2 `plusFV` fvs3) }}
       -- fv_expr shouldn't really be filtered by the rnPatsAndThen
	-- but it does not matter because the names are unique

rnStmt ctxt (L loc (LetStmt binds)) thing_inside 
  = do	{ checkLetStmt ctxt binds
	; rnLocalBindsAndThen binds $ \binds' -> do
	{ (thing, fvs) <- thing_inside
        ; return (([L loc (LetStmt binds')], thing), fvs) }  }

rnStmt ctxt (L _ (RecStmt { recS_stmts = rec_stmts })) thing_inside
  = do	{ checkRecStmt ctxt

	-- Step1: Bring all the binders of the mdo into scope
	-- (Remember that this also removes the binders from the
	-- finally-returned free-vars.)
   	-- And rename each individual stmt, making a
	-- singleton segment.  At this stage the FwdRefs field
	-- isn't finished: it's empty for all except a BindStmt
	-- for which it's the fwd refs within the bind itself
	-- (This set may not be empty, because we're in a recursive 
	-- context.)
        ; rn_rec_stmts_and_then rec_stmts	$ \ segs -> do

	{ (thing, fvs_later) <- thing_inside
	; (return_op, fvs1)  <- lookupSyntaxName returnMName
	; (mfix_op,   fvs2)  <- lookupSyntaxName mfixName
	; (bind_op,   fvs3)  <- lookupSyntaxName bindMName
	; let
		-- Step 2: Fill in the fwd refs.
		-- 	   The segments are all singletons, but their fwd-ref
		--	   field mentions all the things used by the segment
		--	   that are bound after their use
	    segs_w_fwd_refs          = addFwdRefs segs

		-- Step 3: Group together the segments to make bigger segments
		--	   Invariant: in the result, no segment uses a variable
		--	   	      bound in a later segment
	    grouped_segs = glomSegments segs_w_fwd_refs

		-- Step 4: Turn the segments into Stmts
		--	   Use RecStmt when and only when there are fwd refs
		--	   Also gather up the uses from the end towards the
		--	   start, so we can tell the RecStmt which things are
		--	   used 'after' the RecStmt
	    empty_rec_stmt = emptyRecStmt { recS_ret_fn  = return_op
                                          , recS_mfix_fn = mfix_op
                                          , recS_bind_fn = bind_op }
	    (rec_stmts', fvs) = segsToStmts empty_rec_stmt grouped_segs fvs_later

	; return ((rec_stmts', thing), fvs `plusFV` fvs1 `plusFV` fvs2 `plusFV` fvs3) } }

rnStmt ctxt (L loc (ParStmt segs)) thing_inside
  = do	{ checkParStmt ctxt
	; ((segs', thing), fvs) <- rnParallelStmts (ParStmtCtxt ctxt) segs thing_inside
	; return (([L loc (ParStmt segs')], thing), fvs) }

rnStmt ctxt (L loc (TransformStmt (stmts, _) usingExpr maybeByExpr)) thing_inside = do
    checkTransformStmt ctxt
    
    (usingExpr', fv_usingExpr) <- rnLExpr usingExpr
    ((stmts', binders, (maybeByExpr', thing)), fvs) <- 
        rnNormalStmtsAndFindUsedBinders (TransformStmtCtxt ctxt) stmts $ \_unshadowed_bndrs -> do
            (maybeByExpr', fv_maybeByExpr)  <- rnMaybeLExpr maybeByExpr
            (thing, fv_thing)               <- thing_inside
            
            return ((maybeByExpr', thing), fv_maybeByExpr `plusFV` fv_thing)
    
    return (([L loc (TransformStmt (stmts', binders) usingExpr' maybeByExpr')], thing), 
             fv_usingExpr `plusFV` fvs)
  where
    rnMaybeLExpr Nothing = return (Nothing, emptyFVs)
    rnMaybeLExpr (Just expr) = do
        (expr', fv_expr) <- rnLExpr expr
        return (Just expr', fv_expr)
        
rnStmt ctxt (L loc (GroupStmt (stmts, _) groupByClause)) thing_inside = do
    checkTransformStmt ctxt
    
    -- We must rename the using expression in the context before the transform is begun
    groupByClauseAction <- 
        case groupByClause of
            GroupByNothing usingExpr -> do
                (usingExpr', fv_usingExpr) <- rnLExpr usingExpr
                (return . return) (GroupByNothing usingExpr', fv_usingExpr)
            GroupBySomething eitherUsingExpr byExpr -> do
                (eitherUsingExpr', fv_eitherUsingExpr) <- 
                    case eitherUsingExpr of
                        Right _ -> return (Right $ HsVar groupWithName, unitNameSet groupWithName)
                        Left usingExpr -> do
                            (usingExpr', fv_usingExpr) <- rnLExpr usingExpr
                            return (Left usingExpr', fv_usingExpr)
                            
                return $ do
                    (byExpr', fv_byExpr) <- rnLExpr byExpr
                    return (GroupBySomething eitherUsingExpr' byExpr', fv_eitherUsingExpr `plusFV` fv_byExpr)
    
    -- We only use rnNormalStmtsAndFindUsedBinders to get unshadowed_bndrs, so
    -- perhaps we could refactor this to use rnNormalStmts directly?
    ((stmts', _, (groupByClause', usedBinderMap, thing)), fvs) <- 
        rnNormalStmtsAndFindUsedBinders (TransformStmtCtxt ctxt) stmts $ \unshadowed_bndrs -> do
            (groupByClause', fv_groupByClause) <- groupByClauseAction
            
            unshadowed_bndrs' <- mapM newLocalName unshadowed_bndrs
            let binderMap = zip unshadowed_bndrs unshadowed_bndrs'
            
            -- Bind the "thing" inside a context where we have REBOUND everything
            -- bound by the statements before the group. This is necessary since after
            -- the grouping the same identifiers actually have different meanings
            -- i.e. they refer to lists not singletons!
            (thing, fv_thing) <- bindLocalNames unshadowed_bndrs' thing_inside
            
            -- We remove entries from the binder map that are not used in the thing_inside.
            -- We can then use that usage information to ensure that the free variables do 
            -- not contain the things we just bound, but do contain the things we need to
            -- make those bindings (i.e. the corresponding non-listy variables)
            
            -- Note that we also retain those entries which have an old binder in our
            -- own free variables (the using or by expression). This is because this map
            -- is reused in the desugarer to create the type to bind from the statements
            -- that occur before this one. If the binders we need are not in the map, they
            -- will never get bound into our desugared expression and hence the simplifier
            -- crashes as we refer to variables that don't exist!
            let usedBinderMap = filter 
                    (\(old_binder, new_binder) -> 
                        (new_binder `elemNameSet` fv_thing) || 
                        (old_binder `elemNameSet` fv_groupByClause)) binderMap
                (usedOldBinders, usedNewBinders) = unzip usedBinderMap
                real_fv_thing = (delListFromNameSet fv_thing usedNewBinders) `plusFV` (mkNameSet usedOldBinders)
            
            return ((groupByClause', usedBinderMap, thing), fv_groupByClause `plusFV` real_fv_thing)
    
    traceRn (text "rnStmt: implicitly rebound these used binders:" <+> ppr usedBinderMap)
    return (([L loc (GroupStmt (stmts', usedBinderMap) groupByClause')], thing), fvs)
  
rnNormalStmtsAndFindUsedBinders :: HsStmtContext Name 
          -> [LStmt RdrName]
          -> ([Name] -> RnM (thing, FreeVars))
          -> RnM (([LStmt Name], [Name], thing), FreeVars)	
rnNormalStmtsAndFindUsedBinders ctxt stmts thing_inside = do
    ((stmts', (used_bndrs, inner_thing)), fvs) <- rnNormalStmts ctxt stmts $ do
        -- Find the Names that are bound by stmts that
        -- by assumption we have just renamed
        local_env <- getLocalRdrEnv
        let 
            stmts_binders = collectLStmtsBinders stmts
            bndrs = map (expectJust "rnStmt"
                        . lookupLocalRdrEnv local_env
                        . unLoc) stmts_binders
                        
            -- If shadow, we'll look up (Unqual x) twice, getting
            -- the second binding both times, which is the
            -- one we want
            unshadowed_bndrs = nub bndrs
                        
        -- Typecheck the thing inside, passing on all 
        -- the Names bound before it for its information
        (thing, fvs) <- thing_inside unshadowed_bndrs

        -- Figure out which of the bound names are used
        -- after the statements we renamed
        let used_bndrs = filter (`elemNameSet` fvs) bndrs
        return ((used_bndrs, thing), fvs)

    -- Flatten the tuple returned by the above call a bit!
    return ((stmts', used_bndrs, inner_thing), fvs)

rnParallelStmts :: HsStmtContext Name -> [([LStmt RdrName], [RdrName])]
                -> RnM (thing, FreeVars)
                -> RnM (([([LStmt Name], [Name])], thing), FreeVars)
rnParallelStmts ctxt segs thing_inside = do
        orig_lcl_env <- getLocalRdrEnv
        go orig_lcl_env [] segs
    where
        go orig_lcl_env bndrs [] = do 
            let (bndrs', dups) = removeDups cmpByOcc bndrs
                inner_env = extendLocalRdrEnvList orig_lcl_env bndrs'
            
            mapM_ dupErr dups
            (thing, fvs) <- setLocalRdrEnv inner_env thing_inside
            return (([], thing), fvs)

        go orig_lcl_env bndrs_so_far ((stmts, _) : segs) = do 
            ((stmts', bndrs, (segs', thing)), fvs) <- rnNormalStmtsAndFindUsedBinders ctxt stmts $ \new_bndrs -> do
                -- Typecheck the thing inside, passing on all
                -- the Names bound, but separately; revert the envt
                setLocalRdrEnv orig_lcl_env $ do
                    go orig_lcl_env (new_bndrs ++ bndrs_so_far) segs

            let seg' = (stmts', bndrs)
            return (((seg':segs'), thing), delListFromNameSet fvs bndrs)

        cmpByOcc n1 n2 = nameOccName n1 `compare` nameOccName n2
        dupErr vs = addErr (ptext (sLit "Duplicate binding in parallel list comprehension for:")
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
  = rn_rec_stmts_and_then stmts $ \ segs -> do
    { (thing, fvs_later) <- thing_inside
    ; let   segs_w_fwd_refs = addFwdRefs segs
	    grouped_segs = glomSegments segs_w_fwd_refs
	    (stmts', fvs) = segsToStmts emptyRecStmt grouped_segs fvs_later
    ; return ((stmts', thing), fvs) }

---------------------------------------------

-- wrapper that does both the left- and right-hand sides
rn_rec_stmts_and_then :: [LStmt RdrName]
                         -- assumes that the FreeVars returned includes
                         -- the FreeVars of the Segments
                      -> ([Segment (LStmt Name)] -> RnM (a, FreeVars))
                      -> RnM (a, FreeVars)
rn_rec_stmts_and_then s cont
  = do	{ -- (A) Make the mini fixity env for all of the stmts
	  fix_env <- makeMiniFixityEnv (collectRecStmtsFixities s)

	  -- (B) Do the LHSes
	; new_lhs_and_fv <- rn_rec_stmts_lhs fix_env s

	  --    ...bring them and their fixities into scope
	; let bound_names = map unLoc $ collectLStmtsBinders (map fst new_lhs_and_fv)
	; bindLocalNamesFV_WithFixities bound_names fix_env $ do

	  -- (C) do the right-hand-sides and thing-inside
	{ segs <- rn_rec_stmts bound_names new_lhs_and_fv
	; (res, fvs) <- cont segs 
	; warnUnusedLocalBinds bound_names fvs
	; return (res, fvs) }}

-- get all the fixity decls in any Let stmt
collectRecStmtsFixities :: [LStmtLR RdrName RdrName] -> [LFixitySig RdrName]
collectRecStmtsFixities l = 
    foldr (\ s -> \acc -> case s of 
                            (L _ (LetStmt (HsValBinds (ValBindsIn _ sigs)))) -> 
                                foldr (\ sig -> \ acc -> case sig of 
                                                           (L loc (FixSig s)) -> (L loc s) : acc
                                                           _ -> acc) acc sigs
                            _ -> acc) [] l
                             
-- left-hand sides

rn_rec_stmt_lhs :: MiniFixityEnv
                -> LStmt RdrName
                   -- rename LHS, and return its FVs
                   -- Warning: we will only need the FreeVars below in the case of a BindStmt,
                   -- so we don't bother to compute it accurately in the other cases
                -> RnM [(LStmtLR Name RdrName, FreeVars)]

rn_rec_stmt_lhs _ (L loc (ExprStmt expr a b)) = return [(L loc (ExprStmt expr a b), 
                                                       -- this is actually correct
                                                       emptyFVs)]

rn_rec_stmt_lhs fix_env (L loc (BindStmt pat expr a b)) 
  = do 
      -- should the ctxt be MDo instead?
      (pat', fv_pat) <- rnBindPat (localRecNameMaker fix_env) pat 
      return [(L loc (BindStmt pat' expr a b),
               fv_pat)]

rn_rec_stmt_lhs _ (L _ (LetStmt binds@(HsIPBinds _)))
  = failWith (badIpBinds (ptext (sLit "an mdo expression")) binds)

rn_rec_stmt_lhs fix_env (L loc (LetStmt (HsValBinds binds))) 
    = do (_bound_names, binds') <- rnValBindsLHS fix_env binds
         return [(L loc (LetStmt (HsValBinds binds')),
                 -- Warning: this is bogus; see function invariant
                 emptyFVs
                 )]

-- XXX Do we need to do something with the return and mfix names?
rn_rec_stmt_lhs fix_env (L _ (RecStmt { recS_stmts = stmts }))	-- Flatten Rec inside Rec
    = rn_rec_stmts_lhs fix_env stmts

rn_rec_stmt_lhs _ stmt@(L _ (ParStmt _))	-- Syntactically illegal in mdo
  = pprPanic "rn_rec_stmt" (ppr stmt)
  
rn_rec_stmt_lhs _ stmt@(L _ (TransformStmt _ _ _))	-- Syntactically illegal in mdo
  = pprPanic "rn_rec_stmt" (ppr stmt)
  
rn_rec_stmt_lhs _ stmt@(L _ (GroupStmt _ _))	-- Syntactically illegal in mdo
  = pprPanic "rn_rec_stmt" (ppr stmt)

rn_rec_stmt_lhs _ (L _ (LetStmt EmptyLocalBinds))
  = panic "rn_rec_stmt LetStmt EmptyLocalBinds"

rn_rec_stmts_lhs :: MiniFixityEnv
                 -> [LStmt RdrName] 
                 -> RnM [(LStmtLR Name RdrName, FreeVars)]
rn_rec_stmts_lhs fix_env stmts
  = do { let boundNames = collectLStmtsBinders stmts
            -- First do error checking: we need to check for dups here because we
            -- don't bind all of the variables from the Stmt at once
            -- with bindLocatedLocals.
       ; checkDupRdrNames boundNames
       ; ls <- mapM (rn_rec_stmt_lhs fix_env) stmts
       ; return (concat ls) }


-- right-hand-sides

rn_rec_stmt :: [Name] -> LStmtLR Name RdrName -> FreeVars -> RnM [Segment (LStmt Name)]
	-- Rename a Stmt that is inside a RecStmt (or mdo)
	-- Assumes all binders are already in scope
	-- Turns each stmt into a singleton Stmt
rn_rec_stmt _ (L loc (ExprStmt expr _ _)) _
  = rnLExpr expr `thenM` \ (expr', fvs) ->
    lookupSyntaxName thenMName	`thenM` \ (then_op, fvs1) ->
    return [(emptyNameSet, fvs `plusFV` fvs1, emptyNameSet,
	      L loc (ExprStmt expr' then_op placeHolderType))]

rn_rec_stmt _ (L loc (BindStmt pat' expr _ _)) fv_pat
  = rnLExpr expr		`thenM` \ (expr', fv_expr) ->
    lookupSyntaxName bindMName	`thenM` \ (bind_op, fvs1) ->
    lookupSyntaxName failMName	`thenM` \ (fail_op, fvs2) ->
    let
	bndrs = mkNameSet (collectPatBinders pat')
	fvs   = fv_expr `plusFV` fv_pat `plusFV` fvs1 `plusFV` fvs2
    in
    return [(bndrs, fvs, bndrs `intersectNameSet` fvs,
	      L loc (BindStmt pat' expr' bind_op fail_op))]

rn_rec_stmt _ (L _ (LetStmt binds@(HsIPBinds _))) _
  = failWith (badIpBinds (ptext (sLit "an mdo expression")) binds)

rn_rec_stmt all_bndrs (L loc (LetStmt (HsValBinds binds'))) _ = do 
  (binds', du_binds) <- 
      -- fixities and unused are handled above in rn_rec_stmts_and_then
      rnValBindsRHS (mkNameSet all_bndrs) binds'
  return [(duDefs du_binds, duUses du_binds, 
	    emptyNameSet, L loc (LetStmt (HsValBinds binds')))]

-- no RecStmt case becuase they get flattened above when doing the LHSes
rn_rec_stmt _ stmt@(L _ (RecStmt {})) _
  = pprPanic "rn_rec_stmt: RecStmt" (ppr stmt)

rn_rec_stmt _ stmt@(L _ (ParStmt {})) _	-- Syntactically illegal in mdo
  = pprPanic "rn_rec_stmt: ParStmt" (ppr stmt)

rn_rec_stmt _ stmt@(L _ (TransformStmt {})) _	-- Syntactically illegal in mdo
  = pprPanic "rn_rec_stmt: TransformStmt" (ppr stmt)

rn_rec_stmt _ stmt@(L _ (GroupStmt {})) _	-- Syntactically illegal in mdo
  = pprPanic "rn_rec_stmt: GroupStmt" (ppr stmt)

rn_rec_stmt _ (L _ (LetStmt EmptyLocalBinds)) _
  = panic "rn_rec_stmt: LetStmt EmptyLocalBinds"

rn_rec_stmts :: [Name] -> [(LStmtLR Name RdrName, FreeVars)] -> RnM [Segment (LStmt Name)]
rn_rec_stmts bndrs stmts = mapM (uncurry (rn_rec_stmt bndrs)) stmts	`thenM` \ segs_s ->
		   	   return (concat segs_s)

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
segsToStmts :: Stmt Name		-- A RecStmt with the SyntaxOps filled in
            -> [Segment [LStmt Name]] 
	    -> FreeVars			-- Free vars used 'later'
	    -> ([LStmt Name], FreeVars)

segsToStmts _ [] fvs_later = ([], fvs_later)
segsToStmts empty_rec_stmt ((defs, uses, fwds, ss) : segs) fvs_later
  = ASSERT( not (null ss) )
    (new_stmt : later_stmts, later_uses `plusFV` uses)
  where
    (later_stmts, later_uses) = segsToStmts empty_rec_stmt segs fvs_later
    new_stmt | non_rec	 = head ss
	     | otherwise = L (getLoc (head ss)) rec_stmt 
    rec_stmt = empty_rec_stmt { recS_stmts     = ss
                              , recS_later_ids = nameSetToList used_later
                              , recS_rec_ids   = nameSetToList fwds }
    non_rec    = isSingleton ss && isEmptyNameSet fwds
    used_later = defs `intersectNameSet` later_uses
				-- The ones needed after the RecStmt
\end{code}

%************************************************************************
%*									*
\subsubsection{Assertion utils}
%*									*
%************************************************************************

\begin{code}
srcSpanPrimLit :: SrcSpan -> HsExpr Name
srcSpanPrimLit span = HsLit (HsStringPrim (mkFastString (showSDocOneLine (ppr span))))

mkAssertErrorExpr :: RnM (HsExpr Name)
-- Return an expression for (assertError "Foo.hs:27")
mkAssertErrorExpr
  = getSrcSpanM    			`thenM` \ sloc ->
    return (HsApp (L sloc (HsVar assertErrorName)) 
		  (L sloc (srcSpanPrimLit sloc)))
\end{code}

Note [Adding the implicit parameter to 'assert']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The renamer transforms (assert e1 e2) to (assert "Foo.hs:27" e1 e2).
By doing this in the renamer we allow the typechecker to just see the
expanded application and do the right thing. But it's not really 
the Right Thing because there's no way to "undo" if you want to see
the original source code.  We'll have fix this in due course, when
we care more about being able to reconstruct the exact original 
program.

%************************************************************************
%*									*
\subsubsection{Errors}
%*									*
%************************************************************************

\begin{code}

---------------------- 
-- Checking when a particular Stmt is ok
checkLetStmt :: HsStmtContext Name -> HsLocalBinds RdrName -> RnM ()
checkLetStmt (ParStmtCtxt _) (HsIPBinds binds) = addErr (badIpBinds (ptext (sLit "a parallel list comprehension:")) binds)
checkLetStmt _ctxt 	     _binds	       = return ()
  	-- We do not allow implicit-parameter bindings in a parallel
	-- list comprehension.  I'm not sure what it might mean.

---------
checkRecStmt :: HsStmtContext Name -> RnM ()
checkRecStmt (MDoExpr {}) = return ()	-- Recursive stmt ok in 'mdo'
checkRecStmt (DoExpr {})  = return ()	-- and in 'do'
checkRecStmt ctxt	  = addErr msg
  where
    msg = ptext (sLit "Illegal 'rec' stmt in") <+> pprStmtContext ctxt

---------
checkParStmt :: HsStmtContext Name -> RnM ()
checkParStmt _
  = do	{ parallel_list_comp <- doptM Opt_ParallelListComp
	; checkErr parallel_list_comp msg }
  where
    msg = ptext (sLit "Illegal parallel list comprehension: use -XParallelListComp")

---------
checkTransformStmt :: HsStmtContext Name -> RnM ()
checkTransformStmt ListComp  -- Ensure we are really within a list comprehension because otherwise the
			     -- desugarer will break when we come to operate on a parallel array
  = do	{ transform_list_comp <- doptM Opt_TransformListComp
	; checkErr transform_list_comp msg }
  where
    msg = ptext (sLit "Illegal transform or grouping list comprehension: use -XTransformListComp")
checkTransformStmt (ParStmtCtxt       ctxt) = checkTransformStmt ctxt	-- Ok to nest inside a parallel comprehension
checkTransformStmt (TransformStmtCtxt ctxt) = checkTransformStmt ctxt	-- Ok to nest inside a parallel comprehension
checkTransformStmt ctxt = addErr msg
  where
    msg = ptext (sLit "Illegal transform or grouping in") <+> pprStmtContext ctxt

---------
checkTupleSection :: [HsTupArg RdrName] -> RnM ()
checkTupleSection args
  = do	{ tuple_section <- doptM Opt_TupleSections
	; checkErr (all tupArgPresent args || tuple_section) msg }
  where
    msg = ptext (sLit "Illegal tuple section: use -XTupleSections")

---------
sectionErr :: HsExpr RdrName -> SDoc
sectionErr expr
  = hang (ptext (sLit "A section must be enclosed in parentheses"))
       2 (ptext (sLit "thus:") <+> (parens (ppr expr)))

patSynErr :: HsExpr RdrName -> RnM (HsExpr Name, FreeVars)
patSynErr e = do { addErr (sep [ptext (sLit "Pattern syntax in expression context:"),
			 	nest 4 (ppr e)])
		 ; return (EWildPat, emptyFVs) }

badIpBinds :: Outputable a => SDoc -> a -> SDoc
badIpBinds what binds
  = hang (ptext (sLit "Implicit-parameter bindings illegal in") <+> what)
	 2 (ppr binds)
\end{code}
