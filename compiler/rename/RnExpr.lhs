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
#endif  /* GHCI */

import RnBinds   ( rnLocalBindsAndThen, rnLocalValBindsLHS, rnLocalValBindsRHS,
                   rnMatchGroup, rnGRHS, makeMiniFixityEnv)
import HsSyn
import TcRnMonad
import RnEnv
import RnSplice
import RnTypes
import RnPat
import DynFlags
import BasicTypes       ( FixityDirection(..) )
import PrelNames

import Name
import NameSet
import RdrName
import UniqSet
import Data.List
import Util
import ListSetOps       ( removeDups )
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
%*                                                                      *
\subsubsection{Expressions}
%*                                                                      *
%************************************************************************

\begin{code}
rnExprs :: [LHsExpr RdrName] -> RnM ([LHsExpr Name], FreeVars)
rnExprs ls = rnExprs' ls emptyUniqSet
 where
  rnExprs' [] acc = return ([], acc)
  rnExprs' (expr:exprs) acc
   = rnLExpr expr               `thenM` \ (expr', fvExpr) ->

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
  = return (HsIPVar v, emptyFVs)

rnExpr (HsLit lit@(HsString s))
  = do {
         opt_OverloadedStrings <- xoptM Opt_OverloadedStrings
       ; if opt_OverloadedStrings then
            rnExpr (HsOverLit (mkHsIsString s placeHolderType))
         else -- Same as below
            rnLit lit           `thenM_`
            return (HsLit lit, emptyFVs)
       }

rnExpr (HsLit lit)
  = rnLit lit           `thenM_`
    return (HsLit lit, emptyFVs)

rnExpr (HsOverLit lit)
  = rnOverLit lit               `thenM` \ (lit', fvs) ->
    return (HsOverLit lit', fvs)

rnExpr (HsApp fun arg)
  = rnLExpr fun         `thenM` \ (fun',fvFun) ->
    rnLExpr arg         `thenM` \ (arg',fvArg) ->
    return (HsApp fun' arg', fvFun `plusFV` fvArg)

rnExpr (OpApp e1 (L op_loc (HsVar op_rdr)) _ e2)
  = do  { (e1', fv_e1) <- rnLExpr e1
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
rnExpr (OpApp _ other_op _ _)
  = failWith (vcat [ hang (ptext (sLit "Infix application with a non-variable operator:"))
                        2 (ppr other_op)
                   , ptext (sLit "(Probably resulting from a Template Haskell splice)") ])

rnExpr (NegApp e _)
  = rnLExpr e                   `thenM` \ (e', fv_e) ->
    lookupSyntaxName negateName `thenM` \ (neg_name, fv_neg) ->
    mkNegAppRn e' neg_name      `thenM` \ final_e ->
    return (final_e, fv_e `plusFV` fv_neg)

------------------------------------------
-- Template Haskell extensions
-- Don't ifdef-GHCI them because we want to fail gracefully
-- (not with an rnExpr crash) in a stage-1 compiler.
rnExpr e@(HsBracket br_body)
  = do
    thEnabled <- xoptM Opt_TemplateHaskell
    unless thEnabled $
      failWith ( vcat [ ptext (sLit "Syntax error on") <+> ppr e
                      , ptext (sLit "Perhaps you intended to use -XTemplateHaskell") ] )
    checkTH e "bracket"
    (body', fvs_e) <- rnBracket br_body
    return (HsBracket body', fvs_e)

rnExpr (HsSpliceE splice)
  = rnSplice splice             `thenM` \ (splice', fvs) ->
    return (HsSpliceE splice', fvs)

#ifndef GHCI
rnExpr e@(HsQuasiQuoteE _) = pprPanic "Cant do quasiquotation without GHCi" (ppr e)
#else
rnExpr (HsQuasiQuoteE qq)
  = runQuasiQuoteExpr qq        `thenM` \ (L _ expr') ->
    rnExpr expr'
#endif  /* GHCI */

---------------------------------------------
--      Sections
-- See Note [Parsing sections] in Parser.y.pp
rnExpr (HsPar (L loc (section@(SectionL {}))))
  = do  { (section', fvs) <- rnSection section
        ; return (HsPar (L loc section'), fvs) }

rnExpr (HsPar (L loc (section@(SectionR {}))))
  = do  { (section', fvs) <- rnSection section
        ; return (HsPar (L loc section'), fvs) }

rnExpr (HsPar e)
  = do  { (e', fvs_e) <- rnLExpr e
        ; return (HsPar e', fvs_e) }

rnExpr expr@(SectionL {})
  = do  { addErr (sectionErr expr); rnSection expr }
rnExpr expr@(SectionR {})
  = do  { addErr (sectionErr expr); rnSection expr }

---------------------------------------------
rnExpr (HsCoreAnn ann expr)
  = rnLExpr expr `thenM` \ (expr', fvs_expr) ->
    return (HsCoreAnn ann expr', fvs_expr)

rnExpr (HsSCC lbl expr)
  = rnLExpr expr                `thenM` \ (expr', fvs_expr) ->
    return (HsSCC lbl expr', fvs_expr)
rnExpr (HsTickPragma info expr)
  = rnLExpr expr                `thenM` \ (expr', fvs_expr) ->
    return (HsTickPragma info expr', fvs_expr)

rnExpr (HsLam matches)
  = rnMatchGroup LambdaExpr matches     `thenM` \ (matches', fvMatch) ->
    return (HsLam matches', fvMatch)

rnExpr (HsLamCase arg matches)
  = rnMatchGroup CaseAlt matches        `thenM` \ (matches', fvs_ms) ->
    return (HsLamCase arg matches', fvs_ms)

rnExpr (HsCase expr matches)
  = rnLExpr expr                        `thenM` \ (new_expr, e_fvs) ->
    rnMatchGroup CaseAlt matches        `thenM` \ (new_matches, ms_fvs) ->
    return (HsCase new_expr new_matches, e_fvs `plusFV` ms_fvs)

rnExpr (HsLet binds expr)
  = rnLocalBindsAndThen binds           $ \ binds' ->
    rnLExpr expr                         `thenM` \ (expr',fvExpr) ->
    return (HsLet binds' expr', fvExpr)

rnExpr (HsDo do_or_lc stmts _)
  = do  { ((stmts', _), fvs) <- rnStmts do_or_lc stmts (\ _ -> return ((), emptyFVs))
        ; return ( HsDo do_or_lc stmts' placeHolderType, fvs ) }

rnExpr (ExplicitList _ exps)
  = rnExprs exps                        `thenM` \ (exps', fvs) ->
    return  (ExplicitList placeHolderType exps', fvs)

rnExpr (ExplicitPArr _ exps)
  = rnExprs exps                        `thenM` \ (exps', fvs) ->
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
  = do  { conname <- lookupLocatedOccRn con_id
        ; (rbinds', fvRbinds) <- rnHsRecBinds (HsRecFieldCon (unLoc conname)) rbinds
        ; return (RecordCon conname noPostTcExpr rbinds',
                  fvRbinds `addOneFV` unLoc conname) }

rnExpr (RecordUpd expr rbinds _ _ _)
  = do  { (expr', fvExpr) <- rnLExpr expr
        ; (rbinds', fvRbinds) <- rnHsRecBinds HsRecFieldUpd rbinds
        ; return (RecordUpd expr' rbinds' [] [] [],
                  fvExpr `plusFV` fvRbinds) }

rnExpr (ExprWithTySig expr pty)
  = do  { (pty', fvTy) <- rnLHsType ExprWithTySigCtx pty
        ; (expr', fvExpr) <- bindSigTyVarsFV (hsExplicitTvs pty') $
                             rnLExpr expr
        ; return (ExprWithTySig expr' pty', fvExpr `plusFV` fvTy) }

rnExpr (HsIf _ p b1 b2)
  = do { (p', fvP) <- rnLExpr p
       ; (b1', fvB1) <- rnLExpr b1
       ; (b2', fvB2) <- rnLExpr b2
       ; (mb_ite, fvITE) <- lookupIfThenElse
       ; return (HsIf mb_ite p' b1' b2', plusFVs [fvITE, fvP, fvB1, fvB2]) }

rnExpr (HsMultiIf ty alts)
  = do { (alts', fvs) <- mapFvRn (rnGRHS IfAlt) alts
       ; return (HsMultiIf ty alts', fvs) }

rnExpr (HsType a)
  = rnLHsType HsTypeCtx a       `thenM` \ (t, fvT) ->
    return (HsType t, fvT)

rnExpr (ArithSeq _ seq)
  = rnArithSeq seq       `thenM` \ (new_seq, fvs) ->
    return (ArithSeq noPostTcExpr new_seq, fvs)

rnExpr (PArrSeq _ seq)
  = rnArithSeq seq       `thenM` \ (new_seq, fvs) ->
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
%*                                                                      *
        Arrow notation
%*                                                                      *
%************************************************************************

\begin{code}
rnExpr (HsProc pat body)
  = newArrowScope $
    rnPat ProcExpr pat $ \ pat' ->
    rnCmdTop body                `thenM` \ (body',fvBody) ->
    return (HsProc pat' body', fvBody)

rnExpr (HsArrApp arrow arg _ ho rtl)
  = select_arrow_scope (rnLExpr arrow)  `thenM` \ (arrow',fvArrow) ->
    rnLExpr arg                         `thenM` \ (arg',fvArg) ->
    return (HsArrApp arrow' arg' placeHolderType ho rtl,
             fvArrow `plusFV` fvArg)
  where
    select_arrow_scope tc = case ho of
        HsHigherOrderApp -> tc
        HsFirstOrderApp  -> escapeArrowScope tc

-- infix form
rnExpr (HsArrForm op (Just _) [arg1, arg2])
  = escapeArrowScope (rnLExpr op)
                        `thenM` \ (op',fv_op) ->
    let L _ (HsVar op_name) = op' in
    rnCmdTop arg1       `thenM` \ (arg1',fv_arg1) ->
    rnCmdTop arg2       `thenM` \ (arg2',fv_arg2) ->

        -- Deal with fixity

    lookupFixityRn op_name              `thenM` \ fixity ->
    mkOpFormRn arg1' op' fixity arg2'   `thenM` \ final_e ->

    return (final_e,
              fv_arg1 `plusFV` fv_op `plusFV` fv_arg2)

rnExpr (HsArrForm op fixity cmds)
  = escapeArrowScope (rnLExpr op)       `thenM` \ (op',fvOp) ->
    rnCmdArgs cmds                      `thenM` \ (cmds',fvCmds) ->
    return (HsArrForm op' fixity cmds', fvOp `plusFV` fvCmds)

rnExpr other = pprPanic "rnExpr: unexpected expression" (ppr other)
        -- HsWrap

----------------------
-- See Note [Parsing sections] in Parser.y.pp
rnSection :: HsExpr RdrName -> RnM (HsExpr Name, FreeVars)
rnSection section@(SectionR op expr)
  = do  { (op', fvs_op)     <- rnLExpr op
        ; (expr', fvs_expr) <- rnLExpr expr
        ; checkSectionPrec InfixR section op' expr'
        ; return (SectionR op' expr', fvs_op `plusFV` fvs_expr) }

rnSection section@(SectionL expr op)
  = do  { (expr', fvs_expr) <- rnLExpr expr
        ; (op', fvs_op)     <- rnLExpr op
        ; checkSectionPrec InfixL section op' expr'
        ; return (SectionL expr' op', fvs_op `plusFV` fvs_expr) }

rnSection other = pprPanic "rnSection" (ppr other)
\end{code}

%************************************************************************
%*                                                                      *
        Records
%*                                                                      *
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
%*                                                                      *
        Arrow commands
%*                                                                      *
%************************************************************************

\begin{code}
rnCmdArgs :: [LHsCmdTop RdrName] -> RnM ([LHsCmdTop Name], FreeVars)
rnCmdArgs [] = return ([], emptyFVs)
rnCmdArgs (arg:args)
  = rnCmdTop arg        `thenM` \ (arg',fvArg) ->
    rnCmdArgs args      `thenM` \ (args',fvArgs) ->
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
     lookupSyntaxTable cmd_names        `thenM` \ (cmd_names', cmd_fvs) ->

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

convertOpFormsCmd (HsIf f exp c1 c2)
  = HsIf f exp (convertOpFormsLCmd c1) (convertOpFormsLCmd c2)

convertOpFormsCmd (HsLet binds cmd)
  = HsLet binds (convertOpFormsLCmd cmd)

convertOpFormsCmd (HsDo DoExpr stmts ty)
  = HsDo ArrowExpr (map (fmap convertOpFormsStmt) stmts) ty
    -- Mark the HsDo as begin the body of an arrow command

-- Anything else is unchanged.  This includes HsArrForm (already done),
-- things with no sub-commands, and illegal commands (which will be
-- caught by the type checker)
convertOpFormsCmd c = c

convertOpFormsStmt :: StmtLR id id -> StmtLR id id
convertOpFormsStmt (BindStmt pat cmd _ _)
  = BindStmt pat (convertOpFormsLCmd cmd) noSyntaxExpr noSyntaxExpr
convertOpFormsStmt (ExprStmt cmd _ _ _)
  = ExprStmt (convertOpFormsLCmd cmd) noSyntaxExpr noSyntaxExpr placeHolderType
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
type CmdNeeds = FreeVars        -- Only inhabitants are
                                --      appAName, choiceAName, loopAName

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

methodNamesCmd (HsIf _ _ c1 c2)
  = methodNamesLCmd c1 `plusFV` methodNamesLCmd c2 `addOneFV` choiceAName

methodNamesCmd (HsLet _ c)      = methodNamesLCmd c
methodNamesCmd (HsDo _ stmts _) = methodNamesStmts stmts
methodNamesCmd (HsApp c _)      = methodNamesLCmd c
methodNamesCmd (HsLam match)    = methodNamesMatch match

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
methodNamesStmt (LastStmt cmd _)                 = methodNamesLCmd cmd
methodNamesStmt (ExprStmt cmd _ _ _)             = methodNamesLCmd cmd
methodNamesStmt (BindStmt _ cmd _ _)             = methodNamesLCmd cmd
methodNamesStmt (RecStmt { recS_stmts = stmts }) = methodNamesStmts stmts `addOneFV` loopAName
methodNamesStmt (LetStmt {})                     = emptyFVs
methodNamesStmt (ParStmt {})                     = emptyFVs
methodNamesStmt (TransStmt {})                   = emptyFVs
   -- ParStmt and TransStmt can't occur in commands, but it's not convenient to error
   -- here so we just do what's convenient
\end{code}


%************************************************************************
%*                                                                      *
        Arithmetic sequences
%*                                                                      *
%************************************************************************

\begin{code}
rnArithSeq :: ArithSeqInfo RdrName -> RnM (ArithSeqInfo Name, FreeVars)
rnArithSeq (From expr)
 = rnLExpr expr         `thenM` \ (expr', fvExpr) ->
   return (From expr', fvExpr)

rnArithSeq (FromThen expr1 expr2)
 = rnLExpr expr1        `thenM` \ (expr1', fvExpr1) ->
   rnLExpr expr2        `thenM` \ (expr2', fvExpr2) ->
   return (FromThen expr1' expr2', fvExpr1 `plusFV` fvExpr2)

rnArithSeq (FromTo expr1 expr2)
 = rnLExpr expr1        `thenM` \ (expr1', fvExpr1) ->
   rnLExpr expr2        `thenM` \ (expr2', fvExpr2) ->
   return (FromTo expr1' expr2', fvExpr1 `plusFV` fvExpr2)

rnArithSeq (FromThenTo expr1 expr2 expr3)
 = rnLExpr expr1        `thenM` \ (expr1', fvExpr1) ->
   rnLExpr expr2        `thenM` \ (expr2', fvExpr2) ->
   rnLExpr expr3        `thenM` \ (expr3', fvExpr3) ->
   return (FromThenTo expr1' expr2' expr3',
            plusFVs [fvExpr1, fvExpr2, fvExpr3])
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection{@Stmt@s: in @do@ expressions}
%*                                                                      *
%************************************************************************

\begin{code}
rnStmts :: HsStmtContext Name -> [LStmt RdrName]
        -> ([Name] -> RnM (thing, FreeVars))
        -> RnM (([LStmt Name], thing), FreeVars)
-- Variables bound by the Stmts, and mentioned in thing_inside,
-- do not appear in the result FreeVars

rnStmts ctxt [] thing_inside
  = do { checkEmptyStmts ctxt
       ; (thing, fvs) <- thing_inside []
       ; return (([], thing), fvs) }

rnStmts MDoExpr stmts thing_inside    -- Deal with mdo
  = -- Behave like do { rec { ...all but last... }; last }
    do { ((stmts1, (stmts2, thing)), fvs)
           <- rnStmt MDoExpr (noLoc $ mkRecStmt all_but_last) $ \ _ ->
              do { last_stmt' <- checkLastStmt MDoExpr last_stmt
                 ; rnStmt MDoExpr last_stmt' thing_inside }
        ; return (((stmts1 ++ stmts2), thing), fvs) }
  where
    Just (all_but_last, last_stmt) = snocView stmts

rnStmts ctxt (lstmt@(L loc _) : lstmts) thing_inside
  | null lstmts
  = setSrcSpan loc $
    do { lstmt' <- checkLastStmt ctxt lstmt
       ; rnStmt ctxt lstmt' thing_inside }

  | otherwise
  = do { ((stmts1, (stmts2, thing)), fvs)
            <- setSrcSpan loc                         $
               do { checkStmt ctxt lstmt
                  ; rnStmt ctxt lstmt    $ \ bndrs1 ->
                    rnStmts ctxt lstmts  $ \ bndrs2 ->
                    thing_inside (bndrs1 ++ bndrs2) }
        ; return (((stmts1 ++ stmts2), thing), fvs) }

----------------------
rnStmt :: HsStmtContext Name
       -> LStmt RdrName
       -> ([Name] -> RnM (thing, FreeVars))
       -> RnM (([LStmt Name], thing), FreeVars)
-- Variables bound by the Stmt, and mentioned in thing_inside,
-- do not appear in the result FreeVars

rnStmt ctxt (L loc (LastStmt expr _)) thing_inside
  = do  { (expr', fv_expr) <- rnLExpr expr
        ; (ret_op, fvs1)   <- lookupStmtName ctxt returnMName
        ; (thing,  fvs3)   <- thing_inside []
        ; return (([L loc (LastStmt expr' ret_op)], thing),
                  fv_expr `plusFV` fvs1 `plusFV` fvs3) }

rnStmt ctxt (L loc (ExprStmt expr _ _ _)) thing_inside
  = do  { (expr', fv_expr) <- rnLExpr expr
        ; (then_op, fvs1)  <- lookupStmtName ctxt thenMName
        ; (guard_op, fvs2) <- if isListCompExpr ctxt
                              then lookupStmtName ctxt guardMName
                              else return (noSyntaxExpr, emptyFVs)
                              -- Only list/parr/monad comprehensions use 'guard'
                              -- Also for sub-stmts of same eg [ e | x<-xs, gd | blah ]
                              -- Here "gd" is a guard
        ; (thing, fvs3)    <- thing_inside []
        ; return (([L loc (ExprStmt expr' then_op guard_op placeHolderType)], thing),
                  fv_expr `plusFV` fvs1 `plusFV` fvs2 `plusFV` fvs3) }

rnStmt ctxt (L loc (BindStmt pat expr _ _)) thing_inside
  = do  { (expr', fv_expr) <- rnLExpr expr
                -- The binders do not scope over the expression
        ; (bind_op, fvs1) <- lookupStmtName ctxt bindMName
        ; (fail_op, fvs2) <- lookupStmtName ctxt failMName
        ; rnPat (StmtCtxt ctxt) pat $ \ pat' -> do
        { (thing, fvs3) <- thing_inside (collectPatBinders pat')
        ; return (([L loc (BindStmt pat' expr' bind_op fail_op)], thing),
                  fv_expr `plusFV` fvs1 `plusFV` fvs2 `plusFV` fvs3) }}
       -- fv_expr shouldn't really be filtered by the rnPatsAndThen
        -- but it does not matter because the names are unique

rnStmt _ (L loc (LetStmt binds)) thing_inside
  = do  { rnLocalBindsAndThen binds $ \binds' -> do
        { (thing, fvs) <- thing_inside (collectLocalBinders binds')
        ; return (([L loc (LetStmt binds')], thing), fvs) }  }

rnStmt ctxt (L _ (RecStmt { recS_stmts = rec_stmts })) thing_inside
  = do  {
        -- Step1: Bring all the binders of the mdo into scope
        -- (Remember that this also removes the binders from the
        -- finally-returned free-vars.)
        -- And rename each individual stmt, making a
        -- singleton segment.  At this stage the FwdRefs field
        -- isn't finished: it's empty for all except a BindStmt
        -- for which it's the fwd refs within the bind itself
        -- (This set may not be empty, because we're in a recursive
        -- context.)
        ; rnRecStmtsAndThen rec_stmts   $ \ segs -> do

        { let bndrs = nameSetToList $ foldr (unionNameSets . (\(ds,_,_,_) -> ds))
                                            emptyNameSet segs
        ; (thing, fvs_later) <- thing_inside bndrs
        ; (return_op, fvs1)  <- lookupStmtName ctxt returnMName
        ; (mfix_op,   fvs2)  <- lookupStmtName ctxt mfixName
        ; (bind_op,   fvs3)  <- lookupStmtName ctxt bindMName
        ; let
                -- Step 2: Fill in the fwd refs.
                --         The segments are all singletons, but their fwd-ref
                --         field mentions all the things used by the segment
                --         that are bound after their use
            segs_w_fwd_refs          = addFwdRefs segs

                -- Step 3: Group together the segments to make bigger segments
                --         Invariant: in the result, no segment uses a variable
                --                    bound in a later segment
            grouped_segs = glomSegments ctxt segs_w_fwd_refs

                -- Step 4: Turn the segments into Stmts
                --         Use RecStmt when and only when there are fwd refs
                --         Also gather up the uses from the end towards the
                --         start, so we can tell the RecStmt which things are
                --         used 'after' the RecStmt
            empty_rec_stmt = emptyRecStmt { recS_ret_fn  = return_op
                                          , recS_mfix_fn = mfix_op
                                          , recS_bind_fn = bind_op }
            (rec_stmts', fvs) = segsToStmts empty_rec_stmt grouped_segs fvs_later

        ; return ((rec_stmts', thing), fvs `plusFV` fvs1 `plusFV` fvs2 `plusFV` fvs3) } }

rnStmt ctxt (L loc (ParStmt segs _ _)) thing_inside
  = do  { (mzip_op, fvs1)   <- lookupStmtName ctxt mzipName
        ; (bind_op, fvs2)   <- lookupStmtName ctxt bindMName
        ; (return_op, fvs3) <- lookupStmtName ctxt returnMName
        ; ((segs', thing), fvs4) <- rnParallelStmts (ParStmtCtxt ctxt) return_op segs thing_inside
        ; return ( ([L loc (ParStmt segs' mzip_op bind_op)], thing)
                 , fvs1 `plusFV` fvs2 `plusFV` fvs3 `plusFV` fvs4) }

rnStmt ctxt (L loc (TransStmt { trS_stmts = stmts, trS_by = by, trS_form = form
                              , trS_using = using })) thing_inside
  = do { -- Rename the 'using' expression in the context before the transform is begun
         (using', fvs1) <- rnLExpr using

         -- Rename the stmts and the 'by' expression
         -- Keep track of the variables mentioned in the 'by' expression
       ; ((stmts', (by', used_bndrs, thing)), fvs2)
             <- rnStmts (TransStmtCtxt ctxt) stmts $ \ bndrs ->
                do { (by',   fvs_by) <- mapMaybeFvRn rnLExpr by
                   ; (thing, fvs_thing) <- thing_inside bndrs
                   ; let fvs = fvs_by `plusFV` fvs_thing
                         used_bndrs = filter (`elemNameSet` fvs) bndrs
                         -- The paper (Fig 5) has a bug here; we must treat any free varaible
                         -- of the "thing inside", **or of the by-expression**, as used
                   ; return ((by', used_bndrs, thing), fvs) }

       -- Lookup `return`, `(>>=)` and `liftM` for monad comprehensions
       ; (return_op, fvs3) <- lookupStmtName ctxt returnMName
       ; (bind_op,   fvs4) <- lookupStmtName ctxt bindMName
       ; (fmap_op,   fvs5) <- case form of
                                ThenForm -> return (noSyntaxExpr, emptyFVs)
                                _        -> lookupStmtName ctxt fmapName

       ; let all_fvs  = fvs1 `plusFV` fvs2 `plusFV` fvs3
                             `plusFV` fvs4 `plusFV` fvs5
             bndr_map = used_bndrs `zip` used_bndrs
             -- See Note [TransStmt binder map] in HsExpr

       ; traceRn (text "rnStmt: implicitly rebound these used binders:" <+> ppr bndr_map)
       ; return (([L loc (TransStmt { trS_stmts = stmts', trS_bndrs = bndr_map
                                    , trS_by = by', trS_using = using', trS_form = form
                                    , trS_ret = return_op, trS_bind = bind_op
                                    , trS_fmap = fmap_op })], thing), all_fvs) }

rnParallelStmts :: forall thing. HsStmtContext Name
                -> SyntaxExpr Name
                -> [ParStmtBlock RdrName RdrName]
                -> ([Name] -> RnM (thing, FreeVars))
                -> RnM (([ParStmtBlock Name Name], thing), FreeVars)
-- Note [Renaming parallel Stmts]
rnParallelStmts ctxt return_op segs thing_inside
  = do { orig_lcl_env <- getLocalRdrEnv
       ; rn_segs orig_lcl_env [] segs }
  where
    rn_segs :: LocalRdrEnv
            -> [Name] -> [ParStmtBlock RdrName RdrName]
            -> RnM (([ParStmtBlock Name Name], thing), FreeVars)
    rn_segs _ bndrs_so_far []
      = do { let (bndrs', dups) = removeDups cmpByOcc bndrs_so_far
           ; mapM_ dupErr dups
           ; (thing, fvs) <- bindLocalNames bndrs' (thing_inside bndrs')
           ; return (([], thing), fvs) }

    rn_segs env bndrs_so_far (ParStmtBlock stmts _ _ : segs)
      = do { ((stmts', (used_bndrs, segs', thing)), fvs)
                    <- rnStmts ctxt stmts $ \ bndrs ->
                       setLocalRdrEnv env       $ do
                       { ((segs', thing), fvs) <- rn_segs env (bndrs ++ bndrs_so_far) segs
                       ; let used_bndrs = filter (`elemNameSet` fvs) bndrs
                       ; return ((used_bndrs, segs', thing), fvs) }

           ; let seg' = ParStmtBlock stmts' used_bndrs return_op
           ; return ((seg':segs', thing), fvs) }

    cmpByOcc n1 n2 = nameOccName n1 `compare` nameOccName n2
    dupErr vs = addErr (ptext (sLit "Duplicate binding in parallel list comprehension for:")
                    <+> quotes (ppr (head vs)))

lookupStmtName :: HsStmtContext Name -> Name -> RnM (HsExpr Name, FreeVars)
-- Like lookupSyntaxName, but ListComp/PArrComp are never rebindable
-- Neither is ArrowExpr, which has its own desugarer in DsArrows
lookupStmtName ctxt n
  = case ctxt of
      ListComp        -> not_rebindable
      PArrComp        -> not_rebindable
      ArrowExpr       -> not_rebindable
      PatGuard {}     -> not_rebindable

      DoExpr          -> rebindable
      MDoExpr         -> rebindable
      MonadComp       -> rebindable
      GhciStmt        -> rebindable   -- I suppose?

      ParStmtCtxt   c -> lookupStmtName c n     -- Look inside to
      TransStmtCtxt c -> lookupStmtName c n     -- the parent context
  where
    rebindable     = lookupSyntaxName n
    not_rebindable = return (HsVar n, emptyFVs)
\end{code}

Note [Renaming parallel Stmts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Renaming parallel statements is painful.  Given, say
     [ a+c | a <- as, bs <- bss
           | c <- bs, a <- ds ]
Note that
  (a) In order to report "Defined by not used" about 'bs', we must rename
      each group of Stmts with a thing_inside whose FreeVars include at least {a,c}

  (b) We want to report that 'a' is illegally bound in both branches

  (c) The 'bs' in the second group must obviously not be captured by
      the binding in the first group

To satisfy (a) we nest the segements.
To satisfy (b) we check for duplicates just before thing_inside.
To satisfy (c) we reset the LocalRdrEnv each time.

%************************************************************************
%*                                                                      *
\subsubsection{mdo expressions}
%*                                                                      *
%************************************************************************

\begin{code}
type FwdRefs = NameSet
type Segment stmts = (Defs,
                      Uses,     -- May include defs
                      FwdRefs,  -- A subset of uses that are
                                --   (a) used before they are bound in this segment, or
                                --   (b) used here, and bound in subsequent segments
                      stmts)    -- Either Stmt or [Stmt]


-- wrapper that does both the left- and right-hand sides
rnRecStmtsAndThen :: [LStmt RdrName]
                         -- assumes that the FreeVars returned includes
                         -- the FreeVars of the Segments
                      -> ([Segment (LStmt Name)] -> RnM (a, FreeVars))
                      -> RnM (a, FreeVars)
rnRecStmtsAndThen s cont
  = do  { -- (A) Make the mini fixity env for all of the stmts
          fix_env <- makeMiniFixityEnv (collectRecStmtsFixities s)

          -- (B) Do the LHSes
        ; new_lhs_and_fv <- rn_rec_stmts_lhs fix_env s

          --    ...bring them and their fixities into scope
        ; let bound_names = collectLStmtsBinders (map fst new_lhs_and_fv)
              -- Fake uses of variables introduced implicitly (warning suppression, see #4404)
              implicit_uses = lStmtsImplicits (map fst new_lhs_and_fv)
        ; bindLocalNamesFV bound_names $
          addLocalFixities fix_env bound_names $ do

          -- (C) do the right-hand-sides and thing-inside
        { segs <- rn_rec_stmts bound_names new_lhs_and_fv
        ; (res, fvs) <- cont segs
        ; warnUnusedLocalBinds bound_names (fvs `unionNameSets` implicit_uses)
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

rn_rec_stmt_lhs _ (L loc (ExprStmt expr a b c))
  = return [(L loc (ExprStmt expr a b c), emptyFVs)]

rn_rec_stmt_lhs _ (L loc (LastStmt expr a))
  = return [(L loc (LastStmt expr a), emptyFVs)]

rn_rec_stmt_lhs fix_env (L loc (BindStmt pat expr a b))
  = do
      -- should the ctxt be MDo instead?
      (pat', fv_pat) <- rnBindPat (localRecNameMaker fix_env) pat
      return [(L loc (BindStmt pat' expr a b),
               fv_pat)]

rn_rec_stmt_lhs _ (L _ (LetStmt binds@(HsIPBinds _)))
  = failWith (badIpBinds (ptext (sLit "an mdo expression")) binds)

rn_rec_stmt_lhs fix_env (L loc (LetStmt (HsValBinds binds)))
    = do (_bound_names, binds') <- rnLocalValBindsLHS fix_env binds
         return [(L loc (LetStmt (HsValBinds binds')),
                 -- Warning: this is bogus; see function invariant
                 emptyFVs
                 )]

-- XXX Do we need to do something with the return and mfix names?
rn_rec_stmt_lhs fix_env (L _ (RecStmt { recS_stmts = stmts }))  -- Flatten Rec inside Rec
    = rn_rec_stmts_lhs fix_env stmts

rn_rec_stmt_lhs _ stmt@(L _ (ParStmt {}))       -- Syntactically illegal in mdo
  = pprPanic "rn_rec_stmt" (ppr stmt)

rn_rec_stmt_lhs _ stmt@(L _ (TransStmt {}))     -- Syntactically illegal in mdo
  = pprPanic "rn_rec_stmt" (ppr stmt)

rn_rec_stmt_lhs _ (L _ (LetStmt EmptyLocalBinds))
  = panic "rn_rec_stmt LetStmt EmptyLocalBinds"

rn_rec_stmts_lhs :: MiniFixityEnv
                 -> [LStmt RdrName]
                 -> RnM [(LStmtLR Name RdrName, FreeVars)]
rn_rec_stmts_lhs fix_env stmts
  = do { ls <- concatMapM (rn_rec_stmt_lhs fix_env) stmts
       ; let boundNames = collectLStmtsBinders (map fst ls)
            -- First do error checking: we need to check for dups here because we
            -- don't bind all of the variables from the Stmt at once
            -- with bindLocatedLocals.
       ; checkDupNames boundNames
       ; return ls }


-- right-hand-sides

rn_rec_stmt :: [Name] -> LStmtLR Name RdrName -> FreeVars -> RnM [Segment (LStmt Name)]
        -- Rename a Stmt that is inside a RecStmt (or mdo)
        -- Assumes all binders are already in scope
        -- Turns each stmt into a singleton Stmt
rn_rec_stmt _ (L loc (LastStmt expr _)) _
  = do  { (expr', fv_expr) <- rnLExpr expr
        ; (ret_op, fvs1)   <- lookupSyntaxName returnMName
        ; return [(emptyNameSet, fv_expr `plusFV` fvs1, emptyNameSet,
                   L loc (LastStmt expr' ret_op))] }

rn_rec_stmt _ (L loc (ExprStmt expr _ _ _)) _
  = rnLExpr expr `thenM` \ (expr', fvs) ->
    lookupSyntaxName thenMName  `thenM` \ (then_op, fvs1) ->
    return [(emptyNameSet, fvs `plusFV` fvs1, emptyNameSet,
              L loc (ExprStmt expr' then_op noSyntaxExpr placeHolderType))]

rn_rec_stmt _ (L loc (BindStmt pat' expr _ _)) fv_pat
  = rnLExpr expr                `thenM` \ (expr', fv_expr) ->
    lookupSyntaxName bindMName  `thenM` \ (bind_op, fvs1) ->
    lookupSyntaxName failMName  `thenM` \ (fail_op, fvs2) ->
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
      -- fixities and unused are handled above in rnRecStmtsAndThen
      rnLocalValBindsRHS (mkNameSet all_bndrs) binds'
  return [(duDefs du_binds, allUses du_binds,
           emptyNameSet, L loc (LetStmt (HsValBinds binds')))]

-- no RecStmt case becuase they get flattened above when doing the LHSes
rn_rec_stmt _ stmt@(L _ (RecStmt {})) _
  = pprPanic "rn_rec_stmt: RecStmt" (ppr stmt)

rn_rec_stmt _ stmt@(L _ (ParStmt {})) _ -- Syntactically illegal in mdo
  = pprPanic "rn_rec_stmt: ParStmt" (ppr stmt)

rn_rec_stmt _ stmt@(L _ (TransStmt {})) _       -- Syntactically illegal in mdo
  = pprPanic "rn_rec_stmt: TransStmt" (ppr stmt)

rn_rec_stmt _ (L _ (LetStmt EmptyLocalBinds)) _
  = panic "rn_rec_stmt: LetStmt EmptyLocalBinds"

rn_rec_stmts :: [Name] -> [(LStmtLR Name RdrName, FreeVars)] -> RnM [Segment (LStmt Name)]
rn_rec_stmts bndrs stmts = mapM (uncurry (rn_rec_stmt bndrs)) stmts     `thenM` \ segs_s ->
                           return (concat segs_s)

---------------------------------------------
addFwdRefs :: [Segment a] -> [Segment a]
-- So far the segments only have forward refs *within* the Stmt
--      (which happens for bind:  x <- ...x...)
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
--      Glomming the singleton segments of an mdo into
--      minimal recursive groups.
--
-- At first I thought this was just strongly connected components, but
-- there's an important constraint: the order of the stmts must not change.
--
-- Consider
--      mdo { x <- ...y...
--            p <- z
--            y <- ...x...
--            q <- x
--            z <- y
--            r <- x }
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
--      { rec { y <- ...x... ; q <- x ; z <- y } ; r <- x }
-- Now one more, which we can add on without a rec
--      { p <- z ;
--        rec { y <- ...x... ; q <- x ; z <- y } ;
--        r <- x }
-- Finally we add the last one; since it mentions y we have to
-- glom it togeher with the first two groups
--      { rec { x <- ...y...; p <- z ; y <- ...x... ;
--              q <- x ; z <- y } ;
--        r <- x }
--
-- NB. June 7 2012: We only glom segments that appear in
-- an explicit mdo; and leave those found in "do rec"'s intact.
-- See http://hackage.haskell.org/trac/ghc/ticket/4148 for
-- the discussion leading to this design choice.

glomSegments :: HsStmtContext Name -> [Segment (LStmt Name)] -> [Segment [LStmt Name]]

glomSegments _ [] = []
glomSegments ctxt ((defs,uses,fwds,stmt) : segs)
        -- Actually stmts will always be a singleton
  = (seg_defs, seg_uses, seg_fwds, seg_stmts)  : others
  where
    segs'            = glomSegments ctxt segs
    (extras, others) = grab uses segs'
    (ds, us, fs, ss) = unzip4 extras

    seg_defs  = plusFVs ds `plusFV` defs
    seg_uses  = plusFVs us `plusFV` uses
    seg_fwds  = plusFVs fs `plusFV` fwds
    seg_stmts = stmt : concat ss

    grab :: NameSet             -- The client
         -> [Segment a]
         -> ([Segment a],       -- Needed by the 'client'
             [Segment a])       -- Not needed by the client
        -- The result is simply a split of the input
    grab uses dus
        = (reverse yeses, reverse noes)
        where
          (noes, yeses)           = span not_needed (reverse dus)
          not_needed (defs,_,_,_) = case ctxt of
                                      MDoExpr -> not (intersectsNameSet defs uses)
                                      _       -> False  -- unless we're in mdo, we *need* everything


----------------------------------------------------
segsToStmts :: Stmt Name                -- A RecStmt with the SyntaxOps filled in
            -> [Segment [LStmt Name]]
            -> FreeVars                 -- Free vars used 'later'
            -> ([LStmt Name], FreeVars)

segsToStmts _ [] fvs_later = ([], fvs_later)
segsToStmts empty_rec_stmt ((defs, uses, fwds, ss) : segs) fvs_later
  = ASSERT( not (null ss) )
    (new_stmt : later_stmts, later_uses `plusFV` uses)
  where
    (later_stmts, later_uses) = segsToStmts empty_rec_stmt segs fvs_later
    new_stmt | non_rec   = head ss
             | otherwise = L (getLoc (head ss)) rec_stmt
    rec_stmt = empty_rec_stmt { recS_stmts     = ss
                              , recS_later_ids = nameSetToList used_later
                              , recS_rec_ids   = nameSetToList fwds }
    non_rec    = isSingleton ss && isEmptyNameSet fwds
    used_later = defs `intersectNameSet` later_uses
                                -- The ones needed after the RecStmt
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection{Assertion utils}
%*                                                                      *
%************************************************************************

\begin{code}
srcSpanPrimLit :: DynFlags -> SrcSpan -> HsExpr Name
srcSpanPrimLit dflags span
    = HsLit (HsStringPrim (mkFastString (showSDocOneLine dflags (ppr span))))

mkAssertErrorExpr :: RnM (HsExpr Name)
-- Return an expression for (assertError "Foo.hs:27")
mkAssertErrorExpr
  = do sloc <- getSrcSpanM
       dflags <- getDynFlags
       return (HsApp (L sloc (HsVar assertErrorName))
                     (L sloc (srcSpanPrimLit dflags sloc)))
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
%*                                                                      *
\subsubsection{Errors}
%*                                                                      *
%************************************************************************

\begin{code}
checkEmptyStmts :: HsStmtContext Name -> RnM ()
-- We've seen an empty sequence of Stmts... is that ok?
checkEmptyStmts ctxt
  = unless (okEmpty ctxt) (addErr (emptyErr ctxt))

okEmpty :: HsStmtContext a -> Bool
okEmpty (PatGuard {}) = True
okEmpty _             = False

emptyErr :: HsStmtContext Name -> SDoc
emptyErr (ParStmtCtxt {})   = ptext (sLit "Empty statement group in parallel comprehension")
emptyErr (TransStmtCtxt {}) = ptext (sLit "Empty statement group preceding 'group' or 'then'")
emptyErr ctxt               = ptext (sLit "Empty") <+> pprStmtContext ctxt

----------------------
checkLastStmt :: HsStmtContext Name
              -> LStmt RdrName
              -> RnM (LStmt RdrName)
checkLastStmt ctxt lstmt@(L loc stmt)
  = case ctxt of
      ListComp  -> check_comp
      MonadComp -> check_comp
      PArrComp  -> check_comp
      ArrowExpr -> check_do
      DoExpr    -> check_do
      MDoExpr   -> check_do
      _         -> check_other
  where
    check_do    -- Expect ExprStmt, and change it to LastStmt
      = case stmt of
          ExprStmt e _ _ _ -> return (L loc (mkLastStmt e))
          LastStmt {}      -> return lstmt   -- "Deriving" clauses may generate a
                                             -- LastStmt directly (unlike the parser)
          _                -> do { addErr (hang last_error 2 (ppr stmt)); return lstmt }
    last_error = (ptext (sLit "The last statement in") <+> pprAStmtContext ctxt
                  <+> ptext (sLit "must be an expression"))

    check_comp  -- Expect LastStmt; this should be enforced by the parser!
      = case stmt of
          LastStmt {} -> return lstmt
          _           -> pprPanic "checkLastStmt" (ppr lstmt)

    check_other -- Behave just as if this wasn't the last stmt
      = do { checkStmt ctxt lstmt; return lstmt }

-- Checking when a particular Stmt is ok
checkStmt :: HsStmtContext Name
          -> LStmt RdrName
          -> RnM ()
checkStmt ctxt (L _ stmt)
  = do { dflags <- getDynFlags
       ; case okStmt dflags ctxt stmt of
           Nothing    -> return ()
           Just extra -> addErr (msg $$ extra) }
  where
   msg = sep [ ptext (sLit "Unexpected") <+> pprStmtCat stmt <+> ptext (sLit "statement")
             , ptext (sLit "in") <+> pprAStmtContext ctxt ]

pprStmtCat :: Stmt a -> SDoc
pprStmtCat (TransStmt {})     = ptext (sLit "transform")
pprStmtCat (LastStmt {})      = ptext (sLit "return expression")
pprStmtCat (ExprStmt {})      = ptext (sLit "exprssion")
pprStmtCat (BindStmt {})      = ptext (sLit "binding")
pprStmtCat (LetStmt {})       = ptext (sLit "let")
pprStmtCat (RecStmt {})       = ptext (sLit "rec")
pprStmtCat (ParStmt {})       = ptext (sLit "parallel")

------------
isOK, notOK :: Maybe SDoc
isOK  = Nothing
notOK = Just empty

okStmt, okDoStmt, okCompStmt, okParStmt, okPArrStmt
   :: DynFlags -> HsStmtContext Name
   -> Stmt RdrName -> Maybe SDoc
-- Return Nothing if OK, (Just extra) if not ok
-- The "extra" is an SDoc that is appended to an generic error message

okStmt dflags ctxt stmt
  = case ctxt of
      PatGuard {}        -> okPatGuardStmt stmt
      ParStmtCtxt ctxt   -> okParStmt  dflags ctxt stmt
      DoExpr             -> okDoStmt   dflags ctxt stmt
      MDoExpr            -> okDoStmt   dflags ctxt stmt
      ArrowExpr          -> okDoStmt   dflags ctxt stmt
      GhciStmt           -> okDoStmt   dflags ctxt stmt
      ListComp           -> okCompStmt dflags ctxt stmt
      MonadComp          -> okCompStmt dflags ctxt stmt
      PArrComp           -> okPArrStmt dflags ctxt stmt
      TransStmtCtxt ctxt -> okStmt dflags ctxt stmt

-------------
okPatGuardStmt :: Stmt RdrName -> Maybe SDoc
okPatGuardStmt stmt
  = case stmt of
      ExprStmt {} -> isOK
      BindStmt {} -> isOK
      LetStmt {}  -> isOK
      _           -> notOK

-------------
okParStmt dflags ctxt stmt
  = case stmt of
      LetStmt (HsIPBinds {}) -> notOK
      _                      -> okStmt dflags ctxt stmt

----------------
okDoStmt dflags ctxt stmt
  = case stmt of
       RecStmt {}
         | Opt_RecursiveDo `xopt` dflags -> isOK
         | ArrowExpr <- ctxt -> isOK    -- Arrows allows 'rec'
         | otherwise         -> Just (ptext (sLit "Use -XRecursiveDo"))
       BindStmt {} -> isOK
       LetStmt {}  -> isOK
       ExprStmt {} -> isOK
       _           -> notOK

----------------
okCompStmt dflags _ stmt
  = case stmt of
       BindStmt {} -> isOK
       LetStmt {}  -> isOK
       ExprStmt {} -> isOK
       ParStmt {}
         | Opt_ParallelListComp `xopt` dflags -> isOK
         | otherwise -> Just (ptext (sLit "Use -XParallelListComp"))
       TransStmt {}
         | Opt_TransformListComp `xopt` dflags -> isOK
         | otherwise -> Just (ptext (sLit "Use -XTransformListComp"))
       RecStmt {}  -> notOK
       LastStmt {} -> notOK  -- Should not happen (dealt with by checkLastStmt)

----------------
okPArrStmt dflags _ stmt
  = case stmt of
       BindStmt {} -> isOK
       LetStmt {}  -> isOK
       ExprStmt {} -> isOK
       ParStmt {}
         | Opt_ParallelListComp `xopt` dflags -> isOK
         | otherwise -> Just (ptext (sLit "Use -XParallelListComp"))
       TransStmt {} -> notOK
       RecStmt {}   -> notOK
       LastStmt {}  -> notOK  -- Should not happen (dealt with by checkLastStmt)

---------
checkTupleSection :: [HsTupArg RdrName] -> RnM ()
checkTupleSection args
  = do  { tuple_section <- xoptM Opt_TupleSections
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
