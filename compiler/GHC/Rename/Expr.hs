
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Renaming of expressions

Basically dependency analysis.

Handles @Match@, @GRHSs@, @HsExpr@, and @Qualifier@ datatypes.  In
general, all of these functions return a renamed thing, and a set of
free variables.
-}

module GHC.Rename.Expr (
        rnLExpr, rnExpr, rnStmts,
        AnnoBody
   ) where

import GHC.Prelude

import GHC.Rename.Bind ( rnLocalBindsAndThen, rnLocalValBindsLHS, rnLocalValBindsRHS
                        , rnMatchGroup, rnGRHS, makeMiniFixityEnv)
import GHC.Hs
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Env ( isBrackStage )
import GHC.Tc.Utils.Monad
import GHC.Unit.Module ( getModule, isInteractiveModule )
import GHC.Rename.Env
import GHC.Rename.Fixity
import GHC.Rename.Utils ( HsDocContext(..), bindLocalNamesFV, checkDupNames
                        , bindLocalNames
                        , mapMaybeFvRn, mapFvRn
                        , warnUnusedLocalBinds, typeAppErr
                        , checkUnusedRecordWildcard
                        , wrapGenSpan, genHsIntegralLit, genHsTyLit
                        , genHsVar, genLHsVar, genHsApp, genHsApps
                        , genAppType )
import GHC.Rename.Unbound ( reportUnboundName )
import GHC.Rename.Splice  ( rnBracket, rnSpliceExpr, checkThLocalName )
import GHC.Rename.HsType
import GHC.Rename.Pat
import GHC.Driver.Session
import GHC.Builtin.Names

import GHC.Types.FieldLabel
import GHC.Types.Fixity
import GHC.Types.Id.Make
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Reader
import GHC.Types.Unique.Set
import GHC.Types.SourceText
import GHC.Utils.Misc
import GHC.Data.List.SetOps ( removeDups )
import GHC.Utils.Error
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Outputable as Outputable
import GHC.Types.SrcLoc
import Control.Monad
import GHC.Builtin.Types ( nilDataConName )
import qualified GHC.LanguageExtensions as LangExt

import Data.List (unzip4, minimumBy)
import Data.Maybe (isJust, isNothing)
import Control.Arrow (first)
import Data.Ord
import Data.Array
import qualified Data.List.NonEmpty as NE

{- Note [Handling overloaded and rebindable constructs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For overloaded constructs (overloaded literals, lists, strings), and
rebindable constructs (e.g. if-then-else), our general plan is this,
using overloaded labels #foo as an example:

* In the RENAMER: transform
      HsOverLabel "foo"
      ==> XExpr (HsExpansion (HsOverLabel #foo)
                             (fromLabel `HsAppType` "foo"))
  We write this more compactly in concrete-syntax form like this
      #foo  ==>  fromLabel @"foo"

  Recall that in (HsExpansion orig expanded), 'orig' is the original term
  the user wrote, and 'expanded' is the expanded or desugared version
  to be typechecked.

* In the TYPECHECKER: typecheck the expansion, in this case
      fromLabel @"foo"
  The typechecker (and desugarer) will never see HsOverLabel

In effect, the renamer does a bit of desugaring. Recall GHC.Hs.Expr
Note [Rebindable syntax and HsExpansion], which describes the use of HsExpansion.

RebindableSyntax:
  If RebindableSyntax is off we use the built-in 'fromLabel', defined in
     GHC.Builtin.Names.fromLabelClassOpName
  If RebindableSyntax if ON, we look up "fromLabel" in the environment
     to get whichever one is in scope.
This is accomplished by lookupSyntaxName, and it applies to all the
constructs below.

See also Note [Handling overloaded and rebindable patterns] in GHC.Rename.Pat
for the story with patterns.

Here are the expressions that we transform in this way. Some are uniform,
but several have a little bit of special treatment:

* HsIf (if-the-else)
     if b then e1 else e2  ==>  ifThenElse b e1 e2
  We do this /only/ if rebindable syntax is on, because the coverage
  checker looks for HsIf (see GHC.HsToCore.Coverage.addTickHsExpr)
  That means the typechecker and desugarer need to understand HsIf
  for the non-rebindable-syntax case.

* OverLabel (overloaded labels, #lbl)
     #lbl  ==>  fromLabel @"lbl"
  As ever, we use lookupSyntaxName to look up 'fromLabel'
  See Note [Overloaded labels]

* ExplicitList (explicit lists [a,b,c])
  When (and only when) OverloadedLists is on
     [e1,e2]  ==>  fromListN 2 [e1,e2]
  NB: the type checker and desugarer still see ExplicitList,
      but to them it always means the built-in lists.

* SectionL and SectionR (left and right sections)
     (`op` e) ==> rightSection op e
     (e `op`) ==> leftSection  (op e)
  where `leftSection` and `rightSection` are representation-polymorphic
  wired-in Ids. See Note [Left and right sections]

* It's a bit painful to transform `OpApp e1 op e2` to a `HsExpansion`
  form, because the renamer does precedence rearrangement after name
  resolution.  So the renamer leaves an OpApp as an OpApp.

  The typechecker turns `OpApp` into a use of `HsExpansion`
  on the fly, in GHC.Tc.Gen.Head.splitHsApps.  RebindableSyntax
  does not affect this.

Note [Overloaded labels]
~~~~~~~~~~~~~~~~~~~~~~~~
For overloaded labels, note that we /only/ apply `fromLabel` to the
Symbol argument, so the resulting expression has type
    fromLabel @"foo" :: forall a. IsLabel "foo" a => a
Now ordinary Visible Type Application can be used to instantiate the 'a':
the user may have written (#foo @Int).

Notice that this all works fine in a kind-polymorphic setting (#19154).
Suppose we have
    fromLabel :: forall {k1} {k2} (a:k1). blah

Then we want to instantiate those inferred quantifiers k1,k2, before
type-applying to "foo", so we get
    fromLabel @Symbol @blah @"foo" ...

And those inferred kind quantifiers will indeed be instantiated when we
typecheck the renamed-syntax call (fromLabel @"foo").
-}

{-
************************************************************************
*                                                                      *
\subsubsection{Expressions}
*                                                                      *
************************************************************************
-}

rnExprs :: [LHsExpr GhcPs] -> RnM ([LHsExpr GhcRn], FreeVars)
rnExprs ls = rnExprs' ls emptyUniqSet
 where
  rnExprs' [] acc = return ([], acc)
  rnExprs' (expr:exprs) acc =
   do { (expr', fvExpr) <- rnLExpr expr
        -- Now we do a "seq" on the free vars because typically it's small
        -- or empty, especially in very long lists of constants
      ; let  acc' = acc `plusFV` fvExpr
      ; (exprs', fvExprs) <- acc' `seq` rnExprs' exprs acc'
      ; return (expr':exprs', fvExprs) }

-- Variables. We look up the variable and return the resulting name.

rnLExpr :: LHsExpr GhcPs -> RnM (LHsExpr GhcRn, FreeVars)
rnLExpr = wrapLocFstMA rnExpr

rnExpr :: HsExpr GhcPs -> RnM (HsExpr GhcRn, FreeVars)

finishHsVar :: LocatedA Name -> RnM (HsExpr GhcRn, FreeVars)
-- Separated from rnExpr because it's also used
-- when renaming infix expressions
finishHsVar (L l name)
 = do { this_mod <- getModule
      ; when (nameIsLocalOrFrom this_mod name) $
        checkThLocalName name
      ; return (HsVar noExtField (L (la2na l) name), unitFV name) }

rnUnboundVar :: RdrName -> RnM (HsExpr GhcRn, FreeVars)
rnUnboundVar v =
  if isUnqual v
  then -- Treat this as a "hole"
       -- Do not fail right now; instead, return HsUnboundVar
       -- and let the type checker report the error
       return (HsUnboundVar noExtField (rdrNameOcc v), emptyFVs)

        else -- Fail immediately (qualified name)
             do { n <- reportUnboundName v
                ; return (HsVar noExtField (noLocA n), emptyFVs) }

rnExpr (HsVar _ (L l v))
  = do { dflags <- getDynFlags
       ; mb_name <- lookupExprOccRn v

       ; case mb_name of {
           Nothing -> rnUnboundVar v ;
           Just (NormalGreName name)
              | name == nilDataConName -- Treat [] as an ExplicitList, so that
                                       -- OverloadedLists works correctly
                                       -- Note [Empty lists] in GHC.Hs.Expr
              , xopt LangExt.OverloadedLists dflags
              -> rnExpr (ExplicitList noAnn [])

              | otherwise
              -> finishHsVar (L (na2la l) name) ;
            Just (FieldGreName fl)
              -> do { let sel_name = flSelector fl
                    ; this_mod <- getModule
                    ; when (nameIsLocalOrFrom this_mod sel_name) $
                        checkThLocalName sel_name
                    ; return ( HsRecSel noExtField (FieldOcc sel_name (L l v) ), unitFV sel_name)
                    }
         }
       }

rnExpr (HsIPVar x v)
  = return (HsIPVar x v, emptyFVs)

rnExpr (HsUnboundVar _ v)
  = return (HsUnboundVar noExtField v, emptyFVs)

-- HsOverLabel: see Note [Handling overloaded and rebindable constructs]
rnExpr (HsOverLabel _ v)
  = do { (from_label, fvs) <- lookupSyntaxName fromLabelClassOpName
       ; return ( mkExpandedExpr (HsOverLabel noAnn v) $
                  HsAppType noExtField (genLHsVar from_label) hs_ty_arg
                , fvs ) }
  where
    hs_ty_arg = mkEmptyWildCardBndrs $ wrapGenSpan $
                HsTyLit noExtField (HsStrTy NoSourceText v)

rnExpr (HsLit x lit@(HsString src s))
  = do { opt_OverloadedStrings <- xoptM LangExt.OverloadedStrings
       ; if opt_OverloadedStrings then
            rnExpr (HsOverLit x (mkHsIsString src s))
         else do {
            ; rnLit lit
            ; return (HsLit x (convertLit lit), emptyFVs) } }

rnExpr (HsLit x lit)
  = do { rnLit lit
       ; return (HsLit x(convertLit lit), emptyFVs) }

rnExpr (HsOverLit x lit)
  = do { ((lit', mb_neg), fvs) <- rnOverLit lit -- See Note [Negative zero]
       ; case mb_neg of
              Nothing -> return (HsOverLit x lit', fvs)
              Just neg ->
                 return (HsApp noComments (noLocA neg) (noLocA (HsOverLit x lit'))
                        , fvs ) }

rnExpr (HsApp x fun arg)
  = do { (fun',fvFun) <- rnLExpr fun
       ; (arg',fvArg) <- rnLExpr arg
       ; return (HsApp x fun' arg', fvFun `plusFV` fvArg) }

rnExpr (HsAppType _ fun arg)
  = do { type_app <- xoptM LangExt.TypeApplications
       ; unless type_app $ addErr $ typeAppErr "type" $ hswc_body arg
       ; (fun',fvFun) <- rnLExpr fun
       ; (arg',fvArg) <- rnHsWcType HsTypeCtx arg
       ; return (HsAppType NoExtField fun' arg', fvFun `plusFV` fvArg) }

rnExpr (OpApp _ e1 op e2)
  = do  { (e1', fv_e1) <- rnLExpr e1
        ; (e2', fv_e2) <- rnLExpr e2
        ; (op', fv_op) <- rnLExpr op

        -- Deal with fixity
        -- When renaming code synthesised from "deriving" declarations
        -- we used to avoid fixity stuff, but we can't easily tell any
        -- more, so I've removed the test.  Adding HsPars in GHC.Tc.Deriv.Generate
        -- should prevent bad things happening.
        ; fixity <- case op' of
              L _ (HsVar _ (L _ n)) -> lookupFixityRn n
              L _ (HsRecSel _ f)    -> lookupFieldFixityRn f
              _ -> return (Fixity NoSourceText minPrecedence InfixL)
                   -- c.f. lookupFixity for unbound

        ; lexical_negation <- xoptM LangExt.LexicalNegation
        ; let negation_handling | lexical_negation = KeepNegationIntact
                                | otherwise = ReassociateNegation
        ; final_e <- mkOpAppRn negation_handling e1' op' fixity e2'
        ; return (final_e, fv_e1 `plusFV` fv_op `plusFV` fv_e2) }

rnExpr (NegApp _ e _)
  = do { (e', fv_e)         <- rnLExpr e
       ; (neg_name, fv_neg) <- lookupSyntax negateName
       ; final_e            <- mkNegAppRn e' neg_name
       ; return (final_e, fv_e `plusFV` fv_neg) }

------------------------------------------
-- Record dot syntax

rnExpr (HsGetField _ e f)
 = do { (getField, fv_getField) <- lookupSyntaxName getFieldName
      ; (e, fv_e) <- rnLExpr e
      ; let f' = rnDotFieldOcc f
      ; return ( mkExpandedExpr
                   (HsGetField noExtField e f')
                   (mkGetField getField e (fmap (unLoc . dfoLabel) f'))
               , fv_e `plusFV` fv_getField ) }

rnExpr (HsProjection _ fs)
  = do { (getField, fv_getField) <- lookupSyntaxName getFieldName
       ; circ <- lookupOccRn compose_RDR
       ; let fs' = fmap rnDotFieldOcc fs
       ; return ( mkExpandedExpr
                    (HsProjection noExtField fs')
                    (mkProjection getField circ (map (fmap (unLoc . dfoLabel)) fs'))
                , unitFV circ `plusFV` fv_getField) }

------------------------------------------
-- Template Haskell extensions
rnExpr e@(HsBracket _ br_body) = rnBracket e br_body

rnExpr (HsSpliceE _ splice) = rnSpliceExpr splice

---------------------------------------------
--      Sections
-- See Note [Parsing sections] in GHC.Parser
rnExpr (HsPar x lpar (L loc (section@(SectionL {}))) rpar)
  = do  { (section', fvs) <- rnSection section
        ; return (HsPar x lpar (L loc section') rpar, fvs) }

rnExpr (HsPar x lpar (L loc (section@(SectionR {}))) rpar)
  = do  { (section', fvs) <- rnSection section
        ; return (HsPar x lpar (L loc section') rpar, fvs) }

rnExpr (HsPar x lpar e rpar)
  = do  { (e', fvs_e) <- rnLExpr e
        ; return (HsPar x lpar e' rpar, fvs_e) }

rnExpr expr@(SectionL {})
  = do  { addErr (sectionErr expr); rnSection expr }
rnExpr expr@(SectionR {})
  = do  { addErr (sectionErr expr); rnSection expr }

---------------------------------------------
rnExpr (HsPragE x prag expr)
  = do { (expr', fvs_expr) <- rnLExpr expr
       ; return (HsPragE x (rn_prag prag) expr', fvs_expr) }
  where
    rn_prag :: HsPragE GhcPs -> HsPragE GhcRn
    rn_prag (HsPragSCC x1 src ann) = HsPragSCC x1 src ann

rnExpr (HsLam x matches)
  = do { (matches', fvMatch) <- rnMatchGroup LambdaExpr rnLExpr matches
       ; return (HsLam x matches', fvMatch) }

rnExpr (HsLamCase x matches)
  = do { (matches', fvs_ms) <- rnMatchGroup CaseAlt rnLExpr matches
       ; return (HsLamCase x matches', fvs_ms) }

rnExpr (HsCase _ expr matches)
  = do { (new_expr, e_fvs) <- rnLExpr expr
       ; (new_matches, ms_fvs) <- rnMatchGroup CaseAlt rnLExpr matches
       ; return (HsCase noExtField new_expr new_matches, e_fvs `plusFV` ms_fvs) }

rnExpr (HsLet _ binds expr)
  = rnLocalBindsAndThen binds $ \binds' _ -> do
      { (expr',fvExpr) <- rnLExpr expr
      ; return (HsLet noExtField binds' expr', fvExpr) }

rnExpr (HsDo _ do_or_lc (L l stmts))
 = do { ((stmts1, _), fvs1) <-
          rnStmtsWithFreeVars (HsDoStmt do_or_lc) rnExpr stmts
            (\ _ -> return ((), emptyFVs))
      ; (pp_stmts, fvs2) <- postProcessStmtsForApplicativeDo do_or_lc stmts1
      ; return ( HsDo noExtField do_or_lc (L l pp_stmts), fvs1 `plusFV` fvs2 ) }

-- ExplicitList: see Note [Handling overloaded and rebindable constructs]
rnExpr (ExplicitList _ exps)
  = do  { (exps', fvs) <- rnExprs exps
        ; opt_OverloadedLists <- xoptM LangExt.OverloadedLists
        ; if not opt_OverloadedLists
          then return  (ExplicitList noExtField exps', fvs)
          else
    do { (from_list_n_name, fvs') <- lookupSyntaxName fromListNName
       ; let rn_list  = ExplicitList noExtField exps'
             lit_n    = mkIntegralLit (length exps)
             hs_lit   = genHsIntegralLit lit_n
             exp_list = genHsApps from_list_n_name [hs_lit, wrapGenSpan rn_list]
       ; return ( mkExpandedExpr rn_list exp_list
                , fvs `plusFV` fvs') } }

rnExpr (ExplicitTuple _ tup_args boxity)
  = do { checkTupleSection tup_args
       ; (tup_args', fvs) <- mapAndUnzipM rnTupArg tup_args
       ; return (ExplicitTuple noExtField tup_args' boxity, plusFVs fvs) }
  where
    rnTupArg (Present x e) = do { (e',fvs) <- rnLExpr e
                                ; return (Present x e', fvs) }
    rnTupArg (Missing _) = return (Missing noExtField, emptyFVs)

rnExpr (ExplicitSum _ alt arity expr)
  = do { (expr', fvs) <- rnLExpr expr
       ; return (ExplicitSum noExtField alt arity expr', fvs) }

rnExpr (RecordCon { rcon_con = con_id
                  , rcon_flds = rec_binds@(HsRecFields { rec_dotdot = dd }) })
  = do { con_lname@(L _ con_name) <- lookupLocatedOccRnConstr con_id
       ; (flds, fvs)   <- rnHsRecFields (HsRecFieldCon con_name) mk_hs_var rec_binds
       ; (flds', fvss) <- mapAndUnzipM rn_field flds
       ; let rec_binds' = HsRecFields { rec_flds = flds', rec_dotdot = dd }
       ; return (RecordCon { rcon_ext = noExtField
                           , rcon_con = con_lname, rcon_flds = rec_binds' }
                , fvs `plusFV` plusFVs fvss `addOneFV` con_name) }
  where
    mk_hs_var l n = HsVar noExtField (L (noAnnSrcSpan l) n)
    rn_field (L l fld) = do { (arg', fvs) <- rnLExpr (hfbRHS fld)
                            ; return (L l (fld { hfbRHS = arg' }), fvs) }

rnExpr (RecordUpd { rupd_expr = expr, rupd_flds = rbinds })
  = case rbinds of
      Left flds -> -- 'OverloadedRecordUpdate' is not in effect. Regular record update.
        do  { ; (e, fv_e) <- rnLExpr expr
              ; (rs, fv_rs) <- rnHsRecUpdFields flds
              ; return ( RecordUpd noExtField e (Left rs), fv_e `plusFV` fv_rs )
            }
      Right flds ->  -- 'OverloadedRecordUpdate' is in effect. Record dot update desugaring.
        do { ; unlessXOptM LangExt.RebindableSyntax $
                 addErr $ TcRnUnknownMessage $ mkPlainError noHints $
                   text "RebindableSyntax is required if OverloadedRecordUpdate is enabled."
             ; let punnedFields = [fld | (L _ fld) <- flds, hfbPun fld]
             ; punsEnabled <-xoptM LangExt.NamedFieldPuns
             ; unless (null punnedFields || punsEnabled) $
                 addErr $ TcRnUnknownMessage $ mkPlainError noHints $
                   text "For this to work enable NamedFieldPuns."
             ; (getField, fv_getField) <- lookupSyntaxName getFieldName
             ; (setField, fv_setField) <- lookupSyntaxName setFieldName
             ; (e, fv_e) <- rnLExpr expr
             ; (us, fv_us) <- rnHsUpdProjs flds
             ; return ( mkExpandedExpr
                          (RecordUpd noExtField e (Right us))
                          (mkRecordDotUpd getField setField e us)
                         , plusFVs [fv_getField, fv_setField, fv_e, fv_us] )
             }

rnExpr (ExprWithTySig _ expr pty)
  = do  { (pty', fvTy)    <- rnHsSigWcType ExprWithTySigCtx pty
        ; (expr', fvExpr) <- bindSigTyVarsFV (hsWcScopedTvs pty') $
                             rnLExpr expr
        ; return (ExprWithTySig noExtField expr' pty', fvExpr `plusFV` fvTy) }

-- HsIf: see Note [Handling overloaded and rebindable constructs]
-- Because of the coverage checker it is most convenient /not/ to
-- expand HsIf; unless we are in rebindable syntax.
rnExpr (HsIf _ p b1 b2)
  = do { (p',  fvP)  <- rnLExpr p
       ; (b1', fvB1) <- rnLExpr b1
       ; (b2', fvB2) <- rnLExpr b2
       ; let fvs_if = plusFVs [fvP, fvB1, fvB2]
             rn_if  = HsIf noExtField  p' b1' b2'

       -- Deal with rebindable syntax
       -- See Note [Handling overloaded and rebindable constructs]
       ; mb_ite <- lookupIfThenElse
       ; case mb_ite of
            Nothing  -- Non rebindable-syntax case
              -> return (rn_if, fvs_if)

            Just ite_name   -- Rebindable-syntax case
              -> do { let ds_if = genHsApps ite_name [p', b1', b2']
                          fvs   = plusFVs [fvs_if, unitFV ite_name]
                    ; return (mkExpandedExpr rn_if ds_if, fvs) } }

rnExpr (HsMultiIf _ alts)
  = do { (alts', fvs) <- mapFvRn (rnGRHS IfAlt rnLExpr) alts
       ; return (HsMultiIf noExtField alts', fvs) }

rnExpr (ArithSeq _ _ seq)
  = do { opt_OverloadedLists <- xoptM LangExt.OverloadedLists
       ; (new_seq, fvs) <- rnArithSeq seq
       ; if opt_OverloadedLists
           then do {
            ; (from_list_name, fvs') <- lookupSyntax fromListName
            ; return (ArithSeq noExtField (Just from_list_name) new_seq
                     , fvs `plusFV` fvs') }
           else
            return (ArithSeq noExtField Nothing new_seq, fvs) }

{-
************************************************************************
*                                                                      *
        Static values
*                                                                      *
************************************************************************

For the static form we check that it is not used in splices.
We also collect the free variables of the term which come from
this module. See Note [Grand plan for static forms] in GHC.Iface.Tidy.StaticPtrTable.
-}

rnExpr e@(HsStatic _ expr) = do
    -- Normally, you wouldn't be able to construct a static expression without
    -- first enabling -XStaticPointers in the first place, since that extension
    -- is what makes the parser treat `static` as a keyword. But this is not a
    -- sufficient safeguard, as one can construct static expressions by another
    -- mechanism: Template Haskell (see #14204). To ensure that GHC is
    -- absolutely prepared to cope with static forms, we check for
    -- -XStaticPointers here as well.
    unlessXOptM LangExt.StaticPointers $
      addErr $ TcRnUnknownMessage $ mkPlainError noHints $
        hang (text "Illegal static expression:" <+> ppr e)
                  2 (text "Use StaticPointers to enable this extension")
    (expr',fvExpr) <- rnLExpr expr
    stage <- getStage
    case stage of
      Splice _ -> addErr $ TcRnUnknownMessage $ mkPlainError noHints $ sep
             [ text "static forms cannot be used in splices:"
             , nest 2 $ ppr e
             ]
      _ -> return ()
    mod <- getModule
    let fvExpr' = filterNameSet (nameIsLocalOrFrom mod) fvExpr
    return (HsStatic fvExpr' expr', fvExpr)

{- *********************************************************************
*                                                                      *
        Arrow notation
*                                                                      *
********************************************************************* -}

rnExpr (HsProc x pat body)
  = newArrowScope $
    rnPat ProcExpr pat $ \ pat' -> do
      { (body',fvBody) <- rnCmdTop body
      ; return (HsProc x pat' body', fvBody) }

rnExpr other = pprPanic "rnExpr: unexpected expression" (ppr other)
        -- HsWrap

{- *********************************************************************
*                                                                      *
        Operator sections
*                                                                      *
********************************************************************* -}


rnSection :: HsExpr GhcPs -> RnM (HsExpr GhcRn, FreeVars)
-- See Note [Parsing sections] in GHC.Parser
-- Also see Note [Handling overloaded and rebindable constructs]

rnSection section@(SectionR x op expr)
  -- See Note [Left and right sections]
  = do  { (op', fvs_op)     <- rnLExpr op
        ; (expr', fvs_expr) <- rnLExpr expr
        ; checkSectionPrec InfixR section op' expr'
        ; let rn_section = SectionR x op' expr'
              ds_section = genHsApps rightSectionName [op',expr']
        ; return ( mkExpandedExpr rn_section ds_section
                 , fvs_op `plusFV` fvs_expr) }

rnSection section@(SectionL x expr op)
  -- See Note [Left and right sections]
  = do  { (expr', fvs_expr) <- rnLExpr expr
        ; (op', fvs_op)     <- rnLExpr op
        ; checkSectionPrec InfixL section op' expr'
        ; postfix_ops <- xoptM LangExt.PostfixOperators
                        -- Note [Left and right sections]
        ; let rn_section = SectionL x expr' op'
              ds_section
                | postfix_ops = HsApp noAnn op' expr'
                | otherwise   = genHsApps leftSectionName
                                   [wrapGenSpan $ HsApp noAnn op' expr']
        ; return ( mkExpandedExpr rn_section ds_section
                 , fvs_op `plusFV` fvs_expr) }

rnSection other = pprPanic "rnSection" (ppr other)

{- Note [Left and right sections]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Dealing with left sections (x *) and right sections (* x) is
surprisingly fiddly.  We expand like this
     (`op` e) ==> rightSection op e
     (e `op`) ==> leftSection  (op e)

Using an auxiliary function in this way avoids the awkwardness of
generating a lambda, esp if `e` is a redex, so we *don't* want
to generate `(\x -> op x e)`. See Historical
Note [Desugaring operator sections]

Here are their definitions:
   leftSection :: forall r1 r2 n (a:TYPE r1) (b:TYPE r2).
                  (a %n-> b) -> a %n-> b
   leftSection f x = f x

   rightSection :: forall r1 r2 r3 (a:TYPE r1) (b:TYPE r2) (c:TYPE r3).
                   (a %n1 -> b %n2-> c) -> b %n2-> a %n1-> c
   rightSection f y x = f x y

Note the wrinkles:

* We do /not/ use lookupSyntaxName, which would make left and right
  section fall under RebindableSyntax.  Reason: it would be a user-
  facing change, and there are some tricky design choices (#19354).
  Plus, infix operator applications would be trickier to make
  rebindable, so it'd be inconsistent to do so for sections.

  TL;DR: we still us the renamer-expansion mechanism for operator
  sections , but only to eliminate special-purpose code paths in the
  renamer and desugarer.

* leftSection and rightSection must be representation-polymorphic, to allow
  (+# 4#) and (4# +#) to work. See GHC.Types.Id.Make.
  Note [Wired-in Ids for rebindable syntax] in

* leftSection and rightSection must be multiplicity-polymorphic.
  (Test linear/should_compile/OldList showed this up.)

* Because they are representation-polymorphic, we have to define them
  as wired-in Ids, with compulsory inlining.  See
  GHC.Types.Id.Make.leftSectionId, rightSectionId.

* leftSection is just ($) really; but unlike ($) it is
  representation-polymorphic in the result type, so we can write
  `(x +#)`, say.

* The type of leftSection must have an arrow in its first argument,
  because (x `ord`) should be rejected, because ord does not take two
  arguments

* It's important that we define leftSection in an eta-expanded way,
  (i.e. not leftSection f = f), so that
      (True `undefined`) `seq` ()
      = (leftSection (undefined True) `seq` ())
  evaluates to () and not undefined

* If PostfixOperators is ON, then we expand a left section like this:
      (e `op`)  ==>   op e
  with no auxiliary function at all.  Simple!


Historical Note [Desugaring operator sections]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This Note explains some historical trickiness in desugaring left and
right sections.  That trickiness has completely disappeared now that
we desugar to calls to 'leftSection` and `rightSection`, but I'm
leaving it here to remind us how nice the new story is.

Desugaring left sections with -XPostfixOperators is straightforward: convert
(expr `op`) to (op expr).

Without -XPostfixOperators it's a bit more tricky. At first it looks as if we
can convert

    (expr `op`)

naively to

    \x -> op expr x

But no!  expr might be a redex, and we can lose laziness badly this
way.  Consider

    map (expr `op`) xs

for example. If expr were a redex then eta-expanding naively would
result in multiple evaluations where the user might only have expected one.

So we convert instead to

    let y = expr in \x -> op y x

Also, note that we must do this for both right and (perhaps surprisingly) left
sections. Why are left sections necessary? Consider the program (found in #18151),

    seq (True `undefined`) ()

according to the Haskell Report this should reduce to () (as it specifies
desugaring via eta expansion). However, if we fail to eta expand we will rather
bottom. Consequently, we must eta expand even in the case of a left section.

If `expr` is actually just a variable, say, then the simplifier
will inline `y`, eliminating the redundant `let`.

Note that this works even in the case that `expr` is unlifted. In this case
bindNonRec will automatically do the right thing, giving us:

    case expr of y -> (\x -> op y x)

See #18151.
-}

{-
************************************************************************
*                                                                      *
        Field Labels
*                                                                      *
************************************************************************
-}

rnDotFieldOcc :: Located (DotFieldOcc GhcPs) -> Located (DotFieldOcc GhcRn)
rnDotFieldOcc (L l (DotFieldOcc x label)) = L l (DotFieldOcc x label)

rnFieldLabelStrings :: FieldLabelStrings GhcPs -> FieldLabelStrings GhcRn
rnFieldLabelStrings (FieldLabelStrings fls) = FieldLabelStrings (map rnDotFieldOcc fls)

{-
************************************************************************
*                                                                      *
        Arrow commands
*                                                                      *
************************************************************************
-}

rnCmdArgs :: [LHsCmdTop GhcPs] -> RnM ([LHsCmdTop GhcRn], FreeVars)
rnCmdArgs [] = return ([], emptyFVs)
rnCmdArgs (arg:args)
  = do { (arg',fvArg) <- rnCmdTop arg
       ; (args',fvArgs) <- rnCmdArgs args
       ; return (arg':args', fvArg `plusFV` fvArgs) }

rnCmdTop :: LHsCmdTop GhcPs -> RnM (LHsCmdTop GhcRn, FreeVars)
rnCmdTop = wrapLocFstM rnCmdTop'
 where
  rnCmdTop' :: HsCmdTop GhcPs -> RnM (HsCmdTop GhcRn, FreeVars)
  rnCmdTop' (HsCmdTop _ cmd)
   = do { (cmd', fvCmd) <- rnLCmd cmd
        ; let cmd_names = [arrAName, composeAName, firstAName] ++
                          nameSetElemsStable (methodNamesCmd (unLoc cmd'))
        -- Generate the rebindable syntax for the monad
        ; (cmd_names', cmd_fvs) <- lookupSyntaxNames cmd_names

        ; return (HsCmdTop (cmd_names `zip` cmd_names') cmd',
                  fvCmd `plusFV` cmd_fvs) }

rnLCmd :: LHsCmd GhcPs -> RnM (LHsCmd GhcRn, FreeVars)
rnLCmd = wrapLocFstMA rnCmd

rnCmd :: HsCmd GhcPs -> RnM (HsCmd GhcRn, FreeVars)

rnCmd (HsCmdArrApp _ arrow arg ho rtl)
  = do { (arrow',fvArrow) <- select_arrow_scope (rnLExpr arrow)
       ; (arg',fvArg) <- rnLExpr arg
       ; return (HsCmdArrApp noExtField arrow' arg' ho rtl,
                 fvArrow `plusFV` fvArg) }
  where
    select_arrow_scope tc = case ho of
        HsHigherOrderApp -> tc
        HsFirstOrderApp  -> escapeArrowScope tc
        -- See Note [Escaping the arrow scope] in GHC.Tc.Types
        -- Before renaming 'arrow', use the environment of the enclosing
        -- proc for the (-<) case.
        -- Local bindings, inside the enclosing proc, are not in scope
        -- inside 'arrow'.  In the higher-order case (-<<), they are.

-- infix form
rnCmd (HsCmdArrForm _ op _ (Just _) [arg1, arg2])
  = do { (op',fv_op) <- escapeArrowScope (rnLExpr op)
       ; let L _ (HsVar _ (L _ op_name)) = op'
       ; (arg1',fv_arg1) <- rnCmdTop arg1
       ; (arg2',fv_arg2) <- rnCmdTop arg2
        -- Deal with fixity
       ; fixity <- lookupFixityRn op_name
       ; final_e <- mkOpFormRn arg1' op' fixity arg2'
       ; return (final_e, fv_arg1 `plusFV` fv_op `plusFV` fv_arg2) }

rnCmd (HsCmdArrForm _ op f fixity cmds)
  = do { (op',fvOp) <- escapeArrowScope (rnLExpr op)
       ; (cmds',fvCmds) <- rnCmdArgs cmds
       ; return ( HsCmdArrForm noExtField op' f fixity cmds'
                , fvOp `plusFV` fvCmds) }

rnCmd (HsCmdApp x fun arg)
  = do { (fun',fvFun) <- rnLCmd  fun
       ; (arg',fvArg) <- rnLExpr arg
       ; return (HsCmdApp x fun' arg', fvFun `plusFV` fvArg) }

rnCmd (HsCmdLam _ matches)
  = do { (matches', fvMatch) <- rnMatchGroup LambdaExpr rnLCmd matches
       ; return (HsCmdLam noExtField matches', fvMatch) }

rnCmd (HsCmdPar x lpar e rpar)
  = do  { (e', fvs_e) <- rnLCmd e
        ; return (HsCmdPar x lpar e' rpar, fvs_e) }

rnCmd (HsCmdCase _ expr matches)
  = do { (new_expr, e_fvs) <- rnLExpr expr
       ; (new_matches, ms_fvs) <- rnMatchGroup CaseAlt rnLCmd matches
       ; return (HsCmdCase noExtField new_expr new_matches
                , e_fvs `plusFV` ms_fvs) }

rnCmd (HsCmdLamCase x matches)
  = do { (new_matches, ms_fvs) <- rnMatchGroup CaseAlt rnLCmd matches
       ; return (HsCmdLamCase x new_matches, ms_fvs) }

rnCmd (HsCmdIf _ _ p b1 b2)
  = do { (p', fvP) <- rnLExpr p
       ; (b1', fvB1) <- rnLCmd b1
       ; (b2', fvB2) <- rnLCmd b2

       ; mb_ite <- lookupIfThenElse
       ; let (ite, fvITE) = case mb_ite of
                Just ite_name -> (mkRnSyntaxExpr ite_name, unitFV ite_name)
                Nothing       -> (NoSyntaxExprRn,          emptyFVs)

       ; return (HsCmdIf noExtField ite p' b1' b2', plusFVs [fvITE, fvP, fvB1, fvB2])}

rnCmd (HsCmdLet _ binds cmd)
  = rnLocalBindsAndThen binds $ \ binds' _ -> do
      { (cmd',fvExpr) <- rnLCmd cmd
      ; return (HsCmdLet noExtField binds' cmd', fvExpr) }

rnCmd (HsCmdDo _ (L l stmts))
  = do  { ((stmts', _), fvs) <-
            rnStmts ArrowExpr rnCmd stmts (\ _ -> return ((), emptyFVs))
        ; return ( HsCmdDo noExtField (L l stmts'), fvs ) }

---------------------------------------------------
type CmdNeeds = FreeVars        -- Only inhabitants are
                                --      appAName, choiceAName, loopAName

-- find what methods the Cmd needs (loop, choice, apply)
methodNamesLCmd :: LHsCmd GhcRn -> CmdNeeds
methodNamesLCmd = methodNamesCmd . unLoc

methodNamesCmd :: HsCmd GhcRn -> CmdNeeds

methodNamesCmd (HsCmdArrApp _ _arrow _arg HsFirstOrderApp _rtl)
  = emptyFVs
methodNamesCmd (HsCmdArrApp _ _arrow _arg HsHigherOrderApp _rtl)
  = unitFV appAName
methodNamesCmd (HsCmdArrForm {}) = emptyFVs

methodNamesCmd (HsCmdPar _ _ c _) = methodNamesLCmd c

methodNamesCmd (HsCmdIf _ _ _ c1 c2)
  = methodNamesLCmd c1 `plusFV` methodNamesLCmd c2 `addOneFV` choiceAName

methodNamesCmd (HsCmdLet _ _ c)          = methodNamesLCmd c
methodNamesCmd (HsCmdDo _ (L _ stmts))   = methodNamesStmts stmts
methodNamesCmd (HsCmdApp _ c _)          = methodNamesLCmd c
methodNamesCmd (HsCmdLam _ match)        = methodNamesMatch match

methodNamesCmd (HsCmdCase _ _ matches)
  = methodNamesMatch matches `addOneFV` choiceAName
methodNamesCmd (HsCmdLamCase _ matches)
  = methodNamesMatch matches `addOneFV` choiceAName

--methodNamesCmd _ = emptyFVs
   -- Other forms can't occur in commands, but it's not convenient
   -- to error here so we just do what's convenient.
   -- The type checker will complain later

---------------------------------------------------
methodNamesMatch :: MatchGroup GhcRn (LHsCmd GhcRn) -> FreeVars
methodNamesMatch (MG { mg_alts = L _ ms })
  = plusFVs (map do_one ms)
 where
    do_one (L _ (Match { m_grhss = grhss })) = methodNamesGRHSs grhss

-------------------------------------------------
-- gaw 2004
methodNamesGRHSs :: GRHSs GhcRn (LHsCmd GhcRn) -> FreeVars
methodNamesGRHSs (GRHSs _ grhss _) = plusFVs (map methodNamesGRHS grhss)

-------------------------------------------------

methodNamesGRHS :: Located (GRHS GhcRn (LHsCmd GhcRn)) -> CmdNeeds
methodNamesGRHS (L _ (GRHS _ _ rhs)) = methodNamesLCmd rhs

---------------------------------------------------
methodNamesStmts :: [LStmtLR GhcRn GhcRn (LHsCmd GhcRn)] -> FreeVars
methodNamesStmts stmts = plusFVs (map methodNamesLStmt stmts)

---------------------------------------------------
methodNamesLStmt :: LStmtLR GhcRn GhcRn (LHsCmd GhcRn) -> FreeVars
methodNamesLStmt = methodNamesStmt . unLoc

methodNamesStmt :: StmtLR GhcRn GhcRn (LHsCmd GhcRn) -> FreeVars
methodNamesStmt (LastStmt _ cmd _ _)           = methodNamesLCmd cmd
methodNamesStmt (BodyStmt _ cmd _ _)           = methodNamesLCmd cmd
methodNamesStmt (BindStmt _ _ cmd)             = methodNamesLCmd cmd
methodNamesStmt (RecStmt { recS_stmts = L _ stmts }) =
  methodNamesStmts stmts `addOneFV` loopAName
methodNamesStmt (LetStmt {})                   = emptyFVs
methodNamesStmt (ParStmt {})                   = emptyFVs
methodNamesStmt (TransStmt {})                 = emptyFVs
methodNamesStmt ApplicativeStmt{}              = emptyFVs
   -- ParStmt and TransStmt can't occur in commands, but it's not
   -- convenient to error here so we just do what's convenient

{-
************************************************************************
*                                                                      *
        Arithmetic sequences
*                                                                      *
************************************************************************
-}

rnArithSeq :: ArithSeqInfo GhcPs -> RnM (ArithSeqInfo GhcRn, FreeVars)
rnArithSeq (From expr)
 = do { (expr', fvExpr) <- rnLExpr expr
      ; return (From expr', fvExpr) }

rnArithSeq (FromThen expr1 expr2)
 = do { (expr1', fvExpr1) <- rnLExpr expr1
      ; (expr2', fvExpr2) <- rnLExpr expr2
      ; return (FromThen expr1' expr2', fvExpr1 `plusFV` fvExpr2) }

rnArithSeq (FromTo expr1 expr2)
 = do { (expr1', fvExpr1) <- rnLExpr expr1
      ; (expr2', fvExpr2) <- rnLExpr expr2
      ; return (FromTo expr1' expr2', fvExpr1 `plusFV` fvExpr2) }

rnArithSeq (FromThenTo expr1 expr2 expr3)
 = do { (expr1', fvExpr1) <- rnLExpr expr1
      ; (expr2', fvExpr2) <- rnLExpr expr2
      ; (expr3', fvExpr3) <- rnLExpr expr3
      ; return (FromThenTo expr1' expr2' expr3',
                plusFVs [fvExpr1, fvExpr2, fvExpr3]) }

{-
************************************************************************
*                                                                      *
\subsubsection{@Stmt@s: in @do@ expressions}
*                                                                      *
************************************************************************
-}

{-
Note [Deterministic ApplicativeDo and RecursiveDo desugaring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Both ApplicativeDo and RecursiveDo need to create tuples not
present in the source text.

For ApplicativeDo we create:

  (a,b,c) <- (\c b a -> (a,b,c)) <$>

For RecursiveDo we create:

  mfix (\ ~(a,b,c) -> do ...; return (a',b',c'))

The order of the components in those tuples needs to be stable
across recompilations, otherwise they can get optimized differently
and we end up with incompatible binaries.
To get a stable order we use nameSetElemsStable.
See Note [Deterministic UniqFM] to learn more about nondeterminism.
-}

type AnnoBody body
  = ( Outputable (body GhcPs)
    , Anno (StmtLR GhcPs GhcPs (LocatedA (body GhcPs))) ~ SrcSpanAnnA
    , Anno (StmtLR GhcRn GhcPs (LocatedA (body GhcPs))) ~ SrcSpanAnnA
    , Anno (StmtLR GhcRn GhcRn (LocatedA (body GhcRn))) ~ SrcSpanAnnA
    )

-- | Rename some Stmts
rnStmts :: AnnoBody body
        => HsStmtContext GhcRn
        -> (body GhcPs -> RnM (body GhcRn, FreeVars))
           -- ^ How to rename the body of each statement (e.g. rnLExpr)
        -> [LStmt GhcPs (LocatedA (body GhcPs))]
           -- ^ Statements
        -> ([Name] -> RnM (thing, FreeVars))
           -- ^ if these statements scope over something, this renames it
           -- and returns the result.
        -> RnM (([LStmt GhcRn (LocatedA (body GhcRn))], thing), FreeVars)
rnStmts ctxt rnBody stmts thing_inside
 = do { ((stmts', thing), fvs) <- rnStmtsWithFreeVars ctxt rnBody stmts thing_inside
      ; return ((map fst stmts', thing), fvs) }

-- | maybe rearrange statements according to the ApplicativeDo transformation
postProcessStmtsForApplicativeDo
  :: HsDoFlavour
  -> [(ExprLStmt GhcRn, FreeVars)]
  -> RnM ([ExprLStmt GhcRn], FreeVars)
postProcessStmtsForApplicativeDo ctxt stmts
  = do {
       -- rearrange the statements using ApplicativeStmt if
       -- -XApplicativeDo is on.  Also strip out the FreeVars attached
       -- to each Stmt body.
         ado_is_on <- xoptM LangExt.ApplicativeDo
       ; let is_do_expr | DoExpr{} <- ctxt = True
                        | otherwise = False
       -- don't apply the transformation inside TH brackets, because
       -- GHC.HsToCore.Quote does not handle ApplicativeDo.
       ; in_th_bracket <- isBrackStage <$> getStage
       ; if ado_is_on && is_do_expr && not in_th_bracket
            then do { traceRn "ppsfa" (ppr stmts)
                    ; rearrangeForApplicativeDo ctxt stmts }
            else noPostProcessStmts (HsDoStmt ctxt) stmts }

-- | strip the FreeVars annotations from statements
noPostProcessStmts
  :: HsStmtContext GhcRn
  -> [(LStmt GhcRn (LocatedA (body GhcRn)), FreeVars)]
  -> RnM ([LStmt GhcRn (LocatedA (body GhcRn))], FreeVars)
noPostProcessStmts _ stmts = return (map fst stmts, emptyNameSet)


rnStmtsWithFreeVars :: AnnoBody body
        => HsStmtContext GhcRn
        -> ((body GhcPs) -> RnM ((body GhcRn), FreeVars))
        -> [LStmt GhcPs (LocatedA (body GhcPs))]
        -> ([Name] -> RnM (thing, FreeVars))
        -> RnM ( ([(LStmt GhcRn (LocatedA (body GhcRn)), FreeVars)], thing)
               , FreeVars)
-- Each Stmt body is annotated with its FreeVars, so that
-- we can rearrange statements for ApplicativeDo.
--
-- Variables bound by the Stmts, and mentioned in thing_inside,
-- do not appear in the result FreeVars

rnStmtsWithFreeVars ctxt _ [] thing_inside
  = do { checkEmptyStmts ctxt
       ; (thing, fvs) <- thing_inside []
       ; return (([], thing), fvs) }

rnStmtsWithFreeVars mDoExpr@(HsDoStmt MDoExpr{}) rnBody stmts thing_inside    -- Deal with mdo
  = -- Behave like do { rec { ...all but last... }; last }
    do { ((stmts1, (stmts2, thing)), fvs)
           <- rnStmt mDoExpr rnBody (noLocA $ mkRecStmt noAnn (noLocA all_but_last)) $ \ _ ->
              do { last_stmt' <- checkLastStmt mDoExpr last_stmt
                 ; rnStmt mDoExpr rnBody last_stmt' thing_inside }
        ; return (((stmts1 ++ stmts2), thing), fvs) }
  where
    Just (all_but_last, last_stmt) = snocView stmts

rnStmtsWithFreeVars ctxt rnBody (lstmt@(L loc _) : lstmts) thing_inside
  | null lstmts
  = setSrcSpanA loc $
    do { lstmt' <- checkLastStmt ctxt lstmt
       ; rnStmt ctxt rnBody lstmt' thing_inside }

  | otherwise
  = do { ((stmts1, (stmts2, thing)), fvs)
            <- setSrcSpanA loc                  $
               do { checkStmt ctxt lstmt
                  ; rnStmt ctxt rnBody lstmt    $ \ bndrs1 ->
                    rnStmtsWithFreeVars ctxt rnBody lstmts  $ \ bndrs2 ->
                    thing_inside (bndrs1 ++ bndrs2) }
        ; return (((stmts1 ++ stmts2), thing), fvs) }

----------------------

{-
Note [Failing pattern matches in Stmts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Many things desugar to HsStmts including monadic things like `do` and `mdo`
statements, pattern guards, and list comprehensions (see 'HsStmtContext' for an
exhaustive list). How we deal with pattern match failure is context-dependent.

 * In the case of list comprehensions and pattern guards we don't need any
   'fail' function; the desugarer ignores the fail function of 'BindStmt'
   entirely. So, for list comprehensions, the fail function is set to 'Nothing'
   for clarity.

* In the case of monadic contexts (e.g. monad comprehensions, do, and mdo
   expressions) we want pattern match failure to be desugared to the
   'fail' function (from MonadFail type class).

At one point we failed to make this distinction, leading to #11216.
-}

rnStmt :: AnnoBody body
       => HsStmtContext GhcRn
       -> (body GhcPs -> RnM (body GhcRn, FreeVars))
          -- ^ How to rename the body of the statement
       -> LStmt GhcPs (LocatedA (body GhcPs))
          -- ^ The statement
       -> ([Name] -> RnM (thing, FreeVars))
          -- ^ Rename the stuff that this statement scopes over
       -> RnM ( ([(LStmt GhcRn (LocatedA (body GhcRn)), FreeVars)], thing)
              , FreeVars)
-- Variables bound by the Stmt, and mentioned in thing_inside,
-- do not appear in the result FreeVars

rnStmt ctxt rnBody (L loc (LastStmt _ (L lb body) noret _)) thing_inside
  = do  { (body', fv_expr) <- rnBody body
        ; (ret_op, fvs1) <- if isMonadCompContext ctxt
                            then lookupStmtName ctxt returnMName
                            else return (noSyntaxExpr, emptyFVs)
                            -- The 'return' in a LastStmt is used only
                            -- for MonadComp; and we don't want to report
                            -- "non in scope: return" in other cases
                            -- #15607

        ; (thing,  fvs3) <- thing_inside []
        ; return (([(L loc (LastStmt noExtField (L lb body') noret ret_op), fv_expr)]
                  , thing), fv_expr `plusFV` fvs1 `plusFV` fvs3) }

rnStmt ctxt rnBody (L loc (BodyStmt _ (L lb body) _ _)) thing_inside
  = do  { (body', fv_expr) <- rnBody body
        ; (then_op, fvs1)  <- lookupQualifiedDoStmtName ctxt thenMName

        ; (guard_op, fvs2) <- if isComprehensionContext ctxt
                              then lookupStmtName ctxt guardMName
                              else return (noSyntaxExpr, emptyFVs)
                              -- Only list/monad comprehensions use 'guard'
                              -- Also for sub-stmts of same eg [ e | x<-xs, gd | blah ]
                              -- Here "gd" is a guard

        ; (thing, fvs3)    <- thing_inside []
        ; return ( ([(L loc (BodyStmt noExtField (L lb body') then_op guard_op), fv_expr)]
                  , thing), fv_expr `plusFV` fvs1 `plusFV` fvs2 `plusFV` fvs3) }

rnStmt ctxt rnBody (L loc (BindStmt _ pat (L lb body))) thing_inside
  = do  { (body', fv_expr) <- rnBody body
                -- The binders do not scope over the expression
        ; (bind_op, fvs1) <- lookupQualifiedDoStmtName ctxt bindMName

        ; (fail_op, fvs2) <- monadFailOp pat ctxt

        ; rnPat (StmtCtxt ctxt) pat $ \ pat' -> do
        { (thing, fvs3) <- thing_inside (collectPatBinders CollNoDictBinders pat')
        ; let xbsrn = XBindStmtRn { xbsrn_bindOp = bind_op, xbsrn_failOp = fail_op }
        ; return (( [( L loc (BindStmt xbsrn pat' (L lb body')), fv_expr )]
                  , thing),
                  fv_expr `plusFV` fvs1 `plusFV` fvs2 `plusFV` fvs3) }}
       -- fv_expr shouldn't really be filtered by the rnPatsAndThen
        -- but it does not matter because the names are unique

rnStmt _ _ (L loc (LetStmt _ binds)) thing_inside
  =     rnLocalBindsAndThen binds $ \binds' bind_fvs -> do
        { (thing, fvs) <- thing_inside (collectLocalBinders CollNoDictBinders binds')
        ; return ( ([(L loc (LetStmt noAnn binds'), bind_fvs)], thing)
                 , fvs) }

rnStmt ctxt rnBody (L loc (RecStmt { recS_stmts = L _ rec_stmts })) thing_inside
  = do  { (return_op, fvs1)  <- lookupQualifiedDoStmtName ctxt returnMName
        ; (mfix_op,   fvs2)  <- lookupQualifiedDoStmtName ctxt mfixName
        ; (bind_op,   fvs3)  <- lookupQualifiedDoStmtName ctxt bindMName
        ; let empty_rec_stmt = emptyRecStmtName { recS_ret_fn  = return_op
                                                , recS_mfix_fn = mfix_op
                                                , recS_bind_fn = bind_op }

        -- Step1: Bring all the binders of the mdo into scope
        -- (Remember that this also removes the binders from the
        -- finally-returned free-vars.)
        -- And rename each individual stmt, making a
        -- singleton segment.  At this stage the FwdRefs field
        -- isn't finished: it's empty for all except a BindStmt
        -- for which it's the fwd refs within the bind itself
        -- (This set may not be empty, because we're in a recursive
        -- context.)
        ; rnRecStmtsAndThen ctxt rnBody rec_stmts   $ \ segs -> do
        { let bndrs = nameSetElemsStable $
                        foldr (unionNameSet . (\(ds,_,_,_) -> ds))
                              emptyNameSet
                              segs
          -- See Note [Deterministic ApplicativeDo and RecursiveDo desugaring]
        ; (thing, fvs_later) <- thing_inside bndrs
        -- In interactive mode, assume that all variables are used later
        ; is_interactive <- isInteractiveModule . tcg_mod <$> getGblEnv
        ; let
             final_fvs_later = if is_interactive then Nothing else Just fvs_later
             (rec_stmts', fvs) = segmentRecStmts (locA loc) ctxt empty_rec_stmt segs final_fvs_later
        -- We aren't going to try to group RecStmts with
        -- ApplicativeDo, so attaching empty FVs is fine.
        ; return ( ((zip rec_stmts' (repeat emptyNameSet)), thing)
                 , fvs `plusFV` fvs1 `plusFV` fvs2 `plusFV` fvs3) } }

rnStmt ctxt _ (L loc (ParStmt _ segs _ _)) thing_inside
  = do  { (mzip_op, fvs1)   <- lookupStmtNamePoly ctxt mzipName
        ; (bind_op, fvs2)   <- lookupStmtName ctxt bindMName
        ; (return_op, fvs3) <- lookupStmtName ctxt returnMName
        ; ((segs', thing), fvs4) <- rnParallelStmts (ParStmtCtxt ctxt) return_op segs thing_inside
        ; return (([(L loc (ParStmt noExtField segs' mzip_op bind_op), fvs4)], thing)
                 , fvs1 `plusFV` fvs2 `plusFV` fvs3 `plusFV` fvs4) }

rnStmt ctxt _ (L loc (TransStmt { trS_stmts = stmts, trS_by = by, trS_form = form
                              , trS_using = using })) thing_inside
  = do { -- Rename the 'using' expression in the context before the transform is begun
         (using', fvs1) <- rnLExpr using

         -- Rename the stmts and the 'by' expression
         -- Keep track of the variables mentioned in the 'by' expression
       ; ((stmts', (by', used_bndrs, thing)), fvs2)
             <- rnStmts (TransStmtCtxt ctxt) rnExpr stmts $ \ bndrs ->
                do { (by',   fvs_by) <- mapMaybeFvRn rnLExpr by
                   ; (thing, fvs_thing) <- thing_inside bndrs
                   ; let fvs = fvs_by `plusFV` fvs_thing
                         used_bndrs = filter (`elemNameSet` fvs) bndrs
                         -- The paper (Fig 5) has a bug here; we must treat any free variable
                         -- of the "thing inside", **or of the by-expression**, as used
                   ; return ((by', used_bndrs, thing), fvs) }

       -- Lookup `return`, `(>>=)` and `liftM` for monad comprehensions
       ; (return_op, fvs3) <- lookupStmtName ctxt returnMName
       ; (bind_op,   fvs4) <- lookupStmtName ctxt bindMName
       ; (fmap_op,   fvs5) <- case form of
                                ThenForm -> return (noExpr, emptyFVs)
                                _        -> lookupStmtNamePoly ctxt fmapName

       ; let all_fvs  = fvs1 `plusFV` fvs2 `plusFV` fvs3
                             `plusFV` fvs4 `plusFV` fvs5
             bndr_map = used_bndrs `zip` used_bndrs
             -- See Note [TransStmt binder map] in GHC.Hs.Expr

       ; traceRn "rnStmt: implicitly rebound these used binders:" (ppr bndr_map)
       ; return (([(L loc (TransStmt { trS_ext = noExtField
                                    , trS_stmts = stmts', trS_bndrs = bndr_map
                                    , trS_by = by', trS_using = using', trS_form = form
                                    , trS_ret = return_op, trS_bind = bind_op
                                    , trS_fmap = fmap_op }), fvs2)], thing), all_fvs) }

rnStmt _ _ (L _ ApplicativeStmt{}) _ =
  panic "rnStmt: ApplicativeStmt"

rnParallelStmts :: forall thing. HsStmtContext GhcRn
                -> SyntaxExpr GhcRn
                -> [ParStmtBlock GhcPs GhcPs]
                -> ([Name] -> RnM (thing, FreeVars))
                -> RnM (([ParStmtBlock GhcRn GhcRn], thing), FreeVars)
-- Note [Renaming parallel Stmts]
rnParallelStmts ctxt return_op segs thing_inside
  = do { orig_lcl_env <- getLocalRdrEnv
       ; rn_segs orig_lcl_env [] segs }
  where
    rn_segs :: LocalRdrEnv
            -> [Name] -> [ParStmtBlock GhcPs GhcPs]
            -> RnM (([ParStmtBlock GhcRn GhcRn], thing), FreeVars)
    rn_segs _ bndrs_so_far []
      = do { let (bndrs', dups) = removeDups cmpByOcc bndrs_so_far
           ; mapM_ dupErr dups
           ; (thing, fvs) <- bindLocalNames bndrs' (thing_inside bndrs')
           ; return (([], thing), fvs) }

    rn_segs env bndrs_so_far (ParStmtBlock x stmts _ _ : segs)
      = do { ((stmts', (used_bndrs, segs', thing)), fvs)
                    <- rnStmts ctxt rnExpr stmts $ \ bndrs ->
                       setLocalRdrEnv env       $ do
                       { ((segs', thing), fvs) <- rn_segs env (bndrs ++ bndrs_so_far) segs
                       ; let used_bndrs = filter (`elemNameSet` fvs) bndrs
                       ; return ((used_bndrs, segs', thing), fvs) }

           ; let seg' = ParStmtBlock x stmts' used_bndrs return_op
           ; return ((seg':segs', thing), fvs) }

    cmpByOcc n1 n2 = nameOccName n1 `compare` nameOccName n2
    dupErr vs = addErr $ TcRnUnknownMessage $ mkPlainError noHints $
                  (text "Duplicate binding in parallel list comprehension for:"
                    <+> quotes (ppr (NE.head vs)))

lookupQualifiedDoStmtName :: HsStmtContext GhcRn -> Name -> RnM (SyntaxExpr GhcRn, FreeVars)
-- Like lookupStmtName, but respects QualifiedDo
lookupQualifiedDoStmtName ctxt n
  = case qualifiedDoModuleName_maybe ctxt of
      Nothing -> lookupStmtName ctxt n
      Just modName ->
        first (mkSyntaxExpr . nl_HsVar) <$> lookupNameWithQualifier n modName

lookupStmtName :: HsStmtContext GhcRn -> Name -> RnM (SyntaxExpr GhcRn, FreeVars)
-- Like lookupSyntax, but respects contexts
lookupStmtName ctxt n
  | rebindableContext ctxt
  = lookupSyntax n
  | otherwise
  = return (mkRnSyntaxExpr n, emptyFVs)

lookupStmtNamePoly :: HsStmtContext GhcRn -> Name -> RnM (HsExpr GhcRn, FreeVars)
lookupStmtNamePoly ctxt name
  | rebindableContext ctxt
  = do { rebindable_on <- xoptM LangExt.RebindableSyntax
       ; if rebindable_on
         then do { fm <- lookupOccRn (nameRdrName name)
                 ; return (HsVar noExtField (noLocA fm), unitFV fm) }
         else not_rebindable }
  | otherwise
  = not_rebindable
  where
    not_rebindable = return (HsVar noExtField (noLocA name), emptyFVs)

-- | Is this a context where we respect RebindableSyntax?
-- but ListComp are never rebindable
-- Neither is ArrowExpr, which has its own desugarer in GHC.HsToCore.Arrows
rebindableContext :: HsStmtContext GhcRn -> Bool
rebindableContext ctxt = case ctxt of
  HsDoStmt flavour -> rebindableDoStmtContext flavour
  ArrowExpr -> False
  PatGuard {} -> False


  ParStmtCtxt   c -> rebindableContext c     -- Look inside to
  TransStmtCtxt c -> rebindableContext c     -- the parent context

rebindableDoStmtContext :: HsDoFlavour -> Bool
rebindableDoStmtContext flavour = case flavour of
  ListComp -> False
  DoExpr m -> isNothing m
  MDoExpr m -> isNothing m
  MonadComp -> True
  GhciStmtCtxt -> True   -- I suppose?

{-
Note [Renaming parallel Stmts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Renaming parallel statements is painful.  Given, say
     [ a+c | a <- as, bs <- bss
           | c <- bs, a <- ds ]
Note that
  (a) In order to report "Defined but not used" about 'bs', we must
      rename each group of Stmts with a thing_inside whose FreeVars
      include at least {a,c}

  (b) We want to report that 'a' is illegally bound in both branches

  (c) The 'bs' in the second group must obviously not be captured by
      the binding in the first group

To satisfy (a) we nest the segements.
To satisfy (b) we check for duplicates just before thing_inside.
To satisfy (c) we reset the LocalRdrEnv each time.

************************************************************************
*                                                                      *
\subsubsection{mdo expressions}
*                                                                      *
************************************************************************
-}

type FwdRefs = NameSet
type Segment stmts = (Defs,
                      Uses,     -- May include defs
                      FwdRefs,  -- A subset of uses that are
                                --   (a) used before they are bound in this segment, or
                                --   (b) used here, and bound in subsequent segments
                      stmts)    -- Either Stmt or [Stmt]


-- wrapper that does both the left- and right-hand sides
rnRecStmtsAndThen :: AnnoBody body =>
                     HsStmtContext GhcRn
                  -> (body GhcPs -> RnM (body GhcRn, FreeVars))
                  -> [LStmt GhcPs (LocatedA (body GhcPs))]
                         -- assumes that the FreeVars returned includes
                         -- the FreeVars of the Segments
                  -> ([Segment (LStmt GhcRn (LocatedA (body GhcRn)))]
                      -> RnM (a, FreeVars))
                  -> RnM (a, FreeVars)
rnRecStmtsAndThen ctxt rnBody s cont
  = do  { -- (A) Make the mini fixity env for all of the stmts
          fix_env <- makeMiniFixityEnv (collectRecStmtsFixities s)

          -- (B) Do the LHSes
        ; new_lhs_and_fv <- rn_rec_stmts_lhs fix_env s

          --    ...bring them and their fixities into scope
        ; let bound_names = collectLStmtsBinders CollNoDictBinders (map fst new_lhs_and_fv)
              -- Fake uses of variables introduced implicitly (warning suppression, see #4404)
              rec_uses = lStmtsImplicits (map fst new_lhs_and_fv)
              implicit_uses = mkNameSet $ concatMap snd $ rec_uses
        ; bindLocalNamesFV bound_names $
          addLocalFixities fix_env bound_names $ do

          -- (C) do the right-hand-sides and thing-inside
        { segs <- rn_rec_stmts ctxt rnBody bound_names new_lhs_and_fv
        ; (res, fvs) <- cont segs
        ; mapM_ (\(loc, ns) -> checkUnusedRecordWildcard loc fvs (Just ns))
                rec_uses
        ; warnUnusedLocalBinds bound_names (fvs `unionNameSet` implicit_uses)
        ; return (res, fvs) }}

-- get all the fixity decls in any Let stmt
collectRecStmtsFixities :: [LStmtLR GhcPs GhcPs body] -> [LFixitySig GhcPs]
collectRecStmtsFixities l =
    foldr (\ s -> \acc -> case s of
            (L _ (LetStmt _ (HsValBinds _ (ValBinds _ _ sigs)))) ->
              foldr (\ sig -> \ acc -> case sig of
                                         (L loc (FixSig _ s)) -> (L loc s) : acc
                                         _ -> acc) acc sigs
            _ -> acc) [] l

-- left-hand sides

rn_rec_stmt_lhs :: AnnoBody body => MiniFixityEnv
                -> LStmt GhcPs (LocatedA (body GhcPs))
                   -- rename LHS, and return its FVs
                   -- Warning: we will only need the FreeVars below in the case of a BindStmt,
                   -- so we don't bother to compute it accurately in the other cases
                -> RnM [(LStmtLR GhcRn GhcPs (LocatedA (body GhcPs)), FreeVars)]

rn_rec_stmt_lhs _ (L loc (BodyStmt _ body a b))
  = return [(L loc (BodyStmt noExtField body a b), emptyFVs)]

rn_rec_stmt_lhs _ (L loc (LastStmt _ body noret a))
  = return [(L loc (LastStmt noExtField body noret a), emptyFVs)]

rn_rec_stmt_lhs fix_env (L loc (BindStmt _ pat body))
  = do
      -- should the ctxt be MDo instead?
      (pat', fv_pat) <- rnBindPat (localRecNameMaker fix_env) pat
      return [(L loc (BindStmt noAnn pat' body), fv_pat)]

rn_rec_stmt_lhs _ (L _ (LetStmt _ binds@(HsIPBinds {})))
  = failWith (badIpBinds (text "an mdo expression") binds)

rn_rec_stmt_lhs fix_env (L loc (LetStmt _ (HsValBinds x binds)))
    = do (_bound_names, binds') <- rnLocalValBindsLHS fix_env binds
         return [(L loc (LetStmt noAnn (HsValBinds x binds')),
                 -- Warning: this is bogus; see function invariant
                 emptyFVs
                 )]

-- XXX Do we need to do something with the return and mfix names?
rn_rec_stmt_lhs fix_env (L _ (RecStmt { recS_stmts = L _ stmts }))  -- Flatten Rec inside Rec
    = rn_rec_stmts_lhs fix_env stmts

rn_rec_stmt_lhs _ stmt@(L _ (ParStmt {}))       -- Syntactically illegal in mdo
  = pprPanic "rn_rec_stmt" (ppr stmt)

rn_rec_stmt_lhs _ stmt@(L _ (TransStmt {}))     -- Syntactically illegal in mdo
  = pprPanic "rn_rec_stmt" (ppr stmt)

rn_rec_stmt_lhs _ stmt@(L _ (ApplicativeStmt {})) -- Shouldn't appear yet
  = pprPanic "rn_rec_stmt" (ppr stmt)

rn_rec_stmt_lhs _ (L _ (LetStmt _ (EmptyLocalBinds _)))
  = panic "rn_rec_stmt LetStmt EmptyLocalBinds"

rn_rec_stmts_lhs :: AnnoBody body => MiniFixityEnv
                 -> [LStmt GhcPs (LocatedA (body GhcPs))]
                 -> RnM [(LStmtLR GhcRn GhcPs (LocatedA (body GhcPs)), FreeVars)]
rn_rec_stmts_lhs fix_env stmts
  = do { ls <- concatMapM (rn_rec_stmt_lhs fix_env) stmts
       ; let boundNames = collectLStmtsBinders CollNoDictBinders (map fst ls)
            -- First do error checking: we need to check for dups here because we
            -- don't bind all of the variables from the Stmt at once
            -- with bindLocatedLocals.
       ; checkDupNames boundNames
       ; return ls }


-- right-hand-sides

rn_rec_stmt :: AnnoBody body =>
               HsStmtContext GhcRn
            -> (body GhcPs -> RnM (body GhcRn, FreeVars))
            -> [Name]
            -> (LStmtLR GhcRn GhcPs (LocatedA (body GhcPs)), FreeVars)
            -> RnM [Segment (LStmt GhcRn (LocatedA (body GhcRn)))]
        -- Rename a Stmt that is inside a RecStmt (or mdo)
        -- Assumes all binders are already in scope
        -- Turns each stmt into a singleton Stmt
rn_rec_stmt ctxt rnBody _ (L loc (LastStmt _ (L lb body) noret _), _)
  = do  { (body', fv_expr) <- rnBody body
        ; (ret_op, fvs1)   <- lookupQualifiedDo ctxt returnMName
        ; return [(emptyNameSet, fv_expr `plusFV` fvs1, emptyNameSet,
                   L loc (LastStmt noExtField (L lb body') noret ret_op))] }

rn_rec_stmt ctxt rnBody _ (L loc (BodyStmt _ (L lb body) _ _), _)
  = do { (body', fvs) <- rnBody body
       ; (then_op, fvs1) <- lookupQualifiedDo ctxt thenMName
       ; return [(emptyNameSet, fvs `plusFV` fvs1, emptyNameSet,
                 L loc (BodyStmt noExtField (L lb body') then_op noSyntaxExpr))] }

rn_rec_stmt ctxt rnBody _ (L loc (BindStmt _ pat' (L lb body)), fv_pat)
  = do { (body', fv_expr) <- rnBody body
       ; (bind_op, fvs1) <- lookupQualifiedDo ctxt bindMName

       ; (fail_op, fvs2) <- getMonadFailOp ctxt

       ; let bndrs = mkNameSet (collectPatBinders CollNoDictBinders pat')
             fvs   = fv_expr `plusFV` fv_pat `plusFV` fvs1 `plusFV` fvs2
       ; let xbsrn = XBindStmtRn { xbsrn_bindOp = bind_op, xbsrn_failOp = fail_op }
       ; return [(bndrs, fvs, bndrs `intersectNameSet` fvs,
                  L loc (BindStmt xbsrn pat' (L lb body')))] }

rn_rec_stmt _ _ _ (L _ (LetStmt _ binds@(HsIPBinds {})), _)
  = failWith (badIpBinds (text "an mdo expression") binds)

rn_rec_stmt _ _ all_bndrs (L loc (LetStmt _ (HsValBinds x binds')), _)
  = do { (binds', du_binds) <- rnLocalValBindsRHS (mkNameSet all_bndrs) binds'
           -- fixities and unused are handled above in rnRecStmtsAndThen
       ; let fvs = allUses du_binds
       ; return [(duDefs du_binds, fvs, emptyNameSet,
                 L loc (LetStmt noAnn (HsValBinds x binds')))] }

-- no RecStmt case because they get flattened above when doing the LHSes
rn_rec_stmt _ _ _ stmt@(L _ (RecStmt {}), _)
  = pprPanic "rn_rec_stmt: RecStmt" (ppr stmt)

rn_rec_stmt _ _ _ stmt@(L _ (ParStmt {}), _)       -- Syntactically illegal in mdo
  = pprPanic "rn_rec_stmt: ParStmt" (ppr stmt)

rn_rec_stmt _ _ _ stmt@(L _ (TransStmt {}), _)     -- Syntactically illegal in mdo
  = pprPanic "rn_rec_stmt: TransStmt" (ppr stmt)

rn_rec_stmt _ _ _ (L _ (LetStmt _ (EmptyLocalBinds _)), _)
  = panic "rn_rec_stmt: LetStmt EmptyLocalBinds"

rn_rec_stmt _ _ _ stmt@(L _ (ApplicativeStmt {}), _)
  = pprPanic "rn_rec_stmt: ApplicativeStmt" (ppr stmt)

rn_rec_stmts :: AnnoBody body =>
                HsStmtContext GhcRn
             -> (body GhcPs -> RnM (body GhcRn, FreeVars))
             -> [Name]
             -> [(LStmtLR GhcRn GhcPs (LocatedA (body GhcPs)), FreeVars)]
             -> RnM [Segment (LStmt GhcRn (LocatedA (body GhcRn)))]
rn_rec_stmts ctxt rnBody bndrs stmts
  = do { segs_s <- mapM (rn_rec_stmt ctxt rnBody bndrs) stmts
       ; return (concat segs_s) }

---------------------------------------------
segmentRecStmts :: AnnoBody body
                => SrcSpan -> HsStmtContext GhcRn
                -> Stmt GhcRn (LocatedA (body GhcRn))
                -> [Segment (LStmt GhcRn (LocatedA (body GhcRn)))]
                -> Maybe FreeVars -- Nothing when in interactive mode, everything can be used later
                                  -- Note [What is "used later" in a rec stmt]
                -> ([LStmt GhcRn (LocatedA (body GhcRn))], FreeVars)

segmentRecStmts loc ctxt empty_rec_stmt segs mfvs_later
  | null segs
  = ([], final_fv_uses)

  | HsDoStmt (MDoExpr _) <- ctxt
  = segsToStmts empty_rec_stmt grouped_segs later_ids
               -- Step 4: Turn the segments into Stmts
                --         Use RecStmt when and only when there are fwd refs
                --         Also gather up the uses from the end towards the
                --         start, so we can tell the RecStmt which things are
                --         used 'after' the RecStmt

  | otherwise
  = ([ L (noAnnSrcSpan loc) $
       empty_rec_stmt { recS_stmts = noLocA ss
                      , recS_later_ids = nameSetElemsStable later_ids
                      , recS_rec_ids   = nameSetElemsStable
                                           (defs `intersectNameSet` uses) }]
          -- See Note [Deterministic ApplicativeDo and RecursiveDo desugaring]
    , uses `plusFV` final_fv_uses)

  where
    final_fv_uses = case mfvs_later of
                  Nothing -> defs
                  Just later -> uses `plusFV` later
    later_ids = case mfvs_later of
                  Nothing -> defs
                  Just fvs_later -> defs `intersectNameSet` fvs_later

    (defs_s, uses_s, _, ss) = unzip4 segs
    defs = plusFVs defs_s
    uses = plusFVs uses_s

                -- Step 2: Fill in the fwd refs.
                --         The segments are all singletons, but their fwd-ref
                --         field mentions all the things used by the segment
                --         that are bound after their use
    segs_w_fwd_refs = addFwdRefs segs

                -- Step 3: Group together the segments to make bigger segments
                --         Invariant: in the result, no segment uses a variable
                --                    bound in a later segment
    grouped_segs = glomSegments ctxt segs_w_fwd_refs

----------------------------
addFwdRefs :: [Segment a] -> [Segment a]
-- So far the segments only have forward refs *within* the Stmt
--      (which happens for bind:  x <- ...x...)
-- This function adds the cross-seg fwd ref info

addFwdRefs segs
  = fst (foldr mk_seg ([], emptyNameSet) segs)
  where
    mk_seg (defs, uses, fwds, stmts) (segs, later_defs)
        = (new_seg : segs, all_defs)
        where
          new_seg = (defs, uses, new_fwds, stmts)
          all_defs = later_defs `unionNameSet` defs
          new_fwds = fwds `unionNameSet` (uses `intersectNameSet` later_defs)
                -- Add the downstream fwd refs here

{-
Note [Segmenting mdo]
~~~~~~~~~~~~~~~~~~~~~
NB. June 7 2012: We only glom segments that appear in an explicit mdo;
and leave those found in "do rec"'s intact.  See
https://gitlab.haskell.org/ghc/ghc/issues/4148 for the discussion
leading to this design choice.  Hence the test in segmentRecStmts.

Note [Glomming segments]
~~~~~~~~~~~~~~~~~~~~~~~~
Glomming the singleton segments of an mdo into minimal recursive groups.

At first I thought this was just strongly connected components, but
there's an important constraint: the order of the stmts must not change.

Consider
     mdo { x <- ...y...
           p <- z
           y <- ...x...
           q <- x
           z <- y
           r <- x }

Here, the first stmt mention 'y', which is bound in the third.
But that means that the innocent second stmt (p <- z) gets caught
up in the recursion.  And that in turn means that the binding for
'z' has to be included... and so on.

Start at the tail { r <- x }
Now add the next one { z <- y ; r <- x }
Now add one more     { q <- x ; z <- y ; r <- x }
Now one more... but this time we have to group a bunch into rec
     { rec { y <- ...x... ; q <- x ; z <- y } ; r <- x }
Now one more, which we can add on without a rec
     { p <- z ;
       rec { y <- ...x... ; q <- x ; z <- y } ;
       r <- x }
Finally we add the last one; since it mentions y we have to
glom it together with the first two groups
     { rec { x <- ...y...; p <- z ; y <- ...x... ;
             q <- x ; z <- y } ;
       r <- x }

Note [What is "used later" in a rec stmt]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We desugar a recursive Stmt to somethign like

  (a,_,c) <- mfix (\(a,b,_) -> do { ... ; return (a,b,c) })
  ...stuff after the rec...

The knot-tied tuple must contain
* All the variables that are used before they are bound in the `rec` block
* All the variables that are used after the entire `rec` block

In the case of GHCi, however, we don't know what variables will be used
after the `rec` (#20206).  For example, we might have
    ghci>  rec { x <- e1; y <- e2 }
    ghci>  print x
    ghci>  print y

So we have to assume that *all* the variables bound in the `rec` are used
afterwards.  We use `Nothing` in the argument to segmentRecStmts to signal
that all the variables are used.
-}

glomSegments :: HsStmtContext GhcRn
             -> [Segment (LStmt GhcRn body)]
             -> [Segment [LStmt GhcRn body]]
                                  -- Each segment has a non-empty list of Stmts
-- See Note [Glomming segments]

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
          not_needed (defs,_,_,_) = disjointNameSet defs uses

----------------------------------------------------
segsToStmts :: Stmt GhcRn (LocatedA (body GhcRn))
                                  -- A RecStmt with the SyntaxOps filled in
            -> [Segment [LStmt GhcRn (LocatedA (body GhcRn))]]
                                  -- Each Segment has a non-empty list of Stmts
            -> FreeVars           -- Free vars used 'later'
            -> ([LStmt GhcRn (LocatedA (body GhcRn))], FreeVars)

segsToStmts _ [] fvs_later = ([], fvs_later)
segsToStmts empty_rec_stmt ((defs, uses, fwds, ss) : segs) fvs_later
  = assert (not (null ss))
    (new_stmt : later_stmts, later_uses `plusFV` uses)
  where
    (later_stmts, later_uses) = segsToStmts empty_rec_stmt segs fvs_later
    new_stmt | non_rec   = head ss
             | otherwise = L (getLoc (head ss)) rec_stmt
    rec_stmt = empty_rec_stmt { recS_stmts     = noLocA ss
                              , recS_later_ids = nameSetElemsStable used_later
                              , recS_rec_ids   = nameSetElemsStable fwds }
          -- See Note [Deterministic ApplicativeDo and RecursiveDo desugaring]
    non_rec    = isSingleton ss && isEmptyNameSet fwds
    used_later = defs `intersectNameSet` later_uses
                                -- The ones needed after the RecStmt

{-
************************************************************************
*                                                                      *
ApplicativeDo
*                                                                      *
************************************************************************

Note [ApplicativeDo]

= Example =

For a sequence of statements

 do
     x <- A
     y <- B x
     z <- C
     return (f x y z)

We want to transform this to

  (\(x,y) z -> f x y z) <$> (do x <- A; y <- B x; return (x,y)) <*> C

It would be easy to notice that "y <- B x" and "z <- C" are
independent and do something like this:

 do
     x <- A
     (y,z) <- (,) <$> B x <*> C
     return (f x y z)

But this isn't enough! A and C were also independent, and this
transformation loses the ability to do A and C in parallel.

The algorithm works by first splitting the sequence of statements into
independent "segments", and a separate "tail" (the final statement). In
our example above, the segements would be

     [ x <- A
     , y <- B x ]

     [ z <- C ]

and the tail is:

     return (f x y z)

Then we take these segments and make an Applicative expression from them:

     (\(x,y) z -> return (f x y z))
       <$> do { x <- A; y <- B x; return (x,y) }
       <*> C

Finally, we recursively apply the transformation to each segment, to
discover any nested parallelism.

= Syntax & spec =

  expr ::= ... | do {stmt_1; ..; stmt_n} expr | ...

  stmt ::= pat <- expr
         | (arg_1 | ... | arg_n)  -- applicative composition, n>=1
         | ...                    -- other kinds of statement (e.g. let)

  arg ::= pat <- expr
        | {stmt_1; ..; stmt_n} {var_1..var_n}

(note that in the actual implementation,the expr in a do statement is
represented by a LastStmt as the final stmt, this is just a
representational issue and may change later.)

== Transformation to introduce applicative stmts ==

ado {} tail = tail
ado {pat <- expr} {return expr'} = (mkArg(pat <- expr)); return expr'
ado {one} tail = one : tail
ado stmts tail
  | n == 1 = ado before (ado after tail)
    where (before,after) = split(stmts_1)
  | n > 1  = (mkArg(stmts_1) | ... | mkArg(stmts_n)); tail
  where
    {stmts_1 .. stmts_n} = segments(stmts)

segments(stmts) =
  -- divide stmts into segments with no interdependencies

mkArg({pat <- expr}) = (pat <- expr)
mkArg({stmt_1; ...; stmt_n}) =
  {stmt_1; ...; stmt_n} {vars(stmt_1) u .. u vars(stmt_n)}

split({stmt_1; ..; stmt_n) =
  ({stmt_1; ..; stmt_i}, {stmt_i+1; ..; stmt_n})
  -- 1 <= i <= n
  -- i is a good place to insert a bind

== Desugaring for do ==

dsDo {} expr = expr

dsDo {pat <- rhs; stmts} expr =
   rhs >>= \pat -> dsDo stmts expr

dsDo {(arg_1 | ... | arg_n)} (return expr) =
  (\argpat (arg_1) .. argpat(arg_n) -> expr)
     <$> argexpr(arg_1)
     <*> ...
     <*> argexpr(arg_n)

dsDo {(arg_1 | ... | arg_n); stmts} expr =
  join (\argpat (arg_1) .. argpat(arg_n) -> dsDo stmts expr)
     <$> argexpr(arg_1)
     <*> ...
     <*> argexpr(arg_n)

= Relevant modules in the rest of the compiler =

ApplicativeDo touches a few phases in the compiler:

* Renamer: The journey begins here in the renamer, where do-blocks are
  scheduled as outlined above and transformed into applicative
  combinators.  However, the code is still represented as a do-block
  with special forms of applicative statements. This allows us to
  recover the original do-block when e.g. printing type errors, where
  we don't want to show any of the applicative combinators since they
  don't exist in the source code.
  See ApplicativeStmt and ApplicativeArg in HsExpr.

* Typechecker: ApplicativeDo passes through the typechecker much like any
  other form of expression. The only crux is that the typechecker has to
  be aware of the special ApplicativeDo statements in the do-notation, and
  typecheck them appropriately.
  Relevant module: GHC.Tc.Gen.Match

* Desugarer: Any do-block which contains applicative statements is desugared
  as outlined above, to use the Applicative combinators.
  Relevant module: GHC.HsToCore.Expr

-}

-- | The 'Name's of @return@ and @pure@. These may not be 'returnName' and
-- 'pureName' due to @QualifiedDo@ or @RebindableSyntax@.
data MonadNames = MonadNames { return_name, pure_name :: Name }

instance Outputable MonadNames where
  ppr (MonadNames {return_name=return_name,pure_name=pure_name}) =
    hcat
    [text "MonadNames { return_name = "
    ,ppr return_name
    ,text ", pure_name = "
    ,ppr pure_name
    ,text "}"
    ]

-- | rearrange a list of statements using ApplicativeDoStmt.  See
-- Note [ApplicativeDo].
rearrangeForApplicativeDo
  :: HsDoFlavour
  -> [(ExprLStmt GhcRn, FreeVars)]
  -> RnM ([ExprLStmt GhcRn], FreeVars)

rearrangeForApplicativeDo _ [] = return ([], emptyNameSet)
rearrangeForApplicativeDo _ [(one,_)] = return ([one], emptyNameSet)
rearrangeForApplicativeDo ctxt stmts0 = do
  optimal_ado <- goptM Opt_OptimalApplicativeDo
  let stmt_tree | optimal_ado = mkStmtTreeOptimal stmts
                | otherwise = mkStmtTreeHeuristic stmts
  traceRn "rearrangeForADo" (ppr stmt_tree)
  (return_name, _) <- lookupQualifiedDoName (HsDoStmt ctxt) returnMName
  (pure_name, _)   <- lookupQualifiedDoName (HsDoStmt ctxt) pureAName
  let monad_names = MonadNames { return_name = return_name
                               , pure_name   = pure_name }
  stmtTreeToStmts monad_names ctxt stmt_tree [last] last_fvs
  where
    (stmts,(last,last_fvs)) = findLast stmts0
    findLast [] = error "findLast"
    findLast [last] = ([],last)
    findLast (x:xs) = (x:rest,last) where (rest,last) = findLast xs

-- | A tree of statements using a mixture of applicative and bind constructs.
data StmtTree a
  = StmtTreeOne a
  | StmtTreeBind (StmtTree a) (StmtTree a)
  | StmtTreeApplicative [StmtTree a]

instance Outputable a => Outputable (StmtTree a) where
  ppr (StmtTreeOne x)          = parens (text "StmtTreeOne" <+> ppr x)
  ppr (StmtTreeBind x y)       = parens (hang (text "StmtTreeBind")
                                            2 (sep [ppr x, ppr y]))
  ppr (StmtTreeApplicative xs) = parens (hang (text "StmtTreeApplicative")
                                            2 (vcat (map ppr xs)))

flattenStmtTree :: StmtTree a -> [a]
flattenStmtTree t = go t []
 where
  go (StmtTreeOne a) as = a : as
  go (StmtTreeBind l r) as = go l (go r as)
  go (StmtTreeApplicative ts) as = foldr go as ts

type ExprStmtTree = StmtTree (ExprLStmt GhcRn, FreeVars)
type Cost = Int

-- | Turn a sequence of statements into an ExprStmtTree using a
-- heuristic algorithm.  /O(n^2)/
mkStmtTreeHeuristic :: [(ExprLStmt GhcRn, FreeVars)] -> ExprStmtTree
mkStmtTreeHeuristic [one] = StmtTreeOne one
mkStmtTreeHeuristic stmts =
  case segments stmts of
    [one] -> split one
    segs -> StmtTreeApplicative (map split segs)
 where
  split [one] = StmtTreeOne one
  split stmts =
    StmtTreeBind (mkStmtTreeHeuristic before) (mkStmtTreeHeuristic after)
    where (before, after) = splitSegment stmts

-- | Turn a sequence of statements into an ExprStmtTree optimally,
-- using dynamic programming.  /O(n^3)/
mkStmtTreeOptimal :: [(ExprLStmt GhcRn, FreeVars)] -> ExprStmtTree
mkStmtTreeOptimal stmts =
  assert (not (null stmts)) $ -- the empty case is handled by the caller;
                              -- we don't support empty StmtTrees.
  fst (arr ! (0,n))
  where
    n = length stmts - 1
    stmt_arr = listArray (0,n) stmts

    -- lazy cache of optimal trees for subsequences of the input
    arr :: Array (Int,Int) (ExprStmtTree, Cost)
    arr = array ((0,0),(n,n))
             [ ((lo,hi), tree lo hi)
             | lo <- [0..n]
             , hi <- [lo..n] ]

    -- compute the optimal tree for the sequence [lo..hi]
    tree lo hi
      | hi == lo = (StmtTreeOne (stmt_arr ! lo), 1)
      | otherwise =
         case segments [ stmt_arr ! i | i <- [lo..hi] ] of
           [] -> panic "mkStmtTree"
           [_one] -> split lo hi
           segs -> (StmtTreeApplicative trees, maximum costs)
             where
               bounds = scanl (\(_,hi) a -> (hi+1, hi + length a)) (0,lo-1) segs
               (trees,costs) = unzip (map (uncurry split) (tail bounds))

    -- find the best place to split the segment [lo..hi]
    split :: Int -> Int -> (ExprStmtTree, Cost)
    split lo hi
      | hi == lo = (StmtTreeOne (stmt_arr ! lo), 1)
      | otherwise = (StmtTreeBind before after, c1+c2)
        where
         -- As per the paper, for a sequence s1...sn, we want to find
         -- the split with the minimum cost, where the cost is the
         -- sum of the cost of the left and right subsequences.
         --
         -- As an optimisation (also in the paper) if the cost of
         -- s1..s(n-1) is different from the cost of s2..sn, we know
         -- that the optimal solution is the lower of the two.  Only
         -- in the case that these two have the same cost do we need
         -- to do the exhaustive search.
         --
         ((before,c1),(after,c2))
           | hi - lo == 1
           = ((StmtTreeOne (stmt_arr ! lo), 1),
              (StmtTreeOne (stmt_arr ! hi), 1))
           | left_cost < right_cost
           = ((left,left_cost), (StmtTreeOne (stmt_arr ! hi), 1))
           | left_cost > right_cost
           = ((StmtTreeOne (stmt_arr ! lo), 1), (right,right_cost))
           | otherwise = minimumBy (comparing cost) alternatives
           where
             (left, left_cost) = arr ! (lo,hi-1)
             (right, right_cost) = arr ! (lo+1,hi)
             cost ((_,c1),(_,c2)) = c1 + c2
             alternatives = [ (arr ! (lo,k), arr ! (k+1,hi))
                            | k <- [lo .. hi-1] ]


-- | Turn the ExprStmtTree back into a sequence of statements, using
-- ApplicativeStmt where necessary.
stmtTreeToStmts
  :: MonadNames
  -> HsDoFlavour
  -> ExprStmtTree
  -> [ExprLStmt GhcRn]             -- ^ the "tail"
  -> FreeVars                     -- ^ free variables of the tail
  -> RnM ( [ExprLStmt GhcRn]       -- ( output statements,
         , FreeVars )             -- , things we needed

-- If we have a single bind, and we can do it without a join, transform
-- to an ApplicativeStmt.  This corresponds to the rule
--   dsBlock [pat <- rhs] (return expr) = expr <$> rhs
-- In the spec, but we do it here rather than in the desugarer,
-- because we need the typechecker to typecheck the <$> form rather than
-- the bind form, which would give rise to a Monad constraint.
stmtTreeToStmts monad_names ctxt (StmtTreeOne (L _ (BindStmt xbs pat rhs), _))
                tail _tail_fvs
  | not (isStrictPattern pat), (False,tail') <- needJoin monad_names tail
  -- See Note [ApplicativeDo and strict patterns]
  = mkApplicativeStmt ctxt [ApplicativeArgOne
                            { xarg_app_arg_one = xbsrn_failOp xbs
                            , app_arg_pattern  = pat
                            , arg_expr         = rhs
                            , is_body_stmt     = False
                            }]
                      False tail'
stmtTreeToStmts monad_names ctxt (StmtTreeOne (L _ (BodyStmt _ rhs _ _),_))
                tail _tail_fvs
  | (False,tail') <- needJoin monad_names tail
  = mkApplicativeStmt ctxt
      [ApplicativeArgOne
       { xarg_app_arg_one = Nothing
       , app_arg_pattern  = nlWildPatName
       , arg_expr         = rhs
       , is_body_stmt     = True
       }] False tail'

stmtTreeToStmts _monad_names _ctxt (StmtTreeOne (s,_)) tail _tail_fvs =
  return (s : tail, emptyNameSet)

stmtTreeToStmts monad_names ctxt (StmtTreeBind before after) tail tail_fvs = do
  (stmts1, fvs1) <- stmtTreeToStmts monad_names ctxt after tail tail_fvs
  let tail1_fvs = unionNameSets (tail_fvs : map snd (flattenStmtTree after))
  (stmts2, fvs2) <- stmtTreeToStmts monad_names ctxt before stmts1 tail1_fvs
  return (stmts2, fvs1 `plusFV` fvs2)

stmtTreeToStmts monad_names ctxt (StmtTreeApplicative trees) tail tail_fvs = do
   pairs <- mapM (stmtTreeArg ctxt tail_fvs) trees
   dflags <- getDynFlags
   let (stmts', fvss) = unzip pairs
   let (need_join, tail') =
     -- See Note [ApplicativeDo and refutable patterns]
         if any (hasRefutablePattern dflags) stmts'
         then (True, tail)
         else needJoin monad_names tail

   (stmts, fvs) <- mkApplicativeStmt ctxt stmts' need_join tail'
   return (stmts, unionNameSets (fvs:fvss))
 where
   stmtTreeArg _ctxt _tail_fvs (StmtTreeOne (L _ (BindStmt xbs pat exp), _))
     = return (ApplicativeArgOne
               { xarg_app_arg_one = xbsrn_failOp xbs
               , app_arg_pattern  = pat
               , arg_expr         = exp
               , is_body_stmt     = False
               }, emptyFVs)
   stmtTreeArg _ctxt _tail_fvs (StmtTreeOne (L _ (BodyStmt _ exp _ _), _)) =
     return (ApplicativeArgOne
             { xarg_app_arg_one = Nothing
             , app_arg_pattern  = nlWildPatName
             , arg_expr         = exp
             , is_body_stmt     = True
             }, emptyFVs)
   stmtTreeArg ctxt tail_fvs tree = do
     let stmts = flattenStmtTree tree
         pvarset = mkNameSet (concatMap (collectStmtBinders CollNoDictBinders . unLoc . fst) stmts)
                     `intersectNameSet` tail_fvs
         pvars = nameSetElemsStable pvarset
           -- See Note [Deterministic ApplicativeDo and RecursiveDo desugaring]
         pat = mkBigLHsVarPatTup pvars
         tup = mkBigLHsVarTup pvars noExtField
     (stmts',fvs2) <- stmtTreeToStmts monad_names ctxt tree [] pvarset
     (mb_ret, fvs1) <-
        if | L _ ApplicativeStmt{} <- last stmts' ->
             return (unLoc tup, emptyNameSet)
           | otherwise -> do
             (ret, _) <- lookupQualifiedDoExpr (HsDoStmt ctxt) returnMName
             let expr = HsApp noComments (noLocA ret) tup
             return (expr, emptyFVs)
     return ( ApplicativeArgMany
              { xarg_app_arg_many = noExtField
              , app_stmts         = stmts'
              , final_expr        = mb_ret
              , bv_pattern        = pat
              , stmt_context      = ctxt
              }
            , fvs1 `plusFV` fvs2)


-- | Divide a sequence of statements into segments, where no segment
-- depends on any variables defined by a statement in another segment.
segments
  :: [(ExprLStmt GhcRn, FreeVars)]
  -> [[(ExprLStmt GhcRn, FreeVars)]]
segments stmts = map fst $ merge $ reverse $ map reverse $ walk (reverse stmts)
  where
    allvars = mkNameSet (concatMap (collectStmtBinders CollNoDictBinders . unLoc . fst) stmts)

    -- We would rather not have a segment that just has LetStmts in
    -- it, so combine those with an adjacent segment where possible.
    merge [] = []
    merge (seg : segs)
       = case rest of
          [] -> [(seg,all_lets)]
          ((s,s_lets):ss) | all_lets || s_lets
               -> (seg ++ s, all_lets && s_lets) : ss
          _otherwise -> (seg,all_lets) : rest
      where
        rest = merge segs
        all_lets = all (isLetStmt . fst) seg

    -- walk splits the statement sequence into segments, traversing
    -- the sequence from the back to the front, and keeping track of
    -- the set of free variables of the current segment.  Whenever
    -- this set of free variables is empty, we have a complete segment.
    walk :: [(ExprLStmt GhcRn, FreeVars)] -> [[(ExprLStmt GhcRn, FreeVars)]]
    walk [] = []
    walk ((stmt,fvs) : stmts) = ((stmt,fvs) : seg) : walk rest
      where (seg,rest) = chunter fvs' stmts
            (_, fvs') = stmtRefs stmt fvs

    chunter _ [] = ([], [])
    chunter vars ((stmt,fvs) : rest)
       | not (isEmptyNameSet vars)
       || isStrictPatternBind stmt
           -- See Note [ApplicativeDo and strict patterns]
       = ((stmt,fvs) : chunk, rest')
       where (chunk,rest') = chunter vars' rest
             (pvars, evars) = stmtRefs stmt fvs
             vars' = (vars `minusNameSet` pvars) `unionNameSet` evars
    chunter _ rest = ([], rest)

    stmtRefs stmt fvs
      | isLetStmt stmt = (pvars, fvs' `minusNameSet` pvars)
      | otherwise      = (pvars, fvs')
      where fvs' = fvs `intersectNameSet` allvars
            pvars = mkNameSet (collectStmtBinders CollNoDictBinders (unLoc stmt))

    isStrictPatternBind :: ExprLStmt GhcRn -> Bool
    isStrictPatternBind (L _ (BindStmt _ pat _)) = isStrictPattern pat
    isStrictPatternBind _ = False

{-
Note [ApplicativeDo and strict patterns]

A strict pattern match is really a dependency.  For example,

do
  (x,y) <- A
  z <- B
  return C

The pattern (_,_) must be matched strictly before we do B.  If we
allowed this to be transformed into

  (\(x,y) -> \z -> C) <$> A <*> B

then it could be lazier than the standard desuraging using >>=.  See #13875
for more examples.

Thus, whenever we have a strict pattern match, we treat it as a
dependency between that statement and the following one.  The
dependency prevents those two statements from being performed "in
parallel" in an ApplicativeStmt, but doesn't otherwise affect what we
can do with the rest of the statements in the same "do" expression.
-}

isStrictPattern :: forall p. IsPass p => LPat (GhcPass p) -> Bool
isStrictPattern (L loc pat) =
  case pat of
    WildPat{}       -> False
    VarPat{}        -> False
    LazyPat{}       -> False
    AsPat _ _ p     -> isStrictPattern p
    ParPat _ _ p _  -> isStrictPattern p
    ViewPat _ _ p   -> isStrictPattern p
    SigPat _ p _    -> isStrictPattern p
    BangPat{}       -> True
    ListPat{}       -> True
    TuplePat{}      -> True
    SumPat{}        -> True
    ConPat{}        -> True
    LitPat{}        -> True
    NPat{}          -> True
    NPlusKPat{}     -> True
    SplicePat{}     -> True
    XPat ext        -> case ghcPass @p of
#if __GLASGOW_HASKELL__ < 811
      GhcPs -> noExtCon ext
#endif
      GhcRn
        | HsPatExpanded _ p <- ext
        -> isStrictPattern (L loc p)
      GhcTc -> case ext of
        ExpansionPat _ p -> isStrictPattern (L loc p)
        CoPat {} -> panic "isStrictPattern: CoPat"

{-
Note [ApplicativeDo and refutable patterns]

Refutable patterns in do blocks are desugared to use the monadic 'fail' operation.
This means that sometimes an applicative block needs to be wrapped in 'join' simply because
of a refutable pattern, in order for the types to work out.

-}

hasRefutablePattern :: DynFlags -> ApplicativeArg GhcRn -> Bool
hasRefutablePattern dflags (ApplicativeArgOne { app_arg_pattern = pat
                                              , is_body_stmt = False}) =
                                         not (isIrrefutableHsPat dflags pat)
hasRefutablePattern _ _ = False

isLetStmt :: LStmt (GhcPass a) b -> Bool
isLetStmt (L _ LetStmt{}) = True
isLetStmt _ = False

-- | Find a "good" place to insert a bind in an indivisible segment.
-- This is the only place where we use heuristics.  The current
-- heuristic is to peel off the first group of independent statements
-- and put the bind after those.
splitSegment
  :: [(ExprLStmt GhcRn, FreeVars)]
  -> ( [(ExprLStmt GhcRn, FreeVars)]
     , [(ExprLStmt GhcRn, FreeVars)] )
splitSegment [one,two] = ([one],[two])
  -- there is no choice when there are only two statements; this just saves
  -- some work in a common case.
splitSegment stmts
  | Just (lets,binds,rest) <- slurpIndependentStmts stmts
  =  if not (null lets)
       then (lets, binds++rest)
       else (lets++binds, rest)
  | otherwise
  = case stmts of
      (x:xs) -> ([x],xs)
      _other -> (stmts,[])

slurpIndependentStmts
   :: [(LStmt GhcRn (LocatedA (body GhcRn)), FreeVars)]
   -> Maybe ( [(LStmt GhcRn (LocatedA (body GhcRn)), FreeVars)] -- LetStmts
            , [(LStmt GhcRn (LocatedA (body GhcRn)), FreeVars)] -- BindStmts
            , [(LStmt GhcRn (LocatedA (body GhcRn)), FreeVars)] )
slurpIndependentStmts stmts = go [] [] emptyNameSet stmts
 where
  -- If we encounter a BindStmt that doesn't depend on a previous BindStmt
  -- in this group, then add it to the group. We have to be careful about
  -- strict patterns though; splitSegments expects that if we return Just
  -- then we have actually done some splitting. Otherwise it will go into
  -- an infinite loop (#14163).
  go lets indep bndrs ((L loc (BindStmt xbs pat body), fvs): rest)
    | disjointNameSet bndrs fvs && not (isStrictPattern pat)
    = go lets ((L loc (BindStmt xbs pat body), fvs) : indep)
         bndrs' rest
    where bndrs' = bndrs `unionNameSet` mkNameSet (collectPatBinders CollNoDictBinders pat)
  -- If we encounter a LetStmt that doesn't depend on a BindStmt in this
  -- group, then move it to the beginning, so that it doesn't interfere with
  -- grouping more BindStmts.
  -- TODO: perhaps we shouldn't do this if there are any strict bindings,
  -- because we might be moving evaluation earlier.
  go lets indep bndrs ((L loc (LetStmt noExtField binds), fvs) : rest)
    | disjointNameSet bndrs fvs
    = go ((L loc (LetStmt noExtField binds), fvs) : lets) indep bndrs rest
  go _ []  _ _ = Nothing
  go _ [_] _ _ = Nothing
  go lets indep _ stmts = Just (reverse lets, reverse indep, stmts)

-- | Build an ApplicativeStmt, and strip the "return" from the tail
-- if necessary.
--
-- For example, if we start with
--   do x <- E1; y <- E2; return (f x y)
-- then we get
--   do (E1[x] | E2[y]); f x y
--
-- the LastStmt in this case has the return removed, but we set the
-- flag on the LastStmt to indicate this, so that we can print out the
-- original statement correctly in error messages.  It is easier to do
-- it this way rather than try to ignore the return later in both the
-- typechecker and the desugarer (I tried it that way first!).
mkApplicativeStmt
  :: HsDoFlavour
  -> [ApplicativeArg GhcRn]             -- ^ The args
  -> Bool                               -- ^ True <=> need a join
  -> [ExprLStmt GhcRn]        -- ^ The body statements
  -> RnM ([ExprLStmt GhcRn], FreeVars)
mkApplicativeStmt ctxt args need_join body_stmts
  = do { (fmap_op, fvs1) <- lookupQualifiedDoStmtName (HsDoStmt ctxt) fmapName
       ; (ap_op, fvs2) <- lookupQualifiedDoStmtName (HsDoStmt ctxt) apAName
       ; (mb_join, fvs3) <-
           if need_join then
             do { (join_op, fvs) <- lookupQualifiedDoStmtName (HsDoStmt ctxt) joinMName
                ; return (Just join_op, fvs) }
           else
             return (Nothing, emptyNameSet)
       ; let applicative_stmt = noLocA $ ApplicativeStmt noExtField
               (zip (fmap_op : repeat ap_op) args)
               mb_join
       ; return ( applicative_stmt : body_stmts
                , fvs1 `plusFV` fvs2 `plusFV` fvs3) }

-- | Given the statements following an ApplicativeStmt, determine whether
-- we need a @join@ or not, and remove the @return@ if necessary.
needJoin :: MonadNames
         -> [ExprLStmt GhcRn]
         -> (Bool, [ExprLStmt GhcRn])
needJoin _monad_names [] = (False, [])  -- we're in an ApplicativeArg
needJoin monad_names  [L loc (LastStmt _ e _ t)]
 | Just (arg, wasDollar) <- isReturnApp monad_names e =
       (False, [L loc (LastStmt noExtField arg (Just wasDollar) t)])
needJoin _monad_names stmts = (True, stmts)

-- | @(Just e, False)@, if the expression is @return e@
--   @(Just e, True)@ if the expression is @return $ e@,
--   otherwise @Nothing@.
isReturnApp :: MonadNames
            -> LHsExpr GhcRn
            -> Maybe (LHsExpr GhcRn, Bool)
isReturnApp monad_names (L _ (HsPar _ _ expr _)) = isReturnApp monad_names expr
isReturnApp monad_names (L _ e) = case e of
  OpApp _ l op r | is_return l, is_dollar op -> Just (r, True)
  HsApp _ f arg  | is_return f               -> Just (arg, False)
  _otherwise -> Nothing
 where
  is_var f (L _ (HsPar _ _ e _)) = is_var f e
  is_var f (L _ (HsAppType _ e _)) = is_var f e
  is_var f (L _ (HsVar _ (L _ r))) = f r
       -- TODO: I don't know how to get this right for rebindable syntax
  is_var _ _ = False

  is_return = is_var (\n -> n == return_name monad_names
                         || n == pure_name monad_names)
  is_dollar = is_var (`hasKey` dollarIdKey)

{-
************************************************************************
*                                                                      *
\subsubsection{Errors}
*                                                                      *
************************************************************************
-}

checkEmptyStmts :: HsStmtContext GhcRn -> RnM ()
-- We've seen an empty sequence of Stmts... is that ok?
checkEmptyStmts ctxt
  = unless (okEmpty ctxt) (addErr (emptyErr ctxt))

okEmpty :: HsStmtContext a -> Bool
okEmpty (PatGuard {}) = True
okEmpty _             = False

emptyErr :: HsStmtContext GhcRn -> TcRnMessage
emptyErr (ParStmtCtxt {})   = TcRnUnknownMessage $ mkPlainError noHints $
  text "Empty statement group in parallel comprehension"
emptyErr (TransStmtCtxt {}) = TcRnUnknownMessage $ mkPlainError noHints $
  text "Empty statement group preceding 'group' or 'then'"
emptyErr ctxt               = TcRnUnknownMessage $ mkPlainError noHints $
  text "Empty" <+> pprStmtContext ctxt

----------------------
checkLastStmt :: AnnoBody body => HsStmtContext GhcRn
              -> LStmt GhcPs (LocatedA (body GhcPs))
              -> RnM (LStmt GhcPs (LocatedA (body GhcPs)))
checkLastStmt ctxt lstmt@(L loc stmt)
  = case ctxt of
      HsDoStmt ListComp  -> check_comp
      HsDoStmt MonadComp -> check_comp
      HsDoStmt DoExpr{}  -> check_do
      HsDoStmt MDoExpr{} -> check_do
      ArrowExpr -> check_do
      _         -> check_other
  where
    check_do    -- Expect BodyStmt, and change it to LastStmt
      = case stmt of
          BodyStmt _ e _ _ -> return (L loc (mkLastStmt e))
          LastStmt {}      -> return lstmt   -- "Deriving" clauses may generate a
                                             -- LastStmt directly (unlike the parser)
          _                -> do { addErr $ TcRnUnknownMessage $ mkPlainError noHints $
                                     (hang last_error 2 (ppr stmt))
                                 ; return lstmt }
    last_error = (text "The last statement in" <+> pprAStmtContext ctxt
                  <+> text "must be an expression")

    check_comp  -- Expect LastStmt; this should be enforced by the parser!
      = case stmt of
          LastStmt {} -> return lstmt
          _           -> pprPanic "checkLastStmt" (ppr lstmt)

    check_other -- Behave just as if this wasn't the last stmt
      = do { checkStmt ctxt lstmt; return lstmt }

-- Checking when a particular Stmt is ok
checkStmt :: HsStmtContext GhcRn
          -> LStmt GhcPs (LocatedA (body GhcPs))
          -> RnM ()
checkStmt ctxt (L _ stmt)
  = do { dflags <- getDynFlags
       ; case okStmt dflags ctxt stmt of
           IsValid        -> return ()
           NotValid extra -> addErr $ TcRnUnknownMessage $ mkPlainError noHints (msg $$ extra) }
  where
   msg = sep [ text "Unexpected" <+> pprStmtCat stmt <+> text "statement"
             , text "in" <+> pprAStmtContext ctxt ]

pprStmtCat :: Stmt (GhcPass a) body -> SDoc
pprStmtCat (TransStmt {})     = text "transform"
pprStmtCat (LastStmt {})      = text "return expression"
pprStmtCat (BodyStmt {})      = text "body"
pprStmtCat (BindStmt {})      = text "binding"
pprStmtCat (LetStmt {})       = text "let"
pprStmtCat (RecStmt {})       = text "rec"
pprStmtCat (ParStmt {})       = text "parallel"
pprStmtCat (ApplicativeStmt {}) = panic "pprStmtCat: ApplicativeStmt"

------------
emptyInvalid :: Validity  -- Payload is the empty document
emptyInvalid = NotValid Outputable.empty

okStmt, okDoStmt, okCompStmt, okParStmt
   :: DynFlags -> HsStmtContext GhcRn
   -> Stmt GhcPs (LocatedA (body GhcPs)) -> Validity
-- Return Nothing if OK, (Just extra) if not ok
-- The "extra" is an SDoc that is appended to a generic error message

okStmt dflags ctxt stmt
  = case ctxt of
      PatGuard {}        -> okPatGuardStmt stmt
      ParStmtCtxt ctxt   -> okParStmt  dflags ctxt stmt
      HsDoStmt flavour   -> okDoFlavourStmt dflags flavour ctxt stmt
      ArrowExpr          -> okDoStmt   dflags ctxt stmt
      TransStmtCtxt ctxt -> okStmt dflags ctxt stmt

okDoFlavourStmt
  :: DynFlags -> HsDoFlavour -> HsStmtContext GhcRn
  -> Stmt GhcPs (LocatedA (body GhcPs)) -> Validity
okDoFlavourStmt dflags flavour ctxt stmt = case flavour of
      DoExpr{}     -> okDoStmt   dflags ctxt stmt
      MDoExpr{}    -> okDoStmt   dflags ctxt stmt
      GhciStmtCtxt -> okDoStmt   dflags ctxt stmt
      ListComp     -> okCompStmt dflags ctxt stmt
      MonadComp    -> okCompStmt dflags ctxt stmt

-------------
okPatGuardStmt :: Stmt GhcPs (LocatedA (body GhcPs)) -> Validity
okPatGuardStmt stmt
  = case stmt of
      BodyStmt {} -> IsValid
      BindStmt {} -> IsValid
      LetStmt {}  -> IsValid
      _           -> emptyInvalid

-------------
okParStmt dflags ctxt stmt
  = case stmt of
      LetStmt _ (HsIPBinds {}) -> emptyInvalid
      _                        -> okStmt dflags ctxt stmt

----------------
okDoStmt dflags ctxt stmt
  = case stmt of
       RecStmt {}
         | LangExt.RecursiveDo `xopt` dflags -> IsValid
         | ArrowExpr <- ctxt -> IsValid    -- Arrows allows 'rec'
         | otherwise         -> NotValid (text "Use RecursiveDo")
       BindStmt {} -> IsValid
       LetStmt {}  -> IsValid
       BodyStmt {} -> IsValid
       _           -> emptyInvalid

----------------
okCompStmt dflags _ stmt
  = case stmt of
       BindStmt {} -> IsValid
       LetStmt {}  -> IsValid
       BodyStmt {} -> IsValid
       ParStmt {}
         | LangExt.ParallelListComp `xopt` dflags -> IsValid
         | otherwise -> NotValid (text "Use ParallelListComp")
       TransStmt {}
         | LangExt.TransformListComp `xopt` dflags -> IsValid
         | otherwise -> NotValid (text "Use TransformListComp")
       RecStmt {}  -> emptyInvalid
       LastStmt {} -> emptyInvalid  -- Should not happen (dealt with by checkLastStmt)
       ApplicativeStmt {} -> emptyInvalid

---------
checkTupleSection :: [HsTupArg GhcPs] -> RnM ()
checkTupleSection args
  = do  { tuple_section <- xoptM LangExt.TupleSections
        ; checkErr (all tupArgPresent args || tuple_section) msg }
  where
    msg :: TcRnMessage
    msg = TcRnUnknownMessage $ mkPlainError noHints $
      text "Illegal tuple section: use TupleSections"

---------
sectionErr :: HsExpr GhcPs -> TcRnMessage
sectionErr expr
  = TcRnUnknownMessage $ mkPlainError noHints $
    hang (text "A section must be enclosed in parentheses")
       2 (text "thus:" <+> (parens (ppr expr)))

badIpBinds :: Outputable a => SDoc -> a -> TcRnMessage
badIpBinds what binds
  = TcRnUnknownMessage $ mkPlainError noHints $
    hang (text "Implicit-parameter bindings illegal in" <+> what)
         2 (ppr binds)

---------

monadFailOp :: LPat GhcPs
            -> HsStmtContext GhcRn
            -> RnM (FailOperator GhcRn, FreeVars)
monadFailOp pat ctxt = do
    dflags <- getDynFlags
        -- If the pattern is irrefutable (e.g.: wildcard, tuple, ~pat, etc.)
        -- we should not need to fail.
    if | isIrrefutableHsPat dflags pat -> return (Nothing, emptyFVs)

        -- For non-monadic contexts (e.g. guard patterns, list
        -- comprehensions, etc.) we should not need to fail, or failure is handled in
        -- a different way. See Note [Failing pattern matches in Stmts].
       | not (isMonadStmtContext ctxt) -> return (Nothing, emptyFVs)

       | otherwise -> getMonadFailOp ctxt

{-
Note [Monad fail : Rebindable syntax, overloaded strings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given the code
  foo x = do { Just y <- x; return y }

we expect it to desugar as
  foo x = x >>= \r -> case r of
                        Just y  -> return y
                        Nothing -> fail "Pattern match error"

But with RebindableSyntax and OverloadedStrings, we really want
it to desugar thus:
  foo x = x >>= \r -> case r of
                        Just y  -> return y
                        Nothing -> fail (fromString "Patterm match error")

So, in this case, we synthesize the function
  \x -> fail (fromString x)

(rather than plain 'fail') for the 'fail' operation. This is done in
'getMonadFailOp'.

Similarly with QualifiedDo and OverloadedStrings, we also want to desugar
using fromString:

  foo x = M.do { Just y <- x; return y }

  ===>

  foo x = x M.>>= \r -> case r of
                        Just y  -> return y
                        Nothing -> M.fail (fromString "Pattern match error")

-}
getMonadFailOp :: HsStmtContext p -> RnM (FailOperator GhcRn, FreeVars) -- Syntax expr fail op
getMonadFailOp ctxt
 = do { xOverloadedStrings <- fmap (xopt LangExt.OverloadedStrings) getDynFlags
      ; xRebindableSyntax <- fmap (xopt LangExt.RebindableSyntax) getDynFlags
      ; (fail, fvs) <- reallyGetMonadFailOp xRebindableSyntax xOverloadedStrings
      ; return (Just fail, fvs)
      }
  where
    isQualifiedDo = isJust (qualifiedDoModuleName_maybe ctxt)

    reallyGetMonadFailOp rebindableSyntax overloadedStrings
      | (isQualifiedDo || rebindableSyntax) && overloadedStrings = do
        (failExpr, failFvs) <- lookupQualifiedDoExpr ctxt failMName
        (fromStringExpr, fromStringFvs) <- lookupSyntaxExpr fromStringName
        let arg_lit = mkVarOcc "arg"
        arg_name <- newSysName arg_lit
        let arg_syn_expr = nlHsVar arg_name
            body :: LHsExpr GhcRn =
              nlHsApp (noLocA failExpr)
                      (nlHsApp (noLocA $ fromStringExpr) arg_syn_expr)
        let failAfterFromStringExpr :: HsExpr GhcRn =
              unLoc $ mkHsLam [noLocA $ VarPat noExtField $ noLocA arg_name] body
        let failAfterFromStringSynExpr :: SyntaxExpr GhcRn =
              mkSyntaxExpr failAfterFromStringExpr
        return (failAfterFromStringSynExpr, failFvs `plusFV` fromStringFvs)
      | otherwise = lookupQualifiedDo ctxt failMName


{- *********************************************************************
*                                                                      *
              Generating code for HsExpanded
      See Note [Handling overloaded and rebindable constructs]
*                                                                      *
********************************************************************* -}

-- | Build a 'HsExpansion' out of an extension constructor,
--   and the two components of the expansion: original and
--   desugared expressions.
mkExpandedExpr
  :: HsExpr GhcRn           -- ^ source expression
  -> HsExpr GhcRn           -- ^ expanded expression
  -> HsExpr GhcRn           -- ^ suitably wrapped 'HsExpansion'
mkExpandedExpr a b = XExpr (HsExpanded a b)

-----------------------------------------
-- Bits and pieces for RecordDotSyntax.
--
-- See Note [Overview of record dot syntax] in GHC.Hs.Expr.

-- mkGetField arg field calcuates a get_field @field arg expression.
-- e.g. z.x = mkGetField z x = get_field @x z
mkGetField :: Name -> LHsExpr GhcRn -> Located FieldLabelString -> HsExpr GhcRn
mkGetField get_field arg field = unLoc (head $ mkGet get_field [arg] field)

-- mkSetField a field b calculates a set_field @field expression.
-- e.g mkSetSetField a field b = set_field @"field" a b (read as "set field 'field' on a to b").
mkSetField :: Name -> LHsExpr GhcRn -> Located FieldLabelString -> LHsExpr GhcRn -> HsExpr GhcRn
mkSetField set_field a (L _ field) b =
  genHsApp (genHsApp (genHsVar set_field `genAppType` genHsTyLit field)  a) b

mkGet :: Name -> [LHsExpr GhcRn] -> Located FieldLabelString -> [LHsExpr GhcRn]
mkGet get_field l@(r : _) (L _ field) =
  wrapGenSpan (genHsApp (genHsVar get_field `genAppType` genHsTyLit field) r) : l
mkGet _ [] _ = panic "mkGet : The impossible has happened!"

mkSet :: Name -> LHsExpr GhcRn -> (Located FieldLabelString, LHsExpr GhcRn) -> LHsExpr GhcRn
mkSet set_field acc (field, g) = wrapGenSpan (mkSetField set_field g field acc)

-- mkProjection fields calculates a projection.
-- e.g. .x = mkProjection [x] = getField @"x"
--      .x.y = mkProjection [.x, .y] = (.y) . (.x) = getField @"y" . getField @"x"
mkProjection :: Name -> Name -> [Located FieldLabelString] -> HsExpr GhcRn
mkProjection getFieldName circName (field : fields) = foldl' f (proj field) fields
  where
    f :: HsExpr GhcRn -> Located FieldLabelString -> HsExpr GhcRn
    f acc field = genHsApps circName $ map wrapGenSpan [proj field, acc]

    proj :: Located FieldLabelString -> HsExpr GhcRn
    proj (L _ f) = genHsVar getFieldName `genAppType` genHsTyLit f
mkProjection _ _ [] = panic "mkProjection: The impossible happened"

-- mkProjUpdateSetField calculates functions representing dot notation record updates.
-- e.g. Suppose an update like foo.bar = 1.
--      We calculate the function \a -> setField @"foo" a (setField @"bar" (getField @"foo" a) 1).
mkProjUpdateSetField :: Name -> Name -> LHsRecProj GhcRn (LHsExpr GhcRn) -> (LHsExpr GhcRn -> LHsExpr GhcRn)
mkProjUpdateSetField get_field set_field (L _ (HsFieldBind { hfbLHS = (L _ (FieldLabelStrings flds')), hfbRHS = arg } ))
  = let {
      ; flds = map (fmap (unLoc . dfoLabel)) flds'
      ; final = last flds  -- quux
      ; fields = init flds   -- [foo, bar, baz]
      ; getters = \a -> foldl' (mkGet get_field) [a] fields  -- Ordered from deep to shallow.
          -- [getField@"baz"(getField@"bar"(getField@"foo" a), getField@"bar"(getField@"foo" a), getField@"foo" a, a]
      ; zips = \a -> (final, head (getters a)) : zip (reverse fields) (tail (getters a)) -- Ordered from deep to shallow.
          -- [("quux", getField@"baz"(getField@"bar"(getField@"foo" a)), ("baz", getField@"bar"(getField@"foo" a)), ("bar", getField@"foo" a), ("foo", a)]
      }
    in (\a -> foldl' (mkSet set_field) arg (zips a))
          -- setField@"foo" (a) (setField@"bar" (getField @"foo" (a))(setField@"baz" (getField @"bar" (getField @"foo" (a)))(setField@"quux" (getField @"baz" (getField @"bar" (getField @"foo" (a))))(quux))))

mkRecordDotUpd :: Name -> Name -> LHsExpr GhcRn -> [LHsRecUpdProj GhcRn] -> HsExpr GhcRn
mkRecordDotUpd get_field set_field exp updates = foldl' fieldUpdate (unLoc exp) updates
  where
    fieldUpdate :: HsExpr GhcRn -> LHsRecUpdProj GhcRn -> HsExpr GhcRn
    fieldUpdate acc lpu =  unLoc $ (mkProjUpdateSetField get_field set_field lpu) (wrapGenSpan acc)

rnHsUpdProjs :: [LHsRecUpdProj GhcPs] -> RnM ([LHsRecUpdProj GhcRn], FreeVars)
rnHsUpdProjs us = do
  (u, fvs) <- unzip <$> mapM rnRecUpdProj us
  pure (u, plusFVs fvs)
  where
    rnRecUpdProj :: LHsRecUpdProj GhcPs -> RnM (LHsRecUpdProj GhcRn, FreeVars)
    rnRecUpdProj (L l (HsFieldBind _ fs arg pun))
      = do { (arg, fv) <- rnLExpr arg
           ; return $
               (L l (HsFieldBind {
                         hfbAnn = noAnn
                       , hfbLHS = fmap rnFieldLabelStrings fs
                       , hfbRHS = arg
                       , hfbPun = pun}), fv ) }
