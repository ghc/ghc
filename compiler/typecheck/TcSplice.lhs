%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

TcSplice: Template Haskell splices


\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TcSplice(
     -- These functions are defined in stage1 and stage2
     -- The raise civilised errors in stage1
     tcSpliceExpr, tcTypedBracket, tcUntypedBracket,
     runQuasiQuoteExpr, runQuasiQuotePat,
     runQuasiQuoteDecl, runQuasiQuoteType,
     runAnnotation,

#ifdef GHCI
     -- These ones are defined only in stage2, and are
     -- called only in stage2 (ie GHCI is on)
     runMetaE, runMetaP, runMetaT, runMetaD, runQuasi,
     tcTopSpliceExpr, lookupThName_maybe,
#endif
      ) where

#include "HsVersions.h"

import HsSyn
import Annotations
import Name
import TcRnMonad
import RdrName
import TcType

#ifdef GHCI
import HscMain
        -- These imports are the reason that TcSplice
        -- is very high up the module hierarchy

import HscTypes
import Convert
import RnExpr
import RnEnv
import RnTypes
import TcExpr
import TcHsSyn
import TcSimplify
import TcUnify
import Type
import Kind
import NameSet
import TcEnv
import TcMType
import TcHsType
import TcIface
import TypeRep
import FamInst
import FamInstEnv
import InstEnv
import NameEnv
import PrelNames
import OccName
import Hooks
import Var
import Module
import LoadIface
import Class
import Inst
import TyCon
import CoAxiom
import PatSyn ( patSynId )
import ConLike
import DataCon
import TcEvidence( TcEvBinds(..) )
import Id
import IdInfo
import DsExpr
import DsMonad hiding (Splice)
import Serialized
import ErrUtils
import SrcLoc
import Util
import Data.List        ( mapAccumL )
import Unique
import Data.Maybe
import BasicTypes hiding( SuccessFlag(..) )
import Maybes( MaybeErr(..) )
import DynFlags
import Panic
import FastString
import Outputable
import Control.Monad    ( when )

import DsMeta
import qualified Language.Haskell.TH as TH
-- THSyntax gives access to internal functions and data types
import qualified Language.Haskell.TH.Syntax as TH

-- Because GHC.Desugar might not be in the base library of the bootstrapping compiler
import GHC.Desugar      ( AnnotationWrapper(..) )

import qualified Data.Map as Map
import Data.Dynamic  ( fromDynamic, toDyn )
import Data.Typeable ( typeOf )
import Data.Data (Data)
import GHC.Exts         ( unsafeCoerce# )
#endif
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Main interface + stubs for the non-GHCI case
%*                                                                      *
%************************************************************************

\begin{code}
tcTypedBracket   :: HsBracket Name -> TcRhoType -> TcM (HsExpr TcId)
tcUntypedBracket :: HsBracket Name -> [PendingRnSplice] -> TcRhoType -> TcM (HsExpr TcId)
tcSpliceExpr     :: HsSplice Name  -> TcRhoType -> TcM (HsExpr TcId)
        -- None of these functions add constraints to the LIE

runQuasiQuoteExpr :: HsQuasiQuote RdrName -> RnM (LHsExpr RdrName)
runQuasiQuotePat  :: HsQuasiQuote RdrName -> RnM (LPat RdrName)
runQuasiQuoteType :: HsQuasiQuote RdrName -> RnM (LHsType RdrName)
runQuasiQuoteDecl :: HsQuasiQuote RdrName -> RnM [LHsDecl RdrName]

runAnnotation     :: CoreAnnTarget -> LHsExpr Name -> TcM Annotation

#ifndef GHCI
tcTypedBracket   x _   = failTH x "Template Haskell bracket"
tcUntypedBracket x _ _ = failTH x "Template Haskell bracket"
tcSpliceExpr  e _      = failTH e "Template Haskell splice"

runQuasiQuoteExpr q = failTH q "quasiquote"
runQuasiQuotePat  q = failTH q "pattern quasiquote"
runQuasiQuoteType q = failTH q "type quasiquote"
runQuasiQuoteDecl q = failTH q "declaration quasiquote"
runAnnotation   _ q = failTH q "annotation"

#else
  -- The whole of the rest of the file is the else-branch (ie stage2 only)
\end{code}


Note [How top-level splices are handled]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Top-level splices (those not inside a [| .. |] quotation bracket) are handled
very straightforwardly:

  1. tcTopSpliceExpr: typecheck the body e of the splice $(e)

  2. runMetaT: desugar, compile, run it, and convert result back to
     HsSyn RdrName (of the appropriate flavour, eg HsType RdrName,
     HsExpr RdrName etc)

  3. treat the result as if that's what you saw in the first place
     e.g for HsType, rename and kind-check
         for HsExpr, rename and type-check

     (The last step is different for decls, because they can *only* be
      top-level: we return the result of step 2.)

Note [How brackets and nested splices are handled]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Nested splices (those inside a [| .. |] quotation bracket),
are treated quite differently.

Remember, there are two forms of bracket
         typed   [|| e ||]
   and untyped   [|  e  |]

The life cycle of a typed bracket:
   * Starts as HsBracket

   * When renaming:
        * Set the ThStage to (Brack s RnPendingTyped)
        * Rename the body
        * Result is still a HsBracket

   * When typechecking:
        * Set the ThStage to (Brack s (TcPending ps_var lie_var))
        * Typecheck the body, and throw away the elaborated result
        * Nested splices (which must be typed) are typechecked, and
          the results accumulated in ps_var; their constraints
          accumulate in lie_var
        * Result is a HsTcBracketOut rn_brack pending_splices
          where rn_brack is the incoming renamed bracket

The life cycle of a un-typed bracket:
   * Starts as HsBracket

   * When renaming:
        * Set the ThStage to (Brack s (RnPendingUntyped ps_var))
        * Rename the body
        * Nested splices (which must be untyped) are renamed, and the
          results accumulated in ps_var
        * Result is still (HsRnBracketOut rn_body pending_splices)

   * When typechecking a HsRnBracketOut
        * Typecheck the pending_splices individually
        * Ignore the body of the bracket; just check that the context
          expects a bracket of that type (e.g. a [p| pat |] bracket should
          be in a context needing a (Q Pat)
        * Result is a HsTcBracketOut rn_brack pending_splices
          where rn_brack is the incoming renamed bracket


In both cases, desugaring happens like this:
  * HsTcBracketOut is desugared by DsMeta.dsBracket.  It

      a) Extends the ds_meta environment with the PendingSplices
         attached to the bracket

      b) Converts the quoted (HsExpr Name) to a CoreExpr that, when
         run, will produce a suitable TH expression/type/decl.  This
         is why we leave the *renamed* expression attached to the bracket:
         the quoted expression should not be decorated with all the goop
         added by the type checker

  * Each splice carries a unique Name, called a "splice point", thus
    ${n}(e).  The name is initialised to an (Unqual "splice") when the
    splice is created; the renamer gives it a unique.

  * When DsMeta (used to desugar the body of the bracket) comes across
    a splice, it looks up the splice's Name, n, in the ds_meta envt,
    to find an (HsExpr Id) that should be substituted for the splice;
    it just desugars it to get a CoreExpr (DsMeta.repSplice).

Example:
    Source:       f = [| Just $(g 3) |]
      The [| |] part is a HsBracket

    Typechecked:  f = [| Just ${s7}(g 3) |]{s7 = g Int 3}
      The [| |] part is a HsBracketOut, containing *renamed*
        (not typechecked) expression
      The "s7" is the "splice point"; the (g Int 3) part
        is a typechecked expression

    Desugared:    f = do { s7 <- g Int 3
                         ; return (ConE "Data.Maybe.Just" s7) }


Note [Template Haskell state diagram]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here are the ThStages, s, their corresponding level numbers
(the result of (thLevel s)), and their state transitions.

      -----------     $      ------------   $
      |  Comp   | ---------> |  Splice  | -----|
      |   1     |            |    0     | <----|
      -----------            ------------
        ^     |                ^      |
      $ |     | [||]         $ |      | [||]
        |     v                |      v
   --------------          ----------------
   | Brack Comp |          | Brack Splice |
   |     2      |          |      1       |
   --------------          ----------------

* Normal top-level declarations start in state Comp
       (which has level 1).
  Annotations start in state Splice, since they are
       treated very like a splice (only without a '$')

* Code compiled in state Splice (and only such code)
  will be *run at compile time*, with the result replacing
  the splice

* The original paper used level -1 instead of 0, etc.

* The original paper did not allow a splice within a
  splice, but there is no reason not to. This is the
  $ transition in the top right.

Note [Template Haskell levels]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Imported things are impLevel (= 0)

* However things at level 0 are not *necessarily* imported.
      eg  $( \b -> ... )   here b is bound at level 0

* In GHCi, variables bound by a previous command are treated
  as impLevel, because we have bytecode for them.

* Variables are bound at the "current level"

* The current level starts off at outerLevel (= 1)

* The level is decremented by splicing $(..)
               incremented by brackets [| |]
               incremented by name-quoting 'f

When a variable is used, we compare
        bind:  binding level, and
        use:   current level at usage site

  Generally
        bind > use      Always error (bound later than used)
                        [| \x -> $(f x) |]

        bind = use      Always OK (bound same stage as used)
                        [| \x -> $(f [| x |]) |]

        bind < use      Inside brackets, it depends
                        Inside splice, OK
                        Inside neither, OK

  For (bind < use) inside brackets, there are three cases:
    - Imported things   OK      f = [| map |]
    - Top-level things  OK      g = [| f |]
    - Non-top-level     Only if there is a liftable instance
                                h = \(x:Int) -> [| x |]

  To track top-level-ness we use the ThBindEnv in TcLclEnv

  For example:
           f = ...
           g1 = $(map ...)         is OK
           g2 = $(f ...)           is not OK; because we havn't compiled f yet


%************************************************************************
%*                                                                      *
\subsection{Quoting an expression}
%*                                                                      *
%************************************************************************


\begin{code}
-- See Note [How brackets and nested splices are handled]
-- tcTypedBracket :: HsBracket Name -> TcRhoType -> TcM (HsExpr TcId)
tcTypedBracket brack@(TExpBr expr) res_ty
  = addErrCtxt (quotationCtxtDoc brack) $
    do { cur_stage <- getStage
       ; ps_ref <- newMutVar []
       ; lie_var <- getConstraintVar   -- Any constraints arising from nested splices
                                       -- should get thrown into the constraint set
                                       -- from outside the bracket

       -- Typecheck expr to make sure it is valid,
       -- Throw away the typechecked expression but return its type.
       -- We'll typecheck it again when we splice it in somewhere
       ; (_tc_expr, expr_ty) <- setStage (Brack cur_stage (TcPending ps_ref lie_var)) $
                                tcInferRhoNC expr
                                -- NC for no context; tcBracket does that

       ; meta_ty <- tcTExpTy expr_ty
       ; co <- unifyType meta_ty res_ty
       ; ps' <- readMutVar ps_ref
       ; texpco <- tcLookupId unsafeTExpCoerceName
       ; return (mkHsWrapCo co (unLoc (mkHsApp (nlHsTyApp texpco [expr_ty])
                                               (noLoc (HsTcBracketOut brack ps'))))) }
tcTypedBracket other_brack _
  = pprPanic "tcTypedBracket" (ppr other_brack)

-- tcUntypedBracket :: HsBracket Name -> [PendingRnSplice] -> TcRhoType -> TcM (HsExpr TcId)
tcUntypedBracket brack ps res_ty
  = do { traceTc "tc_bracket untyped" (ppr brack $$ ppr ps)
       ; ps' <- mapM tcPendingSplice ps
       ; meta_ty <- tcBrackTy brack
       ; co <- unifyType meta_ty res_ty
       ; traceTc "tc_bracket done untyped" (ppr meta_ty)
       ; return (mkHsWrapCo co (HsTcBracketOut brack ps'))  }

---------------
tcBrackTy :: HsBracket Name -> TcM TcType
tcBrackTy (VarBr _ _) = tcMetaTy nameTyConName  -- Result type is Var (not Q-monadic)
tcBrackTy (ExpBr _)   = tcMetaTy expQTyConName  -- Result type is ExpQ (= Q Exp)
tcBrackTy (TypBr _)   = tcMetaTy typeQTyConName -- Result type is Type (= Q Typ)
tcBrackTy (DecBrG _)  = tcMetaTy decsQTyConName -- Result type is Q [Dec]
tcBrackTy (PatBr _)   = tcMetaTy patQTyConName  -- Result type is PatQ (= Q Pat)
tcBrackTy (DecBrL _)  = panic "tcBrackTy: Unexpected DecBrL"
tcBrackTy (TExpBr _)  = panic "tcUntypedBracket: Unexpected TExpBr"

---------------
tcPendingSplice :: PendingRnSplice -> TcM PendingTcSplice
tcPendingSplice (PendingRnExpSplice (HsSplice n expr))
  = do { res_ty <- tcMetaTy expQTyConName
       ; tc_pending_splice n expr res_ty }
tcPendingSplice (PendingRnPatSplice (HsSplice n expr))
  = do { res_ty <- tcMetaTy patQTyConName
       ; tc_pending_splice n expr res_ty }
tcPendingSplice (PendingRnTypeSplice (HsSplice n expr))
  = do { res_ty <- tcMetaTy typeQTyConName
       ; tc_pending_splice n expr res_ty }
tcPendingSplice (PendingRnDeclSplice (HsSplice n expr))
  = do { res_ty <- tcMetaTy decsQTyConName
       ; tc_pending_splice n expr res_ty }

tcPendingSplice (PendingRnCrossStageSplice n)
  -- Behave like $(lift x); not very pretty
  = do { res_ty <- tcMetaTy expQTyConName
       ; tc_pending_splice n (nlHsApp (nlHsVar liftName) (nlHsVar n)) res_ty }

---------------
tc_pending_splice :: Name -> LHsExpr Name -> TcRhoType -> TcM PendingTcSplice
tc_pending_splice splice_name expr res_ty
  = do { expr' <- tcMonoExpr expr res_ty
       ; return (splice_name, expr') }

---------------
-- Takes a type tau and returns the type Q (TExp tau)
tcTExpTy :: TcType -> TcM TcType
tcTExpTy tau = do
    q <- tcLookupTyCon qTyConName
    texp <- tcLookupTyCon tExpTyConName
    return (mkTyConApp q [mkTyConApp texp [tau]])
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Splicing an expression}
%*                                                                      *
%************************************************************************

\begin{code}
tcSpliceExpr splice@(HsSplice name expr) res_ty
  = addErrCtxt (spliceCtxtDoc splice) $
    setSrcSpan (getLoc expr)    $ do
    { stage <- getStage
    ; case stage of
        Splice {}            -> tcTopSplice expr res_ty
        Comp                 -> tcTopSplice expr res_ty
        Brack pop_stage pend -> tcNestedSplice pop_stage pend name expr res_ty }

tcNestedSplice :: ThStage -> PendingStuff -> Name
                -> LHsExpr Name -> TcRhoType -> TcM (HsExpr Id)
    -- See Note [How brackets and nested splices are handled]
    -- A splice inside brackets
tcNestedSplice pop_stage (TcPending ps_var lie_var) splice_name expr res_ty
  = do { meta_exp_ty <- tcTExpTy res_ty
       ; expr' <- setStage pop_stage $
                  setConstraintVar lie_var $
                  tcMonoExpr expr meta_exp_ty
       ; untypeq <- tcLookupId unTypeQName
       ; let expr'' = mkHsApp (nlHsTyApp untypeq [res_ty]) expr'
       ; ps <- readMutVar ps_var
       ; writeMutVar ps_var ((splice_name, expr'') : ps)

       -- The returned expression is ignored; it's in the pending splices
       ; return (panic "tcSpliceExpr") }

tcNestedSplice _ _ splice_name _ _
  = pprPanic "tcNestedSplice: rename stage found" (ppr splice_name)

tcTopSplice :: LHsExpr Name -> TcRhoType -> TcM (HsExpr Id)
tcTopSplice expr res_ty
  = do { -- Typecheck the expression,
         -- making sure it has type Q (T res_ty)
         meta_exp_ty <- tcTExpTy res_ty
       ; zonked_q_expr <- tcTopSpliceExpr True $
                          tcMonoExpr expr meta_exp_ty

         -- Run the expression
       ; expr2 <- runMetaE zonked_q_expr
       ; showSplice "expression" expr (ppr expr2)

         -- Rename and typecheck the spliced-in expression,
         -- making sure it has type res_ty
         -- These steps should never fail; this is a *typed* splice
       ; addErrCtxt (spliceResultDoc expr) $ do
       { (exp3, _fvs) <- rnLExpr expr2
       ; exp4 <- tcMonoExpr exp3 res_ty
       ; return (unLoc exp4) } }
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Error messages}
%*                                                                      *
%************************************************************************

\begin{code}
quotationCtxtDoc :: HsBracket Name -> SDoc
quotationCtxtDoc br_body
  = hang (ptext (sLit "In the Template Haskell quotation"))
         2 (ppr br_body)

spliceCtxtDoc :: HsSplice Name -> SDoc
spliceCtxtDoc splice
  = hang (ptext (sLit "In the Template Haskell splice"))
         2 (pprTypedSplice splice)

spliceResultDoc :: LHsExpr Name -> SDoc
spliceResultDoc expr
  = sep [ ptext (sLit "In the result of the splice:")
        , nest 2 (char '$' <> pprParendExpr expr)
        , ptext (sLit "To see what the splice expanded to, use -ddump-splices")]

-------------------
tcTopSpliceExpr :: Bool -> TcM (LHsExpr Id) -> TcM (LHsExpr Id)
-- Note [How top-level splices are handled]
-- Type check an expression that is the body of a top-level splice
--   (the caller will compile and run it)
-- Note that set the level to Splice, regardless of the original level,
-- before typechecking the expression.  For example:
--      f x = $( ...$(g 3) ... )
-- The recursive call to tcMonoExpr will simply expand the
-- inner escape before dealing with the outer one

tcTopSpliceExpr isTypedSplice tc_action
  = checkNoErrs $  -- checkNoErrs: must not try to run the thing
                   -- if the type checker fails!
    unsetGOptM Opt_DeferTypeErrors $
                   -- Don't defer type errors.  Not only are we
                   -- going to run this code, but we do an unsafe
                   -- coerce, so we get a seg-fault if, say we
                   -- splice a type into a place where an expression
                   -- is expected (Trac #7276)
    setStage (Splice isTypedSplice) $
    do {    -- Typecheck the expression
         (expr', lie) <- captureConstraints tc_action

        -- Solve the constraints
        ; const_binds <- simplifyTop lie

          -- Zonk it and tie the knot of dictionary bindings
       ; zonkTopLExpr (mkHsDictLet (EvBinds const_binds) expr') }
\end{code}

Note [Renamer errors]
~~~~~~~~~~~~~~~~~~~~~
It's important to wrap renamer calls in checkNoErrs, because the
renamer does not fail for out of scope variables etc. Instead it
returns a bogus term/type, so that it can report more than one error.
We don't want the type checker to see these bogus unbound variables.


%************************************************************************
%*                                                                      *
        Annotations
%*                                                                      *
%************************************************************************

\begin{code}
runAnnotation target expr = do
    -- Find the classes we want instances for in order to call toAnnotationWrapper
    loc <- getSrcSpanM
    data_class <- tcLookupClass dataClassName
    to_annotation_wrapper_id <- tcLookupId toAnnotationWrapperName

    -- Check the instances we require live in another module (we want to execute it..)
    -- and check identifiers live in other modules using TH stage checks. tcSimplifyStagedExpr
    -- also resolves the LIE constraints to detect e.g. instance ambiguity
    zonked_wrapped_expr' <- tcTopSpliceExpr False $
           do { (expr', expr_ty) <- tcInferRhoNC expr
                -- We manually wrap the typechecked expression in a call to toAnnotationWrapper
                -- By instantiating the call >here< it gets registered in the
                -- LIE consulted by tcTopSpliceExpr
                -- and hence ensures the appropriate dictionary is bound by const_binds
              ; wrapper <- instCall AnnOrigin [expr_ty] [mkClassPred data_class [expr_ty]]
              ; let specialised_to_annotation_wrapper_expr
                      = L loc (HsWrap wrapper (HsVar to_annotation_wrapper_id))
              ; return (L loc (HsApp specialised_to_annotation_wrapper_expr expr')) }

    -- Run the appropriately wrapped expression to get the value of
    -- the annotation and its dictionaries. The return value is of
    -- type AnnotationWrapper by construction, so this conversion is
    -- safe
    flip runMetaAW zonked_wrapped_expr' $ \annotation_wrapper ->
        case annotation_wrapper of
            AnnotationWrapper value | let serialized = toSerialized serializeWithData value ->
                -- Got the value and dictionaries: build the serialized value and
                -- call it a day. We ensure that we seq the entire serialized value
                -- in order that any errors in the user-written code for the
                -- annotation are exposed at this point.  This is also why we are
                -- doing all this stuff inside the context of runMeta: it has the
                -- facilities to deal with user error in a meta-level expression
                seqSerialized serialized `seq` Annotation {
                    ann_target = target,
                    ann_value = serialized
                }
\end{code}


%************************************************************************
%*                                                                      *
        Quasi-quoting
%*                                                                      *
%************************************************************************

Note [Quasi-quote overview]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The GHC "quasi-quote" extension is described by Geoff Mainland's paper
"Why it's nice to be quoted: quasiquoting for Haskell" (Haskell
Workshop 2007).

Briefly, one writes
        [p| stuff |]
and the arbitrary string "stuff" gets parsed by the parser 'p', whose
type should be Language.Haskell.TH.Quote.QuasiQuoter.  'p' must be
defined in another module, because we are going to run it here.  It's
a bit like a TH splice:
        $(p "stuff")

However, you can do this in patterns as well as terms.  Because of this,
the splice is run by the *renamer* rather than the type checker.

%************************************************************************
%*                                                                      *
\subsubsection{Quasiquotation}
%*                                                                      *
%************************************************************************

See Note [Quasi-quote overview] in TcSplice.

\begin{code}
runQuasiQuote :: Outputable hs_syn
              => HsQuasiQuote RdrName   -- Contains term of type QuasiQuoter, and the String
              -> Name                   -- Of type QuasiQuoter -> String -> Q th_syn
              -> Name                   -- Name of th_syn type
              -> MetaOps th_syn hs_syn
              -> RnM hs_syn
runQuasiQuote (HsQuasiQuote quoter q_span quote) quote_selector meta_ty meta_ops
  = do  {     -- Drop the leading "$" from the quoter name, if present
              -- This is old-style syntax, now deprecated
              -- NB: when removing this backward-compat, remove
              --     the matching code in Lexer.x (around line 310)
          let occ_str = occNameString (rdrNameOcc quoter)
        ; quoter <- ASSERT( not (null occ_str) )  -- Lexer ensures this
                    if head occ_str /= '$' then return quoter
                    else do { addWarn (deprecatedDollar quoter)
                            ; return (mkRdrUnqual (mkVarOcc (tail occ_str))) }

        ; quoter' <- lookupOccRn quoter
                -- We use lookupOcc rather than lookupGlobalOcc because in the
                -- erroneous case of \x -> [x| ...|] we get a better error message
                -- (stage restriction rather than out of scope).

        ; when (isUnboundName quoter') failM
                -- If 'quoter' is not in scope, proceed no further
                -- The error message was generated by lookupOccRn, but it then
                -- succeeds with an "unbound name", which makes the subsequent
                -- attempt to run the quote fail in a confusing way

          -- Check that the quoter is not locally defined, otherwise the TH
          -- machinery will not be able to run the quasiquote.
        ; this_mod <- getModule
        ; let is_local = nameIsLocalOrFrom this_mod quoter'
        ; checkTc (not is_local) (quoteStageError quoter')

        ; traceTc "runQQ" (ppr quoter <+> ppr is_local)
        ; HsQuasiQuote quoter'' _ quote' <- getHooked runQuasiQuoteHook return >>=
             ($ HsQuasiQuote quoter' q_span quote)

          -- Build the expression
        ; let quoterExpr = L q_span $! HsVar $! quoter''
        ; let quoteExpr = L q_span $! HsLit $! HsString quote'
        ; let expr = L q_span $
                     HsApp (L q_span $
                            HsApp (L q_span (HsVar quote_selector)) quoterExpr) quoteExpr
        ; meta_exp_ty <- tcMetaTy meta_ty

        -- Typecheck the expression
        ; zonked_q_expr <- tcTopSpliceExpr False (tcMonoExpr expr meta_exp_ty)

        -- Run the expression
        ; result <- runMetaQ meta_ops zonked_q_expr
        ; showSplice (mt_desc meta_ops) quoteExpr (ppr result)

        ; return result }

runQuasiQuoteExpr qq = runQuasiQuote qq quoteExpName  expQTyConName  exprMetaOps
runQuasiQuotePat  qq = runQuasiQuote qq quotePatName  patQTyConName  patMetaOps
runQuasiQuoteType qq = runQuasiQuote qq quoteTypeName typeQTyConName typeMetaOps
runQuasiQuoteDecl qq = runQuasiQuote qq quoteDecName  decsQTyConName declMetaOps

quoteStageError :: Name -> SDoc
quoteStageError quoter
  = sep [ptext (sLit "GHC stage restriction:") <+> ppr quoter,
         nest 2 (ptext (sLit "is used in a quasiquote, and must be imported, not defined locally"))]

deprecatedDollar :: RdrName -> SDoc
deprecatedDollar quoter
  = hang (ptext (sLit "Deprecated syntax:"))
       2 (ptext (sLit "quasiquotes no longer need a dollar sign:")
          <+> ppr quoter)
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Running an expression}
%*                                                                      *
%************************************************************************


\begin{code}
runQuasi :: TH.Q a -> TcM a
runQuasi act = TH.runQ act
\end{code}

\begin{code}
data MetaOps th_syn hs_syn
  = MT { mt_desc :: String             -- Type of beast (expression, type etc)
       , mt_show :: th_syn -> String   -- How to show the th_syn thing
       , mt_cvt  :: SrcSpan -> th_syn -> Either MsgDoc hs_syn
                                       -- How to convert to hs_syn
    }

exprMetaOps :: MetaOps TH.Exp (LHsExpr RdrName)
exprMetaOps = MT { mt_desc = "expression", mt_show = TH.pprint, mt_cvt = convertToHsExpr }

patMetaOps :: MetaOps TH.Pat (LPat RdrName)
patMetaOps = MT { mt_desc = "pattern", mt_show = TH.pprint, mt_cvt = convertToPat }

typeMetaOps :: MetaOps TH.Type (LHsType RdrName)
typeMetaOps = MT { mt_desc = "type", mt_show = TH.pprint, mt_cvt = convertToHsType }

declMetaOps :: MetaOps [TH.Dec] [LHsDecl RdrName]
declMetaOps = MT { mt_desc = "declarations", mt_show = TH.pprint, mt_cvt = convertToHsDecls }

----------------
runMetaAW :: Outputable output
          => (AnnotationWrapper -> output)
          -> LHsExpr Id         -- Of type AnnotationWrapper
          -> TcM output
runMetaAW k = runMeta False (\_ -> return . Right . k)
    -- We turn off showing the code in meta-level exceptions because doing so exposes
    -- the toAnnotationWrapper function that we slap around the users code

-----------------
runMetaQ :: Outputable hs_syn
         => MetaOps th_syn hs_syn
         -> LHsExpr Id
         -> TcM hs_syn
runMetaQ (MT { mt_show = show_th, mt_cvt = cvt }) expr
  = runMeta True run_and_cvt expr
  where
    run_and_cvt expr_span hval
       = do { th_result <- TH.runQ hval
            ; traceTc "Got TH result:" (text (show_th th_result))
            ; return (cvt expr_span th_result) }

runMetaE :: LHsExpr Id          -- Of type (Q Exp)
         -> TcM (LHsExpr RdrName)
runMetaE = runMetaQ exprMetaOps

runMetaP :: LHsExpr Id          -- Of type (Q Pat)
         -> TcM (LPat RdrName)
runMetaP = runMetaQ patMetaOps

runMetaT :: LHsExpr Id          -- Of type (Q Type)
         -> TcM (LHsType RdrName)
runMetaT = runMetaQ typeMetaOps

runMetaD :: LHsExpr Id          -- Of type Q [Dec]
         -> TcM [LHsDecl RdrName]
runMetaD = runMetaQ declMetaOps

---------------
runMeta :: (Outputable hs_syn)
        => Bool                 -- Whether code should be printed in the exception message
        -> (SrcSpan -> x -> TcM (Either MsgDoc hs_syn))        -- How to run x
        -> LHsExpr Id           -- Of type x; typically x = Q TH.Exp, or something like that
        -> TcM hs_syn           -- Of type t
runMeta show_code run_and_convert expr
  = do  { traceTc "About to run" (ppr expr)
        ; recordThSpliceUse -- seems to be the best place to do this,
                            -- we catch all kinds of splices and annotations.

        -- Check that we've had no errors of any sort so far.
        -- For example, if we found an error in an earlier defn f, but
        -- recovered giving it type f :: forall a.a, it'd be very dodgy
        -- to carry ont.  Mind you, the staging restrictions mean we won't
        -- actually run f, but it still seems wrong. And, more concretely,
        -- see Trac #5358 for an example that fell over when trying to
        -- reify a function with a "?" kind in it.  (These don't occur
        -- in type-correct programs.
        ; failIfErrsM

        -- Desugar
        ; ds_expr <- initDsTc (dsLExpr expr)
        -- Compile and link it; might fail if linking fails
        ; hsc_env <- getTopEnv
        ; src_span <- getSrcSpanM
        ; traceTc "About to run (desugared)" (ppr ds_expr)
        ; either_hval <- tryM $ liftIO $
                         HscMain.hscCompileCoreExpr hsc_env src_span ds_expr
        ; case either_hval of {
            Left exn   -> fail_with_exn "compile and link" exn ;
            Right hval -> do

        {       -- Coerce it to Q t, and run it

                -- Running might fail if it throws an exception of any kind (hence tryAllM)
                -- including, say, a pattern-match exception in the code we are running
                --
                -- We also do the TH -> HS syntax conversion inside the same
                -- exception-cacthing thing so that if there are any lurking
                -- exceptions in the data structure returned by hval, we'll
                -- encounter them inside the try
                --
                -- See Note [Exceptions in TH]
          let expr_span = getLoc expr
        ; either_tval <- tryAllM $
                         setSrcSpan expr_span $ -- Set the span so that qLocation can
                                                -- see where this splice is
             do { mb_result <- run_and_convert expr_span (unsafeCoerce# hval)
                ; case mb_result of
                    Left err     -> failWithTc err
                    Right result -> do { traceTc "Got HsSyn result:" (ppr result)
                                       ; return $! result } }

        ; case either_tval of
            Right v -> return v
            Left se -> case fromException se of
                         Just IOEnvFailure -> failM -- Error already in Tc monad
                         _ -> fail_with_exn "run" se -- Exception
        }}}
  where
    -- see Note [Concealed TH exceptions]
    fail_with_exn phase exn = do
        exn_msg <- liftIO $ Panic.safeShowException exn
        let msg = vcat [text "Exception when trying to" <+> text phase <+> text "compile-time code:",
                        nest 2 (text exn_msg),
                        if show_code then text "Code:" <+> ppr expr else empty]
        failWithTc msg
\end{code}

Note [Exceptions in TH]
~~~~~~~~~~~~~~~~~~~~~~~
Supppose we have something like this
        $( f 4 )
where
        f :: Int -> Q [Dec]
        f n | n>3       = fail "Too many declarations"
            | otherwise = ...

The 'fail' is a user-generated failure, and should be displayed as a
perfectly ordinary compiler error message, not a panic or anything
like that.  Here's how it's processed:

  * 'fail' is the monad fail.  The monad instance for Q in TH.Syntax
    effectively transforms (fail s) to
        qReport True s >> fail
    where 'qReport' comes from the Quasi class and fail from its monad
    superclass.

  * The TcM monad is an instance of Quasi (see TcSplice), and it implements
    (qReport True s) by using addErr to add an error message to the bag of errors.
    The 'fail' in TcM raises an IOEnvFailure exception

  * So, when running a splice, we catch all exceptions; then for
        - an IOEnvFailure exception, we assume the error is already
                in the error-bag (above)
        - other errors, we add an error to the bag
    and then fail

Note [Concealed TH exceptions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When displaying the error message contained in an exception originated from TH
code, we need to make sure that the error message itself does not contain an
exception.  For example, when executing the following splice:

    $( error ("foo " ++ error "bar") )

the message for the outer exception is a thunk which will throw the inner
exception when evaluated.

For this reason, we display the message of a TH exception using the
'safeShowException' function, which recursively catches any exception thrown
when showing an error message.


To call runQ in the Tc monad, we need to make TcM an instance of Quasi:

\begin{code}
instance TH.Quasi (IOEnv (Env TcGblEnv TcLclEnv)) where
  qNewName s = do { u <- newUnique
                  ; let i = getKey u
                  ; return (TH.mkNameU s i) }

  qReport True msg  = addErr  (text msg)
  qReport False msg = addWarn (text msg)

  qLocation = do { m <- getModule
                 ; l <- getSrcSpanM
                 ; r <- case l of
                        UnhelpfulSpan _ -> pprPanic "qLocation: Unhelpful location"
                                                    (ppr l)
                        RealSrcSpan s -> return s
                 ; return (TH.Loc { TH.loc_filename = unpackFS (srcSpanFile r)
                                  , TH.loc_module   = moduleNameString (moduleName m)
                                  , TH.loc_package  = packageIdString (modulePackageId m)
                                  , TH.loc_start = (srcSpanStartLine r, srcSpanStartCol r)
                                  , TH.loc_end = (srcSpanEndLine   r, srcSpanEndCol   r) }) }

  qLookupName       = lookupName
  qReify            = reify
  qReifyInstances   = reifyInstances
  qReifyRoles       = reifyRoles
  qReifyAnnotations = reifyAnnotations
  qReifyModule      = reifyModule

        -- For qRecover, discard error messages if
        -- the recovery action is chosen.  Otherwise
        -- we'll only fail higher up.  c.f. tryTcLIE_
  qRecover recover main = do { (msgs, mb_res) <- tryTcErrs main
                             ; case mb_res of
                                 Just val -> do { addMessages msgs      -- There might be warnings
                                                ; return val }
                                 Nothing  -> recover                    -- Discard all msgs
                          }

  qRunIO io = liftIO io

  qAddDependentFile fp = do
    ref <- fmap tcg_dependent_files getGblEnv
    dep_files <- readTcRef ref
    writeTcRef ref (fp:dep_files)

  qAddTopDecls thds = do
      l <- getSrcSpanM
      let either_hval = convertToHsDecls l thds
      ds <- case either_hval of
              Left exn -> pprPanic "qAddTopDecls: can't convert top-level declarations" exn
              Right ds -> return ds
      mapM_ (checkTopDecl . unLoc) ds
      th_topdecls_var <- fmap tcg_th_topdecls getGblEnv
      updTcRef th_topdecls_var (\topds -> ds ++ topds)
    where
      checkTopDecl :: HsDecl RdrName -> TcM ()
      checkTopDecl (ValD binds)
        = mapM_ bindName (collectHsBindBinders binds)
      checkTopDecl (SigD _)
        = return ()
      checkTopDecl (ForD (ForeignImport (L _ name) _ _ _))
        = bindName name
      checkTopDecl _
        = addErr $ text "Only function, value, and foreign import declarations may be added with addTopDecl"

      bindName :: RdrName -> TcM ()
      bindName (Exact n)
        = do { th_topnames_var <- fmap tcg_th_topnames getGblEnv
             ; updTcRef th_topnames_var (\ns -> addOneToNameSet ns n)
             }

      bindName name =
          addErr $
          hang (ptext (sLit "The binder") <+> quotes (ppr name) <+> ptext (sLit "is not a NameU."))
             2 (text "Probable cause: you used mkName instead of newName to generate a binding.")

  qAddModFinalizer fin = do
      th_modfinalizers_var <- fmap tcg_th_modfinalizers getGblEnv
      updTcRef th_modfinalizers_var (\fins -> fin:fins)

  qGetQ = do
      th_state_var <- fmap tcg_th_state getGblEnv
      th_state <- readTcRef th_state_var
      let x = Map.lookup (typeOf x) th_state >>= fromDynamic
      return x

  qPutQ x = do
      th_state_var <- fmap tcg_th_state getGblEnv
      updTcRef th_state_var (\m -> Map.insert (typeOf x) (toDyn x) m)
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Errors and contexts}
%*                                                                      *
%************************************************************************

\begin{code}
showSplice :: String -> LHsExpr Name -> SDoc -> TcM ()
-- Note that 'before' is *renamed* but not *typechecked*
-- Reason (a) less typechecking crap
--        (b) data constructors after type checking have been
--            changed to their *wrappers*, and that makes them
--            print always fully qualified
showSplice what before after
  = do { loc <- getSrcSpanM
       ; traceSplice (vcat [ppr loc <> colon <+> text "Splicing" <+> text what,
                            nest 2 (sep [nest 2 (ppr before),
                                         text "======>",
                                         nest 2 after])]) }
\end{code}


%************************************************************************
%*                                                                      *
            Instance Testing
%*                                                                      *
%************************************************************************

\begin{code}
reifyInstances :: TH.Name -> [TH.Type] -> TcM [TH.Dec]
reifyInstances th_nm th_tys
   = addErrCtxt (ptext (sLit "In the argument of reifyInstances:")
                 <+> ppr_th th_nm <+> sep (map ppr_th th_tys)) $
     do { loc <- getSrcSpanM
        ; rdr_ty <- cvt loc (mkThAppTs (TH.ConT th_nm) th_tys)
        ; (rn_ty, _fvs) <- checkNoErrs $ rnLHsType doc rdr_ty   -- Rename  to HsType Name
                         -- checkNoErrs: see Note [Renamer errors]
        ; (ty, _kind)  <- tcLHsType rn_ty
        ; ty <- zonkTcTypeToType emptyZonkEnv ty   -- Substitute out the meta type variables
                                                   -- In particular, the type might have kind
                                                   -- variables inside it (Trac #7477)

        ; traceTc "reifyInstances" (ppr ty $$ ppr (typeKind ty))
        ; case splitTyConApp_maybe ty of   -- This expands any type synonyms
            Just (tc, tys)                 -- See Trac #7910
               | Just cls <- tyConClass_maybe tc
               -> do { inst_envs <- tcGetInstEnvs
                     ; let (matches, unifies, _) = lookupInstEnv inst_envs cls tys
                     ; traceTc "reifyInstances1" (ppr matches)
                     ; mapM reifyClassInstance (map fst matches ++ unifies) }
               | isOpenFamilyTyCon tc
               -> do { inst_envs <- tcGetFamInstEnvs
                     ; let matches = lookupFamInstEnv inst_envs tc tys
                     ; traceTc "reifyInstances2" (ppr matches)
                     ; mapM (reifyFamilyInstance . fim_instance) matches }
            _  -> bale_out (hang (ptext (sLit "reifyInstances:") <+> quotes (ppr ty))
                               2 (ptext (sLit "is not a class constraint or type family application"))) }
  where
    doc = ClassInstanceCtx
    bale_out msg = failWithTc msg

    cvt :: SrcSpan -> TH.Type -> TcM (LHsType RdrName)
    cvt loc th_ty = case convertToHsType loc th_ty of
                      Left msg -> failWithTc msg
                      Right ty -> return ty
\end{code}


%************************************************************************
%*                                                                      *
                        Reification
%*                                                                      *
%************************************************************************


\begin{code}
lookupName :: Bool      -- True  <=> type namespace
                        -- False <=> value namespace
           -> String -> TcM (Maybe TH.Name)
lookupName is_type_name s
  = do { lcl_env <- getLocalRdrEnv
       ; case lookupLocalRdrEnv lcl_env rdr_name of
           Just n  -> return (Just (reifyName n))
           Nothing -> do { mb_nm <- lookupGlobalOccRn_maybe rdr_name
                         ; return (fmap reifyName mb_nm) } }
  where
    th_name = TH.mkName s       -- Parses M.x into a base of 'x' and a module of 'M'

    occ_fs :: FastString
    occ_fs = mkFastString (TH.nameBase th_name)

    occ :: OccName
    occ | is_type_name
        = if isLexCon occ_fs then mkTcOccFS    occ_fs
                             else mkTyVarOccFS occ_fs
        | otherwise
        = if isLexCon occ_fs then mkDataOccFS occ_fs
                             else mkVarOccFS  occ_fs

    rdr_name = case TH.nameModule th_name of
                 Nothing  -> mkRdrUnqual occ
                 Just mod -> mkRdrQual (mkModuleName mod) occ

getThing :: TH.Name -> TcM TcTyThing
getThing th_name
  = do  { name <- lookupThName th_name
        ; traceIf (text "reify" <+> text (show th_name) <+> brackets (ppr_ns th_name) <+> ppr name)
        ; tcLookupTh name }
        -- ToDo: this tcLookup could fail, which would give a
        --       rather unhelpful error message
  where
    ppr_ns (TH.Name _ (TH.NameG TH.DataName _pkg _mod)) = text "data"
    ppr_ns (TH.Name _ (TH.NameG TH.TcClsName _pkg _mod)) = text "tc"
    ppr_ns (TH.Name _ (TH.NameG TH.VarName _pkg _mod)) = text "var"
    ppr_ns _ = panic "reify/ppr_ns"

reify :: TH.Name -> TcM TH.Info
reify th_name
  = do  { thing <- getThing th_name
        ; reifyThing thing }

lookupThName :: TH.Name -> TcM Name
lookupThName th_name = do
    mb_name <- lookupThName_maybe th_name
    case mb_name of
        Nothing   -> failWithTc (notInScope th_name)
        Just name -> return name

lookupThName_maybe :: TH.Name -> TcM (Maybe Name)
lookupThName_maybe th_name
  =  do { names <- mapMaybeM lookup (thRdrNameGuesses th_name)
          -- Pick the first that works
          -- E.g. reify (mkName "A") will pick the class A in preference to the data constructor A
        ; return (listToMaybe names) }
  where
    lookup rdr_name
        = do {  -- Repeat much of lookupOccRn, becase we want
                -- to report errors in a TH-relevant way
             ; rdr_env <- getLocalRdrEnv
             ; case lookupLocalRdrEnv rdr_env rdr_name of
                 Just name -> return (Just name)
                 Nothing   -> lookupGlobalOccRn_maybe rdr_name }

tcLookupTh :: Name -> TcM TcTyThing
-- This is a specialised version of TcEnv.tcLookup; specialised mainly in that
-- it gives a reify-related error message on failure, whereas in the normal
-- tcLookup, failure is a bug.
tcLookupTh name
  = do  { (gbl_env, lcl_env) <- getEnvs
        ; case lookupNameEnv (tcl_env lcl_env) name of {
                Just thing -> return thing;
                Nothing    ->

          case lookupNameEnv (tcg_type_env gbl_env) name of {
                Just thing -> return (AGlobal thing);
                Nothing    ->

          if nameIsLocalOrFrom (tcg_mod gbl_env) name
          then  -- It's defined in this module
                failWithTc (notInEnv name)

          else
     do { mb_thing <- tcLookupImported_maybe name
        ; case mb_thing of
            Succeeded thing -> return (AGlobal thing)
            Failed msg      -> failWithTc msg
    }}}}

notInScope :: TH.Name -> SDoc
notInScope th_name = quotes (text (TH.pprint th_name)) <+>
                     ptext (sLit "is not in scope at a reify")
        -- Ugh! Rather an indirect way to display the name

notInEnv :: Name -> SDoc
notInEnv name = quotes (ppr name) <+>
                     ptext (sLit "is not in the type environment at a reify")

------------------------------
reifyRoles :: TH.Name -> TcM [TH.Role]
reifyRoles th_name
  = do { thing <- getThing th_name
       ; case thing of
           AGlobal (ATyCon tc) -> return (map reify_role (tyConRoles tc))
           _ -> failWithTc (ptext (sLit "No roles associated with") <+> (ppr thing))
       }
  where
    reify_role Nominal          = TH.NominalR
    reify_role Representational = TH.RepresentationalR
    reify_role Phantom          = TH.PhantomR

------------------------------
reifyThing :: TcTyThing -> TcM TH.Info
-- The only reason this is monadic is for error reporting,
-- which in turn is mainly for the case when TH can't express
-- some random GHC extension

reifyThing (AGlobal (AnId id))
  = do  { ty <- reifyType (idType id)
        ; fix <- reifyFixity (idName id)
        ; let v = reifyName id
        ; case idDetails id of
            ClassOpId cls -> return (TH.ClassOpI v ty (reifyName cls) fix)
            _             -> return (TH.VarI     v ty Nothing fix)
    }

reifyThing (AGlobal (ATyCon tc))   = reifyTyCon tc
reifyThing (AGlobal (AConLike (RealDataCon dc)))
  = do  { let name = dataConName dc
        ; ty <- reifyType (idType (dataConWrapId dc))
        ; fix <- reifyFixity name
        ; return (TH.DataConI (reifyName name) ty
                              (reifyName (dataConOrigTyCon dc)) fix)
        }
reifyThing (AGlobal (AConLike (PatSynCon ps)))
  = noTH (sLit "pattern synonyms") (ppr $ patSynId ps)

reifyThing (ATcId {tct_id = id})
  = do  { ty1 <- zonkTcType (idType id) -- Make use of all the info we have, even
                                        -- though it may be incomplete
        ; ty2 <- reifyType ty1
        ; fix <- reifyFixity (idName id)
        ; return (TH.VarI (reifyName id) ty2 Nothing fix) }

reifyThing (ATyVar tv tv1)
  = do { ty1 <- zonkTcTyVar tv1
       ; ty2 <- reifyType ty1
       ; return (TH.TyVarI (reifyName tv) ty2) }

reifyThing thing = pprPanic "reifyThing" (pprTcTyThingCategory thing)

-------------------------------------------
reifyAxBranch :: CoAxBranch -> TcM TH.TySynEqn
reifyAxBranch (CoAxBranch { cab_lhs = args, cab_rhs = rhs })
            -- remove kind patterns (#8884)
  = do { args' <- mapM reifyType (filter (not . isKind) args)
       ; rhs'  <- reifyType rhs
       ; return (TH.TySynEqn args' rhs') }

reifyTyCon :: TyCon -> TcM TH.Info
reifyTyCon tc
  | Just cls <- tyConClass_maybe tc
  = reifyClass cls

  | isFunTyCon tc
  = return (TH.PrimTyConI (reifyName tc) 2                False)

  | isPrimTyCon tc
  = return (TH.PrimTyConI (reifyName tc) (tyConArity tc) (isUnLiftedTyCon tc))

  | isFamilyTyCon tc
  = do { let tvs      = tyConTyVars tc
             kind     = tyConKind tc

             -- we need the *result kind* (see #8884)
             (kvs, mono_kind) = splitForAllTys kind
                                -- tyConArity includes *kind* params
             (_, res_kind)    = splitKindFunTysN (tyConArity tc - length kvs)
                                                 mono_kind
       ; kind' <- fmap Just (reifyKind res_kind)

       ; tvs' <- reifyTyVars tvs
       ; flav' <- reifyFamFlavour tc
       ; case flav' of
         { Left flav ->  -- open type/data family
             do { fam_envs <- tcGetFamInstEnvs
                ; instances <- mapM reifyFamilyInstance (familyInstances fam_envs tc)
                ; return (TH.FamilyI
                            (TH.FamilyD flav (reifyName tc) tvs' kind')
                            instances) }
         ; Right eqns -> -- closed type family
             return (TH.FamilyI
                      (TH.ClosedTypeFamilyD (reifyName tc) tvs' kind' eqns)
                      []) } }

  | Just (tvs, rhs) <- synTyConDefn_maybe tc  -- Vanilla type synonym
  = do { rhs' <- reifyType rhs
       ; tvs' <- reifyTyVars tvs
       ; return (TH.TyConI
                   (TH.TySynD (reifyName tc) tvs' rhs'))
       }

  | otherwise
  = do  { cxt <- reifyCxt (tyConStupidTheta tc)
        ; let tvs = tyConTyVars tc
        ; cons <- mapM (reifyDataCon (mkTyVarTys tvs)) (tyConDataCons tc)
        ; r_tvs <- reifyTyVars tvs
        ; let name = reifyName tc
              deriv = []        -- Don't know about deriving
              decl | isNewTyCon tc = TH.NewtypeD cxt name r_tvs (head cons) deriv
                   | otherwise     = TH.DataD    cxt name r_tvs cons        deriv
        ; return (TH.TyConI decl) }

reifyDataCon :: [Type] -> DataCon -> TcM TH.Con
-- For GADTs etc, see Note [Reifying data constructors]
reifyDataCon tys dc
  = do { let (tvs, theta, arg_tys, _) = dataConSig dc
             subst             = mkTopTvSubst (tvs `zip` tys)   -- Dicard ex_tvs
             (subst', ex_tvs') = mapAccumL substTyVarBndr subst (dropList tys tvs)
             theta'   = substTheta subst' theta
             arg_tys' = substTys subst' arg_tys
             stricts  = map reifyStrict (dataConStrictMarks dc)
             fields   = dataConFieldLabels dc
             name     = reifyName dc

       ; r_arg_tys <- reifyTypes arg_tys'

       ; let main_con | not (null fields)
                      = TH.RecC name (zip3 (map reifyName fields) stricts r_arg_tys)
                      | dataConIsInfix dc
                      = ASSERT( length arg_tys == 2 )
                        TH.InfixC (s1,r_a1) name (s2,r_a2)
                      | otherwise
                      = TH.NormalC name (stricts `zip` r_arg_tys)
             [r_a1, r_a2] = r_arg_tys
             [s1,   s2]   = stricts

       ; ASSERT( length arg_tys == length stricts )
         if null ex_tvs' && null theta then
             return main_con
         else do
         { cxt <- reifyCxt theta'
         ; ex_tvs'' <- reifyTyVars ex_tvs'
         ; return (TH.ForallC ex_tvs'' cxt main_con) } }

------------------------------
reifyClass :: Class -> TcM TH.Info
reifyClass cls
  = do  { cxt <- reifyCxt theta
        ; inst_envs <- tcGetInstEnvs
        ; insts <- mapM reifyClassInstance (InstEnv.classInstances inst_envs cls)
        ; ops <- mapM reify_op op_stuff
        ; tvs' <- reifyTyVars tvs
        ; let dec = TH.ClassD cxt (reifyName cls) tvs' fds' ops
        ; return (TH.ClassI dec insts ) }
  where
    (tvs, fds, theta, _, _, op_stuff) = classExtraBigSig cls
    fds' = map reifyFunDep fds
    reify_op (op, _) = do { ty <- reifyType (idType op)
                          ; return (TH.SigD (reifyName op) ty) }

------------------------------
reifyClassInstance :: ClsInst -> TcM TH.Dec
reifyClassInstance i
  = do { cxt <- reifyCxt (drop n_silent theta)
       ; thtypes <- reifyTypes types
       ; let head_ty = mkThAppTs (TH.ConT (reifyName cls)) thtypes
       ; return $ (TH.InstanceD cxt head_ty []) }
  where
     (_tvs, theta, cls, types) = tcSplitDFunTy (idType dfun)
     dfun     = instanceDFunId i
     n_silent = dfunNSilent dfun

------------------------------
reifyFamilyInstance :: FamInst -> TcM TH.Dec
reifyFamilyInstance (FamInst { fi_flavor = flavor
                             , fi_fam = fam
                             , fi_tys = lhs
                             , fi_rhs = rhs })
  = case flavor of
      SynFamilyInst ->
               -- remove kind patterns (#8884)
        do { th_lhs <- reifyTypes (filter (not . isKind) lhs)
           ; th_rhs <- reifyType  rhs
           ; return (TH.TySynInstD (reifyName fam) (TH.TySynEqn th_lhs th_rhs)) }

      DataFamilyInst rep_tc ->
        do { let tvs = tyConTyVars rep_tc
                 fam' = reifyName fam
           ; cons <- mapM (reifyDataCon (mkTyVarTys tvs)) (tyConDataCons rep_tc)
           ; th_tys <- reifyTypes lhs
           ; return (if isNewTyCon rep_tc
                     then TH.NewtypeInstD [] fam' th_tys (head cons) []
                     else TH.DataInstD    [] fam' th_tys cons        []) }

------------------------------
reifyType :: TypeRep.Type -> TcM TH.Type
-- Monadic only because of failure
reifyType ty@(ForAllTy _ _)        = reify_for_all ty
reifyType (LitTy t)         = do { r <- reifyTyLit t; return (TH.LitT r) }
reifyType (TyVarTy tv)      = return (TH.VarT (reifyName tv))
reifyType (TyConApp tc tys) = reify_tc_app tc tys   -- Do not expand type synonyms here
reifyType (AppTy t1 t2)     = do { [r1,r2] <- reifyTypes [t1,t2] ; return (r1 `TH.AppT` r2) }
reifyType ty@(FunTy t1 t2)
  | isPredTy t1 = reify_for_all ty  -- Types like ((?x::Int) => Char -> Char)
  | otherwise   = do { [r1,r2] <- reifyTypes [t1,t2] ; return (TH.ArrowT `TH.AppT` r1 `TH.AppT` r2) }

reify_for_all :: TypeRep.Type -> TcM TH.Type
reify_for_all ty
  = do { cxt' <- reifyCxt cxt;
       ; tau' <- reifyType tau
       ; tvs' <- reifyTyVars tvs
       ; return (TH.ForallT tvs' cxt' tau') }
  where
    (tvs, cxt, tau) = tcSplitSigmaTy ty

reifyTyLit :: TypeRep.TyLit -> TcM TH.TyLit
reifyTyLit (NumTyLit n) = return (TH.NumTyLit n)
reifyTyLit (StrTyLit s) = return (TH.StrTyLit (unpackFS s))

reifyTypes :: [Type] -> TcM [TH.Type]
reifyTypes = mapM reifyType

reifyKind :: Kind -> TcM TH.Kind
reifyKind  ki
  = do { let (kis, ki') = splitKindFunTys ki
       ; ki'_rep <- reifyNonArrowKind ki'
       ; kis_rep <- mapM reifyKind kis
       ; return (foldr (TH.AppT . TH.AppT TH.ArrowT) ki'_rep kis_rep) }
  where
    reifyNonArrowKind k | isLiftedTypeKind k = return TH.StarT
                        | isConstraintKind k = return TH.ConstraintT
    reifyNonArrowKind (TyVarTy v)            = return (TH.VarT (reifyName v))
    reifyNonArrowKind (ForAllTy _ k)         = reifyKind k
    reifyNonArrowKind (TyConApp kc kis)      = reify_kc_app kc kis
    reifyNonArrowKind (AppTy k1 k2)          = do { k1' <- reifyKind k1
                                                  ; k2' <- reifyKind k2
                                                  ; return (TH.AppT k1' k2')
                                                  }
    reifyNonArrowKind k                      = noTH (sLit "this kind") (ppr k)

reify_kc_app :: TyCon -> [TypeRep.Kind] -> TcM TH.Kind
reify_kc_app kc kis
  = fmap (mkThAppTs r_kc) (mapM reifyKind kis)
  where
    r_kc | Just tc <- isPromotedTyCon_maybe kc
         , isTupleTyCon tc          = TH.TupleT (tyConArity kc)
         | kc `hasKey` listTyConKey = TH.ListT
         | otherwise                = TH.ConT (reifyName kc)

reifyCxt :: [PredType] -> TcM [TH.Pred]
reifyCxt   = mapM reifyPred

reifyFunDep :: ([TyVar], [TyVar]) -> TH.FunDep
reifyFunDep (xs, ys) = TH.FunDep (map reifyName xs) (map reifyName ys)

reifyFamFlavour :: TyCon -> TcM (Either TH.FamFlavour [TH.TySynEqn])
reifyFamFlavour tc
  | isOpenSynFamilyTyCon tc = return $ Left TH.TypeFam
  | isDataFamilyTyCon    tc = return $ Left TH.DataFam

    -- this doesn't really handle abstract closed families, but let's not worry
    -- about that now
  | Just ax <- isClosedSynFamilyTyCon_maybe tc
  = do { eqns <- brListMapM reifyAxBranch $ coAxiomBranches ax
       ; return $ Right eqns }

  | otherwise
  = panic "TcSplice.reifyFamFlavour: not a type family"

reifyTyVars :: [TyVar]
            -> TcM [TH.TyVarBndr]
reifyTyVars tvs = mapM reify_tv $ filter isTypeVar tvs
  where
    reify_tv tv | isLiftedTypeKind kind = return (TH.PlainTV  name)
                | otherwise             = do kind' <- reifyKind kind
                                             return (TH.KindedTV name kind')
      where
        kind = tyVarKind tv
        name = reifyName tv

reify_tc_app :: TyCon -> [TypeRep.Type] -> TcM TH.Type
reify_tc_app tc tys
  = do { tys' <- reifyTypes (removeKinds (tyConKind tc) tys)
       ; return (mkThAppTs r_tc tys') }
  where
    arity = tyConArity tc
    r_tc | isTupleTyCon tc            = if isPromotedDataCon tc
                                        then TH.PromotedTupleT arity
                                        else TH.TupleT arity
         | tc `hasKey` listTyConKey   = TH.ListT
         | tc `hasKey` nilDataConKey  = TH.PromotedNilT
         | tc `hasKey` consDataConKey = TH.PromotedConsT
         | tc `hasKey` eqTyConKey     = TH.EqualityT
         | otherwise                  = TH.ConT (reifyName tc)
    removeKinds :: Kind -> [TypeRep.Type] -> [TypeRep.Type]
    removeKinds (FunTy k1 k2) (h:t)
      | isSuperKind k1          = removeKinds k2 t
      | otherwise               = h : removeKinds k2 t
    removeKinds (ForAllTy v k) (h:t)
      | isSuperKind (varType v) = removeKinds k t
      | otherwise               = h : removeKinds k t
    removeKinds _ tys           = tys

reifyPred :: TypeRep.PredType -> TcM TH.Pred
reifyPred ty
  -- We could reify the implicit paramter as a class but it seems
  -- nicer to support them properly...
  | isIPPred ty = noTH (sLit "implicit parameters") (ppr ty)
  | otherwise   = reifyType ty

------------------------------
reifyName :: NamedThing n => n -> TH.Name
reifyName thing
  | isExternalName name = mk_varg pkg_str mod_str occ_str
  | otherwise           = TH.mkNameU occ_str (getKey (getUnique name))
        -- Many of the things we reify have local bindings, and
        -- NameL's aren't supposed to appear in binding positions, so
        -- we use NameU.  When/if we start to reify nested things, that
        -- have free variables, we may need to generate NameL's for them.
  where
    name    = getName thing
    mod     = ASSERT( isExternalName name ) nameModule name
    pkg_str = packageIdString (modulePackageId mod)
    mod_str = moduleNameString (moduleName mod)
    occ_str = occNameString occ
    occ     = nameOccName name
    mk_varg | OccName.isDataOcc occ = TH.mkNameG_d
            | OccName.isVarOcc  occ = TH.mkNameG_v
            | OccName.isTcOcc   occ = TH.mkNameG_tc
            | otherwise             = pprPanic "reifyName" (ppr name)

------------------------------
reifyFixity :: Name -> TcM TH.Fixity
reifyFixity name
  = do  { fix <- lookupFixityRn name
        ; return (conv_fix fix) }
    where
      conv_fix (BasicTypes.Fixity i d) = TH.Fixity i (conv_dir d)
      conv_dir BasicTypes.InfixR = TH.InfixR
      conv_dir BasicTypes.InfixL = TH.InfixL
      conv_dir BasicTypes.InfixN = TH.InfixN

reifyStrict :: DataCon.HsBang -> TH.Strict
reifyStrict HsNoBang                      = TH.NotStrict
reifyStrict (HsUserBang _ False)          = TH.NotStrict
reifyStrict (HsUserBang (Just True) True) = TH.Unpacked
reifyStrict (HsUserBang _     True)       = TH.IsStrict
reifyStrict HsStrict                      = TH.IsStrict
reifyStrict (HsUnpack {})                 = TH.Unpacked

------------------------------
lookupThAnnLookup :: TH.AnnLookup -> TcM CoreAnnTarget
lookupThAnnLookup (TH.AnnLookupName th_nm) = fmap NamedTarget (lookupThName th_nm)
lookupThAnnLookup (TH.AnnLookupModule (TH.Module pn mn))
  = return $ ModuleTarget $
    mkModule (stringToPackageId $ TH.pkgString pn) (mkModuleName $ TH.modString mn)

reifyAnnotations :: Data a => TH.AnnLookup -> TcM [a]
reifyAnnotations th_nm
  = do { name <- lookupThAnnLookup th_nm
       ; eps <- getEps
       ; tcg <- getGblEnv
       ; let epsAnns = findAnns deserializeWithData (eps_ann_env eps) name
       ; let envAnns = findAnns deserializeWithData (tcg_ann_env tcg) name
       ; return (envAnns ++ epsAnns) }

------------------------------
modToTHMod :: Module -> TH.Module
modToTHMod m = TH.Module (TH.PkgName $ packageIdString  $ modulePackageId m)
                         (TH.ModName $ moduleNameString $ moduleName m)

reifyModule :: TH.Module -> TcM TH.ModuleInfo
reifyModule (TH.Module (TH.PkgName pkgString) (TH.ModName mString)) = do
  this_mod <- getModule
  let reifMod = mkModule (stringToPackageId pkgString) (mkModuleName mString)
  if (reifMod == this_mod) then reifyThisModule else reifyFromIface reifMod
    where
      reifyThisModule = do
        usages <- fmap (map modToTHMod . moduleEnvKeys . imp_mods) getImports
        return $ TH.ModuleInfo usages

      reifyFromIface reifMod = do
        iface <- loadInterfaceForModule (ptext (sLit "reifying module from TH for") <+> ppr reifMod) reifMod
        let usages = [modToTHMod m | usage <- mi_usages iface,
                                     Just m <- [usageToModule (modulePackageId reifMod) usage] ]
        return $ TH.ModuleInfo usages

      usageToModule :: PackageId -> Usage -> Maybe Module
      usageToModule _ (UsageFile {}) = Nothing
      usageToModule this_pkg (UsageHomeModule { usg_mod_name = mn }) = Just $ mkModule this_pkg mn
      usageToModule _ (UsagePackageModule { usg_mod = m }) = Just m

------------------------------
mkThAppTs :: TH.Type -> [TH.Type] -> TH.Type
mkThAppTs fun_ty arg_tys = foldl TH.AppT fun_ty arg_tys

noTH :: LitString -> SDoc -> TcM a
noTH s d = failWithTc (hsep [ptext (sLit "Can't represent") <+> ptext s <+>
                                ptext (sLit "in Template Haskell:"),
                             nest 2 d])

ppr_th :: TH.Ppr a => a -> SDoc
ppr_th x = text (TH.pprint x)
\end{code}

Note [Reifying data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Template Haskell syntax is rich enough to express even GADTs,
provided we do so in the equality-predicate form.  So a GADT
like

  data T a where
     MkT1 :: a -> T [a]
     MkT2 :: T Int

will appear in TH syntax like this

  data T a = forall b. (a ~ [b]) => MkT1 b
           | (a ~ Int) => MkT2

\begin{code}
#endif  /* GHCI */
\end{code}
