%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

TcSplice: Template Haskell splices


\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TcSplice( tcSpliceType, tcSpliceExpr, tcSpliceDecls, tcBracket,
                 lookupThName_maybe,
                 runQuasiQuoteExpr, runQuasiQuotePat,
                 runQuasiQuoteDecl, runQuasiQuoteType,
                 runAnnotation ) where

#include "HsVersions.h"

import HscMain
import TcRnDriver
        -- These imports are the reason that TcSplice
        -- is very high up the module hierarchy

import HsSyn
import Convert
import RnExpr
import RnEnv
import RdrName
import RnTypes
import TcPat
import TcExpr
import TcHsSyn
import TcSimplify
import TcUnify
import Type
import Kind
import TcType
import TcEnv
import TcMType
import TcHsType
import TcIface
import TypeRep
import FamInst
import FamInstEnv
import InstEnv
import Name
import NameEnv
import NameSet
import PrelNames
import HscTypes
import OccName
import Hooks
import Var
import Module
import Annotations
import TcRnMonad
import Class
import Inst
import TyCon
import CoAxiom
import DataCon
import TcEvidence( TcEvBinds(..) )
import Id
import IdInfo
import DsMeta
import DsExpr
import DsMonad hiding (Splice)
import Serialized
import ErrUtils
import SrcLoc
import Outputable
import Util
import Data.List        ( mapAccumL )
import Unique
import Data.Maybe
import BasicTypes hiding( SuccessFlag(..) )
import Maybes( MaybeErr(..) )
import DynFlags
import Panic
import FastString
import Control.Monad    ( when )

import qualified Language.Haskell.TH as TH
-- THSyntax gives access to internal functions and data types
import qualified Language.Haskell.TH.Syntax as TH

#ifdef GHCI
-- Because GHC.Desugar might not be in the base library of the bootstrapping compiler
import GHC.Desugar      ( AnnotationWrapper(..) )
#endif

import GHC.Exts         ( unsafeCoerce# )
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
Nested splices (those inside a [| .. |] quotation bracket), are treated
quite differently.

  * After typechecking, the bracket [| |] carries

     a) A mutable list of PendingSplice
          type PendingSplice = (Name, LHsExpr Id)

     b) The quoted expression e, *renamed*: (HsExpr Name)
          The expression e has been typechecked, but the result of
          that typechecking is discarded.

  * The brakcet is desugared by DsMeta.dsBracket.  It

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

  * When the type checker type-checks a nested splice ${n}(e), it
        - typechecks e
        - adds the typechecked expression (of type (HsExpr Id))
          as a pending splice to the enclosing bracket
        - returns something non-committal
    Eg for [| f ${n}(g x) |], the typechecker
        - attaches the typechecked term (g x) to the pending splices for n
          in the outer bracket
        - returns a non-committal type \alpha.
        Remember that the bracket discards the typechecked term altogether

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

See Note [What is a top-level Id?]

Note [Quoting names]
~~~~~~~~~~~~~~~~~~~~
A quoted name 'n is a bit like a quoted expression [| n |], except that we
have no cross-stage lifting (c.f. TcExpr.thBrackId).  So, after incrementing
the use-level to account for the brackets, the cases are:

        bind > use                      Error
        bind = use                      OK
        bind < use
                Imported things         OK
                Top-level things        OK
                Non-top-level           Error

See Note [What is a top-level Id?] in TcEnv.  Examples:

  f 'map        -- OK; also for top-level defns of this module

  \x. f 'x      -- Not ok (whereas \x. f [| x |] might have been ok, by
                --                               cross-stage lifting

  \y. [| \x. $(f 'y) |] -- Not ok (same reason)

  [| \x. $(f 'x) |]     -- OK


Note [What is a top-level Id?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the level-control criteria above, we need to know what a "top level Id" is.
There are three kinds:
  * Imported from another module                (GlobalId, ExternalName)
  * Bound at the top level of this module       (ExternalName)
  * In GHCi, bound by a previous stmt           (GlobalId)
It's strange that there is no one criterion tht picks out all three, but that's
how it is right now.  (The obvious thing is to give an ExternalName to GHCi Ids
bound in an earlier Stmt, but what module would you choose?  See
Note [Interactively-bound Ids in GHCi] in TcRnDriver.)

The predicate we use is TcEnv.thTopLevelId.


%************************************************************************
%*                                                                      *
\subsection{Main interface + stubs for the non-GHCI case
%*                                                                      *
%************************************************************************

\begin{code}
tcBracket     :: HsBracket Name -> TcRhoType -> TcM (HsExpr TcId)
tcSpliceDecls :: LHsExpr Name -> TcM [LHsDecl RdrName]
tcSpliceExpr  :: HsSplice Name -> TcRhoType -> TcM (HsExpr TcId)
tcSpliceType  :: HsSplice Name -> FreeVars -> TcM (TcType, TcKind)
        -- None of these functions add constraints to the LIE

lookupThName_maybe :: TH.Name -> TcM (Maybe Name)

runQuasiQuoteExpr :: HsQuasiQuote RdrName -> RnM (LHsExpr RdrName)
runQuasiQuotePat  :: HsQuasiQuote RdrName -> RnM (LPat RdrName)
runQuasiQuoteType :: HsQuasiQuote RdrName -> RnM (LHsType RdrName)
runQuasiQuoteDecl :: HsQuasiQuote RdrName -> RnM [LHsDecl RdrName]

runAnnotation     :: CoreAnnTarget -> LHsExpr Name -> TcM Annotation

#ifndef GHCI
tcBracket     x _ = pprPanic "Cant do tcBracket without GHCi"     (ppr x)
tcSpliceExpr  e   = pprPanic "Cant do tcSpliceExpr without GHCi"  (ppr e)
tcSpliceDecls x   = pprPanic "Cant do tcSpliceDecls without GHCi" (ppr x)
tcSpliceType  x fvs = pprPanic "Cant do kcSpliceType without GHCi"  (ppr x)

lookupThName_maybe n = pprPanic "Cant do lookupThName_maybe without GHCi" (ppr n)

runQuasiQuoteExpr q = pprPanic "Cant do runQuasiQuoteExpr without GHCi" (ppr q)
runQuasiQuotePat  q = pprPanic "Cant do runQuasiQuotePat without GHCi" (ppr q)
runQuasiQuoteType q = pprPanic "Cant do runQuasiQuoteType without GHCi" (ppr q)
runQuasiQuoteDecl q = pprPanic "Cant do runQuasiQuoteDecl without GHCi" (ppr q)
runAnnotation   _ q = pprPanic "Cant do runAnnotation without GHCi" (ppr q)
#else
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Quoting an expression}
%*                                                                      *
%************************************************************************


\begin{code}
-- See Note [How brackets and nested splices are handled]
tcBracket brack res_ty
  = addErrCtxt (hang (ptext (sLit "In the Template Haskell quotation"))
                   2 (ppr brack)) $
    do {        -- Check for nested brackets
         cur_stage <- getStage
       ; case cur_stage of
           { Splice True  -> checkTc (isTypedBracket brack) illegalUntypedBracket
           ; Splice False -> checkTc (not (isTypedBracket brack)) illegalTypedBracket
           ; Comp         -> return ()
           ; Brack {}     -> failWithTc illegalBracket
           }

        -- Brackets are desugared to code that mentions the TH package
       ; recordThUse

        -- Typecheck expr to make sure it is valid,
        -- but throw away the results.  We'll type check
        -- it again when we actually use it.
       ; pending_splices <- newMutVar []
       ; lie_var <- getConstraintVar
       ; let brack_stage = Brack (isTypedBracket brack) cur_stage pending_splices lie_var

          -- We want to check that there aren't any constraints that
          -- can't be satisfied (e.g. Show Foo, where Foo has no Show
          -- instance), but we aren't otherwise interested in the
          -- results. Nor do we care about ambiguous dictionaries etc.
          -- We will type check this bracket again at its usage site.
          --
          -- We build a single implication constraint with a BracketSkol;
          -- that in turn tells simplifyTop to report only definite
          -- errors
       ; ((_binds1, meta_ty), lie) <- captureConstraints $
                          newImplication BracketSkol [] [] $
                          setStage brack_stage $
                          tc_bracket cur_stage brack

          -- It's best to simplify the constraint now, even though in
          -- principle some later unification might be useful for it,
          -- because we don't want these essentially-junk TH implication
          -- constraints floating around nested inside other constraints
          -- See for example Trac #4949
       ; _binds2 <- simplifyTop lie

        -- Return the original expression, not the type-decorated one
       ; pendings <- readMutVar pending_splices
       ; co <- unifyType meta_ty res_ty
       ; return (mkHsWrapCo co (HsBracketOut brack pendings)) }

tc_bracket :: ThStage -> HsBracket Name -> TcM TcType
tc_bracket outer_stage br@(VarBr _ name)     -- Note [Quoting names]
  = do  { thing <- tcLookup name
        ; case thing of
            AGlobal {} -> return ()
            ATyVar {}  -> return ()
            ATcId { tct_level = bind_lvl, tct_id = id }
                | thTopLevelId id       -- C.f TcExpr.checkCrossStageLifting
                -> keepAliveTc id
                | otherwise
                -> do { checkTc (thLevel outer_stage + 1 == bind_lvl)
                                (quotedNameStageErr br) }
            _ -> pprPanic "th_bracket" (ppr name $$ ppr thing)

        ; tcMetaTy nameTyConName        -- Result type is Var (not Q-monadic)
        }

tc_bracket _ (ExpBr expr)
  = do  { any_ty <- newFlexiTyVarTy openTypeKind
        ; _ <- tcMonoExprNC expr any_ty  -- NC for no context; tcBracket does that
        ; tcMetaTy expQTyConName }
        -- Result type is ExpQ (= Q Exp)

tc_bracket _ (TypBr typ)
  = do  { _ <- tcLHsType typ    -- Do not check type validity; we can have a bracket
                                -- inside a "knot" where things are not yet settled
                                --    eg   data T a = MkT $(foo  [t| a |])
        ; tcMetaTy typeQTyConName }
        -- Result type is Type (= Q Typ)

tc_bracket _ (DecBrG decls)
  = do  { _ <- setXOptM Opt_ExistentialQuantification $
                   -- This is an EGREGIOUS HACK to make T5737 work.  That test splices
                   -- in a type in a data constructor arg type, and then we try to 
                   -- check validity for the data type decl, which fails because of
                   -- the pseudo-existential.  Stupid.  Will go away after the TH reorg
               tcTopSrcDecls emptyModDetails decls
               -- Typecheck the declarations, discarding the result
               -- We'll get all that stuff later, when we splice it in

               -- Top-level declarations in the bracket get unqualified names
               -- See Note [Top-level Names in Template Haskell decl quotes] in RnNames

        ; tcMetaTy decsQTyConName } -- Result type is Q [Dec]

tc_bracket _ (PatBr pat)
  = do  { any_ty <- newFlexiTyVarTy openTypeKind
        ; _ <- tcPat ThPatQuote pat any_ty $
               return ()
        ; tcMetaTy patQTyConName }
        -- Result type is PatQ (= Q Pat)

tc_bracket _ (DecBrL _)
  = panic "tc_bracket: Unexpected DecBrL"

tc_bracket _ (TExpBr expr)
  = do  { any_ty <- newFlexiTyVarTy openTypeKind
        ; _ <- tcMonoExprNC expr any_ty  -- NC for no context; tcBracket does that
        ; tcTExpTy any_ty }
        -- Result type is TExp tau

tcTExpTy :: TcType -> TcM TcType
tcTExpTy tau = do
    t <- tcLookupTyCon tExpTyConName
    return (mkTyConApp t [tau])

quotedNameStageErr :: HsBracket Name -> SDoc
quotedNameStageErr br
  = sep [ ptext (sLit "Stage error: the non-top-level quoted name") <+> ppr br
        , ptext (sLit "must be used at the same stage at which is is bound")]
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Splicing an expression}
%*                                                                      *
%************************************************************************

\begin{code}
tcSpliceExpr (HsSplice isTypedSplice name expr) res_ty
  = setSrcSpan (getLoc expr)    $ do
    { stage <- getStage
    ; case stage of {
        Splice {} -> tcTopSplice isTypedSplice expr res_ty ;
        Comp      -> tcTopSplice isTypedSplice expr res_ty ;

        Brack isTypedBrack pop_stage ps_var lie_var -> do

        -- See Note [How brackets and nested splices are handled]
        -- A splice inside brackets
        -- NB: ignore res_ty, apart from zapping it to a mono-type
        -- e.g.   [| reverse $(h 4) |]
        -- Here (h 4) :: Q Exp
        -- but $(h 4) :: forall a.a     i.e. anything!

     { when (isTypedBrack && not isTypedSplice) $
           failWithTc illegalUntypedSplice
     ; when (not isTypedBrack && isTypedSplice) $
           failWithTc illegalTypedSplice
     ; meta_exp_ty <- if isTypedSplice
                      then do { any_ty <- newFlexiTyVarTy openTypeKind
                              ; tcTExpTy any_ty
                              }
                      else tcMetaTy expQTyConName

     ; expr' <- setStage pop_stage $
                setConstraintVar lie_var    $
                tcMonoExpr expr meta_exp_ty

        -- Write the pending splice into the bucket
     ; ps <- readMutVar ps_var
     ; writeMutVar ps_var ((name,expr') : ps)

     ; return (panic "tcSpliceExpr")    -- The returned expression is ignored
     }}}

tcTopSplice :: Bool -> LHsExpr Name -> TcRhoType -> TcM (HsExpr Id)
-- Note [How top-level splices are handled]
tcTopSplice isTypedSplice expr res_ty
  = do { meta_exp_ty <- if isTypedSplice
                        then do { any_ty <- newFlexiTyVarTy openTypeKind
                                ; tcTExpTy any_ty
                                }
                        else tcMetaTy expQTyConName

        -- Typecheck the expression
       ; zonked_q_expr <- tcTopSpliceExpr isTypedSplice (tcMonoExpr expr meta_exp_ty)

        -- Run the expression
       ; expr2 <- runMetaE zonked_q_expr
       ; showSplice "expression" expr (ppr expr2)

       ; addErrCtxt (spliceResultDoc expr) $ do
       { (exp3, _fvs) <- checkNoErrs $ rnLExpr expr2
                         -- checkNoErrs: see Note [Renamer errors]
       ; exp4 <- tcMonoExpr exp3 res_ty
       ; return (unLoc exp4) } }

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
                Splicing a type
%*                                                                      *
%************************************************************************

Very like splicing an expression, but we don't yet share code.

\begin{code}
tcSpliceType (HsSplice _ name hs_expr) _
  = setSrcSpan (getLoc hs_expr) $ do
    { stage <- getStage
    ; case stage of {
        Splice {} -> tcTopSpliceType hs_expr ;
        Comp      -> tcTopSpliceType hs_expr ;

        Brack _ pop_level ps_var lie_var -> do
           -- See Note [How brackets and nested splices are handled]
           -- A splice inside brackets
    { meta_ty <- tcMetaTy typeQTyConName
    ; expr' <- setStage pop_level $
               setConstraintVar lie_var $
               tcMonoExpr hs_expr meta_ty

        -- Write the pending splice into the bucket
    ; ps <- readMutVar ps_var
    ; writeMutVar ps_var ((name,expr') : ps)

    -- e.g.   [| f (g :: Int -> $(h 4)) |]
    -- Here (h 4) :: Q Type
    -- but $(h 4) :: a  i.e. any type, of any kind

    ; kind <- newMetaKindVar
    ; ty <- newFlexiTyVarTy kind
    ; return (ty, kind)
    }}}

tcTopSpliceType :: LHsExpr Name -> TcM (TcType, TcKind)
-- Note [How top-level splices are handled]
tcTopSpliceType expr
  = do  { meta_ty <- tcMetaTy typeQTyConName

        -- Typecheck the expression
        ; zonked_q_expr <- tcTopSpliceExpr False (tcMonoExpr expr meta_ty)

        -- Run the expression
        ; hs_ty2 <- runMetaT zonked_q_expr
        ; showSplice "type" expr (ppr hs_ty2)
  
        ; addErrCtxt (spliceResultDoc expr) $ do 
        { let doc = SpliceTypeCtx hs_ty2
        ; (hs_ty3, _fvs) <- checkNoErrs $ rnLHsType doc hs_ty2
                         -- checkNoErrs: see Note [Renamer errors]
        ; tcLHsType hs_ty3 }}
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Splicing an expression}
%*                                                                      *
%************************************************************************

\begin{code}
-- Note [How top-level splices are handled]
-- Always at top level
-- Type sig at top of file:
--      tcSpliceDecls :: LHsExpr Name -> TcM [LHsDecl RdrName]
tcSpliceDecls expr
  = do  { list_q <- tcMetaTy decsQTyConName     -- Q [Dec]
        ; zonked_q_expr <- tcTopSpliceExpr False (tcMonoExpr expr list_q)

                -- Run the expression
        ; decls <- runMetaD zonked_q_expr
        ; showSplice "declarations" expr
                     (ppr (getLoc expr) $$ (vcat (map ppr decls)))

        ; return decls }
\end{code}


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

  qLookupName     = lookupName
  qReify          = reify
  qReifyInstances = reifyInstances
  qReifyRoles     = reifyRoles

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

illegalBracket :: SDoc
illegalBracket = ptext (sLit "Template Haskell brackets cannot be nested (without intervening splices)")

illegalTypedBracket :: SDoc
illegalTypedBracket = ptext (sLit "Typed brackets may only appear in typed slices.")

illegalUntypedBracket :: SDoc
illegalUntypedBracket = ptext (sLit "Untyped brackets may only appear in untyped slices.")

illegalTypedSplice :: SDoc
illegalTypedSplice = ptext (sLit "Typed splices may not appear in untyped brackets")

illegalUntypedSplice :: SDoc
illegalUntypedSplice = ptext (sLit "Untyped splices may not appear in typed brackets")
#endif  /* GHCI */
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

        ; case splitTyConApp_maybe ty of   -- This expands any type synonyms
            Just (tc, tys)                 -- See Trac #7910
               | Just cls <- tyConClass_maybe tc
               -> do { inst_envs <- tcGetInstEnvs
                     ; let (matches, unifies, _) = lookupInstEnv inst_envs cls tys
                     ; mapM reifyClassInstance (map fst matches ++ unifies) }
               | isOpenFamilyTyCon tc
               -> do { inst_envs <- tcGetFamInstEnvs
                     ; let matches = lookupFamInstEnv inst_envs tc tys
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
                Nothing    -> do
        { if nameIsLocalOrFrom (tcg_mod gbl_env) name
          then  -- It's defined in this module
              case lookupNameEnv (tcg_type_env gbl_env) name of
                Just thing -> return (AGlobal thing)
                Nothing    -> failWithTc (notInEnv name)

          else do               -- It's imported
        { mb_thing <- tcLookupImported_maybe name
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
reifyThing (AGlobal (ADataCon dc))
  = do  { let name = dataConName dc
        ; ty <- reifyType (idType (dataConWrapId dc))
        ; fix <- reifyFixity name
        ; return (TH.DataConI (reifyName name) ty
                              (reifyName (dataConOrigTyCon dc)) fix)
        }

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
  = do { args' <- mapM reifyType args
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
  = do { let tvs     = tyConTyVars tc
             kind    = tyConKind tc
       ; kind' <- if isLiftedTypeKind kind then return Nothing
                  else fmap Just (reifyKind kind)

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
        do { th_lhs <- reifyTypes lhs
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
  | otherwise
   = case classifyPredType ty of
  ClassPred cls tys -> do { tys' <- reifyTypes tys 
                          ; return $ TH.ClassP (reifyName cls) tys' }
  EqPred ty1 ty2    -> do { ty1' <- reifyType ty1
                          ; ty2' <- reifyType ty2
                          ; return $ TH.EqualP ty1' ty2'
                          }
  TuplePred _ -> noTH (sLit "tuple predicates") (ppr ty)
  IrredPred _ -> noTH (sLit "irreducible predicates") (ppr ty)


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

