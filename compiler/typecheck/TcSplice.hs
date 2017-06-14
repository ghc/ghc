{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


TcSplice: Template Haskell splices
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TcSplice(
     tcSpliceExpr, tcTypedBracket, tcUntypedBracket,
--     runQuasiQuoteExpr, runQuasiQuotePat,
--     runQuasiQuoteDecl, runQuasiQuoteType,
     runAnnotation,

     runMetaE, runMetaP, runMetaT, runMetaD, runQuasi,
     tcTopSpliceExpr, lookupThName_maybe,
     defaultRunMeta, runMeta', runRemoteModFinalizers,
     finishTH
      ) where

#include "HsVersions.h"

import HsSyn
import Annotations
import Name
import TcRnMonad
import TcType

import Outputable
import TcExpr
import SrcLoc
import THNames
import TcUnify
import TcEnv

import Control.Monad

import GHCi.Message
import GHCi.RemoteTypes
import GHCi
import HscMain
        -- These imports are the reason that TcSplice
        -- is very high up the module hierarchy
import RnSplice( traceSplice, SpliceInfo(..) )
import RdrName
import HscTypes
import Convert
import RnExpr
import RnEnv
import RnUtils ( HsDocContext(..) )
import RnFixity ( lookupFixityRn_help )
import RnTypes
import TcHsSyn
import TcSimplify
import Type
import Kind
import NameSet
import TcMType
import TcHsType
import TcIface
import TyCoRep
import FamInst
import FamInstEnv
import InstEnv
import Inst
import NameEnv
import PrelNames
import TysWiredIn
import OccName
import Hooks
import Var
import Module
import LoadIface
import Class
import TyCon
import CoAxiom
import PatSyn
import ConLike
import DataCon
import TcEvidence( TcEvBinds(..) )
import Id
import IdInfo
import DsExpr
import DsMonad
import GHC.Serialized
import ErrUtils
import Util
import Unique
import VarSet           ( isEmptyVarSet, filterVarSet, mkVarSet, elemVarSet )
import Data.List        ( find )
import Data.Maybe
import FastString
import BasicTypes hiding( SuccessFlag(..) )
import Maybes( MaybeErr(..) )
import DynFlags
import Panic
import Lexeme
import qualified EnumSet

import qualified Language.Haskell.TH as TH
-- THSyntax gives access to internal functions and data types
import qualified Language.Haskell.TH.Syntax as TH

-- Because GHC.Desugar might not be in the base library of the bootstrapping compiler
import GHC.Desugar      ( AnnotationWrapper(..) )

import Control.Exception
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Dynamic  ( fromDynamic, toDyn )
import qualified Data.Map as Map
import Data.Typeable ( typeOf, Typeable, TypeRep, typeRep )
import Data.Data (Data)
import Data.Proxy    ( Proxy (..) )
import GHC.Exts         ( unsafeCoerce# )

{-
************************************************************************
*                                                                      *
\subsection{Main interface + stubs for the non-GHCI case
*                                                                      *
************************************************************************
-}

tcTypedBracket   :: HsExpr GhcRn -> HsBracket GhcRn -> ExpRhoType -> TcM (HsExpr GhcTcId)
tcUntypedBracket :: HsExpr GhcRn -> HsBracket GhcRn -> [PendingRnSplice] -> ExpRhoType
                 -> TcM (HsExpr GhcTcId)
tcSpliceExpr     :: HsSplice GhcRn  -> ExpRhoType -> TcM (HsExpr GhcTcId)
        -- None of these functions add constraints to the LIE

-- runQuasiQuoteExpr :: HsQuasiQuote RdrName -> RnM (LHsExpr RdrName)
-- runQuasiQuotePat  :: HsQuasiQuote RdrName -> RnM (LPat RdrName)
-- runQuasiQuoteType :: HsQuasiQuote RdrName -> RnM (LHsType RdrName)
-- runQuasiQuoteDecl :: HsQuasiQuote RdrName -> RnM [LHsDecl RdrName]

runAnnotation     :: CoreAnnTarget -> LHsExpr GhcRn -> TcM Annotation
{-
************************************************************************
*                                                                      *
\subsection{Quoting an expression}
*                                                                      *
************************************************************************
-}

-- See Note [How brackets and nested splices are handled]
-- tcTypedBracket :: HsBracket Name -> TcRhoType -> TcM (HsExpr TcId)
tcTypedBracket rn_expr brack@(TExpBr expr) res_ty
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
       ; ps' <- readMutVar ps_ref
       ; texpco <- tcLookupId unsafeTExpCoerceName
       ; tcWrapResultO (Shouldn'tHappenOrigin "TExpBr")
                       rn_expr
                       (unLoc (mkHsApp (nlHsTyApp texpco [expr_ty])
                                              (noLoc (HsTcBracketOut brack ps'))))
                       meta_ty res_ty }
tcTypedBracket _ other_brack _
  = pprPanic "tcTypedBracket" (ppr other_brack)

-- tcUntypedBracket :: HsBracket Name -> [PendingRnSplice] -> ExpRhoType -> TcM (HsExpr TcId)
tcUntypedBracket rn_expr brack ps res_ty
  = do { traceTc "tc_bracket untyped" (ppr brack $$ ppr ps)
       ; ps' <- mapM tcPendingSplice ps
       ; meta_ty <- tcBrackTy brack
       ; traceTc "tc_bracket done untyped" (ppr meta_ty)
       ; tcWrapResultO (Shouldn'tHappenOrigin "untyped bracket")
                       rn_expr (HsTcBracketOut brack ps') meta_ty res_ty }

---------------
tcBrackTy :: HsBracket GhcRn -> TcM TcType
tcBrackTy (VarBr _ _) = tcMetaTy nameTyConName  -- Result type is Var (not Q-monadic)
tcBrackTy (ExpBr _)   = tcMetaTy expQTyConName  -- Result type is ExpQ (= Q Exp)
tcBrackTy (TypBr _)   = tcMetaTy typeQTyConName -- Result type is Type (= Q Typ)
tcBrackTy (DecBrG _)  = tcMetaTy decsQTyConName -- Result type is Q [Dec]
tcBrackTy (PatBr _)   = tcMetaTy patQTyConName  -- Result type is PatQ (= Q Pat)
tcBrackTy (DecBrL _)  = panic "tcBrackTy: Unexpected DecBrL"
tcBrackTy (TExpBr _)  = panic "tcUntypedBracket: Unexpected TExpBr"

---------------
tcPendingSplice :: PendingRnSplice -> TcM PendingTcSplice
tcPendingSplice (PendingRnSplice flavour splice_name expr)
  = do { res_ty <- tcMetaTy meta_ty_name
       ; expr' <- tcMonoExpr expr (mkCheckExpType res_ty)
       ; return (PendingTcSplice splice_name expr') }
  where
     meta_ty_name = case flavour of
                       UntypedExpSplice  -> expQTyConName
                       UntypedPatSplice  -> patQTyConName
                       UntypedTypeSplice -> typeQTyConName
                       UntypedDeclSplice -> decsQTyConName

---------------
-- Takes a tau and returns the type Q (TExp tau)
tcTExpTy :: TcType -> TcM TcType
tcTExpTy exp_ty
  = do { unless (isTauTy exp_ty) $ addErr (err_msg exp_ty)
       ; q    <- tcLookupTyCon qTyConName
       ; texp <- tcLookupTyCon tExpTyConName
       ; return (mkTyConApp q [mkTyConApp texp [exp_ty]]) }
  where
    err_msg ty
      = vcat [ text "Illegal polytype:" <+> ppr ty
             , text "The type of a Typed Template Haskell expression must" <+>
               text "not have any quantification." ]

quotationCtxtDoc :: HsBracket GhcRn -> SDoc
quotationCtxtDoc br_body
  = hang (text "In the Template Haskell quotation")
         2 (ppr br_body)


  -- The whole of the rest of the file is the else-branch (ie stage2 only)

{-
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
The top level of the program is stage Comp:

     Start here
         |
         V
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

-}

{-
************************************************************************
*                                                                      *
\subsection{Splicing an expression}
*                                                                      *
************************************************************************
-}

tcSpliceExpr splice@(HsTypedSplice _ name expr) res_ty
  = addErrCtxt (spliceCtxtDoc splice) $
    setSrcSpan (getLoc expr)    $ do
    { stage <- getStage
    ; case stage of
          Splice {}            -> tcTopSplice expr res_ty
          Brack pop_stage pend -> tcNestedSplice pop_stage pend name expr res_ty
          RunSplice _          ->
            -- See Note [RunSplice ThLevel] in "TcRnTypes".
            pprPanic ("tcSpliceExpr: attempted to typecheck a splice when " ++
                      "running another splice") (ppr splice)
          Comp                 -> tcTopSplice expr res_ty
    }
tcSpliceExpr splice _
  = pprPanic "tcSpliceExpr" (ppr splice)

{- Note [Collecting modFinalizers in typed splices]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

'qAddModFinalizer' of the @Quasi TcM@ instance adds finalizers in the local
environment (see Note [Delaying modFinalizers in untyped splices] in
"RnSplice"). Thus after executing the splice, we move the finalizers to the
finalizer list in the global environment and set them to use the current local
environment (with 'addModFinalizersWithLclEnv').

-}

tcNestedSplice :: ThStage -> PendingStuff -> Name
                -> LHsExpr GhcRn -> ExpRhoType -> TcM (HsExpr GhcTc)
    -- See Note [How brackets and nested splices are handled]
    -- A splice inside brackets
tcNestedSplice pop_stage (TcPending ps_var lie_var) splice_name expr res_ty
  = do { res_ty <- expTypeToType res_ty
       ; meta_exp_ty <- tcTExpTy res_ty
       ; expr' <- setStage pop_stage $
                  setConstraintVar lie_var $
                  tcMonoExpr expr (mkCheckExpType meta_exp_ty)
       ; untypeq <- tcLookupId unTypeQName
       ; let expr'' = mkHsApp (nlHsTyApp untypeq [res_ty]) expr'
       ; ps <- readMutVar ps_var
       ; writeMutVar ps_var (PendingTcSplice splice_name expr'' : ps)

       -- The returned expression is ignored; it's in the pending splices
       ; return (panic "tcSpliceExpr") }

tcNestedSplice _ _ splice_name _ _
  = pprPanic "tcNestedSplice: rename stage found" (ppr splice_name)

tcTopSplice :: LHsExpr GhcRn -> ExpRhoType -> TcM (HsExpr GhcTc)
tcTopSplice expr res_ty
  = do { -- Typecheck the expression,
         -- making sure it has type Q (T res_ty)
         res_ty <- expTypeToType res_ty
       ; meta_exp_ty <- tcTExpTy res_ty
       ; zonked_q_expr <- tcTopSpliceExpr Typed $
                          tcMonoExpr expr (mkCheckExpType meta_exp_ty)

         -- See Note [Collecting modFinalizers in typed splices].
       ; modfinalizers_ref <- newTcRef []
         -- Run the expression
       ; expr2 <- setStage (RunSplice modfinalizers_ref) $
                    runMetaE zonked_q_expr
       ; mod_finalizers <- readTcRef modfinalizers_ref
       ; addModFinalizersWithLclEnv $ ThModFinalizers mod_finalizers
       ; traceSplice (SpliceInfo { spliceDescription = "expression"
                                 , spliceIsDecl      = False
                                 , spliceSource      = Just expr
                                 , spliceGenerated   = ppr expr2 })

         -- Rename and typecheck the spliced-in expression,
         -- making sure it has type res_ty
         -- These steps should never fail; this is a *typed* splice
       ; addErrCtxt (spliceResultDoc expr) $ do
       { (exp3, _fvs) <- rnLExpr expr2
       ; exp4 <- tcMonoExpr exp3 (mkCheckExpType res_ty)
       ; return (unLoc exp4) } }

{-
************************************************************************
*                                                                      *
\subsection{Error messages}
*                                                                      *
************************************************************************
-}

spliceCtxtDoc :: HsSplice GhcRn -> SDoc
spliceCtxtDoc splice
  = hang (text "In the Template Haskell splice")
         2 (pprSplice splice)

spliceResultDoc :: LHsExpr GhcRn -> SDoc
spliceResultDoc expr
  = sep [ text "In the result of the splice:"
        , nest 2 (char '$' <> ppr expr)
        , text "To see what the splice expanded to, use -ddump-splices"]

-------------------
tcTopSpliceExpr :: SpliceType -> TcM (LHsExpr GhcTc) -> TcM (LHsExpr GhcTc)
-- Note [How top-level splices are handled]
-- Type check an expression that is the body of a top-level splice
--   (the caller will compile and run it)
-- Note that set the level to Splice, regardless of the original level,
-- before typechecking the expression.  For example:
--      f x = $( ...$(g 3) ... )
-- The recursive call to tcPolyExpr will simply expand the
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
         (expr', wanted) <- captureConstraints tc_action
       ; const_binds     <- simplifyTop wanted

          -- Zonk it and tie the knot of dictionary bindings
       ; zonkTopLExpr (mkHsDictLet (EvBinds const_binds) expr') }

{-
************************************************************************
*                                                                      *
        Annotations
*                                                                      *
************************************************************************
-}

runAnnotation target expr = do
    -- Find the classes we want instances for in order to call toAnnotationWrapper
    loc <- getSrcSpanM
    data_class <- tcLookupClass dataClassName
    to_annotation_wrapper_id <- tcLookupId toAnnotationWrapperName

    -- Check the instances we require live in another module (we want to execute it..)
    -- and check identifiers live in other modules using TH stage checks. tcSimplifyStagedExpr
    -- also resolves the LIE constraints to detect e.g. instance ambiguity
    zonked_wrapped_expr' <- tcTopSpliceExpr Untyped $
           do { (expr', expr_ty) <- tcInferRhoNC expr
                -- We manually wrap the typechecked expression in a call to toAnnotationWrapper
                -- By instantiating the call >here< it gets registered in the
                -- LIE consulted by tcTopSpliceExpr
                -- and hence ensures the appropriate dictionary is bound by const_binds
              ; wrapper <- instCall AnnOrigin [expr_ty] [mkClassPred data_class [expr_ty]]
              ; let specialised_to_annotation_wrapper_expr
                      = L loc (mkHsWrap wrapper
                                        (HsVar (L loc to_annotation_wrapper_id)))
              ; return (L loc (HsApp specialised_to_annotation_wrapper_expr expr')) }

    -- Run the appropriately wrapped expression to get the value of
    -- the annotation and its dictionaries. The return value is of
    -- type AnnotationWrapper by construction, so this conversion is
    -- safe
    serialized <- runMetaAW zonked_wrapped_expr'
    return Annotation {
               ann_target = target,
               ann_value = serialized
           }

convertAnnotationWrapper :: ForeignHValue -> TcM (Either MsgDoc Serialized)
convertAnnotationWrapper fhv = do
  dflags <- getDynFlags
  if gopt Opt_ExternalInterpreter dflags
    then do
      Right <$> runTH THAnnWrapper fhv
    else do
      annotation_wrapper <- liftIO $ wormhole dflags fhv
      return $ Right $
        case unsafeCoerce# annotation_wrapper of
           AnnotationWrapper value | let serialized = toSerialized serializeWithData value ->
               -- Got the value and dictionaries: build the serialized value and
               -- call it a day. We ensure that we seq the entire serialized value
               -- in order that any errors in the user-written code for the
               -- annotation are exposed at this point.  This is also why we are
               -- doing all this stuff inside the context of runMeta: it has the
               -- facilities to deal with user error in a meta-level expression
               seqSerialized serialized `seq` serialized

-- | Force the contents of the Serialized value so weknow it doesn't contain any bottoms
seqSerialized :: Serialized -> ()
seqSerialized (Serialized the_type bytes) = the_type `seq` bytes `seqList` ()


{-
************************************************************************
*                                                                      *
\subsection{Running an expression}
*                                                                      *
************************************************************************
-}

runQuasi :: TH.Q a -> TcM a
runQuasi act = TH.runQ act

runRemoteModFinalizers :: ThModFinalizers -> TcM ()
runRemoteModFinalizers (ThModFinalizers finRefs) = do
  dflags <- getDynFlags
  let withForeignRefs [] f = f []
      withForeignRefs (x : xs) f = withForeignRef x $ \r ->
        withForeignRefs xs $ \rs -> f (r : rs)
  if gopt Opt_ExternalInterpreter dflags then do
    hsc_env <- env_top <$> getEnv
    withIServ hsc_env $ \i -> do
      tcg <- getGblEnv
      th_state <- readTcRef (tcg_th_remote_state tcg)
      case th_state of
        Nothing -> return () -- TH was not started, nothing to do
        Just fhv -> do
          liftIO $ withForeignRef fhv $ \st ->
            withForeignRefs finRefs $ \qrefs ->
              writeIServ i (putMessage (RunModFinalizers st qrefs))
          () <- runRemoteTH i []
          readQResult i
  else do
    qs <- liftIO (withForeignRefs finRefs $ mapM localRef)
    runQuasi $ sequence_ qs

runQResult
  :: (a -> String)
  -> (SrcSpan -> a -> b)
  -> (ForeignHValue -> TcM a)
  -> SrcSpan
  -> ForeignHValue {- TH.Q a -}
  -> TcM b
runQResult show_th f runQ expr_span hval
  = do { th_result <- runQ hval
       ; traceTc "Got TH result:" (text (show_th th_result))
       ; return (f expr_span th_result) }


-----------------
runMeta :: (MetaHook TcM -> LHsExpr GhcTc -> TcM hs_syn)
        -> LHsExpr GhcTc
        -> TcM hs_syn
runMeta unwrap e
  = do { h <- getHooked runMetaHook defaultRunMeta
       ; unwrap h e }

defaultRunMeta :: MetaHook TcM
defaultRunMeta (MetaE r)
  = fmap r . runMeta' True ppr (runQResult TH.pprint convertToHsExpr runTHExp)
defaultRunMeta (MetaP r)
  = fmap r . runMeta' True ppr (runQResult TH.pprint convertToPat runTHPat)
defaultRunMeta (MetaT r)
  = fmap r . runMeta' True ppr (runQResult TH.pprint convertToHsType runTHType)
defaultRunMeta (MetaD r)
  = fmap r . runMeta' True ppr (runQResult TH.pprint convertToHsDecls runTHDec)
defaultRunMeta (MetaAW r)
  = fmap r . runMeta' False (const empty) (const convertAnnotationWrapper)
    -- We turn off showing the code in meta-level exceptions because doing so exposes
    -- the toAnnotationWrapper function that we slap around the user's code

----------------
runMetaAW :: LHsExpr GhcTc         -- Of type AnnotationWrapper
          -> TcM Serialized
runMetaAW = runMeta metaRequestAW

runMetaE :: LHsExpr GhcTc          -- Of type (Q Exp)
         -> TcM (LHsExpr GhcPs)
runMetaE = runMeta metaRequestE

runMetaP :: LHsExpr GhcTc          -- Of type (Q Pat)
         -> TcM (LPat GhcPs)
runMetaP = runMeta metaRequestP

runMetaT :: LHsExpr GhcTc          -- Of type (Q Type)
         -> TcM (LHsType GhcPs)
runMetaT = runMeta metaRequestT

runMetaD :: LHsExpr GhcTc          -- Of type Q [Dec]
         -> TcM [LHsDecl GhcPs]
runMetaD = runMeta metaRequestD

---------------
runMeta' :: Bool                 -- Whether code should be printed in the exception message
         -> (hs_syn -> SDoc)                                    -- how to print the code
         -> (SrcSpan -> ForeignHValue -> TcM (Either MsgDoc hs_syn))        -- How to run x
         -> LHsExpr GhcTc        -- Of type x; typically x = Q TH.Exp, or
                                 --    something like that
         -> TcM hs_syn           -- Of type t
runMeta' show_code ppr_hs run_and_convert expr
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
             do { mb_result <- run_and_convert expr_span hval
                ; case mb_result of
                    Left err     -> failWithTc err
                    Right result -> do { traceTc "Got HsSyn result:" (ppr_hs result)
                                       ; return $! result } }

        ; case either_tval of
            Right v -> return v
            Left se -> case fromException se of
                         Just IOEnvFailure -> failM -- Error already in Tc monad
                         _ -> fail_with_exn "run" se -- Exception
        }}}
  where
    -- see Note [Concealed TH exceptions]
    fail_with_exn :: Exception e => String -> e -> TcM a
    fail_with_exn phase exn = do
        exn_msg <- liftIO $ Panic.safeShowException exn
        let msg = vcat [text "Exception when trying to" <+> text phase <+> text "compile-time code:",
                        nest 2 (text exn_msg),
                        if show_code then text "Code:" <+> ppr expr else empty]
        failWithTc msg

{-
Note [Exceptions in TH]
~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have something like this
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

 * 'qReport' forces the message to ensure any exception hidden in unevaluated
   thunk doesn't get into the bag of errors. Otherwise the following splice
   will triger panic (Trac #8987):
        $(fail undefined)
   See also Note [Concealed TH exceptions]

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
-}

instance TH.Quasi TcM where
  qNewName s = do { u <- newUnique
                  ; let i = getKey u
                  ; return (TH.mkNameU s i) }

  -- 'msg' is forced to ensure exceptions don't escape,
  -- see Note [Exceptions in TH]
  qReport True msg  = seqList msg $ addErr  (text msg)
  qReport False msg = seqList msg $ addWarn NoReason (text msg)

  qLocation = do { m <- getModule
                 ; l <- getSrcSpanM
                 ; r <- case l of
                        UnhelpfulSpan _ -> pprPanic "qLocation: Unhelpful location"
                                                    (ppr l)
                        RealSrcSpan s -> return s
                 ; return (TH.Loc { TH.loc_filename = unpackFS (srcSpanFile r)
                                  , TH.loc_module   = moduleNameString (moduleName m)
                                  , TH.loc_package  = unitIdString (moduleUnitId m)
                                  , TH.loc_start = (srcSpanStartLine r, srcSpanStartCol r)
                                  , TH.loc_end = (srcSpanEndLine   r, srcSpanEndCol   r) }) }

  qLookupName       = lookupName
  qReify            = reify
  qReifyFixity nm   = lookupThName nm >>= reifyFixity
  qReifyInstances   = reifyInstances
  qReifyRoles       = reifyRoles
  qReifyAnnotations = reifyAnnotations
  qReifyModule      = reifyModule
  qReifyConStrictness nm = do { nm' <- lookupThName nm
                              ; dc  <- tcLookupDataCon nm'
                              ; let bangs = dataConImplBangs dc
                              ; return (map reifyDecidedStrictness bangs) }

        -- For qRecover, discard error messages if
        -- the recovery action is chosen.  Otherwise
        -- we'll only fail higher up.
  qRecover recover main = tryTcDiscardingErrs recover main
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
      checkTopDecl :: HsDecl GhcPs -> TcM ()
      checkTopDecl (ValD binds)
        = mapM_ bindName (collectHsBindBinders binds)
      checkTopDecl (SigD _)
        = return ()
      checkTopDecl (AnnD _)
        = return ()
      checkTopDecl (ForD (ForeignImport { fd_name = L _ name }))
        = bindName name
      checkTopDecl _
        = addErr $ text "Only function, value, annotation, and foreign import declarations may be added with addTopDecl"

      bindName :: RdrName -> TcM ()
      bindName (Exact n)
        = do { th_topnames_var <- fmap tcg_th_topnames getGblEnv
             ; updTcRef th_topnames_var (\ns -> extendNameSet ns n)
             }

      bindName name =
          addErr $
          hang (text "The binder" <+> quotes (ppr name) <+> ptext (sLit "is not a NameU."))
             2 (text "Probable cause: you used mkName instead of newName to generate a binding.")

  qAddForeignFile lang str = do
    var <- fmap tcg_th_foreign_files getGblEnv
    updTcRef var ((lang, str) :)

  qAddModFinalizer fin = do
      r <- liftIO $ mkRemoteRef fin
      fref <- liftIO $ mkForeignRef r (freeRemoteRef r)
      addModFinalizerRef fref

  qGetQ :: forall a. Typeable a => TcM (Maybe a)
  qGetQ = do
      th_state_var <- fmap tcg_th_state getGblEnv
      th_state <- readTcRef th_state_var
      -- See #10596 for why we use a scoped type variable here.
      return (Map.lookup (typeRep (Proxy :: Proxy a)) th_state >>= fromDynamic)

  qPutQ x = do
      th_state_var <- fmap tcg_th_state getGblEnv
      updTcRef th_state_var (\m -> Map.insert (typeOf x) (toDyn x) m)

  qIsExtEnabled = xoptM

  qExtsEnabled =
    EnumSet.toList . extensionFlags . hsc_dflags <$> getTopEnv

-- | Adds a mod finalizer reference to the local environment.
addModFinalizerRef :: ForeignRef (TH.Q ()) -> TcM ()
addModFinalizerRef finRef = do
    th_stage <- getStage
    case th_stage of
      RunSplice th_modfinalizers_var -> updTcRef th_modfinalizers_var (finRef :)
      -- This case happens only if a splice is executed and the caller does
      -- not set the 'ThStage' to 'RunSplice' to collect finalizers.
      -- See Note [Delaying modFinalizers in untyped splices] in RnSplice.
      _ ->
        pprPanic "addModFinalizer was called when no finalizers were collected"
                 (ppr th_stage)

-- | Releases the external interpreter state.
finishTH :: TcM ()
finishTH = do
  dflags <- getDynFlags
  when (gopt Opt_ExternalInterpreter dflags) $ do
    tcg <- getGblEnv
    writeTcRef (tcg_th_remote_state tcg) Nothing

runTHExp :: ForeignHValue -> TcM TH.Exp
runTHExp = runTH THExp

runTHPat :: ForeignHValue -> TcM TH.Pat
runTHPat = runTH THPat

runTHType :: ForeignHValue -> TcM TH.Type
runTHType = runTH THType

runTHDec :: ForeignHValue -> TcM [TH.Dec]
runTHDec = runTH THDec

runTH :: Binary a => THResultType -> ForeignHValue -> TcM a
runTH ty fhv = do
  hsc_env <- env_top <$> getEnv
  dflags <- getDynFlags
  if not (gopt Opt_ExternalInterpreter dflags)
    then do
       -- Run it in the local TcM
      hv <- liftIO $ wormhole dflags fhv
      r <- runQuasi (unsafeCoerce# hv :: TH.Q a)
      return r
    else
      -- Run it on the server.  For an overview of how TH works with
      -- Remote GHCi, see Note [Remote Template Haskell] in
      -- libraries/ghci/GHCi/TH.hs.
      withIServ hsc_env $ \i -> do
        rstate <- getTHState i
        loc <- TH.qLocation
        liftIO $
          withForeignRef rstate $ \state_hv ->
          withForeignRef fhv $ \q_hv ->
            writeIServ i (putMessage (RunTH state_hv q_hv ty (Just loc)))
        runRemoteTH i []
        bs <- readQResult i
        return $! runGet get (LB.fromStrict bs)


-- | communicate with a remotely-running TH computation until it finishes.
-- See Note [Remote Template Haskell] in libraries/ghci/GHCi/TH.hs.
runRemoteTH
  :: IServ
  -> [Messages]   --  saved from nested calls to qRecover
  -> TcM ()
runRemoteTH iserv recovers = do
  THMsg msg <- liftIO $ readIServ iserv getTHMessage
  case msg of
    RunTHDone -> return ()
    StartRecover -> do -- Note [TH recover with -fexternal-interpreter]
      v <- getErrsVar
      msgs <- readTcRef v
      writeTcRef v emptyMessages
      runRemoteTH iserv (msgs : recovers)
    EndRecover caught_error -> do
      v <- getErrsVar
      let (prev_msgs, rest) = case recovers of
             [] -> panic "EndRecover"
             a : b -> (a,b)
      if caught_error
        then writeTcRef v prev_msgs
        else updTcRef v (unionMessages prev_msgs)
      runRemoteTH iserv rest
    _other -> do
      r <- handleTHMessage msg
      liftIO $ writeIServ iserv (put r)
      runRemoteTH iserv recovers

-- | Read a value of type QResult from the iserv
readQResult :: Binary a => IServ -> TcM a
readQResult i = do
  qr <- liftIO $ readIServ i get
  case qr of
    QDone a -> return a
    QException str -> liftIO $ throwIO (ErrorCall str)
    QFail str -> fail str

{- Note [TH recover with -fexternal-interpreter]

Recover is slightly tricky to implement.

The meaning of "recover a b" is
 - Do a
   - If it finished successfully, then keep the messages it generated
   - If it failed, discard any messages it generated, and do b

The messages are managed by GHC in the TcM monad, whereas the
exception-handling is done in the ghc-iserv process, so we have to
coordinate between the two.

On the server:
  - emit a StartRecover message
  - run "a" inside a catch
    - if it finishes, emit EndRecover False
    - if it fails, emit EndRecover True, then run "b"

Back in GHC, when we receive:

  StartRecover
    save the current messages and start with an empty set.
  EndRecover caught_error
    Restore the previous messages,
    and merge in the new messages if caught_error is false.
-}

-- | Retrieve (or create, if it hasn't been created already), the
-- remote TH state.  The TH state is a remote reference to an IORef
-- QState living on the server, and we have to pass this to each RunTH
-- call we make.
--
-- The TH state is stored in tcg_th_remote_state in the TcGblEnv.
--
getTHState :: IServ -> TcM (ForeignRef (IORef QState))
getTHState i = do
  tcg <- getGblEnv
  th_state <- readTcRef (tcg_th_remote_state tcg)
  case th_state of
    Just rhv -> return rhv
    Nothing -> do
      hsc_env <- env_top <$> getEnv
      fhv <- liftIO $ mkFinalizedHValue hsc_env =<< iservCall i StartTH
      writeTcRef (tcg_th_remote_state tcg) (Just fhv)
      return fhv

wrapTHResult :: TcM a -> TcM (THResult a)
wrapTHResult tcm = do
  e <- tryM tcm   -- only catch 'fail', treat everything else as catastrophic
  case e of
    Left e -> return (THException (show e))
    Right a -> return (THComplete a)

handleTHMessage :: THMessage a -> TcM a
handleTHMessage msg = case msg of
  NewName a -> wrapTHResult $ TH.qNewName a
  Report b str -> wrapTHResult $ TH.qReport b str
  LookupName b str -> wrapTHResult $ TH.qLookupName b str
  Reify n -> wrapTHResult $ TH.qReify n
  ReifyFixity n -> wrapTHResult $ TH.qReifyFixity n
  ReifyInstances n ts -> wrapTHResult $ TH.qReifyInstances n ts
  ReifyRoles n -> wrapTHResult $ TH.qReifyRoles n
  ReifyAnnotations lookup tyrep ->
    wrapTHResult $ (map B.pack <$> getAnnotationsByTypeRep lookup tyrep)
  ReifyModule m -> wrapTHResult $ TH.qReifyModule m
  ReifyConStrictness nm -> wrapTHResult $ TH.qReifyConStrictness nm
  AddDependentFile f -> wrapTHResult $ TH.qAddDependentFile f
  AddModFinalizer r -> do
    hsc_env <- env_top <$> getEnv
    wrapTHResult $ liftIO (mkFinalizedHValue hsc_env r) >>= addModFinalizerRef
  AddTopDecls decs -> wrapTHResult $ TH.qAddTopDecls decs
  AddForeignFile lang str -> wrapTHResult $ TH.qAddForeignFile lang str
  IsExtEnabled ext -> wrapTHResult $ TH.qIsExtEnabled ext
  ExtsEnabled -> wrapTHResult $ TH.qExtsEnabled
  _ -> panic ("handleTHMessage: unexpected message " ++ show msg)

getAnnotationsByTypeRep :: TH.AnnLookup -> TypeRep -> TcM [[Word8]]
getAnnotationsByTypeRep th_name tyrep
  = do { name <- lookupThAnnLookup th_name
       ; topEnv <- getTopEnv
       ; epsHptAnns <- liftIO $ prepareAnnotations topEnv Nothing
       ; tcg <- getGblEnv
       ; let selectedEpsHptAnns = findAnnsByTypeRep epsHptAnns name tyrep
       ; let selectedTcgAnns = findAnnsByTypeRep (tcg_ann_env tcg) name tyrep
       ; return (selectedEpsHptAnns ++ selectedTcgAnns) }

{-
************************************************************************
*                                                                      *
            Instance Testing
*                                                                      *
************************************************************************
-}

reifyInstances :: TH.Name -> [TH.Type] -> TcM [TH.Dec]
reifyInstances th_nm th_tys
   = addErrCtxt (text "In the argument of reifyInstances:"
                 <+> ppr_th th_nm <+> sep (map ppr_th th_tys)) $
     do { loc <- getSrcSpanM
        ; rdr_ty <- cvt loc (mkThAppTs (TH.ConT th_nm) th_tys)
          -- #9262 says to bring vars into scope, like in HsForAllTy case
          -- of rnHsTyKi
        ; free_vars <- extractHsTyRdrTyVars rdr_ty
        ; let tv_rdrs = freeKiTyVarsAllVars free_vars
          -- Rename  to HsType Name
        ; ((tv_names, rn_ty), _fvs)
            <- bindLRdrNames tv_rdrs $ \ tv_names ->
               do { (rn_ty, fvs) <- rnLHsType doc rdr_ty
                  ; return ((tv_names, rn_ty), fvs) }
        ; (_tvs, ty)
            <- solveEqualities $
               tcImplicitTKBndrsType tv_names $
               fst <$> tcLHsType rn_ty
        ; ty <- zonkTcTypeToType emptyZonkEnv ty
                -- Substitute out the meta type variables
                -- In particular, the type might have kind
                -- variables inside it (Trac #7477)

        ; traceTc "reifyInstances" (ppr ty $$ ppr (typeKind ty))
        ; case splitTyConApp_maybe ty of   -- This expands any type synonyms
            Just (tc, tys)                 -- See Trac #7910
               | Just cls <- tyConClass_maybe tc
               -> do { inst_envs <- tcGetInstEnvs
                     ; let (matches, unifies, _) = lookupInstEnv False inst_envs cls tys
                     ; traceTc "reifyInstances1" (ppr matches)
                     ; reifyClassInstances cls (map fst matches ++ unifies) }
               | isOpenFamilyTyCon tc
               -> do { inst_envs <- tcGetFamInstEnvs
                     ; let matches = lookupFamInstEnv inst_envs tc tys
                     ; traceTc "reifyInstances2" (ppr matches)
                     ; reifyFamilyInstances tc (map fim_instance matches) }
            _  -> bale_out (hang (text "reifyInstances:" <+> quotes (ppr ty))
                               2 (text "is not a class constraint or type family application")) }
  where
    doc = ClassInstanceCtx
    bale_out msg = failWithTc msg

    cvt :: SrcSpan -> TH.Type -> TcM (LHsType GhcPs)
    cvt loc th_ty = case convertToHsType loc th_ty of
                      Left msg -> failWithTc msg
                      Right ty -> return ty

{-
************************************************************************
*                                                                      *
                        Reification
*                                                                      *
************************************************************************
-}

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
        = if isLexVarSym occ_fs || isLexCon occ_fs
                             then mkTcOccFS    occ_fs
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
    ppr_ns (TH.Name _ (TH.NameG TH.DataName  _pkg _mod)) = text "data"
    ppr_ns (TH.Name _ (TH.NameG TH.TcClsName _pkg _mod)) = text "tc"
    ppr_ns (TH.Name _ (TH.NameG TH.VarName   _pkg _mod)) = text "var"
    ppr_ns _ = panic "reify/ppr_ns"

reify :: TH.Name -> TcM TH.Info
reify th_name
  = do  { traceTc "reify 1" (text (TH.showName th_name))
        ; thing <- getThing th_name
        ; traceTc "reify 2" (ppr thing)
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
        = do {  -- Repeat much of lookupOccRn, because we want
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

          -- EZY: I don't think this choice matters, no TH in signatures!
          if nameIsLocalOrFrom (tcg_semantic_mod gbl_env) name
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
                     text "is not in scope at a reify"
        -- Ugh! Rather an indirect way to display the name

notInEnv :: Name -> SDoc
notInEnv name = quotes (ppr name) <+>
                     text "is not in the type environment at a reify"

------------------------------
reifyRoles :: TH.Name -> TcM [TH.Role]
reifyRoles th_name
  = do { thing <- getThing th_name
       ; case thing of
           AGlobal (ATyCon tc) -> return (map reify_role (tyConRoles tc))
           _ -> failWithTc (text "No roles associated with" <+> (ppr thing))
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
        ; let v = reifyName id
        ; case idDetails id of
            ClassOpId cls -> return (TH.ClassOpI v ty (reifyName cls))
            RecSelId{sel_tycon=RecSelData tc}
                          -> return (TH.VarI (reifySelector id tc) ty Nothing)
            _             -> return (TH.VarI     v ty Nothing)
    }

reifyThing (AGlobal (ATyCon tc))   = reifyTyCon tc
reifyThing (AGlobal (AConLike (RealDataCon dc)))
  = do  { let name = dataConName dc
        ; ty <- reifyType (idType (dataConWrapId dc))
        ; return (TH.DataConI (reifyName name) ty
                              (reifyName (dataConOrigTyCon dc)))
        }

reifyThing (AGlobal (AConLike (PatSynCon ps)))
  = do { let name = reifyName ps
       ; ty <- reifyPatSynType (patSynSig ps)
       ; return (TH.PatSynI name ty) }

reifyThing (ATcId {tct_id = id})
  = do  { ty1 <- zonkTcType (idType id) -- Make use of all the info we have, even
                                        -- though it may be incomplete
        ; ty2 <- reifyType ty1
        ; return (TH.VarI (reifyName id) ty2 Nothing) }

reifyThing (ATyVar tv tv1)
  = do { ty1 <- zonkTcTyVar tv1
       ; ty2 <- reifyType ty1
       ; return (TH.TyVarI (reifyName tv) ty2) }

reifyThing thing = pprPanic "reifyThing" (pprTcTyThingCategory thing)

-------------------------------------------
reifyAxBranch :: TyCon -> CoAxBranch -> TcM TH.TySynEqn
reifyAxBranch fam_tc (CoAxBranch { cab_lhs = lhs, cab_rhs = rhs })
            -- remove kind patterns (#8884)
  = do { let lhs_types_only = filterOutInvisibleTypes fam_tc lhs
       ; lhs' <- reifyTypes lhs_types_only
       ; annot_th_lhs <- zipWith3M annotThType (mkIsPolyTvs fam_tvs)
                                   lhs_types_only lhs'
       ; rhs'  <- reifyType rhs
       ; return (TH.TySynEqn annot_th_lhs rhs') }
  where
    fam_tvs = filterOutInvisibleTyVars fam_tc (tyConTyVars fam_tc)

reifyTyCon :: TyCon -> TcM TH.Info
reifyTyCon tc
  | Just cls <- tyConClass_maybe tc
  = reifyClass cls

  | isFunTyCon tc
  = return (TH.PrimTyConI (reifyName tc) 2                False)

  | isPrimTyCon tc
  = return (TH.PrimTyConI (reifyName tc) (tyConArity tc) (isUnliftedTyCon tc))

  | isTypeFamilyTyCon tc
  = do { let tvs      = tyConTyVars tc
             res_kind = tyConResKind tc
             resVar   = famTcResVar tc

       ; kind' <- reifyKind res_kind
       ; let (resultSig, injectivity) =
                 case resVar of
                   Nothing   -> (TH.KindSig kind', Nothing)
                   Just name ->
                     let thName   = reifyName name
                         injAnnot = familyTyConInjectivityInfo tc
                         sig = TH.TyVarSig (TH.KindedTV thName kind')
                         inj = case injAnnot of
                                 NotInjective -> Nothing
                                 Injective ms ->
                                     Just (TH.InjectivityAnn thName injRHS)
                                   where
                                     injRHS = map (reifyName . tyVarName)
                                                  (filterByList ms tvs)
                     in (sig, inj)
       ; tvs' <- reifyTyVars tvs (Just tc)
       ; let tfHead =
               TH.TypeFamilyHead (reifyName tc) tvs' resultSig injectivity
       ; if isOpenTypeFamilyTyCon tc
         then do { fam_envs <- tcGetFamInstEnvs
                 ; instances <- reifyFamilyInstances tc
                                  (familyInstances fam_envs tc)
                 ; return (TH.FamilyI (TH.OpenTypeFamilyD tfHead) instances) }
         else do { eqns <-
                     case isClosedSynFamilyTyConWithAxiom_maybe tc of
                       Just ax -> mapM (reifyAxBranch tc) $
                                  fromBranches $ coAxiomBranches ax
                       Nothing -> return []
                 ; return (TH.FamilyI (TH.ClosedTypeFamilyD tfHead eqns)
                      []) } }

  | isDataFamilyTyCon tc
  = do { let tvs      = tyConTyVars tc
             res_kind = tyConResKind tc

       ; kind' <- fmap Just (reifyKind res_kind)

       ; tvs' <- reifyTyVars tvs (Just tc)
       ; fam_envs <- tcGetFamInstEnvs
       ; instances <- reifyFamilyInstances tc (familyInstances fam_envs tc)
       ; return (TH.FamilyI
                       (TH.DataFamilyD (reifyName tc) tvs' kind') instances) }

  | Just (tvs, rhs) <- synTyConDefn_maybe tc  -- Vanilla type synonym
  = do { rhs' <- reifyType rhs
       ; tvs' <- reifyTyVars tvs (Just tc)
       ; return (TH.TyConI
                   (TH.TySynD (reifyName tc) tvs' rhs'))
       }

  | otherwise
  = do  { cxt <- reifyCxt (tyConStupidTheta tc)
        ; let tvs      = tyConTyVars tc
              dataCons = tyConDataCons tc
              -- see Note [Reifying GADT data constructors]
              isGadt   = any (not . null . dataConEqSpec) dataCons
        ; cons <- mapM (reifyDataCon isGadt (mkTyVarTys tvs)) dataCons
        ; r_tvs <- reifyTyVars tvs (Just tc)
        ; let name = reifyName tc
              deriv = []        -- Don't know about deriving
              decl | isNewTyCon tc =
                       TH.NewtypeD cxt name r_tvs Nothing (head cons) deriv
                   | otherwise     =
                       TH.DataD    cxt name r_tvs Nothing       cons  deriv
        ; return (TH.TyConI decl) }

reifyDataCon :: Bool -> [Type] -> DataCon -> TcM TH.Con
-- For GADTs etc, see Note [Reifying GADT data constructors]
reifyDataCon isGadtDataCon tys dc
  = do { let -- used for H98 data constructors
             (ex_tvs, theta, arg_tys)
                 = dataConInstSig dc tys
             -- used for GADTs data constructors
             (g_univ_tvs, g_ex_tvs, g_eq_spec, g_theta, g_arg_tys, g_res_ty)
                 = dataConFullSig dc
             (srcUnpks, srcStricts)
                 = mapAndUnzip reifySourceBang (dataConSrcBangs dc)
             dcdBangs  = zipWith TH.Bang srcUnpks srcStricts
             fields    = dataConFieldLabels dc
             name      = reifyName dc
             -- Universal tvs present in eq_spec need to be filtered out, as
             -- they will not appear anywhere in the type.
             eq_spec_tvs = mkVarSet (map eqSpecTyVar g_eq_spec)
             g_unsbst_univ_tvs = filterOut (`elemVarSet` eq_spec_tvs) g_univ_tvs

       ; r_arg_tys <- reifyTypes (if isGadtDataCon then g_arg_tys else arg_tys)

       ; main_con <-
           if | not (null fields) && not isGadtDataCon ->
                  return $ TH.RecC name (zip3 (map reifyFieldLabel fields)
                                         dcdBangs r_arg_tys)
              | not (null fields) -> do
                  { res_ty <- reifyType g_res_ty
                  ; return $ TH.RecGadtC [name]
                                     (zip3 (map (reifyName . flSelector) fields)
                                      dcdBangs r_arg_tys) res_ty }
                -- We need to check not isGadtDataCon here because GADT
                -- constructors can be declared infix.
                -- See Note [Infix GADT constructors] in TcTyClsDecls.
              | dataConIsInfix dc && not isGadtDataCon ->
                  ASSERT( arg_tys `lengthIs` 2 ) do
                  { let [r_a1, r_a2] = r_arg_tys
                        [s1,   s2]   = dcdBangs
                  ; return $ TH.InfixC (s1,r_a1) name (s2,r_a2) }
              | isGadtDataCon -> do
                  { res_ty <- reifyType g_res_ty
                  ; return $ TH.GadtC [name] (dcdBangs `zip` r_arg_tys) res_ty }
              | otherwise ->
                  return $ TH.NormalC name (dcdBangs `zip` r_arg_tys)

       ; let (ex_tvs', theta') | isGadtDataCon = ( g_unsbst_univ_tvs ++ g_ex_tvs
                                                 , g_theta )
                               | otherwise     = ( ex_tvs, theta )
             ret_con | null ex_tvs' && null theta' = return main_con
                     | otherwise                   = do
                         { cxt <- reifyCxt theta'
                         ; ex_tvs'' <- reifyTyVars ex_tvs' Nothing
                         ; return (TH.ForallC ex_tvs'' cxt main_con) }
       ; ASSERT( arg_tys `equalLength` dcdBangs )
         ret_con }

-- Note [Reifying GADT data constructors]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- At this point in the compilation pipeline we have no way of telling whether a
-- data type was declared as a H98 data type or as a GADT.  We have to rely on
-- heuristics here.  We look at dcEqSpec field of all data constructors in a
-- data type declaration.  If at least one data constructor has non-empty
-- dcEqSpec this means that the data type must have been declared as a GADT.
-- Consider these declarations:
--
--   data T a where
--      MkT :: forall a. (a ~ Int) => T a
--
--   data T a where
--      MkT :: T Int
--
-- First declaration will be reified as a GADT.  Second declaration will be
-- reified as a normal H98 data type declaration.

------------------------------
reifyClass :: Class -> TcM TH.Info
reifyClass cls
  = do  { cxt <- reifyCxt theta
        ; inst_envs <- tcGetInstEnvs
        ; insts <- reifyClassInstances cls (InstEnv.classInstances inst_envs cls)
        ; assocTys <- concatMapM reifyAT ats
        ; ops <- concatMapM reify_op op_stuff
        ; tvs' <- reifyTyVars tvs (Just $ classTyCon cls)
        ; let dec = TH.ClassD cxt (reifyName cls) tvs' fds' (assocTys ++ ops)
        ; return (TH.ClassI dec insts) }
  where
    (tvs, fds, theta, _, ats, op_stuff) = classExtraBigSig cls
    fds' = map reifyFunDep fds
    reify_op (op, def_meth)
      = do { ty <- reifyType (idType op)
           ; let nm' = reifyName op
           ; case def_meth of
                Just (_, GenericDM gdm_ty) ->
                  do { gdm_ty' <- reifyType gdm_ty
                     ; return [TH.SigD nm' ty, TH.DefaultSigD nm' gdm_ty'] }
                _ -> return [TH.SigD nm' ty] }

    reifyAT :: ClassATItem -> TcM [TH.Dec]
    reifyAT (ATI tycon def) = do
      tycon' <- reifyTyCon tycon
      case tycon' of
        TH.FamilyI dec _ -> do
          let (tyName, tyArgs) = tfNames dec
          (dec :) <$> maybe (return [])
                            (fmap (:[]) . reifyDefImpl tyName tyArgs . fst)
                            def
        _ -> pprPanic "reifyAT" (text (show tycon'))

    reifyDefImpl :: TH.Name -> [TH.Name] -> Type -> TcM TH.Dec
    reifyDefImpl n args ty =
      TH.TySynInstD n . TH.TySynEqn (map TH.VarT args) <$> reifyType ty

    tfNames :: TH.Dec -> (TH.Name, [TH.Name])
    tfNames (TH.OpenTypeFamilyD (TH.TypeFamilyHead n args _ _))
      = (n, map bndrName args)
    tfNames d = pprPanic "tfNames" (text (show d))

    bndrName :: TH.TyVarBndr -> TH.Name
    bndrName (TH.PlainTV n)    = n
    bndrName (TH.KindedTV n _) = n

------------------------------
-- | Annotate (with TH.SigT) a type if the first parameter is True
-- and if the type contains a free variable.
-- This is used to annotate type patterns for poly-kinded tyvars in
-- reifying class and type instances. See #8953 and th/T8953.
annotThType :: Bool   -- True <=> annotate
            -> TyCoRep.Type -> TH.Type -> TcM TH.Type
  -- tiny optimization: if the type is annotated, don't annotate again.
annotThType _    _  th_ty@(TH.SigT {}) = return th_ty
annotThType True ty th_ty
  | not $ isEmptyVarSet $ filterVarSet isTyVar $ tyCoVarsOfType ty
  = do { let ki = typeKind ty
       ; th_ki <- reifyKind ki
       ; return (TH.SigT th_ty th_ki) }
annotThType _    _ th_ty = return th_ty

-- | For every type variable in the input,
-- report whether or not the tv is poly-kinded. This is used to eventually
-- feed into 'annotThType'.
mkIsPolyTvs :: [TyVar] -> [Bool]
mkIsPolyTvs = map is_poly_tv
  where
    is_poly_tv tv = not $
                    isEmptyVarSet $
                    filterVarSet isTyVar $
                    tyCoVarsOfType $
                    tyVarKind tv

------------------------------
reifyClassInstances :: Class -> [ClsInst] -> TcM [TH.Dec]
reifyClassInstances cls insts
  = mapM (reifyClassInstance (mkIsPolyTvs tvs)) insts
  where
    tvs = filterOutInvisibleTyVars (classTyCon cls) (classTyVars cls)

reifyClassInstance :: [Bool]  -- True <=> the corresponding tv is poly-kinded
                              -- includes only *visible* tvs
                   -> ClsInst -> TcM TH.Dec
reifyClassInstance is_poly_tvs i
  = do { cxt <- reifyCxt theta
       ; let vis_types = filterOutInvisibleTypes cls_tc types
       ; thtypes <- reifyTypes vis_types
       ; annot_thtypes <- zipWith3M annotThType is_poly_tvs vis_types thtypes
       ; let head_ty = mkThAppTs (TH.ConT (reifyName cls)) annot_thtypes
       ; return $ (TH.InstanceD over cxt head_ty []) }
  where
     (_tvs, theta, cls, types) = tcSplitDFunTy (idType dfun)
     cls_tc   = classTyCon cls
     dfun     = instanceDFunId i
     over     = case overlapMode (is_flag i) of
                  NoOverlap _     -> Nothing
                  Overlappable _  -> Just TH.Overlappable
                  Overlapping _   -> Just TH.Overlapping
                  Overlaps _      -> Just TH.Overlaps
                  Incoherent _    -> Just TH.Incoherent

------------------------------
reifyFamilyInstances :: TyCon -> [FamInst] -> TcM [TH.Dec]
reifyFamilyInstances fam_tc fam_insts
  = mapM (reifyFamilyInstance (mkIsPolyTvs fam_tvs)) fam_insts
  where
    fam_tvs = filterOutInvisibleTyVars fam_tc (tyConTyVars fam_tc)

reifyFamilyInstance :: [Bool] -- True <=> the corresponding tv is poly-kinded
                              -- includes only *visible* tvs
                    -> FamInst -> TcM TH.Dec
reifyFamilyInstance is_poly_tvs inst@(FamInst { fi_flavor = flavor
                                              , fi_fam = fam
                                              , fi_tvs = fam_tvs
                                              , fi_tys = lhs
                                              , fi_rhs = rhs })
  = case flavor of
      SynFamilyInst ->
               -- remove kind patterns (#8884)
        do { let lhs_types_only = filterOutInvisibleTypes fam_tc lhs
           ; th_lhs <- reifyTypes lhs_types_only
           ; annot_th_lhs <- zipWith3M annotThType is_poly_tvs lhs_types_only
                                                   th_lhs
           ; th_rhs <- reifyType rhs
           ; return (TH.TySynInstD (reifyName fam)
                                   (TH.TySynEqn annot_th_lhs th_rhs)) }

      DataFamilyInst rep_tc ->
        do { let rep_tvs = tyConTyVars rep_tc
                 fam' = reifyName fam

                   -- eta-expand lhs types, because sometimes data/newtype
                   -- instances are eta-reduced; See Trac #9692
                   -- See Note [Eta reduction for data family axioms]
                   -- in TcInstDcls
                 (_rep_tc, rep_tc_args) = splitTyConApp rhs
                 etad_tyvars            = dropList rep_tc_args rep_tvs
                 etad_tys               = mkTyVarTys etad_tyvars
                 eta_expanded_tvs = mkTyVarTys fam_tvs `chkAppend` etad_tys
                 eta_expanded_lhs = lhs `chkAppend` etad_tys
                 dataCons         = tyConDataCons rep_tc
                 -- see Note [Reifying GADT data constructors]
                 isGadt   = any (not . null . dataConEqSpec) dataCons
           ; cons <- mapM (reifyDataCon isGadt eta_expanded_tvs) dataCons
           ; let types_only = filterOutInvisibleTypes fam_tc eta_expanded_lhs
           ; th_tys <- reifyTypes types_only
           ; annot_th_tys <- zipWith3M annotThType is_poly_tvs types_only th_tys
           ; return $
               if isNewTyCon rep_tc
               then TH.NewtypeInstD [] fam' annot_th_tys Nothing (head cons) []
               else TH.DataInstD    [] fam' annot_th_tys Nothing       cons  []
           }
  where
    fam_tc = famInstTyCon inst

------------------------------
reifyType :: TyCoRep.Type -> TcM TH.Type
-- Monadic only because of failure
reifyType ty@(ForAllTy {})  = reify_for_all ty
reifyType (LitTy t)         = do { r <- reifyTyLit t; return (TH.LitT r) }
reifyType (TyVarTy tv)      = return (TH.VarT (reifyName tv))
reifyType (TyConApp tc tys) = reify_tc_app tc tys   -- Do not expand type synonyms here
reifyType (AppTy t1 t2)     = do { [r1,r2] <- reifyTypes [t1,t2] ; return (r1 `TH.AppT` r2) }
reifyType ty@(FunTy t1 t2)
  | isPredTy t1 = reify_for_all ty  -- Types like ((?x::Int) => Char -> Char)
  | otherwise   = do { [r1,r2] <- reifyTypes [t1,t2] ; return (TH.ArrowT `TH.AppT` r1 `TH.AppT` r2) }
reifyType ty@(CastTy {})    = noTH (sLit "kind casts") (ppr ty)
reifyType ty@(CoercionTy {})= noTH (sLit "coercions in types") (ppr ty)

reify_for_all :: TyCoRep.Type -> TcM TH.Type
reify_for_all ty
  = do { cxt' <- reifyCxt cxt;
       ; tau' <- reifyType tau
       ; tvs' <- reifyTyVars tvs Nothing
       ; return (TH.ForallT tvs' cxt' tau') }
  where
    (tvs, cxt, tau) = tcSplitSigmaTy ty

reifyTyLit :: TyCoRep.TyLit -> TcM TH.TyLit
reifyTyLit (NumTyLit n) = return (TH.NumTyLit n)
reifyTyLit (StrTyLit s) = return (TH.StrTyLit (unpackFS s))

reifyTypes :: [Type] -> TcM [TH.Type]
reifyTypes = mapM reifyType

reifyPatSynType
  :: ([TyVar], ThetaType, [TyVar], ThetaType, [Type], Type) -> TcM TH.Type
-- reifies a pattern synonym's type and returns its *complete* type
-- signature; see NOTE [Pattern synonym signatures and Template
-- Haskell]
reifyPatSynType (univTyVars, req, exTyVars, prov, argTys, resTy)
  = do { univTyVars' <- reifyTyVars univTyVars Nothing
       ; req'        <- reifyCxt req
       ; exTyVars'   <- reifyTyVars exTyVars Nothing
       ; prov'       <- reifyCxt prov
       ; tau'        <- reifyType (mkFunTys argTys resTy)
       ; return $ TH.ForallT univTyVars' req'
                $ TH.ForallT exTyVars' prov' tau' }

reifyKind :: Kind -> TcM TH.Kind
reifyKind  ki
  = do { let (kis, ki') = splitFunTys ki
       ; ki'_rep <- reifyNonArrowKind ki'
       ; kis_rep <- mapM reifyKind kis
       ; return (foldr (TH.AppT . TH.AppT TH.ArrowT) ki'_rep kis_rep) }
  where
    reifyNonArrowKind k | isLiftedTypeKind k = return TH.StarT
                        | isConstraintKind k = return TH.ConstraintT
    reifyNonArrowKind (TyVarTy v)            = return (TH.VarT (reifyName v))
    reifyNonArrowKind (FunTy _ k)            = reifyKind k
    reifyNonArrowKind (ForAllTy _ k)         = reifyKind k
    reifyNonArrowKind (TyConApp kc kis)      = reify_kc_app kc kis
    reifyNonArrowKind (AppTy k1 k2)          = do { k1' <- reifyKind k1
                                                  ; k2' <- reifyKind k2
                                                  ; return (TH.AppT k1' k2')
                                                  }
    reifyNonArrowKind k                      = noTH (sLit "this kind") (ppr k)

reify_kc_app :: TyCon -> [TyCoRep.Kind] -> TcM TH.Kind
reify_kc_app kc kis
  = fmap (mkThAppTs r_kc) (mapM reifyKind vis_kis)
  where
    r_kc | isTupleTyCon kc          = TH.TupleT (tyConArity kc)
         | kc `hasKey` listTyConKey = TH.ListT
         | otherwise                = TH.ConT (reifyName kc)

    vis_kis = filterOutInvisibleTypes kc kis

reifyCxt :: [PredType] -> TcM [TH.Pred]
reifyCxt   = mapM reifyPred

reifyFunDep :: ([TyVar], [TyVar]) -> TH.FunDep
reifyFunDep (xs, ys) = TH.FunDep (map reifyName xs) (map reifyName ys)

reifyTyVars :: [TyVar]
            -> Maybe TyCon  -- the tycon if the tycovars are from a tycon.
                            -- Used to detect which tvs are implicit.
            -> TcM [TH.TyVarBndr]
reifyTyVars tvs m_tc = mapM reify_tv tvs'
  where
    tvs' = case m_tc of
             Just tc -> filterOutInvisibleTyVars tc tvs
             Nothing -> tvs

    -- even if the kind is *, we need to include a kind annotation,
    -- in case a poly-kind would be inferred without the annotation.
    -- See #8953 or test th/T8953
    reify_tv tv = TH.KindedTV name <$> reifyKind kind
      where
        kind = tyVarKind tv
        name = reifyName tv

{-
Note [Kind annotations on TyConApps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A poly-kinded tycon sometimes needs a kind annotation to be unambiguous.
For example:

   type family F a :: k
   type instance F Int  = (Proxy :: * -> *)
   type instance F Bool = (Proxy :: (* -> *) -> *)

It's hard to figure out where these annotations should appear, so we do this:
Suppose the tycon is applied to n arguments. We strip off the first n
arguments of the tycon's kind. If there are any variables left in the result
kind, we put on a kind annotation. But we must be slightly careful: it's
possible that the tycon's kind will have fewer than n arguments, in the case
that the concrete application instantiates a result kind variable with an
arrow kind. So, if we run out of arguments, we conservatively put on a kind
annotation anyway. This should be a rare case, indeed. Here is an example:

   data T1 :: k1 -> k2 -> *
   data T2 :: k1 -> k2 -> *

   type family G (a :: k) :: k
   type instance G T1 = T2

   type instance F Char = (G T1 Bool :: (* -> *) -> *)   -- F from above

Here G's kind is (forall k. k -> k), and the desugared RHS of that last
instance of F is (G (* -> (* -> *) -> *) (T1 * (* -> *)) Bool). According to
the algorithm above, there are 3 arguments to G so we should peel off 3
arguments in G's kind. But G's kind has only two arguments. This is the
rare special case, and we conservatively choose to put the annotation
in.

See #8953 and test th/T8953.
-}

reify_tc_app :: TyCon -> [Type.Type] -> TcM TH.Type
reify_tc_app tc tys
  = do { tys' <- reifyTypes (filterOutInvisibleTypes tc tys)
       ; maybe_sig_t (mkThAppTs r_tc tys') }
  where
    arity       = tyConArity tc
    tc_binders  = tyConBinders tc
    tc_res_kind = tyConResKind tc

    r_tc | isUnboxedSumTyCon tc           = TH.UnboxedSumT (arity `div` 2)
         | isUnboxedTupleTyCon tc         = TH.UnboxedTupleT (arity `div` 2)
         | isPromotedTupleTyCon tc        = TH.PromotedTupleT (arity `div` 2)
             -- See Note [Unboxed tuple RuntimeRep vars] in TyCon
         | isTupleTyCon tc                = if isPromotedDataCon tc
                                            then TH.PromotedTupleT arity
                                            else TH.TupleT arity
         | tc `hasKey` listTyConKey       = TH.ListT
         | tc `hasKey` nilDataConKey      = TH.PromotedNilT
         | tc `hasKey` consDataConKey     = TH.PromotedConsT
         | tc `hasKey` heqTyConKey        = TH.EqualityT
         | tc `hasKey` eqPrimTyConKey     = TH.EqualityT
         | tc `hasKey` eqReprPrimTyConKey = TH.ConT (reifyName coercibleTyCon)
         | isPromotedDataCon tc           = TH.PromotedT (reifyName tc)
         | otherwise                      = TH.ConT (reifyName tc)

    -- See Note [Kind annotations on TyConApps]
    maybe_sig_t th_type
      | needs_kind_sig
      = do { let full_kind = typeKind (mkTyConApp tc tys)
           ; th_full_kind <- reifyKind full_kind
           ; return (TH.SigT th_type th_full_kind) }
      | otherwise
      = return th_type

    needs_kind_sig
      | GT <- compareLength tys tc_binders
      = tcIsTyVarTy tc_res_kind
      | otherwise
      = not . isEmptyVarSet $
        filterVarSet isTyVar $
        tyCoVarsOfType $
        mkTyConKind (dropList tys tc_binders) tc_res_kind

reifyPred :: TyCoRep.PredType -> TcM TH.Pred
reifyPred ty
  -- We could reify the invisible parameter as a class but it seems
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
    pkg_str = unitIdString (moduleUnitId mod)
    mod_str = moduleNameString (moduleName mod)
    occ_str = occNameString occ
    occ     = nameOccName name
    mk_varg | OccName.isDataOcc occ = TH.mkNameG_d
            | OccName.isVarOcc  occ = TH.mkNameG_v
            | OccName.isTcOcc   occ = TH.mkNameG_tc
            | otherwise             = pprPanic "reifyName" (ppr name)

-- See Note [Reifying field labels]
reifyFieldLabel :: FieldLabel -> TH.Name
reifyFieldLabel fl
  | flIsOverloaded fl
              = TH.Name (TH.mkOccName occ_str) (TH.NameQ (TH.mkModName mod_str))
  | otherwise = TH.mkNameG_v pkg_str mod_str occ_str
  where
    name    = flSelector fl
    mod     = ASSERT( isExternalName name ) nameModule name
    pkg_str = unitIdString (moduleUnitId mod)
    mod_str = moduleNameString (moduleName mod)
    occ_str = unpackFS (flLabel fl)

reifySelector :: Id -> TyCon -> TH.Name
reifySelector id tc
  = case find ((idName id ==) . flSelector) (tyConFieldLabels tc) of
      Just fl -> reifyFieldLabel fl
      Nothing -> pprPanic "reifySelector: missing field" (ppr id $$ ppr tc)

------------------------------
reifyFixity :: Name -> TcM (Maybe TH.Fixity)
reifyFixity name
  = do { (found, fix) <- lookupFixityRn_help name
       ; return (if found then Just (conv_fix fix) else Nothing) }
    where
      conv_fix (BasicTypes.Fixity _ i d) = TH.Fixity i (conv_dir d)
      conv_dir BasicTypes.InfixR = TH.InfixR
      conv_dir BasicTypes.InfixL = TH.InfixL
      conv_dir BasicTypes.InfixN = TH.InfixN

reifyUnpackedness :: DataCon.SrcUnpackedness -> TH.SourceUnpackedness
reifyUnpackedness NoSrcUnpack = TH.NoSourceUnpackedness
reifyUnpackedness SrcNoUnpack = TH.SourceNoUnpack
reifyUnpackedness SrcUnpack   = TH.SourceUnpack

reifyStrictness :: DataCon.SrcStrictness -> TH.SourceStrictness
reifyStrictness NoSrcStrict = TH.NoSourceStrictness
reifyStrictness SrcStrict   = TH.SourceStrict
reifyStrictness SrcLazy     = TH.SourceLazy

reifySourceBang :: DataCon.HsSrcBang
                -> (TH.SourceUnpackedness, TH.SourceStrictness)
reifySourceBang (HsSrcBang _ u s) = (reifyUnpackedness u, reifyStrictness s)

reifyDecidedStrictness :: DataCon.HsImplBang -> TH.DecidedStrictness
reifyDecidedStrictness HsLazy     = TH.DecidedLazy
reifyDecidedStrictness HsStrict   = TH.DecidedStrict
reifyDecidedStrictness HsUnpack{} = TH.DecidedUnpack

------------------------------
lookupThAnnLookup :: TH.AnnLookup -> TcM CoreAnnTarget
lookupThAnnLookup (TH.AnnLookupName th_nm) = fmap NamedTarget (lookupThName th_nm)
lookupThAnnLookup (TH.AnnLookupModule (TH.Module pn mn))
  = return $ ModuleTarget $
    mkModule (stringToUnitId $ TH.pkgString pn) (mkModuleName $ TH.modString mn)

reifyAnnotations :: Data a => TH.AnnLookup -> TcM [a]
reifyAnnotations th_name
  = do { name <- lookupThAnnLookup th_name
       ; topEnv <- getTopEnv
       ; epsHptAnns <- liftIO $ prepareAnnotations topEnv Nothing
       ; tcg <- getGblEnv
       ; let selectedEpsHptAnns = findAnns deserializeWithData epsHptAnns name
       ; let selectedTcgAnns = findAnns deserializeWithData (tcg_ann_env tcg) name
       ; return (selectedEpsHptAnns ++ selectedTcgAnns) }

------------------------------
modToTHMod :: Module -> TH.Module
modToTHMod m = TH.Module (TH.PkgName $ unitIdString  $ moduleUnitId m)
                         (TH.ModName $ moduleNameString $ moduleName m)

reifyModule :: TH.Module -> TcM TH.ModuleInfo
reifyModule (TH.Module (TH.PkgName pkgString) (TH.ModName mString)) = do
  this_mod <- getModule
  let reifMod = mkModule (stringToUnitId pkgString) (mkModuleName mString)
  if (reifMod == this_mod) then reifyThisModule else reifyFromIface reifMod
    where
      reifyThisModule = do
        usages <- fmap (map modToTHMod . moduleEnvKeys . imp_mods) getImports
        return $ TH.ModuleInfo usages

      reifyFromIface reifMod = do
        iface <- loadInterfaceForModule (text "reifying module from TH for" <+> ppr reifMod) reifMod
        let usages = [modToTHMod m | usage <- mi_usages iface,
                                     Just m <- [usageToModule (moduleUnitId reifMod) usage] ]
        return $ TH.ModuleInfo usages

      usageToModule :: UnitId -> Usage -> Maybe Module
      usageToModule _ (UsageFile {}) = Nothing
      usageToModule this_pkg (UsageHomeModule { usg_mod_name = mn }) = Just $ mkModule this_pkg mn
      usageToModule _ (UsagePackageModule { usg_mod = m }) = Just m
      usageToModule _ (UsageMergedRequirement { usg_mod = m }) = Just m

------------------------------
mkThAppTs :: TH.Type -> [TH.Type] -> TH.Type
mkThAppTs fun_ty arg_tys = foldl TH.AppT fun_ty arg_tys

noTH :: LitString -> SDoc -> TcM a
noTH s d = failWithTc (hsep [text "Can't represent" <+> ptext s <+>
                                text "in Template Haskell:",
                             nest 2 d])

ppr_th :: TH.Ppr a => a -> SDoc
ppr_th x = text (TH.pprint x)

{-
Note [Reifying field labels]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When reifying a datatype declared with DuplicateRecordFields enabled, we want
the reified names of the fields to be labels rather than selector functions.
That is, we want (reify ''T) and (reify 'foo) to produce

    data T = MkT { foo :: Int }
    foo :: T -> Int

rather than

    data T = MkT { $sel:foo:MkT :: Int }
    $sel:foo:MkT :: T -> Int

because otherwise TH code that uses the field names as strings will silently do
the wrong thing.  Thus we use the field label (e.g. foo) as the OccName, rather
than the selector (e.g. $sel:foo:MkT).  Since the Orig name M.foo isn't in the
environment, NameG can't be used to represent such fields.  Instead,
reifyFieldLabel uses NameQ.

However, this means that extracting the field name from the output of reify, and
trying to reify it again, may fail with an ambiguity error if there are multiple
such fields defined in the module (see the test case
overloadedrecflds/should_fail/T11103.hs).  The "proper" fix requires changes to
the TH AST to make it able to represent duplicate record fields.
-}
