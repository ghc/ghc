{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}

module GHC.Rename.Splice (
        rnTopSpliceDecls,

        -- Typed splices
        rnTypedSplice,
        -- Untyped splices
        rnSpliceType, rnUntypedSpliceExpr, rnSplicePat, rnSpliceTyPat, rnSpliceDecl,

        -- Brackets
        rnTypedBracket, rnUntypedBracket,

        checkThLocalName, traceSplice, SpliceInfo(..),
        checkThLocalTyName,
  ) where

import GHC.Prelude

import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Hs
import GHC.Types.Name.Reader
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Driver.Env.Types

import GHC.Rename.Env
import GHC.Rename.Utils   ( newLocalBndrRn )
import GHC.Rename.Unbound ( isUnboundName )
import GHC.Rename.Module  ( rnSrcDecls, findSplice )
import GHC.Rename.Pat     ( rnPat )
import GHC.Types.Error
import GHC.Types.Basic    ( TopLevelFlag, isTopLevel, maxPrec )
import GHC.Types.SourceText ( SourceText(..) )
import GHC.Utils.Outputable
import GHC.Unit.Module
import GHC.Types.SrcLoc
import GHC.Rename.HsType ( rnLHsType )

import Control.Monad    ( unless, when )

import {-# SOURCE #-} GHC.Rename.Expr ( rnLExpr )

import GHC.Tc.Utils.Env     ( tcMetaTy )

import GHC.Driver.DynFlags
import GHC.Data.FastString
import GHC.Utils.Logger
import GHC.Utils.Panic
import GHC.Driver.Hooks
import GHC.Builtin.Names.TH ( decsQTyConName, expQTyConName, liftName
                            , patQTyConName, quoteDecName, quoteExpName
                            , quotePatName, quoteTypeName, typeQTyConName)

import {-# SOURCE #-} GHC.Tc.Gen.Expr   ( tcCheckPolyExpr )
import {-# SOURCE #-} GHC.Tc.Gen.Splice
    ( runMetaD
    , runMetaE
    , runMetaP
    , runMetaT
    , tcTopSpliceExpr
    )

import GHC.Tc.Zonk.Type

import GHCi.RemoteTypes ( ForeignRef )
import qualified GHC.Internal.TH.Syntax as TH (Q)

import qualified GHC.LanguageExtensions as LangExt
import qualified Data.Set as Set
import GHC.Stack

{-
************************************************************************
*                                                                      *
        Template Haskell brackets
*                                                                      *
************************************************************************
-}

-- Check that -XTemplateHaskellQuotes is enabled and available
checkForTemplateHaskellQuotes :: HsExpr GhcPs -> RnM ()
checkForTemplateHaskellQuotes e =
  unlessXOptM LangExt.TemplateHaskellQuotes $
    failWith $ thSyntaxError $ IllegalTHQuotes e

{-

Note [Untyped quotes in typed splices and vice versa]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this typed splice
   $$(f [| x |])

Is there anything wrong with that /typed/ splice containing an /untyped/
quote [| x |]?   One could ask the same about an /untyped/ slice containing a
/typed/ quote.

In fact, both are fine (#24190). Presumably f's type looks something like:
   f :: Q Expr -> Code Q Int

It is pretty hard for `f` to use its (untyped code) argument to build a typed
syntax tree, but not impossible:
* `f` could use `unsafeCodeCoerce :: Q Exp -> Code Q a`
* `f` could just perform case analysis on the tree

But in the end all that matters is that in $$( e ), the expression `e` has the
right type.  It doesn't matter how `e` is built.  To put it another way, the
untyped quote `[| x |]` could also be written `varE 'x`, which is an ordinary
expression.

Moreover the ticked variable, 'x :: Name, is itself treated as an untyped quote;
but it is a perfectly fine sub-expression to have in a typed splice.

(Historical note: GHC used to unnecessarily  check that a typed quote only
occurred in a typed splice: #24190.)

-}

rnTypedBracket :: HsExpr GhcPs -> LHsExpr GhcPs -> RnM (HsExpr GhcRn, FreeVars)
rnTypedBracket e br_body
  = addErrCtxt (TypedTHBracketCtxt br_body) $
    do { checkForTemplateHaskellQuotes e

         -- Check for nested brackets
       ; cur_stage <- getStage
       ; case cur_stage of
           { Splice _       -> return ()
               -- See Note [Untyped quotes in typed splices and vice versa]
           ; RunSplice _    ->
               -- See Note [RunSplice ThLevel] in GHC.Tc.Types.
               pprPanic "rnTypedBracket: Renaming typed bracket when running a splice"
                        (ppr e)
           ; Comp           -> return ()
           ; Brack {}       -> failWithTc $ thSyntaxError
                                          $ NestedTHBrackets
           }

         -- Brackets are desugared to code that mentions the TH package
       ; recordThUse

       ; traceRn "Renaming typed TH bracket" empty
       ; (body', fvs_e) <- setStage (Brack cur_stage RnPendingTyped) $ rnLExpr br_body

       ; return (HsTypedBracket noExtField body', fvs_e)

       }

rnUntypedBracket :: HsExpr GhcPs -> HsQuote GhcPs -> RnM (HsExpr GhcRn, FreeVars)
rnUntypedBracket e br_body
  = addErrCtxt (UntypedTHBracketCtxt br_body) $
    do { checkForTemplateHaskellQuotes e

         -- Check for nested brackets
       ; cur_stage <- getStage
       ; case cur_stage of
           { Splice _       -> return ()
               -- See Note [Untyped quotes in typed splices and vice versa]
           ; RunSplice _    ->
               -- See Note [RunSplice ThLevel] in GHC.Tc.Types.
               pprPanic "rnUntypedBracket: Renaming untyped bracket when running a splice"
                        (ppr e)
           ; Comp           -> return ()
           ; Brack {}       -> failWithTc $ thSyntaxError
                                          $ NestedTHBrackets
           }

         -- Brackets are desugared to code that mentions the TH package
       ; recordThUse

       ; traceRn "Renaming untyped TH bracket" empty
       ; ps_var <- newMutVar []
       ; (body', fvs_e) <-
         -- See Note [Rebindable syntax and Template Haskell]
         unsetXOptM LangExt.RebindableSyntax $
         setStage (Brack cur_stage (RnPendingUntyped ps_var)) $
                  rn_utbracket br_body
       ; pendings <- readMutVar ps_var
       ; return (HsUntypedBracket pendings body', fvs_e)

       }

rn_utbracket :: HsQuote GhcPs -> RnM (HsQuote GhcRn, FreeVars)
rn_utbracket (VarBr _ flg rdr_name)
  = do { name <- lookupOccRn (unLoc rdr_name)
       ; checkThLocalName name
       ; check_namespace flg name
       ; this_mod <- getModule
       ; dflags <- getDynFlags
       ; env <- getGlobalRdrEnv

       ; when (flg && nameIsLocalOrFrom this_mod name) $
             -- Type variables can be quoted in TH. See #5721.
                 do { mb_bind_lvl <- getStageAndBindLevel name
                    ; case mb_bind_lvl of
                        { Nothing -> return ()      -- Can happen for data constructors,
                                                    -- but nothing needs to be done for them

                        ; Just (top_lvl, bind_lvl, use_lvl)  -- See Note [Quoting names]
                             | isUnboundName name
                             -> return ()
                             | isTopLevel top_lvl
                             , xopt LangExt.PathCrossStagedPersistence dflags
                             -> when (isExternalName name) (keepAlive name)
                             | otherwise
                             -> do { traceRn "rn_utbracket VarBr"
                                      (ppr name <+> ppr bind_lvl
                                                <+> ppr use_lvl)
                                    ; let mgre = lookupGRE_Name env name
                                    ; checkTc (any (thLevel use_lvl ==) (Set.toList bind_lvl))
                                              (TcRnBadlyStaged (StageCheckSplice name mgre) bind_lvl (thLevel use_lvl))
                                    ; when (isExternalName name) (keepAlive name) }
                        }
                    }
       ; return (VarBr noExtField flg (noLocA name), unitFV name) }

rn_utbracket (ExpBr _ e) = do { (e', fvs) <- rnLExpr e
                                ; return (ExpBr noExtField e', fvs) }

rn_utbracket (PatBr _ p)
  = rnPat ThPatQuote p $ \ p' -> return (PatBr noExtField p', emptyFVs)

rn_utbracket (TypBr _ t) = do { (t', fvs) <- rnLHsType TypBrCtx t
                                ; return (TypBr noExtField t', fvs) }

rn_utbracket (DecBrL _ decls)
  = do { group <- groupDecls decls
       ; gbl_env  <- getGblEnv
       ; let new_gbl_env = gbl_env { tcg_dus = emptyDUs }
                          -- The emptyDUs is so that we just collect uses for this
                          -- group alone in the call to rnSrcDecls below
       ; (tcg_env, group') <- setGblEnv new_gbl_env $
                              rnSrcDecls group

              -- Discard the tcg_env; it contains only extra info about fixity
        ; traceRn "rn_utbracket dec" (ppr (tcg_dus tcg_env) $$
                   ppr (duUses (tcg_dus tcg_env)))
        ; return (DecBrG noExtField group', duUses (tcg_dus tcg_env)) }
  where
    groupDecls :: [LHsDecl GhcPs] -> RnM (HsGroup GhcPs)
    groupDecls decls
      = do { (group, mb_splice) <- findSplice decls
           ; case mb_splice of
           { Nothing -> return group
           ; Just (splice, rest) ->
               do { group' <- groupDecls rest
                  ; let group'' = appendGroups group group'
                  ; return group'' { hs_splcds = noLocA splice : hs_splcds group' }
                  }
           }}

rn_utbracket (DecBrG {}) = panic "rn_ut_bracket: unexpected DecBrG"


-- | Ensure that we are not using a term-level name in a type-level namespace
-- or vice-versa. Throws a 'TcRnIncorrectNameSpace' error if there is a problem.
check_namespace :: Bool -> Name -> RnM ()
check_namespace is_single_tick nm
  = unless (isValNameSpace ns == is_single_tick) $
      failWithTc $ (TcRnIncorrectNameSpace nm True)
  where
    ns = nameNameSpace nm

{-
*********************************************************
*                                                      *
                Splices
*                                                      *
*********************************************************

Note [Free variables of typed splices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider renaming this:
        f = ...
        h = ...$(thing "f")...

where the splice is a *typed* splice.  The splice can expand into
literally anything, so when we do dependency analysis we must assume
that it might mention 'f'.  So we simply treat all locally-defined
names as mentioned by any splice.  This is terribly brutal, but I
don't see what else to do.  For example, it'll mean that every
locally-defined thing will appear to be used, so no unused-binding
warnings.  But if we miss the dependency, then we might typecheck 'h'
before 'f', and that will crash the type checker because 'f' isn't in
scope.

Currently, I'm not treating a splice as also mentioning every import,
which is a bit inconsistent -- but there are a lot of them.  We might
thereby get some bogus unused-import warnings, but we won't crash the
type checker.  Not very satisfactory really.

Note [Renamer errors]
~~~~~~~~~~~~~~~~~~~~~
It's important to wrap renamer calls in checkNoErrs, because the
renamer does not fail for out of scope variables etc. Instead it
returns a bogus term/type, so that it can report more than one error.
We don't want the type checker to see these bogus unbound variables.
-}

rnUntypedSpliceGen :: (HsUntypedSplice GhcRn -> RnM (a, FreeVars))
                                                    -- Outside brackets, run splice
                   -> (Name -> HsUntypedSplice GhcRn -> (PendingRnSplice, a))
                                                   -- Inside brackets, make it pending
                   -> HsUntypedSplice GhcPs
                   -> RnM (a, FreeVars)
rnUntypedSpliceGen run_splice pend_splice splice
  = addErrCtxt (UntypedSpliceCtxt splice) $ do
    { stage <- getStage
    ; case stage of
        Brack _ RnPendingTyped
          -> failWithTc $ thSyntaxError
                        $ MismatchedSpliceType Untyped IsSplice

        Brack pop_stage (RnPendingUntyped ps_var)
          -> do { (splice', fvs) <- setStage pop_stage $
                                    rnUntypedSplice splice
                ; loc  <- getSrcSpanM
                ; splice_name <- newLocalBndrRn (L (noAnnSrcSpan loc) unqualSplice)
                ; let (pending_splice, result) = pend_splice splice_name splice'
                ; ps <- readMutVar ps_var
                ; writeMutVar ps_var (pending_splice : ps)
                ; return (result, fvs) }

        _ ->  do { checkTopSpliceAllowed splice
                 ; (splice', fvs1) <- checkNoErrs $
                                      setStage (Splice Untyped) $
                                      rnUntypedSplice splice
                   -- checkNoErrs: don't attempt to run the splice if
                   -- renaming it failed; otherwise we get a cascade of
                   -- errors from e.g. unbound variables
                 ; (result, fvs2) <- run_splice splice'
                 ; return (result, fvs1 `plusFV` fvs2) } }


-- Nested splices are fine without TemplateHaskell because they
-- are not executed until the top-level splice is run.
checkTopSpliceAllowed :: HsUntypedSplice GhcPs -> RnM ()
checkTopSpliceAllowed splice = do
  let (ext, err) = spliceExtension splice
  unlessXOptM ext $ failWith err
  where
    spliceExtension :: HsUntypedSplice GhcPs -> (LangExt.Extension, TcRnMessage)
    spliceExtension (HsQuasiQuote {}) =
      (LangExt.QuasiQuotes, TcRnIllegalQuasiQuotes)
    spliceExtension (HsUntypedSpliceExpr {}) =
      (LangExt.TemplateHaskell, thSyntaxError $ IllegalTHSplice)

------------------

-- | Returns the result of running a splice and the modFinalizers collected
-- during the execution.
--
-- See Note [Delaying modFinalizers in untyped splices].
runRnSplice :: UntypedSpliceFlavour
            -> (LHsExpr GhcTc -> TcRn res)
            -> (res -> SDoc)    -- How to pretty-print res
                                -- Usually just ppr, but not for [Decl]
            -> HsUntypedSplice GhcRn
            -> TcRn (res, [ForeignRef (TH.Q ())])
runRnSplice flavour run_meta ppr_res splice
  = do { hooks <- hsc_hooks <$> getTopEnv
       ; splice' <- case runRnSpliceHook hooks of
            Nothing -> return splice
            Just h  -> h splice

       ; let the_expr = case splice' of
                HsUntypedSpliceExpr _ e ->  e
                HsQuasiQuote _ q str -> mkQuasiQuoteExpr flavour q str

             -- Typecheck the expression
       ; meta_exp_ty   <- tcMetaTy meta_ty_name
       ; zonked_q_expr <- zonkTopLExpr =<<
                            tcTopSpliceExpr Untyped
                              (tcCheckPolyExpr the_expr meta_exp_ty)

             -- Run the expression
       ; mod_finalizers_ref <- newTcRef []
       ; result <- setStage (RunSplice mod_finalizers_ref) $
                     run_meta zonked_q_expr
       ; mod_finalizers <- readTcRef mod_finalizers_ref
       ; traceSplice (SpliceInfo { spliceDescription = what
                                 , spliceIsDecl      = is_decl
                                 , spliceSource      = Just the_expr
                                 , spliceGenerated   = ppr_res result })

       ; return (result, mod_finalizers) }

  where
    meta_ty_name = case flavour of
                       UntypedExpSplice  -> expQTyConName
                       UntypedPatSplice  -> patQTyConName
                       UntypedTypeSplice -> typeQTyConName
                       UntypedDeclSplice -> decsQTyConName
    what = case flavour of
                  UntypedExpSplice  -> "expression"
                  UntypedPatSplice  -> "pattern"
                  UntypedTypeSplice -> "type"
                  UntypedDeclSplice -> "declarations"
    is_decl = case flavour of
                 UntypedDeclSplice -> True
                 _                 -> False

------------------
makePending :: UntypedSpliceFlavour
            -> Name
            -> HsUntypedSplice GhcRn
            -> PendingRnSplice
makePending flavour n (HsUntypedSpliceExpr _ e)
  = PendingRnSplice flavour n e
makePending flavour n (HsQuasiQuote _ quoter quote)
  = PendingRnSplice flavour n (mkQuasiQuoteExpr flavour quoter quote)

------------------
mkQuasiQuoteExpr :: UntypedSpliceFlavour -> Name
                 -> XRec GhcPs FastString
                 -> LHsExpr GhcRn
-- Return the expression (quoter "...quote...")
-- which is what we must run in a quasi-quote
mkQuasiQuoteExpr flavour quoter (L q_span' quote)
  = L q_span $ HsApp noExtField (L q_span
             $ HsApp noExtField (L q_span
                    (HsVar noExtField (L (l2l q_span) quote_selector)))
                                quoterExpr)
                    quoteExpr
  where
    q_span = noAnnSrcSpan (locA q_span')
    quoterExpr = L q_span $! HsVar noExtField $! (L (l2l q_span) quoter)
    quoteExpr  = L q_span $! HsLit noExtField $! HsString NoSourceText quote
    quote_selector = case flavour of
                       UntypedExpSplice  -> quoteExpName
                       UntypedPatSplice  -> quotePatName
                       UntypedTypeSplice -> quoteTypeName
                       UntypedDeclSplice -> quoteDecName

---------------------
unqualSplice :: RdrName
-- The RdrName for a SplicePointName.  See GHC.Hs.Expr
-- Note [Lifecycle of an untyped splice, and PendingRnSplice]
-- We use "spn" (which is arbitrary) because it is brief but grepable-for.
unqualSplice = mkRdrUnqual (mkVarOccFS (fsLit "spn"))

rnUntypedSplice :: HsUntypedSplice GhcPs -> RnM (HsUntypedSplice GhcRn, FreeVars)
-- Not exported...used for all
rnUntypedSplice (HsUntypedSpliceExpr annCo expr)
  = do  { (expr', fvs) <- rnLExpr expr
        ; return (HsUntypedSpliceExpr annCo expr', fvs) }

rnUntypedSplice (HsQuasiQuote ext quoter quote)
  = do  { -- Rename the quoter; akin to the HsVar case of rnExpr
        ; quoter' <- lookupOccRn quoter
        ; this_mod <- getModule
        ; when (nameIsLocalOrFrom this_mod quoter') $
          checkThLocalName quoter'

        ; return (HsQuasiQuote ext quoter' quote, unitFV quoter') }

---------------------
rnTypedSplice :: LHsExpr GhcPs -- Typed splice expression
              -> RnM (HsExpr GhcRn, FreeVars)
rnTypedSplice expr
  = addErrCtxt (TypedSpliceCtxt Nothing expr) $ do
    { stage <- getStage
    ; case stage of
        Brack pop_stage RnPendingTyped
          -> setStage pop_stage rn_splice

        Brack _ (RnPendingUntyped _)
          -> failWithTc $ thSyntaxError $ MismatchedSpliceType Typed IsSplice

        _ -> do { unlessXOptM LangExt.TemplateHaskell
                    (failWith $ thSyntaxError IllegalTHSplice)

                ; (result, fvs1) <- checkNoErrs $ setStage (Splice Typed) rn_splice
                  -- checkNoErrs: don't attempt to run the splice if
                  -- renaming it failed; otherwise we get a cascade of
                  -- errors from e.g. unbound variables

                  -- Run typed splice later, in the type checker
                  -- Ugh!  See Note [Free variables of typed splices] above
                ; traceRn "rnTypedSplice: typed expression splice" empty
                ; lcl_rdr <- getLocalRdrEnv
                ; gbl_rdr <- getGlobalRdrEnv
                ; let gbl_names = mkNameSet [ greName gre
                                            | gre <- globalRdrEnvElts gbl_rdr
                                            , isLocalGRE gre]
                      lcl_names = mkNameSet (localRdrEnvElts lcl_rdr)
                      fvs2      = lcl_names `plusFV` gbl_names

                ; return (result, fvs1 `plusFV` fvs2) } }
  where
    rn_splice :: RnM (HsExpr GhcRn, FreeVars)
    rn_splice =
      do { loc <- getSrcSpanM
         -- The renamer allocates a splice-point name to every typed splice
         -- (incl the top level ones for which it will not ultimately be used)
         ; n' <- newLocalBndrRn (L (noAnnSrcSpan loc) unqualSplice)
         ; (expr', fvs) <- rnLExpr expr
         ; return (HsTypedSplice n' expr', fvs) }

rnUntypedSpliceExpr :: HsUntypedSplice GhcPs -> RnM (HsExpr GhcRn, FreeVars)
rnUntypedSpliceExpr splice
  = rnUntypedSpliceGen run_expr_splice pend_expr_splice splice
  where
    pend_expr_splice :: Name -> HsUntypedSplice GhcRn -> (PendingRnSplice, HsExpr GhcRn)
    pend_expr_splice name rn_splice
        = (makePending UntypedExpSplice name rn_splice, HsUntypedSplice (HsUntypedSpliceNested name) rn_splice)

    run_expr_splice rn_splice
      = do { traceRn "rnUntypedSpliceExpr: untyped expression splice" empty

           -- Run the splice here, see Note [Running splices in the Renamer]
           ; (expr_ps, mod_finalizers)
                <- runRnSplice UntypedExpSplice runMetaE ppr rn_splice
                -- mod_finalizers: See Note [Delaying modFinalizers in untyped splices].

           -- Rename the expanded expression
           ; (L l expr_rn, fvs) <- checkNoErrs (rnLExpr expr_ps)

           -- rn_splice :: HsUntypedSplice GhcRn is the original TH expression,
           --                                       before expansion
           -- expr_ps   :: LHsExpr GhcPs is the result of running the splice
           -- expr_rn   :: HsExpr GhcRn is the result of renaming ps_expr
           ; let res :: HsUntypedSpliceResult (HsExpr GhcRn)
                 res  = HsUntypedSpliceTop
                          { utsplice_result_finalizers = ThModFinalizers mod_finalizers
                          , utsplice_result            = expr_rn }
           ; return (gHsPar (L l (HsUntypedSplice res rn_splice)), fvs)
           }

thSyntaxError :: THSyntaxError -> TcRnMessage
thSyntaxError err = TcRnTHError $ THSyntaxError err

{- Note [Running splices in the Renamer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Splices used to be run in the typechecker, which led to (#4364). Since the
renamer must decide which expressions depend on which others, and it cannot
reliably do this for arbitrary splices, we used to conservatively say that
splices depend on all other expressions in scope. Unfortunately, this led to
the problem of cyclic type declarations seen in (#4364). Instead, by
running splices in the renamer, we side-step the problem of determining
dependencies: by the time the dependency analysis happens, any splices have
already been run, and expression dependencies can be determined as usual.

However, see (#9813), for an example where we would like to run splices
*after* performing dependency analysis (that is, after renaming). It would be
desirable to typecheck "non-splicy" expressions (those expressions that do not
contain splices directly or via dependence on an expression that does) before
"splicy" expressions, such that types/expressions within the same declaration
group would be available to `reify` calls, for example consider the following:

> module M where
>   data D = C
>   f = 1
>   g = $(mapM reify ['f, 'D, ''C] ...)

Compilation of this example fails since D/C/f are not in the type environment
and thus cannot be reified as they have not been typechecked by the time the
splice is renamed and thus run.

These requirements are at odds: we do not want to run splices in the renamer as
we wish to first determine dependencies and typecheck certain expressions,
making them available to reify, but cannot accurately determine dependencies
without running splices in the renamer!

Indeed, the conclusion of (#9813) was that it is not worth the complexity
to try and
 a) implement and maintain the code for renaming/typechecking non-splicy
    expressions before splicy expressions,
 b) explain to TH users which expressions are/not available to reify at any
    given point.

-}

{- Note [Rebindable syntax and Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When processing Template Haskell quotes with Rebindable Syntax (RS) enabled,
there are two possibilities: apply the RS rules to the quotes or don't.

One might expect that with {-# LANGUAGE RebindableSyntax #-} at the top of a
module, any 'if' expression would end up being turned into a call to whatever
'ifThenElse' function is in scope, regardless of whether the said if expression
appears in "normal" Haskell code or in a TH quote. This however comes with its
problems. Consider the following code:

  {-# LANGUAGE TemplateHaskell, RebindableSyntax #-}

  module X where

  import Prelude ( Monad(..), Bool(..), print, ($) )
  import Language.Haskell.TH.Syntax

  $( do stuff <- [| if True then 10 else 15 |]
        runIO $ print stuff
        return [] )

If we apply the RS rules, then GHC would complain about not having suitable
fromInteger/ifThenElse functions in scope. But this quote is just a bit of
Haskell syntax that has yet to be used, or, to put it differently, placed
(spliced) in some context where the said functions might be available. More
generally, untyped TH quotes are meant to work with yet-unbound identifiers.
This tends to show that untyped TH and Rebindable Syntax overall don't play
well together. Users still have the option to splice "normal" if expressions
into modules where RS is enabled, to turn them into applications of
an 'ifThenElse' function of their choice.

Typed TH (TTH) quotes, on the other hand, come with different constraints. They
don't quite have this "delayed" nature: we typecheck them while processing
them, and TTH users expect RS to Just Work in their quotes, exactly like it does
outside of the quotes. There, we do not have to accept unbound identifiers and
we can apply the RS rules both in the typechecking and desugaring of the quotes
without triggering surprising/bad behaviour for users. For instance, the
following code is expected to be rejected (because of the lack of suitable
'fromInteger'/'ifThenElse' functions in scope):

  {-# LANGUAGE TemplateHaskell, RebindableSyntax #-}

  module X where

  import Prelude ( Monad(..), Bool(..), print, ($) )
  import Language.Haskell.TH.Syntax

  $$( do stuff <- [|| if True then 10 else 15 ||]
         runIO $ print stuff
         return [] )

The conclusion is that even if RS is enabled for a given module, GHC disables it
when processing untyped TH quotes from that module, to avoid the aforementioned
problems, but keeps it on while processing typed TH quotes.

This note and approach originated in #18102.

-}

{- Note [Delaying modFinalizers in untyped splices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When splices run in the renamer, 'reify' does not have access to the local
type environment (#11832, [1]).

For instance, in

> let x = e in $(reify (mkName "x") >>= runIO . print >> [| return () |])

'reify' cannot find @x@, because the local type environment is not yet
populated. To address this, we allow 'reify' execution to be deferred with
'addModFinalizer'.

> let x = e in $(do addModFinalizer (reify (mkName "x") >>= runIO . print)
                    [| return () |]
                )

The finalizer is run with the local type environment when type checking is
complete.

Since the local type environment is not available in the renamer, we annotate
the tree at the splice point [2] with @HsSpliceE (HsSpliced finalizers e)@ where
@e@ is the result of splicing and @finalizers@ are the finalizers that have been
collected during evaluation of the splice [3]. In our example,

> HsLet
>   (x = e)
>   (HsSpliceE $ HsSpliced [reify (mkName "x") >>= runIO . print]
>                          (HsSplicedExpr $ return ())
>   )

When the typechecker finds the annotation, it inserts the finalizers in the
global environment and exposes the current local environment to them [4, 5, 6].

> addModFinalizersWithLclEnv [reify (mkName "x") >>= runIO . print]

References:

[1] https://gitlab.haskell.org/ghc/ghc/wikis/template-haskell/reify
[2] 'rnSpliceExpr'
[3] 'GHC.Tc.Gen.Splice.qAddModFinalizer'
[4] 'GHC.Tc.Gen.Expr.tcExpr' ('HsSpliceE' ('HsSpliced' ...))
[5] 'GHC.Tc.Gen.HsType.tcHsType' ('HsSpliceTy' ('HsSpliced' ...))
[6] 'GHC.Tc.Gen.Pat.tc_pat' ('SplicePat' ('HsSpliced' ...))

-}

----------------------
rnSpliceType :: HsUntypedSplice GhcPs -> RnM (HsType GhcRn, FreeVars)
rnSpliceType splice
  = rnUntypedSpliceGen run_type_splice pend_type_splice splice
  where
    pend_type_splice name rn_splice
       = ( makePending UntypedTypeSplice name rn_splice
         , HsSpliceTy (HsUntypedSpliceNested name) rn_splice)

    run_type_splice :: HsUntypedSplice GhcRn -> RnM (HsType GhcRn, FreeVars)
    run_type_splice rn_splice
      = do { traceRn "rnSpliceType: untyped type splice" empty
           ; (hs_ty2, mod_finalizers) <-
                runRnSplice UntypedTypeSplice runMetaT ppr rn_splice
           ; (hs_ty3, fvs) <- do { let doc = SpliceTypeCtx hs_ty2
                                 ; checkNoErrs $ rnLHsType doc hs_ty2 }
                                         -- checkNoErrs: see Note [Renamer errors]

             -- See Note [Delaying modFinalizers in untyped splices].
           ; return ( HsSpliceTy (HsUntypedSpliceTop (ThModFinalizers mod_finalizers)
                                                     (mb_paren hs_ty3))
                                 rn_splice
                    , fvs
                    ) }
              -- Wrap the result of the splice in parens so that we don't
              -- lose the outermost location set by runQuasiQuote (#7918)

    -- Wrap a non-atomic result in HsParTy parens;
    -- but not if it's atomic to avoid double parens for operators
    -- This is to account for, say  foo :: $(blah) -> Int
    -- when we want $(blah) to expand to (this -> that), with parens.
    -- Sadly, it's awkward add precisely the correct parens, because
    -- that depends on the context.
    mb_paren :: LHsType GhcRn -> LHsType GhcRn
    mb_paren lhs_ty@(L loc hs_ty)
      | hsTypeNeedsParens maxPrec hs_ty = L loc (HsParTy noAnn lhs_ty)
      | otherwise                       = lhs_ty

{- Note [Partial Type Splices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Partial Type Signatures are partially supported in TH type splices: only
anonymous wild cards are allowed.

  -- ToDo: SLPJ says: I don't understand all this

Normally, named wild cards are collected before renaming a (partial) type
signature. However, TH type splices are run during renaming, i.e. after the
initial traversal, leading to out of scope errors for named wild cards. We
can't just extend the initial traversal to collect the named wild cards in TH
type splices, as we'd need to expand them, which is supposed to happen only
once, during renaming.

Similarly, the extra-constraints wild card is handled right before renaming
too, and is therefore also not supported in a TH type splice. Another reason
to forbid extra-constraints wild cards in TH type splices is that a single
signature can contain many TH type splices, whereas it mustn't contain more
than one extra-constraints wild card. Enforcing would this be hard the way
things are currently organised.

Anonymous wild cards pose no problem, because they start out without names and
are given names during renaming. These names are collected right after
renaming. The names generated for anonymous wild cards in TH type splices will
thus be collected as well.

For more details about renaming wild cards, see GHC.Rename.HsType.rnHsSigWcType

Note that partial type signatures are fully supported in TH declaration
splices, e.g.:

     [d| foo :: _ => _
         foo x y = x == y |]

This is because in this case, the partial type signature can be treated as a
whole signature, instead of as an arbitrary type.

-}


----------------------
-- | Rename a splice pattern. See Note [rnSplicePat]
rnSplicePat :: HsUntypedSplice GhcPs -> RnM ( (HsUntypedSplice GhcRn, HsUntypedSpliceResult (LPat GhcPs))
                                            , FreeVars)
rnSplicePat splice
  = rnUntypedSpliceGen run_pat_splice pend_pat_splice splice
  where
    pend_pat_splice name rn_splice
      = (makePending UntypedPatSplice name rn_splice
        , (rn_splice, HsUntypedSpliceNested name)) -- Pat splice is nested and thus simply renamed

    run_pat_splice rn_splice
      = do { traceRn "rnSplicePat: untyped pattern splice" empty
           ; (pat, mod_finalizers) <-
                runRnSplice UntypedPatSplice runMetaP ppr rn_splice
             -- See Note [Delaying modFinalizers in untyped splices].
           ; let p = HsUntypedSpliceTop (ThModFinalizers mod_finalizers) pat
           ; return ((rn_splice, p), emptyFVs) }
              -- Wrap the result of the quasi-quoter in parens so that we don't
              -- lose the outermost location set by runQuasiQuote (#7918)

-- | Rename a splice type pattern. Much the same as `rnSplicePat`, but works with LHsType instead of LPat
rnSpliceTyPat :: HsUntypedSplice GhcPs -> RnM ( (HsUntypedSplice GhcRn, HsUntypedSpliceResult (LHsType GhcPs))
                                            , FreeVars)
rnSpliceTyPat splice
  = rnUntypedSpliceGen run_ty_pat_splice pend_ty_pat_splice splice
  where
    pend_ty_pat_splice name rn_splice
      = (makePending UntypedTypeSplice name rn_splice
        , (rn_splice, HsUntypedSpliceNested name)) -- HsType splice is nested and thus simply renamed

    run_ty_pat_splice rn_splice
      = do { traceRn "rnSpliceTyPat: untyped pattern splice" empty
           ; (ty, mod_finalizers) <-
                runRnSplice UntypedTypeSplice runMetaT ppr rn_splice
             -- See Note [Delaying modFinalizers in untyped splices].
           ; let t = HsUntypedSpliceTop (ThModFinalizers mod_finalizers) ty
           ; return ((rn_splice, t), emptyFVs) }
              -- Wrap the result of the quasi-quoter in parens so that we don't
              -- lose the outermost location set by runQuasiQuote (#7918)

----------------------
rnSpliceDecl :: SpliceDecl GhcPs -> RnM (SpliceDecl GhcRn, FreeVars)
rnSpliceDecl (SpliceDecl _ (L loc splice) flg)
  = rnUntypedSpliceGen run_decl_splice pend_decl_splice splice
  where
    pend_decl_splice name rn_splice
       = ( makePending UntypedDeclSplice name rn_splice
         , SpliceDecl noExtField (L loc rn_splice) flg)

    run_decl_splice rn_splice  = pprPanic "rnSpliceDecl" (pprUntypedSplice True Nothing rn_splice)

rnTopSpliceDecls :: HsUntypedSplice GhcPs -> RnM ([LHsDecl GhcPs], FreeVars)
-- Declaration splice at the very top level of the module
rnTopSpliceDecls splice
   =  do { checkTopSpliceAllowed splice
         ; (rn_splice, fvs) <- checkNoErrs $
                               setStage (Splice Untyped) $
                               rnUntypedSplice splice
           -- As always, be sure to checkNoErrs above lest we end up with
           -- holes making it to typechecking, hence #12584.
           --
           -- Note that we cannot call checkNoErrs for the whole duration
           -- of rnTopSpliceDecls. The reason is that checkNoErrs changes
           -- the local environment to temporarily contain a new
           -- reference to store errors, and add_mod_finalizers would
           -- cause this reference to be stored after checkNoErrs finishes.
           -- This is checked by test TH_finalizer.
         ; traceRn "rnTopSpliceDecls: untyped declaration splice" empty
         ; (decls, mod_finalizers) <- checkNoErrs $
               runRnSplice UntypedDeclSplice runMetaD ppr_decls rn_splice
         ; add_mod_finalizers_now mod_finalizers
         ; return (decls,fvs) }
   where
     ppr_decls :: [LHsDecl GhcPs] -> SDoc
     ppr_decls ds = vcat (map ppr ds)

     -- Adds finalizers to the global environment instead of delaying them
     -- to the type checker.
     --
     -- Declaration splices do not have an interesting local environment so
     -- there is no point in delaying them.
     --
     -- See Note [Delaying modFinalizers in untyped splices].
     add_mod_finalizers_now :: [ForeignRef (TH.Q ())] -> TcRn ()
     add_mod_finalizers_now []             = return ()
     add_mod_finalizers_now mod_finalizers = do
       th_modfinalizers_var <- fmap tcg_th_modfinalizers getGblEnv
       env <- getLclEnv
       updTcRef th_modfinalizers_var $ \fins ->
         (env, ThModFinalizers mod_finalizers) : fins


{-
Note [rnSplicePat]
~~~~~~~~~~~~~~~~~~
Renaming a pattern splice is a bit tricky, because we need the variables
bound in the pattern to be in scope in the RHS of the pattern. This scope
management is effectively done by using continuation-passing style in
GHC.Rename.Pat, through the CpsRn monad. We don't wish to be in that monad here
(it would create import cycles and generally conflict with renaming other
splices), so we really want to return a (Pat GhcPs) -- the result of
running the splice -- which can then be further renamed in GHC.Rename.Pat, in
the CpsRn monad.

The problem is that if we're renaming a splice within a bracket, we
*don't* want to run the splice now. We really do just want to rename
it to an HsUntypedSplice Name. Of course, then we can't know what variables
are bound within the splice. So we accept any unbound variables and
rename them again when the bracket is spliced in.  If a variable is brought
into scope by a pattern splice all is fine.  If it is not then an error is
reported.

In any case, when we're done in rnSplicePat, we'll have both the renamed
splice, and either a Pat RdrName and ThModFinalizers (the result of running a
top-level splice) or a splice point name. Thus, rnSplicePat returns both
HsUntypedSplice GhcRn, and HsUntypedSpliceResult (Pat GhcPs) -- which models
the existence of either the result of running the splice (HsUntypedSpliceTop),
or its splice point name if nested (HsUntypedSpliceNested)
-}

-- | The splice data to be logged
data SpliceInfo
  = SpliceInfo
    { spliceDescription  :: String
    , spliceSource       :: Maybe (LHsExpr GhcRn) -- Nothing <=> top-level decls
                                                  --        added by addTopDecls
    , spliceIsDecl       :: Bool    -- True <=> put the generate code in a file
                                    --          when -dth-dec-file is on
    , spliceGenerated    :: SDoc
    }
        -- Note that 'spliceSource' is *renamed* but not *typechecked*
        -- Reason (a) less typechecking crap
        --        (b) data constructors after type checking have been
        --            changed to their *wrapp----------------ers*, and that makes them
        --            print always fully qualified

-- | outputs splice information for 2 flags which have different output formats:
-- `-ddump-splices` and `-dth-dec-file`
traceSplice :: SpliceInfo -> TcM ()
traceSplice (SpliceInfo { spliceDescription = sd, spliceSource = mb_src
                        , spliceGenerated = gen, spliceIsDecl = is_decl })
  = do loc <- case mb_src of
                 Nothing        -> getSrcSpanM
                 Just (L loc _) -> return (locA loc)
       traceOptTcRn Opt_D_dump_splices (spliceDebugDoc loc)

       when is_decl $ do -- Raw material for -dth-dec-file
        logger <- getLogger
        liftIO $ putDumpFileMaybe logger Opt_D_th_dec_file "" FormatHaskell (spliceCodeDoc loc)
  where
    -- `-ddump-splices`
    spliceDebugDoc :: SrcSpan -> SDoc
    spliceDebugDoc loc
      = let code = case mb_src of
                     Nothing -> ending
                     Just e  -> nest 2 (ppr (stripParensLHsExpr e)) : ending
            ending = [ text "======>", nest 2 gen ]
        in  hang (ppr loc <> colon <+> text "Splicing" <+> text sd)
               2 (sep code)

    -- `-dth-dec-file`
    spliceCodeDoc :: SrcSpan -> SDoc
    spliceCodeDoc loc
      = vcat [ text "--" <+> ppr loc <> colon <+> text "Splicing" <+> text sd
             , gen ]

checkThLocalTyName :: Name -> RnM ()
checkThLocalTyName name
  | isUnboundName name   -- Do not report two errors for
  = return ()            --   $(not_in_scope args)

  | otherwise
  = do  { traceRn "checkThLocalTyName" (ppr name)
        ; mb_local_use <- getStageAndBindLevel name
        ; case mb_local_use of {
             Nothing -> return () ;  -- Not a locally-bound thing
             Just (top_lvl, bind_lvl, use_stage) ->
    do  { let use_lvl = thLevel use_stage
        -- We don't check the well stageness of name here.
        -- this would break test for #20969
        --
        -- Consequently there is no check&restiction for top level splices.
        -- But it's annoying anyway.
        --
        -- Therefore checkCrossStageLiftingTy shouldn't assume anything
        -- about bind_lvl and use_lvl relation.
        --
        -- ; checkWellStaged (StageCheckSplice name) bind_lvl use_lvl

        ; traceRn "checkThLocalTyName" (ppr name <+> ppr bind_lvl
                                                 <+> ppr use_stage
                                                 <+> ppr use_lvl)
        ; dflags <- getDynFlags
        ; checkCrossStageLiftingTy dflags top_lvl bind_lvl use_stage use_lvl name } } }

checkThLocalName :: HasCallStack => Name -> RnM ()
checkThLocalName name
--  | pprTrace "checkTh" (ppr name $$ callStackDoc) False = undefined
  | isUnboundName name   -- Do not report two errors for
  = return ()            --   $(not_in_scope args)

  | isWiredInName name
  = return ()

  | otherwise
  = do  { --pprTraceM "checkThLocalName" (ppr name)
          mb_local_use <- getStageAndBindLevel name
        ; case mb_local_use of {
             Nothing -> return () ;  -- Not a locally-bound thing
             Just (top_lvl, bind_lvl, use_stage) ->
    do  { let use_lvl = thLevel use_stage
        ; cur_mod <- extractModule <$> getGblEnv
        ; let is_local
                  | Just mod <- nameModule_maybe name = mod == cur_mod
                  | otherwise = True
       -- ; checkWellStaged (StageCheckSplice name) bind_lvl use_lvl
       {-} ; pprTraceM "checkThLocalName" (ppr name <+> ppr bind_lvl
                                               <+> ppr use_stage
                                               <+> ppr use_lvl) -}
        ; dflags <- getDynFlags
        ; env <- getGlobalRdrEnv
        ; let mgre = lookupGRE_Name env name
        ; checkCrossStageLifting dflags (StageCheckSplice name mgre) top_lvl is_local bind_lvl use_stage use_lvl name } } }

--------------------------------------
checkCrossStageLifting :: DynFlags
                       -> StageCheckReason
                       -> TopLevelFlag
                       -> Bool
                       -> Set.Set ThLevel
                       -> ThStage
                       -> ThLevel
                       -> Name -> TcM ()
-- We are inside brackets, and (use_lvl > bind_lvl)
-- Now we must check whether there's a cross-stage lift to do
-- Examples   \x -> [| x |]
--            [| map |]
--
-- This code is similar to checkCrossStageLifting in GHC.Tc.Gen.Expr, but
-- this is only run on *untyped* brackets.

checkCrossStageLifting dflags reason top_lvl is_local bind_lvl use_stage use_lvl name
  | use_lvl `Set.member` bind_lvl = return ()
  | Brack _ (RnPendingUntyped ps_var) <- use_stage   -- Only for untyped brackets
  = do
      dflags <- getDynFlags
      let err = TcRnBadlyStaged reason bind_lvl use_lvl
      check_cross_stage_lifting err dflags top_lvl name ps_var
  | Brack _ RnPendingTyped <- use_stage  -- Lift for typed brackets is inserted later.
  , xopt LangExt.LiftCrossStagedPersistence dflags
    = return ()
  | isTopLevel top_lvl
  , is_local
  , any (use_lvl >=) (Set.toList bind_lvl)
  , xopt LangExt.PathCrossStagedPersistence dflags = return ()
  | not is_local
  , xopt LangExt.PathCrossStagedPersistence dflags = return ()
  | otherwise = addErrTc (TcRnBadlyStaged reason bind_lvl use_lvl)

check_cross_stage_lifting :: TcRnMessage -> DynFlags -> TopLevelFlag -> Name -> TcRef [PendingRnSplice] -> TcM ()
check_cross_stage_lifting reason dflags top_lvl name ps_var
  | isTopLevel top_lvl
  , xopt LangExt.PathCrossStagedPersistence dflags
        -- Top-level identifiers in this module,
        -- (which have External Names)
        -- are just like the imported case:
        -- no need for the 'lifting' treatment
        -- E.g.  this is fine:
        --   f x = x
        --   g y = [| f 3 |]
  = when (isExternalName name) (keepAlive name)
    -- See Note [Keeping things alive for Template Haskell]

  | xopt LangExt.LiftCrossStagedPersistence dflags
  =     -- Nested identifiers, such as 'x' in
        -- E.g. \x -> [| h x |]
        -- We must behave as if the reference to x was
        --      h $(lift x)
        -- We use 'x' itself as the SplicePointName, used by
        -- the desugarer to stitch it all back together.
        -- If 'x' occurs many times we may get many identical
        -- bindings of the same SplicePointName, but that doesn't
        -- matter, although it's a mite untidy.
    do  { traceRn "checkCrossStageLifting" (ppr name)

          -- Construct the (lift x) expression
        ; let lift_expr   = nlHsApp (nlHsVar liftName) (nlHsVar name)
              pend_splice = PendingRnSplice UntypedExpSplice name lift_expr

          -- Warning for implicit lift (#17804)
        ; addDetailedDiagnostic (TcRnImplicitLift name)

          -- Update the pending splices
        ; ps <- readMutVar ps_var
        ; writeMutVar ps_var (pend_splice : ps) }
  | otherwise = addErrTc reason

checkCrossStageLiftingTy :: DynFlags -> TopLevelFlag -> Set.Set ThLevel -> ThStage -> ThLevel -> Name -> TcM ()
checkCrossStageLiftingTy dflags top_lvl bind_lvl _use_stage use_lvl name
  | isTopLevel top_lvl
  , xopt LangExt.PathCrossStagedPersistence dflags
  = return ()

  -- There is no liftType (yet), so we could error, or more conservatively, just warn.
  --
  -- For now, we check here for both untyped and typed splices, as we don't create splices.

  -- Can also happen for negative cases
  -- See comment in checkThLocalTyName:
  | use_lvl `notElem` bind_lvl
  = addDiagnostic $ TcRnBadlyStagedType name bind_lvl use_lvl

  | otherwise
  = return ()

{-
Note [Keeping things alive for Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f x = x+1
  g y = [| f 3 |]

Here 'f' is referred to from inside the bracket, which turns into data
and mentions only f's *name*, not 'f' itself. So we need some other
way to keep 'f' alive, lest it get dropped as dead code.  That's what
keepAlive does. It puts it in the keep-alive set, which subsequently
ensures that 'f' stays as a top level binding.

This must be done by the renamer, not the type checker (as of old),
because the type checker doesn't typecheck the body of untyped
brackets (#8540).

A thing can have a bind_lvl of outerLevel, but have an internal name:
   foo = [d| op = 3
             bop = op + 1 |]
Here the bind_lvl of 'op' is (bogusly) outerLevel, even though it is
bound inside a bracket.  That is because we don't even record
binding levels for top-level things; the binding levels are in the
LocalRdrEnv.

So the occurrence of 'op' in the rhs of 'bop' looks a bit like a
cross-stage thing, but it isn't really.  And in fact we never need
to do anything here for top-level bound things, so all is fine, if
a bit hacky.

For these chaps (which have Internal Names) we don't want to put
them in the keep-alive set.

Note [Quoting names]
~~~~~~~~~~~~~~~~~~~~
A quoted name 'n is a bit like a quoted expression [| n |], except that we
have no cross-stage lifting (c.f. GHC.Tc.Gen.Expr.thBrackId).  So, after incrementing
the use-level to account for the brackets, the cases are:

        bind > use                      Error
        bind = use+1                    OK
        bind < use
                Imported things         OK
                Top-level things        OK
                Non-top-level           Error

where 'use' is the binding level of the 'n quote. (So inside the implied
bracket the level would be use+1.)

Examples:

  f 'map        -- OK; also for top-level defns of this module

  \x. f 'x      -- Not ok (bind = 1, use = 1)
                -- (whereas \x. f [| x |] might have been ok, by
                --                               cross-stage lifting

  \y. [| \x. $(f 'y) |] -- Not ok (bind =1, use = 1)

  [| \x. $(f 'x) |]     -- OK (bind = 2, use = 1)
-}
