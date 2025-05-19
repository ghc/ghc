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

        checkThLocalName, checkThLocalNameWithLift, checkThLocalNameNoLift, traceSplice, SpliceInfo(..),
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
import GHC.Builtin.Names.TH ( decsQTyConName, expQTyConName
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
import qualified GHC.Boot.TH.Syntax as TH (Q)

import qualified GHC.LanguageExtensions as LangExt
import qualified Data.Set as Set

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
       ; cur_level <- getThLevel
       ; case cur_level of
           { Splice _ _       -> return ()
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
       ; (body', fvs_e) <- setThLevel (Brack cur_level RnPendingTyped) $ rnLExpr br_body
       ; return (HsTypedBracket noExtField body', fvs_e)

       }

rnUntypedBracket :: HsExpr GhcPs -> HsQuote GhcPs -> RnM (HsExpr GhcRn, FreeVars)
rnUntypedBracket e br_body
  = addErrCtxt (UntypedTHBracketCtxt br_body) $
    do { checkForTemplateHaskellQuotes e

         -- Check for nested brackets
       ; cur_level <- getThLevel
       ; case cur_level of
           { Splice _ _       -> return ()
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
         setThLevel (UntypedBrack cur_level ps_var) $
                  rn_utbracket br_body
       ; pendings <- readMutVar ps_var
       ; return (HsUntypedBracket pendings body', fvs_e)

       }

rn_utbracket :: HsQuote GhcPs -> RnM (HsQuote GhcRn, FreeVars)
rn_utbracket (VarBr _ flg rdr_name)
  = do { name <- lookupOccRn (if flg then WL_Term else WL_Type) (unLoc rdr_name)
       ; let res_name = L (l2l (locA rdr_name)) (WithUserRdr (unLoc rdr_name) name)
       ; if flg then checkThLocalNameNoLift res_name else checkThLocalTyName name
       ; check_namespace flg name
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
                   -> (UntypedSpliceFlavour, HsUntypedSpliceResult z -> HsUntypedSplice GhcRn -> RnM a)
                                                   -- Inside brackets, make it pending
                   -> HsUntypedSplice GhcPs
                   -> RnM (a, FreeVars)
rnUntypedSpliceGen run_splice (flavour, run_pending) splice
  = addErrCtxt (UntypedSpliceCtxt splice) $ do
    { level <- getThLevel
    ; case level of
        TypedBrack {}
          -> failWithTc $ thSyntaxError
                        $ MismatchedSpliceType Untyped IsSplice

        UntypedBrack pop_level ps_var
          -> do { (splice', fvs) <- setThLevel pop_level $
                                    rnUntypedSplice splice flavour
                ; loc  <- getSrcSpanM
                ; splice_name <- newLocalBndrRn (L (noAnnSrcSpan loc) unqualSplice)
                ; result <- run_pending (HsUntypedSpliceNested splice_name) splice'
                ; ps <- readMutVar ps_var
                ; writeMutVar ps_var (PendingRnSplice splice_name splice' : ps)
                ; return (result, fvs) }

        _ ->  do { checkTopSpliceAllowed splice
                 ; cur_level <- getThLevel
                 ; (splice', fvs1) <- checkNoErrs $
                                      setThLevel (Splice Untyped cur_level) $
                                      rnUntypedSplice splice flavour
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

       -- TODO: Should call tcUntypedSplice here
       ; let the_expr = case splice' of
                HsUntypedSpliceExpr _ e ->  e
                HsQuasiQuote _ q str -> mkQuasiQuoteExpr flavour q str
                XUntypedSplice {} -> pprPanic "runRnSplice: XUntypedSplice" (pprUntypedSplice False Nothing splice')

             -- Typecheck the expression
       ; meta_exp_ty   <- tcMetaTy meta_ty_name
       ; zonked_q_expr <- zonkTopLExpr =<<
                            tcTopSpliceExpr Untyped
                              (tcCheckPolyExpr the_expr meta_exp_ty)

             -- Run the expression
       ; mod_finalizers_ref <- newTcRef []
       ; result <- setThLevel (RunSplice mod_finalizers_ref) $
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


-- See Note [Lifecycle of an untyped splice, and PendingRnSplice]
recordPendingSplice :: SplicePointName -> HsImplicitLiftSplice -> PendingStuff -> TcM (HsExpr GhcRn)
recordPendingSplice sp pn (RnPending ref) = do
  let untyped_splice = XUntypedSplice pn
  updTcRef ref (PendingRnSplice sp untyped_splice : )
  return (HsUntypedSplice (HsUntypedSpliceNested sp) untyped_splice)
-- Splices are not lifted for typed brackets
-- See Note [Lifecycle of an typed splice, and PendingTcSplice]
recordPendingSplice sp pn (RnPendingTyped) = do
  let typed_splice = XTypedSplice pn
  return (HsTypedSplice (HsTypedSpliceNested sp) typed_splice)
recordPendingSplice _ _ (TcPending _ _ _) = panic "impossible"

------------------
mkQuasiQuoteExpr :: UntypedSpliceFlavour -> LIdP GhcRn
                 -> XRec GhcPs FastString
                 -> LHsExpr GhcRn
-- Return the expression (quoter "...quote...")
-- which is what we must run in a quasi-quote
mkQuasiQuoteExpr flavour quoter (L q_span' quote)
  = L q_span $ HsApp noExtField (L q_span
             $ HsApp noExtField (L q_span
                    (mkHsVar (L (l2l q_span) quote_selector)))
                                quoterExpr)
                    quoteExpr
  where
    q_span = noAnnSrcSpan (locA q_span')
    quoterExpr = L (l2l quoter) $! mkHsVar          $! quoter
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

rnUntypedSplice :: HsUntypedSplice GhcPs
                -> UntypedSpliceFlavour
                -> RnM ( HsUntypedSplice GhcRn
                       , FreeVars)
-- Not exported...used for all
rnUntypedSplice (HsUntypedSpliceExpr _ expr) flavour
  = do  { (expr', fvs) <- rnLExpr expr
        ; return (HsUntypedSpliceExpr (HsUserSpliceExt flavour) expr', fvs) }

rnUntypedSplice (HsQuasiQuote _ quoter quote) flavour
  = do  { -- Rename the quoter; akin to the HsVar case of rnExpr
        ; quoter' <- lookupLocatedOccRn WL_TermVariable quoter
        ; let res_name = WithUserRdr (unLoc quoter) <$> quoter'
        ; checkThLocalNameNoLift res_name
        ; return (HsQuasiQuote (HsQuasiQuoteExt flavour) quoter' quote, unitFV (unLoc quoter')) }

---------------------
rnTypedSplice :: HsTypedSplice GhcPs -- Typed splice expression
              -> RnM (HsExpr GhcRn, FreeVars)
rnTypedSplice sp@(HsTypedSpliceExpr _ expr)
  = addErrCtxt (TypedSpliceCtxt Nothing sp) $ do
    { level <- getThLevel
    ; case level of
        TypedBrack pop_level
          -> do { loc <- getSrcSpanM
                ; n' <- newLocalBndrRn (L (noAnnSrcSpan loc) unqualSplice)
                ; (e, fvs) <- setThLevel pop_level rn_splice
                ; return (HsTypedSplice (HsTypedSpliceNested n') (HsTypedSpliceExpr noExtField e), fvs)
                }

        UntypedBrack {}
          -> failWithTc $ thSyntaxError $ MismatchedSpliceType Typed IsSplice

        _ -> do { unlessXOptM LangExt.TemplateHaskell
                    (failWith $ thSyntaxError IllegalTHSplice)

                ; cur_level <- getThLevel
                ; (result, fvs1) <- checkNoErrs $ setThLevel (Splice Typed cur_level) rn_splice
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

                ; return (HsTypedSplice HsTypedSpliceTop (HsTypedSpliceExpr noExtField result), fvs1 `plusFV` fvs2) } }
  where
    rn_splice :: RnM (LHsExpr GhcRn, FreeVars)
    rn_splice = rnLExpr expr

rnUntypedSpliceExpr :: HsUntypedSplice GhcPs -> RnM (HsExpr GhcRn, FreeVars)
rnUntypedSpliceExpr splice
  = rnUntypedSpliceGen run_expr_splice pend_expr_splice splice
  where
    pend_expr_splice :: (UntypedSpliceFlavour, HsUntypedSpliceResult (HsExpr GhcRn) -> HsUntypedSplice GhcRn -> RnM (HsExpr GhcRn))
    pend_expr_splice
      = (UntypedExpSplice, \x y -> pure $ HsUntypedSplice x y)

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
    pend_type_splice
       = ( UntypedTypeSplice
         , \x y -> pure $ HsSpliceTy x y)

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
    pend_pat_splice
      = (UntypedPatSplice
        , \x y -> pure (y, x)) -- Pat splice is nested and thus simply renamed

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
    pend_ty_pat_splice
      = (UntypedTypeSplice
        , \x y -> pure (y, x))

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
    pend_decl_splice
       = ( UntypedDeclSplice
         , \_ y -> pure $ SpliceDecl noExtField (L loc y) flg)

    run_decl_splice rn_splice  = pprPanic "rnSpliceDecl" (pprUntypedSplice True Nothing rn_splice)

rnTopSpliceDecls :: HsUntypedSplice GhcPs -> RnM ([LHsDecl GhcPs], FreeVars)
-- Declaration splice at the very top level of the module
rnTopSpliceDecls splice
   =  do { checkTopSpliceAllowed splice
         ; cur_level <- getThLevel
         ; (rn_splice, fvs) <- checkNoErrs $
                               setThLevel (Splice Untyped cur_level) $
                               rnUntypedSplice splice UntypedDeclSplice
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
        --            changed to their *wrappers*, and that makes them
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
        ; mb_local_use <- getCurrentAndBindLevel name
        ; case mb_local_use of {
             Nothing -> return () ;  -- Not a locally-bound thing
             Just (top_lvl, bind_lvl, use_lvl) ->
    do  { let use_lvl_idx = thLevelIndex use_lvl
        -- We don't check the well levelledness of name here.
        -- this would break test for #20969
        --
        -- Consequently there is no check&restiction for top level splices.
        -- But it's annoying anyway.
        --
        -- Therefore checkCrossLevelLiftingTy shouldn't assume anything
        -- about bind_lvl and use_lvl relation.
        --
        ; traceRn "checkThLocalTyName" (ppr name <+> ppr bind_lvl
                                                 <+> ppr use_lvl
                                                 <+> ppr use_lvl)
        ; dflags <- getDynFlags
        ; checkCrossLevelLiftingTy dflags top_lvl bind_lvl use_lvl use_lvl_idx name } } }

-- | Check whether we are allowed to use a Name in this context (for TH purposes)
-- In the case of a level incorrect program, attempt to fix it by using
-- a Lift constraint.
checkThLocalNameWithLift :: LIdOccP GhcRn -> RnM (HsExpr GhcRn)
checkThLocalNameWithLift = checkThLocalName True

-- | Check whether we are allowed to use a Name in this context (for TH purposes)
-- In the case of a level incorrect program, do not attempt to fix it by using
-- a Lift constraint.
checkThLocalNameNoLift :: LIdOccP GhcRn -> RnM ()
checkThLocalNameNoLift name = checkThLocalName False name >> return ()

-- | Implemenation of the level checks
-- See Note [Template Haskell levels]
checkThLocalName :: Bool -> LIdOccP GhcRn -> RnM (HsExpr GhcRn)
checkThLocalName allow_lifting name_var
  -- Exact and Orig names are not imported, so presumed available at all levels.
  | isExact (userRdrName (unLoc name_var)) || isOrig (userRdrName (unLoc name_var))
  = return (HsVar noExtField name_var)
  | isUnboundName name   -- Do not report two errors for
  = return (HsVar noExtField name_var)            --   $(not_in_scope args)
  | isWiredInName name
  = return (HsVar noExtField name_var)
  | otherwise
  = do  {
          mb_local_use <- getCurrentAndBindLevel name
        ; case mb_local_use of {
             Nothing -> return (HsVar noExtField name_var) ;  -- Not a locally-bound thing
             Just (top_lvl, bind_lvl, use_lvl) ->
    do  { let use_lvl_idx = thLevelIndex use_lvl
        ; cur_mod <- extractModule <$> getGblEnv
        ; let is_local
                  | Just mod <- nameModule_maybe name = mod == cur_mod
                  | otherwise = True
        ; traceRn "checkThLocalName" (ppr name <+> ppr bind_lvl <+> ppr use_lvl <+> ppr use_lvl)
        ; dflags <- getDynFlags
        ; env <- getGlobalRdrEnv
        ; let mgre = lookupGRE_Name env name
        ; checkCrossLevelLifting dflags (LevelCheckSplice name mgre) top_lvl is_local allow_lifting bind_lvl use_lvl use_lvl_idx name_var } } }
  where
    name = getName name_var

--------------------------------------
checkCrossLevelLifting :: DynFlags
                       -> LevelCheckReason
                       -> TopLevelFlag
                       -> Bool
                       -> Bool
                       -> Set.Set ThLevelIndex
                       -> ThLevel
                       -> ThLevelIndex
                       -> LIdOccP GhcRn
                       -> TcM (HsExpr GhcRn)
checkCrossLevelLifting dflags reason top_lvl is_local allow_lifting bind_lvl use_lvl use_lvl_idx name_var
  -- 1. If name is in-scope, at the correct level.
  | use_lvl_idx `Set.member` bind_lvl = return (HsVar noExtField name_var)
  -- 2. Name is imported with -XImplicitStagePersistence
  | not is_local
  , xopt LangExt.ImplicitStagePersistence dflags = return (HsVar noExtField name_var)
  -- 3. Name is top-level, with -XImplicitStagePersistence, and needs
  -- to be persisted into the future.
  | isTopLevel top_lvl
  , is_local
  , any (use_lvl_idx >=) (Set.toList bind_lvl)
  , xopt LangExt.ImplicitStagePersistence dflags = when (isExternalName name) (keepAlive name) >> return (HsVar noExtField name_var)
  -- 4. Name is in a bracket, and lifting is allowed
  | Brack _ pending <- use_lvl
  , any (use_lvl_idx >=) (Set.toList bind_lvl)
  , allow_lifting
  = do
       (splice_name :: Name) <- newLocalBndrRn (noLocA unqualSplice)
       let  pend_splice :: HsImplicitLiftSplice
            pend_splice = HsImplicitLiftSplice name_var
       -- Warning for implicit lift (#17804)
       addDetailedDiagnostic (TcRnImplicitLift name)

       -- Update the pending splices if we are renaming a typed bracket
       recordPendingSplice splice_name pend_splice pending
  -- Otherwise, we have a level error, report.
  | otherwise = addErrTc (TcRnBadlyLevelled reason bind_lvl use_lvl_idx ) >> return (HsVar noExtField name_var)
  where
    name = getName name_var

checkCrossLevelLiftingTy :: DynFlags -> TopLevelFlag -> Set.Set ThLevelIndex -> ThLevel -> ThLevelIndex -> Name -> TcM ()
checkCrossLevelLiftingTy dflags top_lvl bind_lvl _use_lvl use_lvl_idx name
  | isTopLevel top_lvl
  , xopt LangExt.ImplicitStagePersistence dflags
  = return ()

  -- There is no liftType (yet), so we could error, or more conservatively, just warn.
  --
  -- For now, we check here for both untyped and typed splices, as we don't create splices.

  -- Can also happen for negative cases
  -- See comment in checkThLocalTyName:
  | use_lvl_idx `notElem` bind_lvl
  = addDiagnostic $ TcRnBadlyLevelledType name bind_lvl use_lvl_idx

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
have no cross-level lifting (c.f. GHC.Tc.Gen.Expr.thBrackId).  So, after incrementing
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
                --                               cross-level lifting

  \y. [| \x. $(f 'y) |] -- Not ok (bind =1, use = 1)

  [| \x. $(f 'x) |]     -- OK (bind = 2, use = 1)
-}
