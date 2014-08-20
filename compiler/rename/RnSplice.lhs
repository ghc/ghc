\begin{code}
{-# LANGUAGE CPP #-}

module RnSplice (
        rnTopSpliceDecls,
        rnSpliceType, rnSpliceExpr, rnSplicePat, rnSpliceDecl,
        rnBracket,
        checkThLocalName
  ) where


import Name
import NameSet
import HsSyn
import RdrName
import TcRnMonad

#ifdef GHCI
import Control.Monad    ( unless, when )
import DynFlags
import DsMeta           ( decsQTyConName, expQTyConName, patQTyConName, typeQTyConName )
import LoadIface        ( loadInterfaceForName )
import Module
import RnEnv
import RnPat            ( rnPat )
import RnSource         ( rnSrcDecls, findSplice )
import RnTypes          ( rnLHsType )
import SrcLoc
import TcEnv            ( checkWellStaged, tcMetaTy )
import Outputable
import BasicTypes       ( TopLevelFlag, isTopLevel )
import FastString
import Hooks

import {-# SOURCE #-} RnExpr   ( rnLExpr )
import {-# SOURCE #-} TcExpr   ( tcMonoExpr )
import {-# SOURCE #-} TcSplice ( runMetaD, runMetaE, runMetaP, runMetaT, tcTopSpliceExpr )
#endif
\end{code}

\begin{code}
#ifndef GHCI
rnBracket :: HsExpr RdrName -> HsBracket RdrName -> RnM (HsExpr Name, FreeVars)
rnBracket e _ = failTH e "Template Haskell bracket"

rnTopSpliceDecls :: HsSplice RdrName -> RnM ([LHsDecl RdrName], FreeVars)
rnTopSpliceDecls e = failTH e "Template Haskell top splice"

rnSpliceType :: HsSplice RdrName -> PostTcKind -> RnM (HsType Name, FreeVars)
rnSpliceType e _ = failTH e "Template Haskell type splice"

rnSpliceExpr :: Bool -> HsSplice RdrName -> RnM (HsExpr Name, FreeVars)
rnSpliceExpr _ e = failTH e "Template Haskell splice"

rnSplicePat :: HsSplice RdrName -> RnM (Pat Name, FreeVars)
rnSplicePat e = failTH e "Template Haskell pattern splice"

rnSpliceDecl :: SpliceDecl RdrName -> RnM (SpliceDecl Name, FreeVars)
rnSpliceDecl e = failTH e "Template Haskell declaration splice"
#else
\end{code}

%*********************************************************
%*                                                      *
                Splices
%*                                                      *
%*********************************************************

Note [Splices]
~~~~~~~~~~~~~~
Consider
        f = ...
        h = ...$(thing "f")...

The splice can expand into literally anything, so when we do dependency
analysis we must assume that it might mention 'f'.  So we simply treat
all locally-defined names as mentioned by any splice.  This is terribly
brutal, but I don't see what else to do.  For example, it'll mean
that every locally-defined thing will appear to be used, so no unused-binding
warnings.  But if we miss the dependency, then we might typecheck 'h' before 'f',
and that will crash the type checker because 'f' isn't in scope.

Currently, I'm not treating a splice as also mentioning every import,
which is a bit inconsistent -- but there are a lot of them.  We might
thereby get some bogus unused-import warnings, but we won't crash the
type checker.  Not very satisfactory really.

\begin{code}
rnSpliceGen :: Bool                                     -- Typed splice?
            -> (HsSplice Name -> RnM (a, FreeVars))     -- Outside brackets, run splice
            -> (HsSplice Name -> (PendingRnSplice, a))  -- Inside brackets, make it pending
            -> HsSplice RdrName
            -> RnM (a, FreeVars)
rnSpliceGen is_typed_splice run_splice pend_splice splice@(HsSplice _ expr)
  = addErrCtxt (spliceCtxt (HsSpliceE is_typed_splice splice)) $
    setSrcSpan (getLoc expr) $ do
    { stage <- getStage
    ; case stage of
        Brack pop_stage RnPendingTyped
          -> do { checkTc is_typed_splice illegalUntypedSplice
                ; (splice', fvs) <- setStage pop_stage $
                                    rnSplice splice
                ; let (_pending_splice, result) = pend_splice splice'
                ; return (result, fvs) }

        Brack pop_stage (RnPendingUntyped ps_var)
          -> do { checkTc (not is_typed_splice) illegalTypedSplice
                ; (splice', fvs) <- setStage pop_stage $
                                    rnSplice splice
                ; let (pending_splice, result) = pend_splice splice'
                ; ps <- readMutVar ps_var
                ; writeMutVar ps_var (pending_splice : ps)
                ; return (result, fvs) }

        _ ->  do { (splice', fvs1) <- setStage (Splice is_typed_splice) $
                                      rnSplice splice

                 ; (result, fvs2) <- run_splice splice'
                 ; return (result, fvs1 `plusFV` fvs2) } }

---------------------
rnSplice :: HsSplice RdrName -> RnM (HsSplice Name, FreeVars)
-- Not exported...used for all
rnSplice (HsSplice n expr)
  = do  { checkTH expr "Template Haskell splice"
        ; loc  <- getSrcSpanM
        ; n' <- newLocalBndrRn (L loc n)
        ; (expr', fvs) <- rnLExpr expr
        ; return (HsSplice n' expr', fvs) }


---------------------
rnSpliceExpr :: Bool -> HsSplice RdrName -> RnM (HsExpr Name, FreeVars)
rnSpliceExpr is_typed splice
  = rnSpliceGen is_typed run_expr_splice pend_expr_splice splice
  where
    pend_expr_splice :: HsSplice Name -> (PendingRnSplice, HsExpr Name)
    pend_expr_splice rn_splice
        = (PendingRnExpSplice rn_splice, HsSpliceE is_typed rn_splice)

    run_expr_splice :: HsSplice Name -> RnM (HsExpr Name, FreeVars)
    run_expr_splice rn_splice@(HsSplice _ expr')
      | is_typed   -- Run it later, in the type checker
      = do {  -- Ugh!  See Note [Splices] above
              lcl_rdr <- getLocalRdrEnv
           ; gbl_rdr <- getGlobalRdrEnv
           ; let gbl_names = mkNameSet [gre_name gre | gre <- globalRdrEnvElts gbl_rdr
                                                     , isLocalGRE gre]
                 lcl_names = mkNameSet (localRdrEnvElts lcl_rdr)

           ; return (HsSpliceE is_typed rn_splice, lcl_names `plusFV` gbl_names) }

      | otherwise  -- Run it here
      = do { expr <- getHooked runRnSpliceHook return >>= ($ expr')
      
             -- The splice must have type ExpQ
           ; meta_exp_ty <- tcMetaTy expQTyConName

             -- Typecheck the expression
           ; zonked_q_expr <- tcTopSpliceExpr False $
                              tcMonoExpr expr meta_exp_ty

             -- Run the expression
           ; expr2 <- runMetaE zonked_q_expr
           ; showSplice "expression" expr (ppr expr2)

           ; (lexpr3, fvs) <- checkNoErrs $
                              rnLExpr expr2
           ; return (unLoc lexpr3, fvs)  }

----------------------
rnSpliceType :: HsSplice RdrName -> PostTcKind -> RnM (HsType Name, FreeVars)
rnSpliceType splice k
  = rnSpliceGen False run_type_splice pend_type_splice splice
  where
    pend_type_splice rn_splice
       = (PendingRnTypeSplice rn_splice, HsSpliceTy rn_splice k)

    run_type_splice (HsSplice _ expr') 
       = do { expr <- getHooked runRnSpliceHook return >>= ($ expr')
              
            ; meta_exp_ty <- tcMetaTy typeQTyConName

              -- Typecheck the expression
            ; zonked_q_expr <- tcTopSpliceExpr False $
                               tcMonoExpr expr meta_exp_ty

              -- Run the expression
            ; hs_ty2 <- runMetaT zonked_q_expr
            ; showSplice "type" expr (ppr hs_ty2)

            ; (hs_ty3, fvs) <- do { let doc = SpliceTypeCtx hs_ty2
                                  ; checkNoErrs $ rnLHsType doc hs_ty2
                                    -- checkNoErrs: see Note [Renamer errors]
                                  }
            ; return (unLoc hs_ty3, fvs) }

----------------------
rnSplicePat :: HsSplice RdrName -> RnM (Pat Name, FreeVars)
rnSplicePat splice
  = rnSpliceGen False run_pat_splice pend_pat_splice splice
  where
    pend_pat_splice rn_splice
      = (PendingRnPatSplice rn_splice, SplicePat rn_splice)

    run_pat_splice (HsSplice _ expr')
      = do { expr <- getHooked runRnSpliceHook return >>= ($ expr')
      
           ; meta_exp_ty <- tcMetaTy patQTyConName

             -- Typecheck the expression
           ; zonked_q_expr <- tcTopSpliceExpr False $
                              tcMonoExpr expr meta_exp_ty

             -- Run the expression
           ; pat <- runMetaP zonked_q_expr
           ; showSplice "pattern" expr (ppr pat)

           ; (pat', fvs) <- checkNoErrs $
                            rnPat ThPatSplice pat $ \pat' -> return (pat', emptyFVs)

           ; return (unLoc pat', fvs) }

----------------------
rnSpliceDecl :: SpliceDecl RdrName -> RnM (SpliceDecl Name, FreeVars)
rnSpliceDecl (SpliceDecl (L loc splice) flg)
  = rnSpliceGen False run_decl_splice pend_decl_splice splice
  where
    pend_decl_splice rn_splice
       = (PendingRnDeclSplice rn_splice, SpliceDecl(L loc rn_splice) flg)

    run_decl_splice rn_splice = pprPanic "rnSpliceDecl" (ppr rn_splice)
\end{code}

\begin{code}
rnTopSpliceDecls :: HsSplice RdrName -> RnM ([LHsDecl RdrName], FreeVars)
-- Declaration splice at the very top level of the module
rnTopSpliceDecls (HsSplice _ expr'')
   = do  { (expr, fvs) <- setStage (Splice False) $
                           rnLExpr expr''

         ; expr' <- getHooked runRnSpliceHook return >>= ($ expr)

         ; list_q <- tcMetaTy decsQTyConName     -- Q [Dec]
         ; zonked_q_expr <- tcTopSpliceExpr False (tcMonoExpr expr' list_q)

                -- Run the expression
         ; decls <- runMetaD zonked_q_expr
         ; showSplice "declarations" expr'
                 (ppr (getLoc expr) $$ (vcat (map ppr decls)))

         ; return (decls,fvs) }
\end{code}

%************************************************************************
%*                                                                      *
        Template Haskell brackets
%*                                                                      *
%************************************************************************

\begin{code}
rnBracket :: HsExpr RdrName -> HsBracket RdrName -> RnM (HsExpr Name, FreeVars)
rnBracket e br_body
  = addErrCtxt (quotationCtxtDoc br_body) $
    do { -- Check that Template Haskell is enabled and available
         thEnabled <- xoptM Opt_TemplateHaskell
       ; unless thEnabled $
           failWith ( vcat [ ptext (sLit "Syntax error on") <+> ppr e
                           , ptext (sLit "Perhaps you intended to use TemplateHaskell") ] )
       ; checkTH e "Template Haskell bracket"

         -- Check for nested brackets
       ; cur_stage <- getStage
       ; case cur_stage of
           { Splice True  -> checkTc (isTypedBracket br_body) illegalUntypedBracket
           ; Splice False -> checkTc (not (isTypedBracket br_body)) illegalTypedBracket
           ; Comp         -> return ()
           ; Brack {}     -> failWithTc illegalBracket
           }

         -- Brackets are desugared to code that mentions the TH package
       ; recordThUse

       ; case isTypedBracket br_body of
            True  -> do { (body', fvs_e) <- setStage (Brack cur_stage RnPendingTyped) $
                                            rn_bracket cur_stage br_body
                        ; return (HsBracket body', fvs_e) }

            False -> do { ps_var <- newMutVar []
                        ; (body', fvs_e) <- setStage (Brack cur_stage (RnPendingUntyped ps_var)) $
                                            rn_bracket cur_stage br_body
                        ; pendings <- readMutVar ps_var
                        ; return (HsRnBracketOut body' pendings, fvs_e) }
       }

rn_bracket :: ThStage -> HsBracket RdrName -> RnM (HsBracket Name, FreeVars)
rn_bracket outer_stage br@(VarBr flg rdr_name)
  = do { name <- lookupOccRn rdr_name
       ; this_mod <- getModule

       ; case flg of
           { -- Type variables can be quoted in TH. See #5721.
             False -> return ()
           ; True | nameIsLocalOrFrom this_mod name ->
                 do { mb_bind_lvl <- lookupLocalOccThLvl_maybe name
                    ; case mb_bind_lvl of
                        { Nothing -> return ()      -- Can happen for data constructors,
                                                    -- but nothing needs to be done for them

                        ; Just (top_lvl, bind_lvl)  -- See Note [Quoting names]
                             | isTopLevel top_lvl
                             -> when (isExternalName name) (keepAlive name)
                             | otherwise
                             -> do { traceRn (text "rn_bracket VarBr" <+> ppr name <+> ppr bind_lvl <+> ppr outer_stage)
                                   ; checkTc (thLevel outer_stage + 1 == bind_lvl)
                                             (quotedNameStageErr br) }
                        }
                    }
           ; True | otherwise ->  -- Imported thing
                 discardResult (loadInterfaceForName msg name)
                     -- Reason for loadInterface: deprecation checking
                     -- assumes that the home interface is loaded, and
                     -- this is the only way that is going to happen
           }
       ; return (VarBr flg name, unitFV name) }
  where
    msg = ptext (sLit "Need interface for Template Haskell quoted Name")

rn_bracket _ (ExpBr e) = do { (e', fvs) <- rnLExpr e
                            ; return (ExpBr e', fvs) }

rn_bracket _ (PatBr p) = rnPat ThPatQuote p $ \ p' -> return (PatBr p', emptyFVs)

rn_bracket _ (TypBr t) = do { (t', fvs) <- rnLHsType TypBrCtx t
                            ; return (TypBr t', fvs) }

rn_bracket _ (DecBrL decls)
  = do { group <- groupDecls decls
       ; gbl_env  <- getGblEnv
       ; let new_gbl_env = gbl_env { tcg_dus = emptyDUs }
                          -- The emptyDUs is so that we just collect uses for this
                          -- group alone in the call to rnSrcDecls below
       ; (tcg_env, group') <- setGblEnv new_gbl_env $
                              rnSrcDecls [] group
   -- The empty list is for extra dependencies coming from .hs-boot files
   -- See Note [Extra dependencies from .hs-boot files] in RnSource

              -- Discard the tcg_env; it contains only extra info about fixity
        ; traceRn (text "rn_bracket dec" <+> (ppr (tcg_dus tcg_env) $$
                   ppr (duUses (tcg_dus tcg_env))))
        ; return (DecBrG group', duUses (tcg_dus tcg_env)) }
  where
    groupDecls :: [LHsDecl RdrName] -> RnM (HsGroup RdrName)
    groupDecls decls
      = do { (group, mb_splice) <- findSplice decls
           ; case mb_splice of
           { Nothing -> return group
           ; Just (splice, rest) ->
               do { group' <- groupDecls rest
                  ; let group'' = appendGroups group group'
                  ; return group'' { hs_splcds = noLoc splice : hs_splcds group' }
                  }
           }}

rn_bracket _ (DecBrG _) = panic "rn_bracket: unexpected DecBrG"

rn_bracket _ (TExpBr e) = do { (e', fvs) <- rnLExpr e
                             ; return (TExpBr e', fvs) }
\end{code}

\begin{code}
spliceCtxt :: HsExpr RdrName -> SDoc
spliceCtxt expr= hang (ptext (sLit "In the splice:")) 2 (ppr expr)

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

quotedNameStageErr :: HsBracket RdrName -> SDoc
quotedNameStageErr br
  = sep [ ptext (sLit "Stage error: the non-top-level quoted name") <+> ppr br
        , ptext (sLit "must be used at the same stage at which is is bound")]

quotationCtxtDoc :: HsBracket RdrName -> SDoc
quotationCtxtDoc br_body
  = hang (ptext (sLit "In the Template Haskell quotation"))
         2 (ppr br_body)

-- spliceResultDoc :: OutputableBndr id => LHsExpr id -> SDoc
-- spliceResultDoc expr
--  = vcat [ hang (ptext (sLit "In the splice:"))
--              2 (char '$' <> pprParendExpr expr)
--        , ptext (sLit "To see what the splice expanded to, use -ddump-splices") ]
#endif
\end{code}

\begin{code}
checkThLocalName :: Name -> RnM ()
#ifndef GHCI  /* GHCI and TH is off */
--------------------------------------
-- Check for cross-stage lifting
checkThLocalName _name
  = return ()

#else         /* GHCI and TH is on */
checkThLocalName name 
  = do  { traceRn (text "checkThLocalName" <+> ppr name)
        ; mb_local_use <- getStageAndBindLevel name
        ; case mb_local_use of {
             Nothing -> return () ;  -- Not a locally-bound thing
             Just (top_lvl, bind_lvl, use_stage) ->
    do  { let use_lvl = thLevel use_stage
        ; checkWellStaged (quotes (ppr name)) bind_lvl use_lvl
        ; traceRn (text "checkThLocalName" <+> ppr name <+> ppr bind_lvl <+> ppr use_stage <+> ppr use_lvl)
        ; when (use_lvl > bind_lvl) $
          checkCrossStageLifting top_lvl name use_stage } } }

--------------------------------------
checkCrossStageLifting :: TopLevelFlag -> Name -> ThStage -> TcM ()
-- We are inside brackets, and (use_lvl > bind_lvl)
-- Now we must check whether there's a cross-stage lift to do
-- Examples   \x -> [| x |]
--            [| map |]

checkCrossStageLifting top_lvl name (Brack _ (RnPendingUntyped ps_var))
  | isTopLevel top_lvl
        -- Top-level identifiers in this module,
        -- (which have External Names)
        -- are just like the imported case:
        -- no need for the 'lifting' treatment
        -- E.g.  this is fine:
        --   f x = x
        --   g y = [| f 3 |]
  = when (isExternalName name) (keepAlive name)
    -- See Note [Keeping things alive for Template Haskell]

  | otherwise
  =     -- Nested identifiers, such as 'x' in
        -- E.g. \x -> [| h x |]
        -- We must behave as if the reference to x was
        --      h $(lift x)
        -- We use 'x' itself as the splice proxy, used by
        -- the desugarer to stitch it all back together.
        -- If 'x' occurs many times we may get many identical
        -- bindings of the same splice proxy, but that doesn't
        -- matter, although it's a mite untidy.
    do  { traceRn (text "checkCrossStageLifting" <+> ppr name)
        ; -- Update the pending splices
        ; ps <- readMutVar ps_var
        ; writeMutVar ps_var (PendingRnCrossStageSplice name : ps) }

checkCrossStageLifting _ _ _ = return ()
#endif /* GHCI */
\end{code}

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
brackets (Trac #8540).

A thing can have a bind_lvl of outerLevel, but have an internal name:
   foo = [d| op = 3
             bop = op + 1 |]
Here the bind_lvl of 'op' is (bogusly) outerLevel, even though it is
bound inside a bracket.  That is because we don't even even record
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
have no cross-stage lifting (c.f. TcExpr.thBrackId).  So, after incrementing
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

