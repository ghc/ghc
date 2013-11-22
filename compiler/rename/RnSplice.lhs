\begin{code}
module RnSplice (
        rnSplice, rnSpliceType, rnSpliceExpr, rnSplicePat, rnSpliceDecl,
        rnBracket, checkTH,
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
import DsMeta           ( expQTyConName, patQTyConName, typeQTyConName )
import LoadIface        ( loadInterfaceForName )
import Module
import RnEnv
import RnPat
import RnSource         ( rnSrcDecls, findSplice )
import RnTypes
import SrcLoc
import TcEnv            ( checkWellStaged, tcMetaTy )
import Outputable
import BasicTypes       ( TopLevelFlag, isTopLevel )
import FastString

import {-# SOURCE #-} RnExpr   ( rnLExpr )
import {-# SOURCE #-} TcExpr   ( tcMonoExpr )
import {-# SOURCE #-} TcSplice ( runMetaE, runMetaP, runMetaT, tcTopSpliceExpr )
#endif
\end{code}

\begin{code}
#ifndef GHCI
rnBracket :: HsExpr RdrName -> HsBracket RdrName -> RnM (HsExpr Name, FreeVars)
rnBracket e _ = failTH e "Template Haskell bracket"

rnSplice :: HsSplice RdrName -> RnM (HsSplice Name, FreeVars)
rnSplice e = failTH e "Template Haskell splice"

rnSpliceType :: HsSplice RdrName -> PostTcKind -> RnM (HsType Name, FreeVars)
rnSpliceType e _ = failTH e "Template Haskell type splice"

rnSpliceExpr :: HsSplice RdrName -> RnM (HsExpr Name, FreeVars)
rnSpliceExpr e = failTH e "Template Haskell splice"

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
rnSplice :: HsSplice RdrName -> RnM (HsSplice Name, FreeVars)
-- Not exported...used for all
rnSplice (HsSplice isTyped n expr)
  = do  { checkTH expr "Template Haskell splice"
        ; loc  <- getSrcSpanM
        ; n' <- newLocalBndrRn (L loc n)
        ; (expr', fvs) <- rnLExpr expr

        ; if isTyped
          then do
            { -- Ugh!  See Note [Splices] above
              lcl_rdr <- getLocalRdrEnv
            ; gbl_rdr <- getGlobalRdrEnv
            ; let gbl_names = mkNameSet [gre_name gre | gre <- globalRdrEnvElts gbl_rdr,
                                                        isLocalGRE gre]
                  lcl_names = mkNameSet (localRdrEnvElts lcl_rdr)

            ; return (HsSplice isTyped n' expr', fvs `plusFV` lcl_names `plusFV` gbl_names)
            }
          else return (HsSplice isTyped n' expr', fvs)
        }
\end{code}

\begin{code}
rnSpliceType :: HsSplice RdrName -> PostTcKind -> RnM (HsType Name, FreeVars)
rnSpliceType splice@(HsSplice isTypedSplice _ expr) k
  = setSrcSpan (getLoc expr) $ do
    { stage <- getStage
    ; case stage of
        { Brack isTypedBrack pop_stage ps_var _ ->
            do { when (isTypedBrack && not isTypedSplice) $
                     failWithTc illegalUntypedSplice
               ; when (not isTypedBrack && isTypedSplice) $
                     failWithTc illegalTypedSplice

                 -- ToDo: deal with fvs
               ; (splice'@(HsSplice _ name expr'), fvs) <- setStage pop_stage $
                                                           rnSplice splice

               ; ps <- readMutVar ps_var
               ; writeMutVar ps_var (PendingRnTypeSplice name expr' : ps)

               ; return (HsSpliceTy splice' fvs k, fvs)
               }
        ; _ ->
            do { -- ToDo: deal with fvs
                 (splice', fvs) <- addErrCtxt (spliceResultDoc expr) $
                                   setStage (Splice isTypedSplice) $
                                   rnSplice splice
               ; maybeExpandTopSplice splice' fvs
               }
        }
    }
  where
    maybeExpandTopSplice :: HsSplice Name -> FreeVars -> RnM (HsType Name, FreeVars)
    maybeExpandTopSplice splice@(HsSplice True _ _) fvs
      = return (HsSpliceTy splice fvs k, fvs)

    maybeExpandTopSplice (HsSplice False _ expr) _
      = do { -- The splice must have type TypeQ
           ; meta_exp_ty <- tcMetaTy typeQTyConName

             -- Typecheck the expression
           ; zonked_q_expr <- tcTopSpliceExpr False $
                              tcMonoExpr expr meta_exp_ty

             -- Run the expression
           ; hs_ty2 <- runMetaT zonked_q_expr
           ; showSplice "type" expr (ppr hs_ty2)

           ; (hs_ty3, fvs) <- addErrCtxt (spliceResultDoc expr) $
                              do { let doc = SpliceTypeCtx hs_ty2
                                 ; checkNoErrs $ rnLHsType doc hs_ty2
                                   -- checkNoErrs: see Note [Renamer errors]
                                 }
           ; return (unLoc hs_ty3, fvs)
           }
\end{code}

\begin{code}
rnSpliceExpr :: HsSplice RdrName -> RnM (HsExpr Name, FreeVars)
rnSpliceExpr splice@(HsSplice isTypedSplice _ expr)
  = addErrCtxt (exprCtxt (HsSpliceE splice)) $
    setSrcSpan (getLoc expr) $ do
    { stage <- getStage
    ; case stage of
        { Brack isTypedBrack pop_stage ps_var _ ->
            do { when (isTypedBrack && not isTypedSplice) $
                     failWithTc illegalUntypedSplice
               ; when (not isTypedBrack && isTypedSplice) $
                     failWithTc illegalTypedSplice

               ; (splice'@(HsSplice _ name expr'), fvs) <- setStage pop_stage $
                                                           rnSplice splice

               ; ps <- readMutVar ps_var
               ; writeMutVar ps_var (PendingRnExpSplice name expr' : ps)

               ; return (HsSpliceE splice', fvs)
               }
        ; _ ->
            do { (splice', fvs) <- addErrCtxt (spliceResultDoc expr) $
                                   setStage (Splice isTypedSplice) $
                                   rnSplice splice
               ; maybeExpandTopSplice splice' fvs
               }
        }
    }
  where
    maybeExpandTopSplice :: HsSplice Name -> FreeVars -> RnM (HsExpr Name, FreeVars)
    maybeExpandTopSplice splice@(HsSplice True _ _) fvs
      = return (HsSpliceE splice, fvs)

    maybeExpandTopSplice (HsSplice False _ expr) _
      = do { -- The splice must have type ExpQ
           ; meta_exp_ty <- tcMetaTy expQTyConName

             -- Typecheck the expression
           ; zonked_q_expr <- tcTopSpliceExpr False $
                              tcMonoExpr expr meta_exp_ty

             -- Run the expression
           ; expr2 <- runMetaE zonked_q_expr
           ; showSplice "expression" expr (ppr expr2)

           ; (lexpr3, fvs) <- addErrCtxt (spliceResultDoc expr) $
                              checkNoErrs $
                              rnLExpr expr2
           ; return (unLoc lexpr3, fvs)
           }
\end{code}

\begin{code}
rnSplicePat :: HsSplice RdrName -> RnM (Pat Name, FreeVars)
rnSplicePat (HsSplice True _ _)
  = panic "rnSplicePat: encountered typed pattern splice"

rnSplicePat splice@(HsSplice False _ expr)
  = addErrCtxt (exprCtxt (HsSpliceE splice)) $
    setSrcSpan (getLoc expr) $ do
    { stage <- getStage
     ; case stage of
         { Brack isTypedBrack pop_stage ps_var _ ->
             do { checkTc (not isTypedBrack) illegalUntypedSplice

                ; (splice'@(HsSplice _ name expr'), fvs) <- setStage pop_stage $
                                                            rnSplice splice

                ; ps <- readMutVar ps_var
                ; writeMutVar ps_var (PendingRnPatSplice name expr' : ps)

                ; return (SplicePat splice', fvs)
                }
         ; _ -> 
             do { (HsSplice _ _ expr', fvs) <- addErrCtxt (spliceResultDoc expr) $
                                               setStage (Splice False) $
                                               rnSplice splice

                  -- The splice must have type Pat
                ; meta_exp_ty <- tcMetaTy patQTyConName

                  -- Typecheck the expression
                ; zonked_q_expr <- tcTopSpliceExpr False $
                                   tcMonoExpr expr' meta_exp_ty

                  -- Run the expression
                ; pat <- runMetaP zonked_q_expr
                ; showSplice "pattern" expr' (ppr pat)

                ; (pat', _) <- addErrCtxt (spliceResultDoc expr) $
                               checkNoErrs $
                               rnPat ThPatSplice pat $ \pat' -> return (pat', emptyFVs)

                ; return (unLoc pat', fvs)
                }
         }
     }
\end{code}

\begin{code}
rnSpliceDecl :: SpliceDecl RdrName -> RnM (SpliceDecl Name, FreeVars)
rnSpliceDecl (SpliceDecl (L _ (HsSplice True _ _)) _)
  = panic "rnSpliceDecls: encountered typed declaration splice"

rnSpliceDecl (SpliceDecl (L loc splice@(HsSplice False _ expr)) flg)
  = addErrCtxt (exprCtxt (HsSpliceE splice)) $
    setSrcSpan (getLoc expr) $ do
    { stage <- getStage
     ; case stage of
         { Brack isTypedBrack pop_stage ps_var _ ->
             do { checkTc (not isTypedBrack) illegalUntypedSplice

                ; (splice'@(HsSplice _ name expr'), fvs) <- setStage pop_stage $
                                                            rnSplice splice

                ; ps <- readMutVar ps_var
                ; writeMutVar ps_var (PendingRnDeclSplice name expr' : ps)

                ; return (SpliceDecl (L loc splice') flg, fvs)
                }
         ; _ -> 
           pprPanic "rnSpliceDecls: should not have been called on top-level splice" (ppr expr)
         }
     }
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

       ; pending_splices <- newMutVar []
       ; let brack_stage = Brack (isTypedBracket br_body)
                                 cur_stage pending_splices
                                 (error "rnBracket: don't neet lie")

       ; (body', fvs_e) <- setStage brack_stage $
                           rn_bracket cur_stage br_body
       ; pendings <- readMutVar pending_splices

       ; return (HsRnBracketOut body' pendings, fvs_e)
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
                        { Nothing -> pprTrace "rn_bracket" (ppr name) $ -- Should not happen for local names
                                     return ()

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
exprCtxt :: HsExpr RdrName -> SDoc
exprCtxt expr
  = hang (ptext (sLit "In the expression:")) 2 (ppr expr)

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

spliceResultDoc :: OutputableBndr id => LHsExpr id -> SDoc
spliceResultDoc expr
  = vcat [ hang (ptext (sLit "In the splice:"))
              2 (char '$' <> pprParendExpr expr)
        , ptext (sLit "To see what the splice expanded to, use -ddump-splices") ]
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

checkCrossStageLifting _ _ Comp      = return ()
checkCrossStageLifting _ _ (Splice _) = return ()

checkCrossStageLifting top_lvl name (Brack _ _ ps_var _)
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
        ; writeMutVar ps_var (PendingRnCrossStageSplice name : ps)
        }
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

