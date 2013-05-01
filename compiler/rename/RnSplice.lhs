\begin{code}
module RnSplice (
        rnSpliceType, rnSpliceExpr,
        rnBracket, checkTH,
        checkThLocalName
  ) where

import FastString
import Name
import NameSet
import HsSyn
import Outputable
import RdrName
import TcRnMonad

#ifdef GHCI
import Control.Monad    ( unless, when )
import DynFlags
import DsMeta           ( expQTyConName, typeQTyConName )
import LoadIface        ( loadInterfaceForName )
import RnEnv
import RnPat
import RnSource         ( rnSrcDecls, findSplice )
import RnTypes
import SrcLoc
import TcEnv            ( checkWellStaged, tcLookup, tcMetaTy, thTopLevelId )

import {-# SOURCE #-} RnExpr   ( rnLExpr )
import {-# SOURCE #-} TcExpr   ( tcMonoExpr )
import {-# SOURCE #-} TcSplice ( runMetaE, runMetaT, tcTopSpliceExpr )
#endif
\end{code}

\begin{code}
#ifndef GHCI
rnBracket :: HsExpr RdrName -> HsBracket RdrName -> RnM (HsExpr Name, FreeVars)
rnBracket e _ = failTH e "bracket"

rnSpliceType :: HsSplice RdrName -> PostTcKind -> RnM (HsType Name, FreeVars)
rnSpliceType e _ = failTH e "splice"

rnSpliceExpr :: HsSplice RdrName -> RnM (HsExpr Name, FreeVars)
rnSpliceExpr e = failTH e "splice"

failTH :: Outputable a => a -> String -> RnM b
failTH e what  -- Raise an error in a stage-1 compiler
  = failWithTc (vcat [ptext (sLit "Template Haskell") <+> text what <+>
                      ptext (sLit "requires GHC with interpreter support"),
                      ptext (sLit "Perhaps you are using a stage-1 compiler?"),
                      nest 2 (ppr e)])
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
rnSplice (HsSplice isTyped n expr)
  = do  { checkTH expr "splice"
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

%************************************************************************
%*                                                                      *
        Template Haskell brackets
%*                                                                      *
%************************************************************************

\begin{code}
rnBracket :: HsExpr RdrName -> HsBracket RdrName -> RnM (HsExpr Name, FreeVars)
rnBracket e br_body
  = addErrCtxt (hang (ptext (sLit "In the Template Haskell quotation"))
                   2 (ppr br_body)) $
    do { -- Check that Template Haskell is enabled and available
         thEnabled <- xoptM Opt_TemplateHaskell
       ; unless thEnabled $
           failWith ( vcat [ ptext (sLit "Syntax error on") <+> ppr e
                           , ptext (sLit "Perhaps you intended to use -XTemplateHaskell") ] )
       ; checkTH e "bracket"

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
       ; let brack_stage = Brack (isTypedBracket br_body) cur_stage pending_splices (error "rnBracket: don't neet lie")

       ; (body', fvs_e) <- setStage brack_stage $
                           rn_bracket cur_stage br_body
       ; pendings <- readMutVar pending_splices

       ; return (HsRnBracketOut body' pendings, fvs_e)
       }

rn_bracket :: ThStage -> HsBracket RdrName -> RnM (HsBracket Name, FreeVars)
rn_bracket outer_stage br@(VarBr flg n)
  = do { name <- lookupOccRn n
       ; this_mod <- getModule
       
       ; case flg of
           { -- Type variables can be quoted in TH. See #5721.
             False -> return ()
           ; True | nameIsLocalOrFrom this_mod name ->
                 do { mb_bind_lvl <- lookupLocalOccThLvl_maybe n
                    ; case mb_bind_lvl of
                        { Nothing -> return ()
                        ; Just bind_lvl
                            | isExternalName name -> return ()
                              -- Local non-external things can still be
                              -- top-level in GHCi, so check for that here.
                            | bind_lvl == impLevel -> return ()
                            | otherwise -> checkTc (thLevel outer_stage + 1 == bind_lvl)
                                                   (quotedNameStageErr br)
                        }
                    }
           ; True | otherwise -> 
                 -- Reason: deprecation checking assumes
                 -- the home interface is loaded, and
                 -- this is the only way that is going
                 -- to happen
                 do { _ <- loadInterfaceForName msg name
                    ; thing <- tcLookup name
                    ; case thing of
                        { AGlobal {} -> return ()
                        ; ATyVar {}  -> return ()
                        ; ATcId { tct_level = bind_lvl, tct_id = id }
                            | thTopLevelId id       -- C.f TcExpr.checkCrossStageLifting
                            -> keepAliveTc id
                            | otherwise
                            -> do { checkTc (thLevel outer_stage + 1 == bind_lvl)
                                            (quotedNameStageErr br) }
                        ; _ -> pprPanic "rh_bracket" (ppr name $$ ppr thing)
                        }
                    }
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
  = do { (group, mb_splice) <- findSplice decls
       ; case mb_splice of
           Nothing -> return ()
           Just (SpliceDecl (L loc _) _, _)
              -> setSrcSpan loc $
                 addErr (ptext (sLit "Declaration splices are not permitted inside declaration brackets"))
                -- Why not?  See Section 7.3 of the TH paper.

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

spliceResultDoc :: OutputableBndr id => LHsExpr id -> SDoc
spliceResultDoc expr
  = sep [ ptext (sLit "In the result of the splice:")
        , nest 2 (char '$' <> pprParendExpr expr)
        , ptext (sLit "To see what the splice expanded to, use -ddump-splices")]
#endif
\end{code}

\begin{code}
checkTH :: Outputable a => a -> String -> RnM ()
#ifdef GHCI
checkTH _ _ = return () -- OK
#else
checkTH e what  -- Raise an error in a stage-1 compiler
  = addErr (vcat [ptext (sLit "Template Haskell") <+> text what <+>
                  ptext (sLit "requires GHC with interpreter support"),
                  ptext (sLit "Perhaps you are using a stage-1 compiler?"),
                  nest 2 (ppr e)])
#endif
\end{code}

\begin{code}
checkThLocalName :: Name -> ThLevel -> RnM ()
#ifndef GHCI  /* GHCI and TH is off */
--------------------------------------
-- Check for cross-stage lifting
checkThLocalName _name _bind_lvl
  = return ()

#else         /* GHCI and TH is on */
checkThLocalName name bind_lvl
  = do  { use_stage <- getStage -- TH case
        ; let use_lvl = thLevel use_stage
        ; traceRn (text "checkThLocalName" <+> ppr name)
        ; checkWellStaged (quotes (ppr name)) bind_lvl use_lvl
        ; traceTc "thLocalId" (ppr name <+> ppr bind_lvl <+> ppr use_stage <+> ppr use_lvl)
        ; when (use_lvl > bind_lvl) $
          checkCrossStageLifting name bind_lvl use_stage }

--------------------------------------
checkCrossStageLifting :: Name -> ThLevel -> ThStage -> TcM ()
-- We are inside brackets, and (use_lvl > bind_lvl)
-- Now we must check whether there's a cross-stage lift to do
-- Examples   \x -> [| x |]
--            [| map |]

checkCrossStageLifting _ _ Comp      = return ()
checkCrossStageLifting _ _ (Splice _) = return ()

checkCrossStageLifting name _ (Brack _ _ ps_var _)
  | isExternalName name
  =     -- Top-level identifiers in this module,
        -- (which have External Names)
        -- are just like the imported case:
        -- no need for the 'lifting' treatment
        -- E.g.  this is fine:
        --   f x = x
        --   g y = [| f 3 |]
        -- But we do need to put f into the keep-alive
        -- set, because after desugaring the code will
        -- only mention f's *name*, not f itself.
        --
        -- The type checker will put f into the keep-alive set.
    return ()
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
