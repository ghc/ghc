\begin{code}
module RnSplice (
        rnSpliceType, rnSpliceExpr,
        rnBracket, checkTH
  ) where

import Control.Monad    ( unless, when )
import DynFlags
import FastString
import Name
import NameSet
import HsSyn
import LoadIface        ( loadInterfaceForName )
import Module
import Outputable
import RdrName
import RnEnv
import RnPat
import RnSource         ( rnSrcDecls, findSplice )
import RnTypes
import SrcLoc
import TcEnv            ( tcLookup, thTopLevelId )
import TcRnMonad

import {-# SOURCE #-} RnExpr( rnLExpr )
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

        -- Ugh!  See Note [Splices] above
        ; lcl_rdr <- getLocalRdrEnv
        ; gbl_rdr <- getGlobalRdrEnv
        ; let gbl_names = mkNameSet [gre_name gre | gre <- globalRdrEnvElts gbl_rdr,
                                                    isLocalGRE gre]
              lcl_names = mkNameSet (localRdrEnvElts lcl_rdr)

        ; return (HsSplice isTyped n' expr', fvs `plusFV` lcl_names `plusFV` gbl_names) }
\end{code}

\begin{code}
rnSpliceType :: HsSplice RdrName -> PostTcKind -> RnM (HsType Name, FreeVars)
rnSpliceType splice@(HsSplice _ _ hs_expr) k
  = setSrcSpan (getLoc hs_expr) $ do
    { stage <- getStage
    ; case stage of {
        Splice {} -> rnTopSpliceType splice k ;
        Comp      -> rnTopSpliceType splice k ;

        Brack _ pop_level _ _ -> do
           -- See Note [How brackets and nested splices are handled]
           -- A splice inside brackets
    { (splice', fvs) <- setStage pop_level $
                        rnSplice splice -- ToDo: deal with fvs
    ; return (HsSpliceTy splice' fvs k, fvs)
    }}}

rnTopSpliceType :: HsSplice RdrName -> PostTcKind -> RnM (HsType Name, FreeVars)
rnTopSpliceType splice@(HsSplice _ _ hs_expr) k
  = do  { (splice', fvs) <- addErrCtxt (spliceResultDoc hs_expr) $
                            rnSplice splice -- ToDo: deal with fvs
        ; return (HsSpliceTy splice' fvs k, fvs)
        }
\end{code}

\begin{code}
rnSpliceExpr :: HsSplice RdrName -> RnM (HsExpr Name, FreeVars)
rnSpliceExpr splice@(HsSplice isTypedSplice _ expr)
  = setSrcSpan (getLoc expr) $ do
    { stage <- getStage
    ; case stage of {
        Splice {} -> rnTopSplice ;
        Comp      -> rnTopSplice ;

        Brack isTypedBrack pop_stage _ _ -> do

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

     ; (splice', fvs) <- setStage pop_stage $
                         rnSplice splice
     ; return (HsSpliceE splice', fvs)
     }}}
  where
      rnTopSplice :: RnM (HsExpr Name, FreeVars)
      rnTopSplice
        = do  { (splice', fvs) <- addErrCtxt (spliceResultDoc expr) $
                                  setStage (Splice isTypedSplice) $
                                  rnSplice splice
              ; return (HsSpliceE splice', fvs)
              }
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
                           , ptext (sLit "Perhaps you intended to use TemplateHaskell") ] )
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

       ; let brack_stage = Brack (isTypedBracket br_body) cur_stage (error "rnBracket1") (error "rnBracket2")

       ; (body', fvs_e) <- setStage brack_stage $
                           rn_bracket cur_stage br_body
       ; return (HsBracket body', fvs_e)
       }

rn_bracket :: ThStage -> HsBracket RdrName -> RnM (HsBracket Name, FreeVars)
rn_bracket outer_stage br@(VarBr flg n)
  = do { name <- lookupOccRn n
       ; this_mod <- getModule
       
         -- Reason: deprecation checking assumes
         -- the home interface is loaded, and
         -- this is the only way that is going
         -- to happen
       ; unless (nameIsLocalOrFrom this_mod name) $
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

spliceResultDoc :: LHsExpr RdrName -> SDoc
spliceResultDoc expr
  = hang (ptext (sLit "In the splice:")) 2 (char '$' <> pprParendExpr expr)
\end{code}
