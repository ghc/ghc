\begin{code}
module RnSplice (
        rnSpliceType, rnSpliceExpr,
        rnBracket, checkTH
  ) where

import Control.Monad    ( unless )
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
import TcEnv            ( thRnBrack )
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
rnSplice (HsSplice n expr)
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

        ; return (HsSplice n' expr', fvs `plusFV` lcl_names `plusFV` gbl_names) }
\end{code}

\begin{code}
rnSpliceType :: HsSplice RdrName -> PostTcKind -> RnM (HsType Name, FreeVars)
rnSpliceType splice k
  = do  { (splice', fvs) <- rnSplice splice -- ToDo: deal with fvs
        ; return (HsSpliceTy splice' fvs k, fvs)
        }
\end{code}

\begin{code}
rnSpliceExpr :: HsSplice RdrName -> RnM (HsExpr Name, FreeVars)
rnSpliceExpr splice = do
    (splice', fvs) <- rnSplice splice
    return (HsSpliceE splice', fvs)
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
  = do { thEnabled <- xoptM Opt_TemplateHaskell
       ; unless thEnabled $
           failWith ( vcat [ ptext (sLit "Syntax error on") <+> ppr e
                           , ptext (sLit "Perhaps you intended to use -XTemplateHaskell") ] )
       ; checkTH e "bracket"
       ; (body', fvs_e) <- rn_bracket br_body
       ; return (HsBracket body', fvs_e)
       }

rn_bracket :: HsBracket RdrName -> RnM (HsBracket Name, FreeVars)
rn_bracket (VarBr flg n)
  = do { name <- lookupOccRn n
       ; this_mod <- getModule
       ; unless (nameIsLocalOrFrom this_mod name) $  -- Reason: deprecation checking assumes
         do { _ <- loadInterfaceForName msg name     -- the home interface is loaded, and
            ; return () }                            -- this is the only way that is going
                                                     -- to happen
       ; return (VarBr flg name, unitFV name) }
  where
    msg = ptext (sLit "Need interface for Template Haskell quoted Name")

rn_bracket (ExpBr e) = do { (e', fvs) <- rnLExpr e
                          ; return (ExpBr e', fvs) }

rn_bracket (PatBr p) = rnPat ThPatQuote p $ \ p' -> return (PatBr p', emptyFVs)

rn_bracket (TypBr t) = do { (t', fvs) <- rnLHsType TypBrCtx t
                          ; return (TypBr t', fvs) }

rn_bracket (DecBrL decls)
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
                              setStage thRnBrack $
                              rnSrcDecls [] group
   -- The empty list is for extra dependencies coming from .hs-boot files
   -- See Note [Extra dependencies from .hs-boot files] in RnSource

              -- Discard the tcg_env; it contains only extra info about fixity
        ; traceRn (text "rn_bracket dec" <+> (ppr (tcg_dus tcg_env) $$
                   ppr (duUses (tcg_dus tcg_env))))
        ; return (DecBrG group', duUses (tcg_dus tcg_env)) }

rn_bracket (DecBrG _) = panic "rn_bracket: unexpected DecBrG"
\end{code}
