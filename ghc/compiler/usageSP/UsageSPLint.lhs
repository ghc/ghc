%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[UsageSPLint]{UsageSP ``lint'' pass}

This code is (based on) PhD work of Keith Wansbrough <kw217@cl.cam.ac.uk>,
September 1998 .. May 1999.

Keith Wansbrough 1998-09-04..1999-06-25

\begin{code}
module UsageSPLint ( doLintUSPAnnotsBinds,
                     doLintUSPConstBinds,
                     doLintUSPBinds,
                     doCheckIfWorseUSP,
                   ) where

#include "HsVersions.h"

import UsageSPUtils
import CoreSyn
import TypeRep          ( Type(..), TyNote(..) )  -- friend
import Type             ( UsageAnn(..), isUsgTy, tyUsg )
import TyCon            ( isAlgTyCon, isPrimTyCon, isSynTyCon, isFunTyCon )
import Var              ( Var, varType )
import Id		( idLBVarInfo )
import IdInfo           ( LBVarInfo(..) )
import SrcLoc           ( noSrcLoc )
import ErrUtils         ( Message, ghcExit )
import Util             ( zipWithEqual )
import PprCore
import Bag
import Outputable
\end{code}

======================================================================

Interface
~~~~~~~~~

@doLintUSPAnnotsBinds@ checks that annotations are in the correct positions.
@doLintUSPConstsBinds@ checks that no @UVar@s remain anywhere (i.e., all annots are constants).
@doLintUSPBinds@ checks that the annotations are consistent.  [unimplemented!]
@doCheckIfWorseUSP@ checks that annots on binders have not changed from Once to Many.

\begin{code}
doLint :: ULintM a -> IO ()

doLint m = case runULM m of
             Nothing -> return ()
             Just bad_news -> do { printDump (display bad_news)
                                 ; ghcExit 1
                                 }
  where display bad_news = vcat [ text "*** LintUSP errors: ***"
                                , bad_news
                                , text "*** end of LintUSP errors ***"
                                ]

doLintUSPAnnotsBinds, doLintUSPConstBinds :: [CoreBind] -> IO ()

doLintUSPAnnotsBinds = doLint . lintUSPAnnotsBinds
doLintUSPConstBinds  = doLint . lintUSPConstBinds

-- doLintUSPBinds is defined below

doCheckIfWorseUSP :: [CoreBind] -> [CoreBind] -> IO ()

doCheckIfWorseUSP binds binds'
  = case checkIfWorseUSP binds binds' of
      Nothing    -> return ()
      Just warns -> printErrs warns
\end{code}

======================================================================

Verifying correct annotation positioning
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following functions check whether the usage annotations are
correctly placed on a type.  They sit inside the lint monad.
@lintUSPAnnots@ assumes there should be an outermost annotation,
@lintUSPAnnotsN@ assumes there shouldn't.

The fact that no general catch-all pattern is given for @NoteTy@s is
entirely intentional.  The meaning of future extensions here is
entirely unknown, so you'll have to decide how to check them
explicitly.

\begin{code}
lintTyUSPAnnots :: Bool        -- die on omitted annotation?
                -> Bool        -- die on extra annotation?
                -> Type        -- type to check
                -> ULintM ()

lintTyUSPAnnots fom fex = lint
  where
    lint     (NoteTy (UsgNote _) ty) = lintTyUSPAnnotsN fom fex ty
    lint ty0                         = do { mayErrULM fom "missing UsgNote" ty0
                                          ; lintTyUSPAnnotsN fom fex ty0
                                          }

lintTyUSPAnnotsN :: Bool        -- die on omitted annotation?
                 -> Bool        -- die on extra annotation?
                 -> Type        -- type to check
                 -> ULintM ()

lintTyUSPAnnotsN fom fex = lintN
  where
    lintN ty0@(NoteTy (UsgNote _)   ty) = do { mayErrULM fex "unexpected UsgNote" ty0
                                             ; lintN ty
                                             }
    lintN     (NoteTy (SynNote sty) ty) = do { lintN sty
                                             ; lintN ty
                                             }
    lintN     (NoteTy (FTVNote _)   ty) = do { lintN ty }

    lintN     (TyVarTy _)               = do { return () }
    lintN     (AppTy ty1 ty2)           = do { lintN ty1
                                             ; lintN ty2
                                             }
    lintN     (TyConApp tc tys)         = ASSERT( isFunTyCon tc || isAlgTyCon tc || isPrimTyCon tc || isSynTyCon tc )
                                          do { let thelint = if isFunTyCon tc
                                                             then lintTyUSPAnnots fom fex
                                                             else lintN
                                             ; mapM thelint tys
                                             ; return ()
                                             }
    lintN     (FunTy ty1 ty2)           = do { lintTyUSPAnnots fom fex ty1
                                             ; lintTyUSPAnnots fom fex ty2
                                             }
    lintN     (ForAllTy _ ty)           = do { lintN ty }
\end{code}


Now the combined function that takes a @MungeFlags@ to tell it what to
do to a particular type.  This is passed to @genAnnotBinds@ to get the
work done.

\begin{code}
lintUSPAnnotsTyM :: MungeFlags -> Type -> AnnotM (ULintM ()) Type

lintUSPAnnotsTyM mf ty = AnnotM $ \ m ve -> 
                           (ty, do { m
                                   ; atLocULM (mfLoc mf) $
                                       (if isSigma mf
                                        then lintTyUSPAnnots
                                        else lintTyUSPAnnotsN) checkOmitted True ty
                                   },
                            ve)
#ifndef USMANY
  where checkOmitted = False  -- OK to omit Many if !USMANY
#else
  where checkOmitted = True   -- require all annotations
#endif

lintUSPAnnotsBinds :: [CoreBind]
                   -> ULintM ()

lintUSPAnnotsBinds binds = case initAnnotM (return ()) $
                                  genAnnotBinds lintUSPAnnotsTyM return binds of
                                           -- **! should check with mungeTerm too!
                             (_,m) -> m
\end{code}

======================================================================

Verifying correct usage typing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following function verifies that all usage annotations are
consistent.  It assumes that there are no usage variables, only
@UsOnce@ and @UsMany@ annotations.

This is very similar to usage inference, however, and so we could
simply use that, with a little work.  For now, it's unimplemented.

\begin{code}
doLintUSPBinds :: [CoreBind] -> IO ()

doLintUSPBinds binds = panic "doLintUSPBinds unimplemented"
                    {- case initUs us (uniqSMMToUs (usgInfBinds binds)) of
                         ((ucs,_),_) -> if isJust (solveUCS ucs)
                                        then return ()
                                        else do { printDump (text "*** LintUSPBinds failed ***")
                                                ; ghcExit 1
                                                }
                     -}
\end{code}

======================================================================

Verifying usage constants only (not vars)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following function checks that all usage annotations are ground,
i.e., @UsOnce@ or @UsMany@: no @UVar@s remain.

\begin{code}
lintTyUSPConst :: Type
               -> ULintM ()

lintTyUSPConst (TyVarTy _)                         = do { return () }

lintTyUSPConst (AppTy ty1 ty2)                     = do { lintTyUSPConst ty1
                                                        ; lintTyUSPConst ty2
                                                        }
lintTyUSPConst (TyConApp tc tys)                   = do { mapM lintTyUSPConst tys
                                                        ; return ()
                                                        }
lintTyUSPConst (FunTy ty1 ty2)                     = do { lintTyUSPConst ty1
                                                        ; lintTyUSPConst ty2
                                                        }
lintTyUSPConst (ForAllTy _ ty)                     = do { lintTyUSPConst ty }

lintTyUSPConst ty0@(NoteTy (UsgNote (UsVar _)) ty) = do { errULM "unexpected usage variable" ty0
                                                        ; lintTyUSPConst ty
                                                        }
lintTyUSPConst ty0@(NoteTy (UsgNote _)         ty) = do { lintTyUSPConst ty }
lintTyUSPConst ty0@(NoteTy (SynNote sty)       ty) = do { lintTyUSPConst sty
                                                        ; lintTyUSPConst ty
                                                        }
lintTyUSPConst ty0@(NoteTy (FTVNote _)         ty) = do { lintTyUSPConst ty }
\end{code}


Now the combined function and the invocation of @genAnnotBinds@ to do the real work.

\begin{code}
lintUSPConstTyM :: MungeFlags -> Type -> AnnotM (ULintM ()) Type

lintUSPConstTyM mf ty = AnnotM $ \ m ve -> 
                           (ty,
                            do { m
                               ; atLocULM (mfLoc mf) $
                                   lintTyUSPConst ty
                               },
                            ve)

lintUSPConstBinds :: [CoreBind]
                  -> ULintM ()

lintUSPConstBinds binds = case initAnnotM (return ()) $
                                 genAnnotBinds lintUSPConstTyM return binds of
                                           -- **! should check with mungeTerm too!
                            (_,m) -> m
\end{code}

======================================================================

Checking annotations don't get any worse
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is assumed that all transformations in GHC are `work-safe', that
is, they do not cause any work to be duplicated.  Thus they should
also be safe wrt the UsageSP analysis: if an identifier has a
used-once type at one point, the identifier should never become
used-many after transformation.  This check verifies that this is the
case.

The arguments are the CoreBinds before and after the inference.  They
must have exactly the same shape apart from usage annotations.

We only bother checking binders; free variables *should* be fixed
already since they are imported and not changeable.

First, the various kinds of worsenings we can have:

\begin{code}
data WorseErr = WorseVar  Var Var  -- variable gets worse
              | WorseTerm CoreExpr  CoreExpr   -- term gets worse
              | WorseLam  Var Var  -- lambda gets worse

instance Outputable WorseErr where
  ppr (WorseVar v0 v)  = ptext SLIT("Identifier:") <+> ppr v0 <+> dcolon
                         <+> (   ptext SLIT("was") <+> ppr (varType v0)
                              $$ ptext SLIT("now") <+> ppr (varType v))
  ppr (WorseTerm e0 e) = ptext SLIT("Term:")
                         <+> (   ptext SLIT("was") <+> ppr e0
                              $$ ptext SLIT("now") <+> ppr e)
  ppr (WorseLam v0 v)  = ptext SLIT("Lambda:")
                         <+> (   ppr v0
                              $$ ptext SLIT("(lambda-bound var info for var worsened)"))
\end{code}

Now the checker.

\begin{code}
checkIfWorseUSP :: [CoreBind]  -- old binds
                -> [CoreBind]  -- new binds
                -> Maybe SDoc  -- maybe warnings

checkIfWorseUSP binds binds'
  = let vvs = checkBinds binds binds'
    in  if isEmptyBag vvs then
          Nothing
        else
          Just $ ptext SLIT("UsageSP warning: annotations worsen for")
                 $$ nest 4 (vcat (map ppr (bagToList vvs)))

checkBinds :: [CoreBind] -> [CoreBind] -> Bag WorseErr
checkBinds binds binds' = unionManyBags $
                            zipWithEqual "UsageSPLint.checkBinds" checkBind binds binds'

checkBind :: CoreBind -> CoreBind -> Bag WorseErr
checkBind (NonRec v e) (NonRec v' e') = (checkVar v v') `unionBags` (checkCE e e')
checkBind (Rec ves)    (Rec ves')     = unionManyBags $
                                          zipWithEqual "UsageSPLint.checkBind"
                                            (\ (v,e) (v',e') -> (checkVar v v')
                                                                `unionBags` (checkCE e e'))
                                            ves ves'
checkBind _            _              = panic "UsageSPLint.checkBind"


checkCE :: CoreExpr -> CoreExpr -> Bag WorseErr

checkCE (Var _)               (Var _)                = emptyBag
checkCE (Lit _)               (Lit _)                = emptyBag

checkCE (App e arg)           (App e' arg')          = (checkCE e e')
                                                       `unionBags` (checkCE arg arg')

checkCE (Lam v e)             (Lam v' e')            = (checkVar v v')
                                                       `unionBags` (checkLamVar v v')
                                                       `unionBags` (checkCE e e')
                                                       
checkCE (Let bind e)          (Let bind' e')         = (checkBind bind bind')
                                                       `unionBags` (checkCE e e')

checkCE (Case e v alts)       (Case e' v' alts')
  = (checkCE e e')
    `unionBags` (checkVar v v')
    `unionBags` (unionManyBags $
                   zipWithEqual "usageSPLint.checkCE:Case"
                     checkAlts alts alts')
  where checkAlts (_,vs,e) (_,vs',e') = (unionManyBags $ zipWithEqual "UsageSPLint.checkCE:Alt"
                                                           checkVar vs vs')
                                        `unionBags` (checkCE e e')

checkCE (Note (SCC _) e)      (Note (SCC _) e')      = checkCE e e'

checkCE (Note (Coerce _ _) e) (Note (Coerce _ _) e') = checkCE e e'

checkCE (Note InlineCall e)   (Note InlineCall e')   = checkCE e e'

checkCE (Note InlineMe   e)   (Note InlineMe   e')   = checkCE e e'

checkCE t@(Note (TermUsg u) e) t'@(Note (TermUsg u') e')
                                                     = checkCE e e'
                                                       `unionBags` (checkUsg u u' (WorseTerm t t'))

checkCE (Type _)              (Type _)               = emptyBag

checkCE t                     t'                     = pprPanic "usageSPLint.checkCE:"
                                                         (ppr t $$ text "doesn't match" <+> ppr t')
                                            

-- does binder change from Once to Many?
-- notice we only check the top-level annotation; this is all that's necessary.  KSW 1999-04.
checkVar :: Var -> Var -> Bag WorseErr
checkVar v v' | isTyVar v       = emptyBag
              | not (isUsgTy y) = emptyBag  -- if initially no annot, definitely OK
              | otherwise       = checkUsg u u' (WorseVar v v')
  where y  = varType v
        y' = varType v'
        u  = tyUsg y
        u' = tyUsg y'

-- does lambda change from Once to Many?
checkLamVar :: Var -> Var -> Bag WorseErr
checkLamVar v v' | isTyVar v = emptyBag
                 | otherwise = case (idLBVarInfo v, idLBVarInfo v') of
                                 (NoLBVarInfo    , _              ) -> emptyBag
                                 (IsOneShotLambda, IsOneShotLambda) -> emptyBag
                                 (IsOneShotLambda, NoLBVarInfo    ) -> unitBag (WorseLam v v')

-- does term usage annotation change from Once to Many?
checkUsg :: UsageAnn -> UsageAnn -> WorseErr -> Bag WorseErr
checkUsg UsMany _      _   = emptyBag
checkUsg UsOnce UsOnce _   = emptyBag
checkUsg UsOnce UsMany err = unitBag err
\end{code}

======================================================================

Lint monad stuff
~~~~~~~~~~~~~~~~

The errors (@ULintErr@s) are collected in the @ULintM@ monad, which
also tracks the location of the current type being checked.

\begin{code}
data ULintErr = ULintErr SDoc String Type

pprULintErr :: ULintErr -> SDoc
pprULintErr (ULintErr loc s ty) = hang (text s <+> ptext SLIT("in") <+> loc <> ptext SLIT(":"))
                                       4 (ppr ty)


newtype ULintM a = ULintM (SDoc -> (a,Bag ULintErr))
unULintM (ULintM f) = f

instance Monad ULintM where
  m >>= f  = ULintM $ \ loc -> let (a ,errs ) = (unULintM m) loc
                                   (a',errs') = (unULintM (f a)) loc
                               in  (a', errs `unionBags` errs')
  return a = ULintM $ \ _   -> (a,emptyBag)

atLocULM :: SDoc -> ULintM a -> ULintM a
atLocULM loc m = ULintM $ \ _ -> (unULintM m) loc

errULM :: String -> Type -> ULintM ()
errULM err ty
  = ULintM $ \ loc -> ((),unitBag $ ULintErr loc err ty)

mayErrULM :: Bool -> String -> Type -> ULintM ()
mayErrULM f err ty
  = if f then errULM err ty else return ()

runULM :: ULintM a -> Maybe SDoc
runULM m = case (unULintM m) (panic "runULM: no location") of
             (_,errs) -> if isEmptyBag errs
                         then Nothing
                         else Just (vcat (map pprULintErr (bagToList errs)))
\end{code}

======================================================================

EOF
