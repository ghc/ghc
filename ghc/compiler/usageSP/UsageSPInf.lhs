%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[UsageSPInf]{UsageSP Inference Engine}

This code is (based on) PhD work of Keith Wansbrough <kw217@cl.cam.ac.uk>,
September 1998 .. May 1999.

Keith Wansbrough 1998-09-04..1999-05-05

\begin{code}
module UsageSPInf ( doUsageSPInf ) where

#include "HsVersions.h"

import UsageSPUtils
import UsageSPLint
import UConSet

import CoreSyn
import Type             ( Type(..), TyNote(..), UsageAnn(..),
                          applyTy, applyTys,
                          splitFunTy_maybe, splitFunTys, splitTyConApp_maybe,
                          mkUsgTy, splitUsgTy, isUsgTy, isNotUsgTy, unUsgTy, tyUsg,
                          mkFunTy, mkForAllTy )
import TyCon            ( tyConArgVrcs_maybe )
import DataCon          ( dataConType )
import Const            ( Con(..), Literal(..), literalType )
import Var              ( IdOrTyVar, UVar, varType, mkUVar, modifyIdInfo )
import IdInfo           ( setLBVarInfo, LBVarInfo(..) )
import VarEnv
import UniqSupply       ( UniqSupply, UniqSM,
                          initUs, splitUniqSupply )
import Outputable
import CmdLineOpts	( opt_D_dump_usagesp, opt_DoUSPLinting )
import ErrUtils		( doIfSet, dumpIfSet )
import PprCore          ( pprCoreBindings )
\end{code}

======================================================================

The whole inference
~~~~~~~~~~~~~~~~~~~

For full details, see _Once Upon a Polymorphic Type_, University of
Glasgow Department of Computing Science Technical Report TR-1998-19,
December 1998, or the summary in POPL'99.

Inference is performed as follows:

  1.  Remove all manipulable[*] annotations and add fresh @UVar@
      annotations.

  2.  Walk over the resulting term applying the type rules and
      collecting the constraints.

  3.  Find the solution to the constraints and apply the substitution
      to the annotations, leaving a @UVar@-free term.

[*] A manipulable annotation is one derived from the current source
module, as opposed to one derived from an import, which we are clearly
not allowed to alter.

As in the paper, a ``tau-type'' is a type that does *not* have an
annotation on top (although it may have some inside), and a
``sigma-type'' is one that does (i.e., is a tau-type with an
annotation added).  This conflicts with the totally unrelated usage of
these terms in the remainder of GHC.  Caveat lector!  KSW 1999-04.


The inference is done over a set of @CoreBind@s, and inside the IO
monad.

\begin{code}
doUsageSPInf :: UniqSupply
             -> [CoreBind]
             -> IO [CoreBind]

doUsageSPInf us binds = do
                           let binds1      = doUnAnnotBinds binds

                               (us1,us2)   = splitUniqSupply us
                               (binds2,_)  = doAnnotBinds us1 binds1

                           dumpIfSet opt_D_dump_usagesp "UsageSPInf reannot'd" $
                             pprCoreBindings binds2

                           doIfSet opt_DoUSPLinting $
                              doLintUSPAnnotsBinds binds2       -- lint check 0

                           let ((ucs,_),_) = initUs us2 (uniqSMMToUs (usgInfBinds binds2))
                               ms          = solveUCS ucs
                               s           = case ms of
                                               Just s  -> s
                                               Nothing -> panic "doUsageSPInf: insol. conset!"
                               binds3      = appUSubstBinds s binds2

                           doIfSet opt_DoUSPLinting $
                             do doLintUSPAnnotsBinds binds3     -- lint check 1
                                doLintUSPConstBinds  binds3     -- lint check 2 (force solution)
                                doCheckIfWorseUSP binds binds3  -- check for worsening of usages

                           dumpIfSet opt_D_dump_usagesp "UsageSPInf" $
                             pprCoreBindings binds3

                           return binds3
\end{code}

======================================================================

Inferring an expression
~~~~~~~~~~~~~~~~~~~~~~~

When we infer types for an expression, we expect it to be already
annotated - normally with usage variables everywhere (or possibly
constants).  No context is required since variables already know their
types.

\begin{code}
usgInfBinds :: [CoreBind]
            -> UniqSMM (UConSet,
                        VarMultiset)

usgInfBinds [] = return (emptyUConSet,
                         emptyMS)

usgInfBinds (b:bs) = do { (ucs2,fv2) <- usgInfBinds bs    -- careful of scoping here
                        ; (ucs1,fv1) <- usgInfBind b fv2
                        ; return (ucs1 `unionUCS` ucs2,
                                  fv1)
                        }

usgInfBind :: CoreBind                      -- CoreBind to infer for
           -> VarMultiset                   -- fvs of `body' (later CoreBinds)
           -> UniqSMM (UConSet,             -- constraints generated by this CoreBind
                       VarMultiset)         -- fvs of this CoreBind and later ones

usgInfBind (NonRec v1 e1) fv0 = do { (ty1u,ucs1,fv1) <- usgInfCE e1
                                   ; let ty2u = varType v1
                                         ucs2 = usgSubTy ty1u ty2u
                                         ucs3 = occChkUConSet v1 fv0
                                   ; return (unionUCSs [ucs1,ucs2,ucs3],
                                             fv1 `plusMS` (fv0 `delFromMS` v1))
                                   }

usgInfBind (Rec ves)      fv0 = do { tuf1s <- mapM (usgInfCE . snd) ves
                                   ; let (ty1us,ucs1s,fv1s) = unzip3 tuf1s
                                         vs    = map fst ves
                                         ucs2s = zipWith usgSubTy ty1us (map varType vs)
                                         fv3   = foldl plusMS fv0 fv1s
                                         ucs3  = occChksUConSet vs fv3
                                   ; return (unionUCSs (ucs1s ++ ucs2s ++ [ucs3]),
                                             foldl delFromMS fv3 vs)
                                   }

usgInfCE :: CoreExpr
         -> UniqSMM (Type,UConSet,VarMultiset)
         -- ^- in the unique supply monad for new uvars
         --          ^- type of the @CoreExpr@ (always a sigma type)
         --               ^- set of constraints arising
         --                       ^- variable appearances for occur()

usgInfCE e0@(Var v) | isTyVar v    = panic "usgInfCE: unexpected TyVar"
                    | otherwise    = return (ASSERT( isUsgTy (varType v) )
                                             varType v,
                                             emptyUConSet,
                                             unitMS v)

usgInfCE e0@(Con (Literal lit) args) = ASSERT( null args )
                                       do { u1 <- newVarUSMM (Left e0)
                                          ; return (mkUsgTy u1 (literalType lit),
                                                    emptyUConSet,
                                                    emptyMS)
                                          }

usgInfCE (Con DEFAULT _) = panic "usgInfCE: DEFAULT"

usgInfCE e0@(Con con args) = -- constant or primop.  guaranteed saturated.
                          do { let (ety1s,e1s) = span isTypeArg args
                                   ty1s = map (\ (Type ty) -> ty) ety1s  -- univ. + exist.
                             ; (ty3us,ty3u) <- case con of
                                                 DataCon c -> do { u4 <- newVarUSMM (Left e0)
                                                                 ; return $ dataConTys c u4 ty1s
                                                                     -- ty1s is exdicts + args
                                                                 }
                                                 PrimOp  p -> return $ primOpUsgTys p ty1s
                                                 otherwise -> panic "usgInfCE: unrecognised Con"
                             ; tuf4s <- mapM usgInfCE e1s
                             ; let (ty4us,ucs4s,fv4s) = unzip3 tuf4s
                                   ucs5s = zipWith usgSubTy
                                                   ty4us ty3us
                             ; return (ty3u,
                                         -- note ty3 is T ty1s, so it already
                                         -- has annotations inside where they
                                         -- should be (for datacons); for
                                         -- primops we assume types are
                                         -- appropriately annotated already.
                                       unionUCSs (ucs4s ++ ucs5s),
                                       foldl plusMS emptyMS fv4s)
                             }
  where dataConTys c u tys = -- compute argtys of a datacon
                             let rawCTy      = dataConType c
                                 cTy         = ASSERT( isUnAnnotated rawCTy )
                                             -- algebraic data types are defined entirely
                                             -- unannotated; we place Many annotations inside
                                             -- them to get the required tau-types (p20(fn) TR)
                                               annotManyN rawCTy
                                             -- we really don't want annots on top of the
                                             -- funargs, but we can't easily avoid
                                             -- this so we use unUsgTy later
                                 (ty3us,ty3) = ASSERT( all isNotUsgTy tys )
                                               splitFunTys (applyTys cTy tys)
                                             -- safe 'cos a DataCon always returns a
                                             -- value of type (TyCon tys), not an
                                             -- arrow type
                                 ty3u        = if null ty3us then mkUsgTy u ty3 else ty3
                                             -- if no args, ty3 is tau; else already sigma
                                 reUsg       = mkUsgTy u . unUsgTy
                             in  (map reUsg ty3us,
                                  reUsg ty3u)

usgInfCE (App e1 (Type ty2)) = do { (ty1u,ucs,fv) <- usgInfCE e1
                                  ; let (u,ty1) = splitUsgTy ty1u
                                  ; ASSERT( isNotUsgTy ty2 )
                                    return (mkUsgTy u (applyTy ty1 ty2),
                                            ucs,
                                            fv)
                                  }

usgInfCE (App e1 e2) = do { (ty1u,ucs1,fv1) <- usgInfCE e1
                          ; (ty2u,ucs2,fv2) <- usgInfCE e2
                          ; let (u1,ty1)    = splitUsgTy ty1u
                                (ty3u,ty4u) = case splitFunTy_maybe ty1 of
                                                Just tys -> tys
                                                Nothing  -> panic "usgInfCE: app of non-funty"
                                ucs5        = usgSubTy ty2u ty3u
                          ; return (ASSERT( isUsgTy ty4u )
                                    ty4u,
                                    unionUCSs [ucs1,ucs2,ucs5],
                                    fv1 `plusMS` fv2)
                          }

usgInfCE (Lam v e) | isTyVar v = do { (ty1u,ucs,fv) <- usgInfCE e  -- safe to ignore free v here
                                    ; let (u,ty1) = splitUsgTy ty1u
                                    ; return (mkUsgTy u (mkForAllTy v ty1),
                                              ucs,
                                              fv)
                                    }
                   | otherwise = panic "usgInfCE: missing lambda usage annot"
                     -- if used for checking also, may need to extend this case to
                     -- look in lbvarInfo instead.

usgInfCE (Note (TermUsg u) (Lam v e))
  = ASSERT( not (isTyVar v) )
    do { (ty1u,ucs1,fv) <- usgInfCE e
       ; let ty2u   = varType v
             ucs2   = occChkUConSet v fv
             fv'    = fv `delFromMS` v
             ucs3s  = foldMS (\v _ ucss -> (leqUConSet u ((tyUsg . varType) v)
                                            : ucss))  -- in reverse order!
                             []
                             fv'
       ; return (mkUsgTy u (mkFunTy ty2u ty1u),
                 unionUCSs ([ucs1,ucs2] ++ ucs3s),
                 fv')
       }

usgInfCE (Let bind e0) = do { (ty0u,ucs0,fv0) <- usgInfCE e0
                            ; (ucs1,fv1) <- usgInfBind bind fv0
                            ; return (ASSERT( isUsgTy ty0u )
                                      ty0u,
                                      ucs0 `unionUCS` ucs1,
                                      fv1)
                            }

usgInfCE (Case e0 v0 [(DEFAULT,[],e1)])
  = -- pure strict let, no selection (could be at polymorphic or function type)
    do { (ty0u,ucs0,fv0) <- usgInfCE e0 
       ; (ty1u,ucs1,fv1) <- usgInfCE e1
       ; let (u0,ty0)   = splitUsgTy ty0u
             ucs2       = usgEqTy ty0u (varType v0)  -- messy! but OK
       ; ty4u <- freshannotTy ty1u
       ; let ucs5 = usgSubTy ty1u ty4u
             ucs7 = occChkUConSet v0 (fv1 `plusMS` (unitMS v0))
       ; return (ASSERT( isUsgTy ty4u )
                 ty4u,
                 unionUCSs [ucs0,ucs1,ucs2,ucs5,ucs7],
                 fv0 `plusMS` (fv1 `delFromMS` v0))
       }

usgInfCE expr@(Case e0 v0 alts)
  = -- general case (tycon of scrutinee must be known)
    do { let (cs,vss,es) = unzip3 alts
       ; (ty0u,ucs0,fv0) <- usgInfCE e0 
       ; tuf2s <- mapM usgInfCE es
       ; let (u0,ty0)   = splitUsgTy ty0u
             ucs1       = usgEqTy ty0u (varType v0)  -- messy! but OK
             (tc,ty0ks) = case splitTyConApp_maybe ty0 of
                            Just tcks -> tcks
                            Nothing   -> pprPanic "usgInfCE: weird:" $
                                           vcat [text "scrutinee:" <+> ppr e0,
                                                 text "type:" <+> ppr ty0u]
       ; let (ty2us,ucs2s,fv2s) = unzip3 tuf2s
             ucs3ss = ASSERT2( all isNotUsgTy ty0ks, text "expression" <+> ppr e0 $$ text "has type" <+> ppr ty0u )
                      zipWith (\ c vs -> zipWith (\ty v ->
                                                   usgSubTy (mkUsgTy u0 ty)
                                                            (varType v))
                                                 (caseAltArgs ty0ks c)
                                                 vs)
                              cs
                              vss
       ; ty4u <- freshannotTy (head ty2us) -- assume at least one alt
       ; let ucs5s = zipWith usgSubTy ty2us (repeat ty4u)
             ucs6s = zipWith occChksUConSet vss fv2s
             fv7   = ASSERT( not (null fv2s) && (length fv2s == length vss) )
                     foldl1 maxMS (zipWith (foldl delFromMS) fv2s vss)
             ucs7  = occChkUConSet v0 (fv7 `plusMS` (unitMS v0))
       ; return (ASSERT( isUsgTy ty4u )
                 ty4u,
                 unionUCSs ([ucs0,ucs1] ++ ucs2s
                            ++ (concat ucs3ss)
                            ++ ucs5s
                            ++ ucs6s
                            ++ [ucs7]),
                 fv0 `plusMS` (fv7 `delFromMS` v0))
       }
  where caseAltArgs                 :: [Type] -> Con -> [Type]
        -- compute list of tau-types required by a case-alt
        caseAltArgs tys (DataCon dc) = let rawCTy = dataConType dc
                                           cTy    = ASSERT2( isUnAnnotated rawCTy, (text "caseAltArgs: rawCTy annotated!:" <+> ppr rawCTy <+> text "in" <+> ppr expr) )
                                                    annotManyN rawCTy
                                       in  ASSERT( all isNotUsgTy tys )
                                           map unUsgTy (fst (splitFunTys (applyTys cTy tys)))
        caseAltArgs tys (Literal _)  = []
        caseAltArgs tys DEFAULT      = []
        caseAltArgs tys (PrimOp _)   = panic "caseAltArgs: unexpected PrimOp"

usgInfCE (Note (SCC _)          e) = usgInfCE e

usgInfCE (Note (Coerce ty1 ty0) e)
  = do { (ty2u,ucs2,fv2) <- usgInfCE e
       ; let (u2,ty2) = splitUsgTy ty2u
             ucs3     = usgEqTy ty0 ty2  -- messy but OK
             ty0'     = (annotManyN . unannotTy) ty0  -- really nasty type
             ucs4     = usgEqTy ty0 ty0'
             ucs5     = emptyUConSet
             -- What this says is that a Coerce does the most general possible
             -- annotation to what's inside it (nasty, nasty), because no information
             -- can pass through a Coerce.  It of course simply ignores the info
             -- that filters down through into ty1, because it can do nothing with it.
             -- It does still pass through the topmost usage annotation, though.
       ; return (mkUsgTy u2 ty1,
                 unionUCSs [ucs2,ucs3,ucs4,ucs5],
                 fv2)
       }

usgInfCE (Note InlineCall       e) = usgInfCE e

usgInfCE (Note (TermUsg u)      e) = pprTrace "usgInfCE: ignoring extra TermUsg:" (ppr u) $
                                       usgInfCE e

usgInfCE (Type ty)                 = panic "usgInfCE: unexpected Type"
\end{code}

======================================================================

Helper functions
~~~~~~~~~~~~~~~~

If a variable appears more than once in an fv set, force its usage to be Many.

\begin{code}
occChkUConSet :: IdOrTyVar
              -> VarMultiset
              -> UConSet

occChkUConSet v fv = if occInMS v fv > 1
                     then eqManyUConSet ((tyUsg . varType) v)
                     else emptyUConSet

occChksUConSet :: [IdOrTyVar]
               -> VarMultiset
               -> UConSet

occChksUConSet vs fv = unionUCSs (map (\v -> occChkUConSet v fv) vs)
\end{code}


Subtyping and equal-typing relations.  These generate constraint sets.
Both assume their arguments are annotated correctly, and are either
both tau-types or both sigma-types (in fact, are both exactly the same
shape).

\begin{code}
usgSubTy ty1 ty2  = genUsgCmpTy cmp ty1 ty2
  where cmp u1 u2 = leqUConSet u2 u1
  
usgEqTy  ty1 ty2  = genUsgCmpTy cmp ty1 ty2  -- **NB** doesn't equate tyconargs that
                                             -- don't appear (see below)
  where cmp u1 u2 = eqUConSet u1 u2

genUsgCmpTy :: (UsageAnn -> UsageAnn -> UConSet)  -- constraint (u1 REL u2), respectively
            -> Type
            -> Type
            -> UConSet

genUsgCmpTy cmp (NoteTy (UsgNote u1) ty1) (NoteTy (UsgNote u2) ty2)
  = cmp u1     u2     `unionUCS` genUsgCmpTy cmp ty1 ty2

#ifndef USMANY
-- deal with omitted == UsMany
genUsgCmpTy cmp (NoteTy (UsgNote u1) ty1) ty2
  = cmp u1     UsMany `unionUCS` genUsgCmpTy cmp ty1 ty2
genUsgCmpTy cmp ty1                       (NoteTy (UsgNote u2) ty2)
  = cmp UsMany u2     `unionUCS` genUsgCmpTy cmp ty1 ty2
#endif

genUsgCmpTy cmp (NoteTy (SynNote sty1) ty1) (NoteTy (SynNote sty2) ty2)
  = genUsgCmpTy cmp sty1 sty2 `unionUCS` genUsgCmpTy cmp ty1 ty2
    -- **! is this right? or should I throw away synonyms, or sth else?

-- if SynNote only on one side, throw it out
genUsgCmpTy cmp (NoteTy (SynNote sty1) ty1) ty2
  = genUsgCmpTy cmp ty1 ty2
genUsgCmpTy cmp ty1                         (NoteTy (SynNote sty2) ty2)
  = genUsgCmpTy cmp ty1 ty2

-- ignore FTVNotes
genUsgCmpTy cmp (NoteTy (FTVNote _) ty1) ty2
  = genUsgCmpTy cmp ty1 ty2
genUsgCmpTy cmp ty1                      (NoteTy (FTVNote _) ty2)
  = genUsgCmpTy cmp ty1 ty2

genUsgCmpTy cmp (TyVarTy _) (TyVarTy _)
  = emptyUConSet

genUsgCmpTy cmp (AppTy tya1 tyb1) (AppTy tya2 tyb2)
  = unionUCSs [genUsgCmpTy cmp tya1 tya2,
               genUsgCmpTy cmp tyb1 tyb2,  -- note, *both* ways for arg, since fun (prob) unknown
               genUsgCmpTy cmp tyb2 tyb1]

genUsgCmpTy cmp (TyConApp tc1 ty1s) (TyConApp tc2 ty2s)
  = case tyConArgVrcs_maybe tc1 of
      Just oi -> unionUCSs (zipWith3 (\ ty1 ty2 (occPos,occNeg) ->
                                        -- strictly this is wasteful (and possibly dangerous) for
                                        -- usgEqTy, but I think it's OK.  KSW 1999-04.
                                       (if occPos then genUsgCmpTy cmp ty1 ty2 else emptyUConSet)
                                       `unionUCS`
                                       (if occNeg then genUsgCmpTy cmp ty2 ty1 else emptyUConSet))
                                     ty1s ty2s oi)
      Nothing -> panic ("genUsgCmpTy: variance info unavailable for " ++ showSDoc (ppr tc1))

genUsgCmpTy cmp (FunTy tya1 tyb1) (FunTy tya2 tyb2)
  = genUsgCmpTy cmp tya2 tya1 `unionUCS` genUsgCmpTy cmp tyb1 tyb2  -- contravariance of arrow

genUsgCmpTy cmp (ForAllTy _ ty1) (ForAllTy _ ty2)
  = genUsgCmpTy cmp ty1 ty2

genUsgCmpTy cmp ty1 ty2
  = pprPanic "genUsgCmpTy: type shapes don't match" $
      vcat [ppr ty1, ppr ty2]
\end{code}


Applying a substitution to all @UVar@s.  This also moves @TermUsg@
notes on lambdas into the @lbvarInfo@ field of the binder.  This
latter is a hack.  KSW 1999-04.

\begin{code}
appUSubstTy :: (UVar -> UsageAnn)
            -> Type
            -> Type

appUSubstTy s    (NoteTy      (UsgNote (UsVar uv)) ty)
                                                = mkUsgTy (s uv) (appUSubstTy s ty)
appUSubstTy s    (NoteTy note@(UsgNote _) ty)   = NoteTy note (appUSubstTy s ty)
appUSubstTy s    (NoteTy note@(SynNote _) ty)   = NoteTy note (appUSubstTy s ty)
appUSubstTy s    (NoteTy note@(FTVNote _) ty)   = NoteTy note (appUSubstTy s ty)
appUSubstTy s ty@(TyVarTy _)                    = ty
appUSubstTy s    (AppTy ty1 ty2)                = AppTy (appUSubstTy s ty1) (appUSubstTy s ty2)
appUSubstTy s    (TyConApp tc tys)              = TyConApp tc (map (appUSubstTy s) tys)
appUSubstTy s    (FunTy ty1 ty2)                = FunTy (appUSubstTy s ty1) (appUSubstTy s ty2)
appUSubstTy s    (ForAllTy tyv ty)              = ForAllTy tyv (appUSubstTy s ty)


appUSubstBinds :: (UVar -> UsageAnn)
               -> [CoreBind]
               -> [CoreBind]

appUSubstBinds s binds = fst $ initAnnotM () $
                           genAnnotBinds mungeType mungeTerm binds
  where mungeType _ ty = -- simply perform substitution
                         return (appUSubstTy s ty)

        mungeTerm   (Note (TermUsg (UsVar uv)) (Lam v e))
          -- perform substitution *and* munge annot on lambda into IdInfo.lbvarInfo
          = let lb = case (s uv) of { UsOnce -> IsOneShotLambda; UsMany -> NoLBVarInfo }
                v' = modifyIdInfo v (setLBVarInfo lb)  -- HACK ALERT!
                     -- see comment in IdInfo.lhs; this is because the info is easier to
                     -- access here, by agreement SLPJ/KSW 1999-04 (as a "short-term hack").
            in  return (Lam v' e)
                -- really should be: return (Note (TermUsg (s uv)) (Lam v e))
        mungeTerm e@(Lam _ _)                     = return e
        mungeTerm e                               = panic "appUSubstBinds: mungeTerm:" (ppr e)
\end{code}


A @VarMultiset@ is what it says: a set of variables with counts
attached to them.  We build one out of a @VarEnv@.

\begin{code}
type VarMultiset = VarEnv (IdOrTyVar,Int)  -- I guess 536 870 911 occurrences is enough

emptyMS      =  emptyVarEnv
unitMS v     =  unitVarEnv v (v,1)
delFromMS    =  delVarEnv
plusMS       :: VarMultiset -> VarMultiset -> VarMultiset
plusMS       =  plusVarEnv_C (\ (v,n) (_,m) -> (v,n+m))
maxMS        :: VarMultiset -> VarMultiset -> VarMultiset
maxMS        =  plusVarEnv_C (\ (v,n) (_,m) -> (v,max n m))
mapMS f      =  mapVarEnv (\ (v,n) -> f v n)
foldMS f     =  foldVarEnv (\ (v,n) a -> f v n a)
occInMS v ms =  case lookupVarEnv ms v of
                  Just (_,n) -> n
                  Nothing    -> 0
\end{code}

And a function used in debugging.  It may give false positives with -DUSMANY turned off.

\begin{code}
isUnAnnotated :: Type -> Bool

isUnAnnotated (NoteTy (UsgNote _  ) _ ) = False
isUnAnnotated (NoteTy (SynNote sty) ty) = isUnAnnotated sty && isUnAnnotated ty
isUnAnnotated (NoteTy (FTVNote _  ) ty) = isUnAnnotated ty
isUnAnnotated (TyVarTy _)               = True
isUnAnnotated (AppTy ty1 ty2)           = isUnAnnotated ty1 && isUnAnnotated ty2
isUnAnnotated (TyConApp tc tys)         = all isUnAnnotated tys
isUnAnnotated (FunTy ty1 ty2)           = isUnAnnotated ty1 && isUnAnnotated ty2
isUnAnnotated (ForAllTy tyv ty)         = isUnAnnotated ty
\end{code}

======================================================================

EOF
