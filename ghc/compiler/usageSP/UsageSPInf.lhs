%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[UsageSPInf]{UsageSP Inference Engine}

This code is (based on) PhD work of Keith Wansbrough <kw217@cl.cam.ac.uk>,
September 1998 .. May 1999.

Keith Wansbrough 1998-09-04..1999-07-06

\begin{code}
module UsageSPInf ( doUsageSPInf ) where

#include "HsVersions.h"

import UsageSPUtils
import UsageSPLint
import UConSet

import CoreSyn
import TypeRep          ( Type(..), TyNote(..) ) -- friend
import Type             ( UsageAnn(..),
                          applyTy, applyTys,
                          splitFunTy_maybe, splitFunTys, splitTyConApp_maybe,
                          mkUsgTy, splitUsgTy, isUsgTy, isNotUsgTy, unUsgTy, tyUsg,
                          splitUsForAllTys, substUsTy,
                          mkFunTy, mkForAllTy )
import PprType		( {- instance Outputable Type -} )
import TyCon            ( tyConArgVrcs_maybe, isFunTyCon )
import Literal          ( Literal(..), literalType )
import Var              ( Var, UVar, varType, setVarType, mkUVar, modifyIdInfo )
import IdInfo           ( setLBVarInfo, LBVarInfo(..) )
import Id               ( mayHaveNoBinding, isExportedId )
import Name             ( isLocallyDefined )
import VarEnv
import VarSet
import UniqSupply       ( UniqSupply, UniqSM,
                          initUs, splitUniqSupply )
import Outputable
import Maybes           ( expectJust )
import List             ( unzip4 )
import CmdLineOpts	( opt_D_dump_usagesp, opt_DoUSPLinting )
import ErrUtils		( doIfSet, dumpIfSet )
import PprCore          ( pprCoreBindings )
\end{code}

======================================================================

-- **!  wasn't I going to do something about not requiring annotations
-- to be correct on unpointed types and/or those without haskell pointers
-- inside?

The whole inference
~~~~~~~~~~~~~~~~~~~

For full details, see _Once Upon a Polymorphic Type_, University of
Glasgow Department of Computing Science Technical Report TR-1998-19,
December 1998, or the summary in POPL'99.

[** NEW VERSION NOW IMPLEMENTED; different from the papers
    above. Hopefully to appear in PLDI'00, and Keith Wansbrough's
    University of Cambridge PhD thesis, c. Sep 2000 **]


Inference is performed as follows:

  1.  Remove all manipulable[*] annotations.

  2.  Walk over the resulting term adding fresh UVar annotations,
      applying the type rules and collecting the constraints.

  3.  Find the solution to the constraints and apply the substitution
      to the annotations, leaving a @UVar@-free term.

[*] A manipulable annotation is one derived from the current source
module, as opposed to one derived from an import, which we are clearly
not allowed to alter.

As in the paper, a ``tau-type'' is a type that does *not* have an
annotation on top (although it may have some inside), and a
``sigma-type'' is one that does (i.e., is a tau-type with an
annotation added).  Also, a ``rho-type'' is one that may have initial
``\/u.''s.  This conflicts with the totally unrelated usage of these
terms in the remainder of GHC.  Caveat lector!  KSW 1999-07.


The inference is done over a set of @CoreBind@s, and inside the IO
monad.

\begin{code}
doUsageSPInf :: UniqSupply
             -> [CoreBind]
             -> IO [CoreBind]

doUsageSPInf us binds = do
                           let binds1      = doUnAnnotBinds binds

                           dumpIfSet opt_D_dump_usagesp "UsageSPInf unannot'd" $
                             pprCoreBindings binds1

                           let ((binds2,ucs,_),_)
                                      = initUs us (uniqSMMToUs (usgInfBinds emptyVarEnv binds1))

                           dumpIfSet opt_D_dump_usagesp "UsageSPInf annot'd" $
                             pprCoreBindings binds2

                           let ms     = solveUCS ucs
                               s      = case ms of
                                          Just s  -> s
                                          Nothing -> panic "doUsageSPInf: insol. conset!"
                               binds3 = appUSubstBinds s binds2

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

Inference takes an annotated (rho-typed) environment and an expression
unannotated except for variables not appearing in the environment.  It
returns an annotated expression, a type, a constraint set, and a
multiset of free variables.  It is in the unique supply monad, which
supplies fresh uvars for annotation.

We conflate usage metavariables and usage variables; the latter are
distinguished by falling within the scope of a usage binder.

\begin{code}
usgInfBinds :: VarEnv Var            -- incoming environment (usu. empty)
            -> [CoreBind]            -- CoreBinds in dependency order
            -> UniqSMM ([CoreBind],  -- annotated CoreBinds
                        UConSet,     -- constraint set
                        VarMultiset) -- usage of environment vars

usgInfBinds ve []
  = return ([],
            emptyUConSet,
            emptyMS)

usgInfBinds ve (b0:b0s)
-- (this clause is almost the same as the Let clause)
  = do (v1s,ve1,b1,h1,fb1,fa1) <- usgInfBind  ve  b0
       (b2s,h2,f2)             <- usgInfBinds ve1 b0s
       let h3 = occChksUConSet v1s (fb1 `plusMS` f2)
       return (b1:b2s,
               unionUCSs [h1,h2,h3],
               fa1 `plusMS` (f2 `delsFromMS` v1s))


usgInfBind :: VarEnv Var
           -> CoreBind               -- CoreBind to infer for
           -> UniqSMM ([Var],        -- variables bound
                       VarEnv Var,   -- extended VarEnv
                       CoreBind,     -- annotated CoreBind
                       UConSet,      -- constraints generated by this CoreBind
                       VarMultiset,  -- this bd's use of vars bound in this bd
                                     --   (could be anything for other vars)
                       VarMultiset)  -- this bd's use of other vars

usgInfBind ve (NonRec v1 e1) 
  = do (v1',y1u) <- annotVar v1
       (e2,y2u,h2,f2) <- usgInfCE (extendVarEnv ve v1 v1') e1
       let h3        = usgSubTy y2u y1u
           h4        = h2 `unionUCS` h3
           (y4r,h4') = usgClos ve y2u h4
           v1''      = setVarType v1 y4r
           h5        = if isExportedId v1 then pessimise y4r else emptyUConSet
       return ([v1''],
               extendVarEnv ve v1 v1'',
               NonRec v1'' e2,
               h4' `unionUCS` h5,
               emptyMS,
               f2)

usgInfBind ve (Rec ves)
  = do let (v1s,e1s) = unzip ves
       vy1s' <- mapM annotVar v1s
       let (v1s',y1us) = unzip vy1s'
           ve'  = ve `plusVarEnv` (zipVarEnv v1s v1s')
       eyhf2s <- mapM (usgInfCE ve') e1s
       let (e2s,y2us,h2s,f2s) = unzip4 eyhf2s
           h3s         = zipWith usgSubTy y2us y1us
           h4s         = zipWith unionUCS h2s h3s
           yh4s        = zipWith (usgClos ve) y2us h4s
           (y4rs,h4s') = unzip yh4s
           v1s''       = zipWith setVarType v1s y4rs
           f5          = foldl plusMS emptyMS f2s
           h6s         = zipWith (\ v y -> if isExportedId v then pessimise y else emptyUConSet)
                                 v1s y4rs
       return (v1s'',
               ve `plusVarEnv` (zipVarEnv v1s v1s''),
               Rec (zip v1s'' e2s),
               unionUCSs (h4s' ++ h6s),
               f5,
               f5 `delsFromMS` v1s')  -- we take pains that v1'==v1'' etc


usgInfCE :: VarEnv Var               -- unannotated -> annotated vars
         -> CoreExpr                 -- expression to annotate / infer
         -> UniqSMM (CoreExpr,       -- annotated expression        (e)
                     Type,           -- (sigma) type of expression  (y)(u=sigma)(r=rho)
                     UConSet,        -- set of constraints arising  (h)
                     VarMultiset)    -- variable occurrences        (f)

usgInfCE ve e0@(Var v) | isTyVar v
  = panic "usgInfCE: unexpected TyVar"
                       | otherwise
  = do v' <- instVar (lookupVar ve v)
       return $ ASSERT( isUsgTy (varType v' {-'cpp-}) )
                (Var v',
                 varType v',
                 emptyUConSet,
                 unitMS v')

usgInfCE ve e0@(Lit lit)
  = do u1 <- newVarUSMM (Left e0)
       return (e0,
               mkUsgTy u1 (literalType lit),
               emptyUConSet,
               emptyMS)

{-  ------------------------------------
	No Con form now; we rely on usage information in the constructor itself
	
usgInfCE ve e0@(Con con args)
  = -- constant or primop.  guaranteed saturated.
    do let (ey1s,e1s) = span isTypeArg args
       y1s <- mapM (\ (Type ty) -> annotTyN (Left e0) ty) ey1s  -- univ. + exist.
       (y2us,y2u) <- case con of
                         DataCon c -> do u2 <- newVarUSMM (Left e0)
                                         return $ dataConTys c u2 y1s
                                         -- y1s is exdicts + args
                         PrimOp  p -> return $ primOpUsgTys p y1s
                         otherwise -> panic "usgInfCE: unrecognised Con"
       eyhf3s <- mapM (usgInfCE ve) e1s
       let (e3s,y3us,h3s,f3s) = unzip4 eyhf3s
           h4s = zipWith usgSubTy y3us y2us
       return $ ASSERT( isUsgTy y2u )
                (Con con (map Type y1s ++ e3s),
                 y2u,
                 unionUCSs (h3s ++ h4s),
                 foldl plusMS emptyMS f3s)

  whered ataConTys c u y1s
        -- compute argtys of a datacon
          = let cTy        = annotMany (dataConType c)  -- extra (sigma) annots later replaced
                (y2us,y2u) = splitFunTys (applyTys cTy y1s)
                             -- safe 'cos a DataCon always returns a value of type (TyCon tys),
                             -- not an arrow type.
                reUsg      = mkUsgTy u . unUsgTy
             in (map reUsg y2us, reUsg y2u)
--------------------------------------------  -}


usgInfCE ve e0@(App ea (Type yb))
  = do (ea1,ya1u,ha1,fa1) <- usgInfCE ve ea
       let (u1,ya1) = splitUsgTy ya1u
       yb1 <- annotTyN (Left e0) yb
       return (App ea1 (Type yb1),
               mkUsgTy u1 (applyTy ya1 yb1),
               ha1,
               fa1)

usgInfCE ve (App ea eb)
  = do (ea1,ya1u,ha1,fa1) <- usgInfCE ve ea
       let ( u1,ya1) = splitUsgTy ya1u
           (y2u,y3u) = expectJust "usgInfCE:App" $ splitFunTy_maybe ya1
       (eb1,yb1u,hb1,fb1) <- usgInfCE ve eb
       let h4 = usgSubTy yb1u y2u
       return $ ASSERT( isUsgTy y3u )
                (App ea1 eb1,
                 y3u,
                 unionUCSs [ha1,hb1,h4],
                 fa1 `plusMS` fb1)

usgInfCE ve e0@(Lam v0 e) | isTyVar v0
  = do (e1,y1u,h1,f1) <- usgInfCE ve e
       let (u1,y1) = splitUsgTy y1u
       return (Lam v0 e1,
               mkUsgTy u1 (mkForAllTy v0 y1),
               h1,
               f1)

                     -- [OLD COMMENT:]
                     -- if used for checking also, may need to extend this case to
                     -- look in lbvarInfo instead.
                          | otherwise
  = do u1  <- newVarUSMM (Left e0)
       (v1,y1u) <- annotVar v0
       (e2,y2u,h2,f2) <- usgInfCE (extendVarEnv ve v0 v1) e
       let h3  = occChkUConSet v1 f2
           f2' = f2 `delFromMS` v1
           h4s = foldMS (\ v _ hs -> (leqUConSet u1 ((tyUsg . varType . lookupVar ve) v)
                                      : hs))  -- in reverse order!
                        []
                        f2'
       return (Note (TermUsg u1) (Lam v1 e2),  -- add annot for lbVarInfo computation
               mkUsgTy u1 (mkFunTy y1u y2u),
               unionUCSs (h2:h3:h4s),
               f2')

usgInfCE ve (Let b0s e0)
  = do (v1s,ve1,b1s,h1,fb1,fa1) <- usgInfBind ve b0s
       (e2,y2u,h2,f2)           <- usgInfCE ve1 e0
       let h3 = occChksUConSet v1s (fb1 `plusMS` f2)
       return $ ASSERT( isUsgTy y2u )
                (Let b1s e2,
                 y2u,
                 unionUCSs [h1,h2,h3],
                 fa1 `plusMS` (f2 `delsFromMS` v1s))

usgInfCE ve (Case e0 v0 [(DEFAULT,[],e1)])
-- pure strict let, no selection (could be at polymorphic or function type)
  = do (v1,y1u) <- annotVar v0
       (e2,y2u,h2,f2) <- usgInfCE ve e0
       (e3,y3u,h3,f3) <- usgInfCE (extendVarEnv ve v0 v1) e1
       let h4 = usgEqTy y2u y1u -- **! why not subty?
           h5 = occChkUConSet v1 f3
       return $ ASSERT( isUsgTy y3u )
                (Case e2 v1 [(DEFAULT,[],e3)],
                 y3u,
                 unionUCSs [h2,h3,h4,h5],
                 f2 `plusMS` (f3 `delFromMS` v1))
 
usgInfCE ve e0@(Case e1 v1 alts)
-- general case (tycon of scrutinee must be known)
-- (assumes well-typed already; so doesn't check constructor)
  = do (v2,y1u) <- annotVar v1
       (e2,y2u,h2,f2) <- usgInfCE ve e1
       let h3       = usgEqTy y2u y1u -- **! why not subty?
           (u2,y2)  = splitUsgTy y2u
           (tc,y2s) = expectJust "usgInfCE:Case" $ splitTyConApp_maybe y2
           (cs,v1ss,es) = unzip3 alts
           v2ss     = map (map (\ v -> setVarType v (mkUsgTy u2 (annotManyN (varType v)))))
                          v1ss
           ve3      = extendVarEnv ve v1 v2
       eyhf4s <- mapM (\ (v1s,v2s,e) -> usgInfCE (ve3 `plusVarEnv` (zipVarEnv v1s v2s)) e)
                      (zip3 v1ss v2ss es)
       let (e4s,y4us,h4s,f4s) = unzip4 eyhf4s
       y5u <- annotTy (Left e0) (unannotTy (head y4us))
       let h5s      = zipWith usgSubTy y4us (repeat y5u)
           h6s      = zipWith occChksUConSet v2ss f4s
           f4       = foldl1 maxMS (zipWith delsFromMS f4s v2ss)
           h7       = occChkUConSet v2 (f4 `plusMS` (unitMS v2))
       return $ ASSERT( isUsgTy y5u )
                (Case e2 v2 (zip3 cs v2ss e4s),
                 y5u,
                 unionUCSs (h2:h3:h7:(h4s ++ h5s ++ h6s)),
                 f2 `plusMS` (f4 `delFromMS` v2))

usgInfCE ve e0@(Note note ea)
  = do (e1,y1u,h1,f1) <- usgInfCE ve ea
       case note of
         Coerce yb ya -> do let (u1,y1) = splitUsgTy y1u
                                ya3 = annotManyN ya   -- really nasty type
                                h3  = usgEqTy y1 ya3  -- messy but OK
                            yb3 <- annotTyN (Left e0) yb
             -- What this says is that a Coerce does the most general possible
             -- annotation to what's inside it (nasty, nasty), because no information
             -- can pass through a Coerce.  It of course simply ignores the info
             -- that filters down through into ty1, because it can do nothing with it.
             -- It does still pass through the topmost usage annotation, though.
                            return (Note (Coerce yb3 ya3) e1,
                                    mkUsgTy u1 yb3,
                                    unionUCSs [h1,h3],
                                    f1)

         SCC _      -> return (Note note e1, y1u, h1, f1)

         InlineCall -> return (Note note e1, y1u, h1, f1)

         InlineMe   -> return (Note note e1, y1u, h1, f1)

         TermUsg _  -> pprPanic "usgInfCE:Note TermUsg" $ ppr e0

usgInfCE ve e0@(Type _)
  = pprPanic "usgInfCE:Type" $ ppr e0
\end{code}


\begin{code}
lookupVar :: VarEnv Var -> Var -> Var
-- if variable in VarEnv then return annotated version,
-- otherwise it's imported and already annotated so leave alone.
--lookupVar ve v = error "lookupVar unimplemented"
lookupVar ve v = case lookupVarEnv ve v of
                   Just v' -> v'
                   Nothing -> ASSERT( not (isLocallyDefined v) || (mayHaveNoBinding v) )
                              ASSERT( isUsgTy (varType v) )
                              v

instVar :: Var -> UniqSMM Var
-- instantiate variable with rho-type, giving it a fresh sigma-type
instVar v = do let (uvs,ty) = splitUsForAllTys (varType v)
               case uvs of
                 [] -> return v
                 _  -> do uvs' <- mapM (\_ -> newVarUSMM (Left (Var v))) uvs
                          let ty' = substUsTy (zipVarEnv uvs uvs') ty
                          return (setVarType v ty')

annotVar :: Var -> UniqSMM (Var,Type)
-- freshly annotates a variable and returns it along with its new type
annotVar v = do y1u <- annotTy (Left (Var v)) (varType v)
                return (setVarType v y1u, y1u)
\end{code}


The closure operation, which does the generalisation at let bindings.

\begin{code}
usgClos :: VarEnv Var        -- environment to close with respect to
        -> Type              -- type to close (sigma)
        -> UConSet           -- constraint set to reduce
        -> (Type,            -- closed type (rho)
            UConSet)         -- residual constraint set

usgClos zz_ve ty ucs = (ty,ucs)  -- dummy definition; no generalisation at all

            -- hmm!  what if it sets some uvars to 1 or omega?
            --  (should it do substitution here, or return a substitution,
            --   or should it leave all that work to the end and just use
            --   an "=" constraint here for now?)
\end{code}

The pessimise operation, which generates constraints to pessimise an
id (applied to exported ids, to ensure that they have fully general
types, since we don't know how they will be used in other modules).

\begin{code}
pessimise :: Type -> UConSet

pessimise ty
  = pess True emptyVarEnv ty

  where
    pess :: Bool -> UVarSet -> Type -> UConSet
    pess co ve     (NoteTy (UsgForAll uv) ty)
      = pess co (ve `extendVarSet` uv) ty
    pess co ve ty0@(NoteTy (UsgNote u)    ty)
      = pessN co ve ty `unionUCS`
          (case (co,u) of
             (False,_       ) -> emptyUConSet
             (True ,UsMany  ) -> emptyUConSet
             (True ,UsOnce  ) -> pprPanic "pessimise: can't force:" (ppr ty0)
             (True ,UsVar uv) -> if uv `elemVarSet` ve
                                 then emptyUConSet  -- if bound by \/u, no need to pessimise
                                 else eqManyUConSet u)
    pess _  _  ty0
      = pprPanic "pessimise: missing annot:" (ppr ty0)

    pessN :: Bool -> UVarSet -> Type -> UConSet
    pessN co ve     (NoteTy (UsgForAll uv) ty) = pessN co (ve `extendVarSet` uv) ty
    pessN co ve ty0@(NoteTy (UsgNote _)    _ ) = pprPanic "pessimise: unexpected annot:" (ppr ty0)
    pessN co ve     (NoteTy (SynNote sty)  ty) = pessN co ve sty `unionUCS` pessN co ve ty
    pessN co ve     (NoteTy (FTVNote _)    ty) = pessN co ve ty
    pessN co ve     (TyVarTy _)                = emptyUConSet
    pessN co ve     (AppTy _ _)                = emptyUConSet
    pessN co ve     (TyConApp tc tys)          = ASSERT( not((isFunTyCon tc)&&(length tys > 1)) )
                                                 emptyUConSet
    pessN co ve     (FunTy ty1 ty2)            = pess (not co) ve ty1 `unionUCS` pess co ve ty2
    pessN co ve     (ForAllTy _ ty)            = pessN co ve ty
\end{code}



======================================================================

Helper functions
~~~~~~~~~~~~~~~~

If a variable appears more than once in an fv set, force its usage to be Many.

\begin{code}
occChkUConSet :: Var
              -> VarMultiset
              -> UConSet

occChkUConSet v fv = if occInMS v fv > 1
                     then ASSERT2( isUsgTy (varType v), ppr v )
                          eqManyUConSet ((tyUsg . varType) v)
                     else emptyUConSet

occChksUConSet :: [Var]
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
                v' = modifyIdInfo (`setLBVarInfo` lb) v  -- HACK ALERT!
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
type VarMultiset = VarEnv (Var,Int)  -- I guess 536 870 911 occurrences is enough

emptyMS      =  emptyVarEnv
unitMS v     =  unitVarEnv v (v,1)
delFromMS    =  delVarEnv
delsFromMS   =  delVarEnvList
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
