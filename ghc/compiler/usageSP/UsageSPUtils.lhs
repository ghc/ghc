%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[UsageSPUtils]{UsageSP Utilities}

This code is (based on) PhD work of Keith Wansbrough <kw217@cl.cam.ac.uk>,
September 1998 .. May 1999.

Keith Wansbrough 1998-09-04..1999-07-07

\begin{code}
module UsageSPUtils ( AnnotM(AnnotM), initAnnotM,
                      genAnnotBinds,
                      MungeFlags(isSigma,isLocal,isExp,hasUsg,mfLoc),

                      doAnnotBinds, doUnAnnotBinds,
                      annotTy, annotTyN, annotMany, annotManyN, unannotTy, freshannotTy,

                      newVarUs, newVarUSMM,
                      UniqSMM, usToUniqSMM, uniqSMMToUs,

                      primOpUsgTys,
                    ) where

#include "HsVersions.h"

import CoreSyn
import Literal          ( Literal(..) )
import Var              ( Var, varName, varType, setVarType, mkUVar )
import Id               ( mayHaveNoBinding, isExportedId )
import Name             ( isLocallyDefined )
import TypeRep          ( Type(..), TyNote(..) )  -- friend
import Type             ( UsageAnn(..), isUsgTy, splitFunTys )
import PprType		( {- instance Outputable Type -} )
import Subst		( substTy, mkTyVarSubst )
import TyCon            ( isAlgTyCon, isPrimTyCon, isSynTyCon, isFunTyCon )
import VarEnv
import PrimOp           ( PrimOp, primOpUsg )
import Maybes           ( expectJust )
import UniqSupply       ( UniqSupply, UniqSM, initUs, getUniqueUs, thenUs, returnUs )
import Outputable
import PprCore          ( )  -- instances only
\end{code}

======================================================================

Walking over (and altering) types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We often need to fiddle with (i.e., add or remove) usage annotations
on a type.  We define here a general framework to do this.  Usage
annotations come from any monad with a function @getAnnM@ which yields
a new annotation.  We use two mutually recursive functions, one for
sigma types and one for tau types.

\begin{code}
genAnnotTy :: Monad m =>
              (m UsageAnn)  -- get new annotation
           -> Type          -- old type
           -> m Type        -- new type

genAnnotTy getAnnM ty = do { u   <- getAnnM
                           ; ty' <- genAnnotTyN getAnnM ty
                           ; return (NoteTy (UsgNote u) ty')
                           }

genAnnotTyN :: Monad m =>
               (m UsageAnn)
            -> Type
            -> m Type

genAnnotTyN getAnnM
  (NoteTy (UsgNote _) ty)     = panic "genAnnotTyN: unexpected UsgNote"
genAnnotTyN getAnnM
  (NoteTy (SynNote sty) ty)   = do { sty' <- genAnnotTyN getAnnM sty
                                -- is this right? shouldn't there be some
                                -- correlation between sty' and ty'?
                                -- But sty is a TyConApp; does this make it safer?
                                   ; ty'  <- genAnnotTyN getAnnM ty
                                   ; return (NoteTy (SynNote sty') ty')
                                   }
genAnnotTyN getAnnM
  (NoteTy fvn@(FTVNote _) ty) = do { ty' <- genAnnotTyN getAnnM ty
                                   ; return (NoteTy fvn ty')
                                   }

genAnnotTyN getAnnM
  ty0@(TyVarTy _)             = do { return ty0 }

genAnnotTyN getAnnM
  (AppTy ty1 ty2)             = do { ty1' <- genAnnotTyN getAnnM ty1
                                   ; ty2' <- genAnnotTyN getAnnM ty2
                                   ; return (AppTy ty1' ty2')
                                   }

genAnnotTyN getAnnM
  (TyConApp tc tys)           = ASSERT( isFunTyCon tc || isAlgTyCon tc || isPrimTyCon tc || isSynTyCon tc )
                                do { let gAT = if isFunTyCon tc
                                               then genAnnotTy  -- sigma for partial apps of (->)
                                               else genAnnotTyN -- tau otherwise
                                   ; tys' <- mapM (gAT getAnnM) tys
                                   ; return (TyConApp tc tys')
                                   }

genAnnotTyN getAnnM
  (FunTy ty1 ty2)             = do { ty1' <- genAnnotTy getAnnM ty1
                                   ; ty2' <- genAnnotTy getAnnM ty2
                                   ; return (FunTy ty1' ty2')
                                   }

genAnnotTyN getAnnM
  (ForAllTy v ty)             = do { ty' <- genAnnotTyN getAnnM ty
                                   ; return (ForAllTy v ty')
                                   }
\end{code}



Walking over (and retyping) terms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We also often need to play with the types in a term.  This is slightly
tricky because of redundancy: we want to change binder types, and keep
the bound types matching these; then there's a special case also with
non-locally-defined bound variables.  We generalise over all this
here.

The name `annot' is a bit of a misnomer, as the code is parameterised
over exactly what it does to the types (and certain terms).  Notice
also that it is possible for this parameter to use
monadically-threaded state: here called `flexi'.  For genuine
annotation, this state will be a UniqSupply.

We may add annotations to the outside of a (term, not type) lambda; a
function passed to @genAnnotBinds@ does this, taking the lambda and
returning the annotated lambda.  It is inside the @AnnotM@ monad.
This term-munging function is applied when we see either a term lambda
or a usage annotation; *IMPORTANT:* it is applied *before* we recurse
down into the term, and it is expected to work only at the top level.
Recursion will subsequently be done by genAnnotBinds.  It may
optionally remove a Note TermUsg, or optionally add one if it is not
already present, but it may perform NO OTHER MODIFICATIONS to the
structure of the term.

We do different things to types of variables bound locally and of
variables bound in other modules, in certain cases: the former get
uvars and the latter keep their existing annotations when we annotate,
for example.  To control this, @MungeFlags@ describes what kind of a
type this is that we're about to munge.

\begin{code}
data MungeFlags = MungeFlags { isSigma :: Bool,  -- want annotated on top (sigma type)
                               isLocal :: Bool,  -- is locally-defined type
                               hasUsg  :: Bool,  -- has fixed usage info, don't touch
                               isExp   :: Bool,  -- is exported (and must be pessimised)
                               mfLoc   :: SDoc   -- location info
                             }

tauTyMF loc  = MungeFlags { isSigma = False, isLocal = True,
                            hasUsg = False,  isExp = False,  mfLoc = loc }
sigVarTyMF v = MungeFlags { isSigma = True,  isLocal = hasLocalDef v, 
                            hasUsg = hasUsgInfo v, isExp = isExportedId v,
                            mfLoc = ptext SLIT("type of binder") <+> ppr v }
\end{code}

The helper functions @tauTyMF@ and @sigVarTyMF@ create @MungeFlags@
for us.  @sigVarTyMF@ checks the variable to see how to set the flags.

@hasLocalDef@ tells us if the given variable has an actual local
definition that we can play with.  This is not quite the same as
@isLocallyDefined@, since @mayHaveNoBindingId@ things (usually) don't have
a local definition - the simplifier will inline whatever their
unfolding is anyway.  We treat these as if they were externally
defined, since we don't have access to their definition (at least not
easily).  This doesn't hurt much, since after the simplifier has run
the unfolding will have been inlined and we can access the unfolding
directly.

@hasUsgInfo@, on the other hand, says if the variable already has
usage info in its type that must at all costs be preserved.  This is
assumed true (exactly) of all imported ids.

\begin{code}
hasLocalDef :: Var -> Bool
hasLocalDef var = isLocallyDefined var
                  && not (mayHaveNoBinding var)

hasUsgInfo :: Var -> Bool
hasUsgInfo var = (not . isLocallyDefined) var
\end{code}

Here's the walk itself.

\begin{code}
genAnnotBinds :: (MungeFlags -> Type -> AnnotM flexi Type)
              -> (CoreExpr -> AnnotM flexi CoreExpr)       -- see caveats above
              -> [CoreBind]
              -> AnnotM flexi [CoreBind]

genAnnotBinds _ _ []     = return []

genAnnotBinds f g (b:bs) = do { (b',vs,vs') <- genAnnotBind f g b
                              ; bs' <- withAnnVars vs vs' $
                                         genAnnotBinds f g bs
                              ; return (b':bs')
                              }

genAnnotBind :: (MungeFlags -> Type -> AnnotM flexi Type)  -- type-altering function
             -> (CoreExpr -> AnnotM flexi CoreExpr)        -- term-altering function
             -> CoreBind                          -- original CoreBind
             -> AnnotM flexi
                       (CoreBind,                 -- annotated CoreBind
                        [Var],              -- old variables, to be mapped to...
                        [Var])              -- ... new variables

genAnnotBind f g (NonRec v1 e1) = do { v1' <- genAnnotVar f v1
                                     ; e1' <- genAnnotCE f g e1
                                     ; return (NonRec v1' e1', [v1], [v1'])
                                     }

genAnnotBind f g (Rec ves)      = do { let (vs,es) = unzip ves
                                     ; vs' <- mapM (genAnnotVar f) vs
                                     ; es' <- withAnnVars vs vs' $
                                                mapM (genAnnotCE f g) es
                                     ; return (Rec (zip vs' es'), vs, vs')
                                     }

genAnnotCE :: (MungeFlags -> Type -> AnnotM flexi Type)  -- type-altering function
           -> (CoreExpr -> AnnotM flexi CoreExpr)        -- term-altering function
           -> CoreExpr                             -- original expression
           -> AnnotM flexi CoreExpr                -- yields new expression

genAnnotCE mungeType mungeTerm = go
  where go e0@(Var v) | isTyVar v    = return e0  -- arises, e.g., as tyargs of constructor
                                                  -- (no it doesn't: (Type (TyVar tyvar))
                      | otherwise    = do { mv' <- lookupAnnVar v
                                          ; v'  <- case mv' of
                                                     Just var -> return var
                                                     Nothing  -> fixedVar v
                                          ; return (Var v')
                                          }

        go (Lit l)                   = -- we know it's saturated
                                       return (Lit l)

        go (App e arg)               = do { e' <- go e
                                          ; arg' <- go arg
                                          ; return (App e' arg')
                                          }

        go e0@(Lam v0 _)              = do { e1 <- (if isTyVar v0 then return else mungeTerm) e0
                                          ; let (v,e2,wrap)
                                                  = case e1 of  -- munge may have added note
                                                      Note tu@(TermUsg _) (Lam v e2)
                                                               -> (v,e2,Note tu)
                                                      Lam v e2 -> (v,e2,id)
                                          ; v' <- genAnnotVar mungeType v
                                          ; e' <- withAnnVar v v' $ go e2
                                          ; return (wrap (Lam v' e'))
                                          }

        go (Let bind e)              = do { (bind',vs,vs') <- genAnnotBind mungeType mungeTerm bind
                                          ; e' <- withAnnVars vs vs' $ go e
                                          ; return (Let bind' e')
                                          }

        go (Case e v alts)           = do { e' <- go e
                                          ; v' <- genAnnotVar mungeType v
                                          ; alts' <- withAnnVar v v' $ mapM genAnnotAlt alts
                                          ; return (Case e' v' alts')
                                          }

        go (Note scc@(SCC _)      e) = do { e' <- go e
                                          ; return (Note scc e')
                                          }
        go e0@(Note (Coerce ty1 ty0)
                                  e) = do { ty1' <- mungeType
                                                      (tauTyMF (ptext SLIT("coercer of")
                                                                <+> ppr e0)) ty1
                                          ; ty0' <- mungeType
                                                      (tauTyMF (ptext SLIT("coercee of")
                                                                <+> ppr e0)) ty0
                                                 -- (Better to specify ty0'
                                                 --  identical to the type of e, including
                                                 --  annotations, right at the beginning, but
                                                 --  not possible at this point.)
                                          ; e' <- go e
                                          ; return (Note (Coerce ty1' ty0') e')
                                          }
        go (Note InlineCall       e) = do { e' <- go e
                                          ; return (Note InlineCall e')
                                          }
        go (Note InlineMe         e) = do { e' <- go e
                                          ; return (Note InlineMe e')
                                          }
        go e0@(Note (TermUsg _)   _) = do { e1 <- mungeTerm e0
                                          ; case e1 of  -- munge may have removed note
                                              Note tu@(TermUsg _) e2 -> do { e3 <- go e2
                                                                           ; return (Note tu e3)
                                                                           }
                                              e2                     -> go e2
                                          }

        go e0@(Type ty)              = -- should only occur at toplevel of Arg,
                                       -- hence tau-type
                                       do { ty' <- mungeType
                                                     (tauTyMF (ptext SLIT("tyarg")
                                                               <+> ppr e0)) ty
                                          ; return (Type ty')
                                          }

        fixedVar v = ASSERT2( not (hasLocalDef v), text "genAnnotCE: locally defined var" <+> ppr v <+> text "not in varenv" )
                     genAnnotVar mungeType v

        genAnnotAlt (c,vs,e)         = do { vs' <- mapM (genAnnotVar mungeType) vs
                                          ; e' <- withAnnVars vs vs' $ go e
                                          ; return (c, vs', e')
                                          }


genAnnotVar :: (MungeFlags -> Type -> AnnotM flexi Type)
            -> Var
            -> AnnotM flexi Var

genAnnotVar mungeType v | isTyVar v = return v
                        | otherwise = do { vty' <- mungeType (sigVarTyMF v) (varType v)
                                         ; return (setVarType v vty')
                                         }
{- ifdef DEBUG
                                         ; return $
                                             pprTrace "genAnnotVar" (ppr (tyUsg vty') <+> ppr v) $
                                             (setVarType v vty')
   endif
 -}
\end{code}

======================================================================

Some specific things to do to types inside terms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@annotTyM@ annotates a type with fresh uvars everywhere the inference
is allowed to go, and leaves alone annotations where it may not go.

We assume there are no annotations already.

\begin{code}
annotTyM :: MungeFlags -> Type -> AnnotM UniqSupply Type
-- general function
annotTyM mf ty = uniqSMtoAnnotM . uniqSMMToUs $
                   case (hasUsg mf, isLocal mf, isSigma mf) of
                     (True ,_    ,_    ) -> ASSERT( isUsgTy ty )
                                            return ty
                     (False,True ,True ) -> if isExp mf then
                                              annotTyP (tag 'p') ty
                                            else
                                              annotTy (tag 's') ty
                     (False,True ,False) -> annotTyN (tag 't') ty
                     (False,False,True ) -> return $ annotMany  ty -- assume worst
                     (False,False,False) -> return $ annotManyN ty
  where tag c = Right $ "annotTyM:" ++ [c] ++ ": " ++ showSDoc (ppr ty)

-- specific functions for annotating tau and sigma types

-- ...with uvars
annotTy  tag = genAnnotTy  (newVarUSMM tag)
annotTyN tag = genAnnotTyN (newVarUSMM tag)

-- ...with uvars and pessimal Manys (for exported ids)
annotTyP tag ty = do { ty' <- annotTy tag ty ; return (pessimise True ty') }

-- ...with Many
annotMany, annotManyN :: Type -> Type
#ifndef USMANY
annotMany  = id
annotManyN = id
#else
annotMany  ty = unId (genAnnotTy  (return UsMany) ty)
annotManyN ty = unId (genAnnotTyN (return UsMany) ty)
#endif

-- monad required for the above
newtype Id a = Id a ; unId (Id a) = a
instance Monad Id where { a >>= f  = f (unId a) ; return a = Id a }

-- lambda-annotating function for use along with the above
annotLam e0@(Lam v e) = do { uv <- uniqSMtoAnnotM $ newVarUs (Left e0)
                           ; return (Note (TermUsg uv) (Lam v e))
                           }
annotLam (Note (TermUsg _) _) = panic "annotLam: unexpected term usage annot"
\end{code}

The above requires a `pessimising' translation.  This is applied to
types of exported ids, and ensures that they have a fully general
type (since we don't know how they will be used in other modules).

\begin{code}
pessimise :: Bool -> Type -> Type

#ifndef USMANY
pessimise  co ty0@(NoteTy  usg@(UsgNote u  ) ty)
  = if co
    then case u of UsMany  -> pty
                   UsVar _ -> pty  -- force to UsMany
                   UsOnce  -> pprPanic "pessimise:" (ppr ty0)
    else NoteTy usg pty
  where pty = pessimiseN co ty
                 
pessimise  co ty0 = pessimiseN co ty0  -- assume UsMany
#else
pessimise  co ty0@(NoteTy  usg@(UsgNote u  ) ty)
  = if co
    then case u of UsMany  -> NoteTy usg pty
                   UsVar _ -> NoteTy (UsgNote UsMany) pty
                   UsOnce  -> pprPanic "pessimise:" (ppr ty0)
    else NoteTy usg pty
  where pty = pessimiseN co ty
                 
pessimise  co ty0                                = pprPanic "pessimise: missing usage note:" $
                                                            ppr ty0
#endif

pessimiseN co ty0@(NoteTy  usg@(UsgNote _  ) ty) = pprPanic "pessimiseN: unexpected usage note:" $
                                                            ppr ty0
pessimiseN co     (NoteTy      (SynNote sty) ty) = NoteTy (SynNote (pessimiseN co sty))
                                                                   (pessimiseN co ty )
pessimiseN co     (NoteTy note@(FTVNote _  ) ty) = NoteTy note (pessimiseN co ty)
pessimiseN co ty0@(TyVarTy _)                    = ty0
pessimiseN co ty0@(AppTy _ _)                    = ty0
pessimiseN co ty0@(TyConApp tc tys)              = ASSERT( not ((isFunTyCon tc) && (length tys > 1)) )
                                                   ty0
pessimiseN co     (FunTy ty1 ty2)                = FunTy (pessimise (not co) ty1)
                                                         (pessimise      co  ty2)
pessimiseN co     (ForAllTy tyv ty)              = ForAllTy tyv (pessimiseN co ty)
\end{code}


@unAnnotTyM@ strips annotations (that the inference is allowed to
touch) from a term, and `fixes' those it isn't permitted to touch (by
putting @Many@ annotations where they are missing, but leaving
existing annotations in the type).

@unTermUsg@ removes from a term any term usage annotations it finds.

\begin{code}
unAnnotTyM :: MungeFlags -> Type -> AnnotM a Type

unAnnotTyM mf ty = if hasUsg mf then
                     ASSERT( isSigma mf )
                     return (fixAnnotTy ty)
                   else return (unannotTy ty)


unTermUsg :: CoreExpr -> AnnotM a CoreExpr
-- strip all term annotations
unTermUsg e@(Lam _ _)          = return e
unTermUsg (Note (TermUsg _) e) = return e
unTermUsg _                    = panic "unTermUsg"

unannotTy :: Type -> Type
-- strip all annotations
unannotTy    (NoteTy     (UsgForAll uv) ty) = unannotTy ty
unannotTy    (NoteTy      (UsgNote _  ) ty) = unannotTy ty
unannotTy    (NoteTy      (SynNote sty) ty) = NoteTy (SynNote (unannotTy sty)) (unannotTy ty)
unannotTy    (NoteTy note@(FTVNote _  ) ty) = NoteTy note (unannotTy ty)
-- IP notes need to be preserved
unannotTy ty@(NoteTy         (IPNote _) _)  = ty
unannotTy ty@(TyVarTy _)                    = ty
unannotTy    (AppTy ty1 ty2)                = AppTy (unannotTy ty1) (unannotTy ty2)
unannotTy    (TyConApp tc tys)              = TyConApp tc (map unannotTy tys)
unannotTy    (FunTy ty1 ty2)                = FunTy (unannotTy ty1) (unannotTy ty2)
unannotTy    (ForAllTy tyv ty)              = ForAllTy tyv (unannotTy ty)


fixAnnotTy :: Type -> Type
-- put Manys where they are missing
#ifndef USMANY
fixAnnotTy = id
#else
fixAnnotTy     (NoteTy note@(UsgForAll uv) ty) = NoteTy note (fixAnnotTy  ty)
fixAnnotTy      (NoteTy note@(UsgNote _  ) ty) = NoteTy note (fixAnnotTyN ty)
fixAnnotTy  ty0                                = NoteTy (UsgNote UsMany) (fixAnnotTyN ty0)

fixAnnotTyN ty0@(NoteTy note@(UsgNote _  ) ty) = pprPanic "fixAnnotTyN: unexpected usage note:" $
                                                          ppr ty0
fixAnnotTyN     (NoteTy      (SynNote sty) ty) = NoteTy (SynNote (fixAnnotTyN sty))
                                                                 (fixAnnotTyN ty )
fixAnnotTyN     (NoteTy note@(FTVNote _  ) ty) = NoteTy note (fixAnnotTyN ty)
fixAnnotTyN ty0@(TyVarTy _)                    = ty0
fixAnnotTyN     (AppTy ty1 ty2)                = AppTy (fixAnnotTyN ty1) (fixAnnotTyN ty2)
fixAnnotTyN     (TyConApp tc tys)              = ASSERT( isFunTyCon tc || isAlgTyCon tc || isPrimTyCon tc || isSynTyCon tc )
                                                 TyConApp tc (map (if isFunTyCon tc then
                                                                     fixAnnotTy
                                                                   else
                                                                     fixAnnotTyN) tys)
fixAnnotTyN     (FunTy ty1 ty2)                = FunTy (fixAnnotTy ty1) (fixAnnotTy ty2)
fixAnnotTyN     (ForAllTy tyv ty)              = ForAllTy tyv (fixAnnotTyN ty)
#endif
\end{code}

The composition (reannotating a type with fresh uvars but the same
structure) is useful elsewhere:

\begin{code}
freshannotTy :: Type -> UniqSMM Type
freshannotTy = annotTy (Right "freshannotTy") . unannotTy
\end{code}


Wrappers apply these functions to sets of bindings.

\begin{code}
doAnnotBinds :: UniqSupply
             -> [CoreBind]
             -> ([CoreBind],UniqSupply)

doAnnotBinds us binds = initAnnotM us (genAnnotBinds annotTyM annotLam binds)


doUnAnnotBinds :: [CoreBind]
               -> [CoreBind]

doUnAnnotBinds binds = fst $ initAnnotM () $
                         genAnnotBinds unAnnotTyM unTermUsg binds
\end{code}

======================================================================

Monadic machinery
~~~~~~~~~~~~~~~~~

The @UniqSM@ type is not an instance of @Monad@, and cannot be made so
since it is merely a synonym rather than a newtype.  Here we define
@UniqSMM@, which *is* an instance of @Monad@.

\begin{code}
newtype UniqSMM a = UsToUniqSMM (UniqSM a)
uniqSMMToUs (UsToUniqSMM us) = us
usToUniqSMM = UsToUniqSMM

instance Monad UniqSMM where
  m >>= f  = UsToUniqSMM $ uniqSMMToUs m `thenUs` \ a ->
                           uniqSMMToUs (f a)
  return   = UsToUniqSMM . returnUs
\end{code}


For annotation, the monad @AnnotM@, we need to carry around our
variable mapping, along with some general state.

\begin{code}
newtype AnnotM flexi a = AnnotM (   flexi                     -- UniqSupply etc
                                  -> VarEnv Var         -- unannotated to annotated variables
                                  -> (a,flexi,VarEnv Var))
unAnnotM (AnnotM f) = f

instance Monad (AnnotM flexi) where
  a >>= f  = AnnotM (\ us ve -> let (r,us',ve') = unAnnotM a us ve
                                in  unAnnotM (f r) us' ve')
  return a = AnnotM (\ us ve -> (a,us,ve))

initAnnotM :: fl -> AnnotM fl a -> (a,fl)
initAnnotM fl m = case (unAnnotM m) fl emptyVarEnv of { (r,fl',_) -> (r,fl') }

withAnnVar :: Var -> Var -> AnnotM fl a -> AnnotM fl a
withAnnVar v v' m = AnnotM (\ us ve -> let ve'          = extendVarEnv ve v v'
                                           (r,us',_)    = (unAnnotM m) us ve'
                                       in  (r,us',ve))

withAnnVars :: [Var] -> [Var] -> AnnotM fl a -> AnnotM fl a
withAnnVars vs vs' m = AnnotM (\ us ve -> let ve'          = plusVarEnv ve (zipVarEnv vs vs')
                                              (r,us',_)    = (unAnnotM m) us ve'
                                          in  (r,us',ve))

lookupAnnVar :: Var -> AnnotM fl (Maybe Var)
lookupAnnVar var = AnnotM (\ us ve -> (lookupVarEnv ve var,
                                       us,
                                       ve))
\end{code}

A useful helper allows us to turn a computation in the unique supply
monad into one in the annotation monad parameterised by a unique
supply.

\begin{code}
uniqSMtoAnnotM :: UniqSM a -> AnnotM UniqSupply a

uniqSMtoAnnotM m = AnnotM (\ us ve -> let (r,us') = initUs us m
                                      in  (r,us',ve))
\end{code}

@newVarUs@ and @newVarUSMM@ generate a new usage variable.  They take
an argument which is used for debugging only, describing what the
variable is to annotate.

\begin{code}
newVarUs :: (Either CoreExpr String) -> UniqSM UsageAnn
-- the first arg is for debugging use only
newVarUs e = getUniqueUs `thenUs` \ u ->
             let uv = mkUVar u in
             returnUs (UsVar uv)
{- #ifdef DEBUG
             let src = case e of
                         Left (Lit _) -> "literal"
                         Left (Lam v e)           -> "lambda: " ++ showSDoc (ppr v)
                         Left _                   -> "unknown"
                         Right s                  -> s
             in pprTrace "newVarUs:" (ppr uv <+> text src) $
   #endif
 -}

newVarUSMM :: (Either CoreExpr String) -> UniqSMM UsageAnn
newVarUSMM = usToUniqSMM . newVarUs
\end{code}

======================================================================

PrimOps and usage information.

Analagously to @DataCon.dataConArgTys@, we determine the argtys and
result ty of a primop, *after* substition (which may reveal more args,
notably for @CCall@s).

\begin{code}
primOpUsgTys :: PrimOp         -- this primop
             -> [Type]         -- instantiated at these (tau) types
             -> ([Type],Type)  -- requires args of these (sigma) types,
                               --  and returns this (sigma) type

primOpUsgTys p tys = let (tyvs,ty0us,rtyu) = primOpUsg p
                         s                 = mkTyVarSubst tyvs tys
                         (ty1us,rty1u)     = splitFunTys (substTy s rtyu)
                                             -- substitution may reveal more args
                     in  ((map (substTy s) ty0us) ++ ty1us,
                          rty1u)
\end{code}

======================================================================

EOF
