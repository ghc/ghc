%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Monadic type operations

This module contains monadic operations over types that contain
mutable type variables

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module TcMType (
  TcTyVar, TcKind, TcType, TcTauType, TcThetaType, TcTyVarSet,

  --------------------------------
  -- Creating new mutable type variables
  newFlexiTyVar,
  newFlexiTyVarTy,		-- Kind -> TcM TcType
  newFlexiTyVarTys,		-- Int -> Kind -> TcM [TcType]
  newPolyFlexiTyVarTy,
  newMetaKindVar, newMetaKindVars, mkKindSigVar,
  mkTcTyVarName, cloneMetaTyVar, 

  newMetaTyVar, readMetaTyVar, writeMetaTyVar, writeMetaTyVarRef,
  newMetaDetails, isFilledMetaTyVar, isFlexiMetaTyVar,

  --------------------------------
  -- Creating new evidence variables
  newEvVar, newEvVars,
  newEq, newDict,

  newWantedEvVar, newWantedEvVars,
  newTcEvBinds, addTcEvBind,

  --------------------------------
  -- Instantiation
  tcInstTyVars, tcInstSigTyVars, newSigTyVar,
  tcInstType, 
  tcInstSkolTyVars, tcInstSkolTyVarsLoc, tcInstSuperSkolTyVars,
  tcInstSkolTyVarsX, tcInstSuperSkolTyVarsX,
  tcInstSkolTyVar, tcInstSkolType,
  tcSkolDFunType, tcSuperSkolTyVars,

  --------------------------------
  -- Checking type validity
  Rank, UserTypeCtxt(..), checkValidType, checkValidMonoType,
  expectedKindInCtxt, 
  checkValidTheta, checkValidFamPats,
  checkValidInstHead, checkValidInstance, validDerivPred,
  checkInstTermination, checkValidTyFamInst, checkTyFamFreeness, 
  arityErr, 
  growThetaTyVars, quantifyPred,

  --------------------------------
  -- Zonking
  zonkTcPredType, 
  skolemiseSigTv, skolemiseUnboundMetaTyVar,
  zonkTcTyVar, zonkTcTyVars, zonkTyVarsAndFV, 
  zonkQuantifiedTyVar, zonkQuantifiedTyVars,
  zonkTcType, zonkTcTypes, zonkTcThetaType,

  zonkTcKind, defaultKindVarToStar,
  zonkEvVar, zonkWC, zonkId, zonkCt, zonkCts, zonkSkolemInfo,

  tcGetGlobalTyVars, 
  ) where

#include "HsVersions.h"

-- friends:
import TypeRep
import TcType
import TcEvidence
import Type
import Kind
import Class
import TyCon
import Var

-- others:
import HsSyn		-- HsType
import TcRnMonad        -- TcType, amongst others
import Id
import FunDeps
import Name
import VarSet
import ErrUtils
import PrelNames
import DynFlags
import Util
import Maybes
import ListSetOps
import SrcLoc
import Outputable
import FastString
import Bag

import Control.Monad
import Data.List        ( (\\), partition, mapAccumL )
\end{code}


%************************************************************************
%*									*
	Kind variables
%*									*
%************************************************************************

\begin{code}
mkKindName :: Unique -> Name
mkKindName unique = mkSystemName unique kind_var_occ

kind_var_occ :: OccName	-- Just one for all MetaKindVars
			-- They may be jiggled by tidying
kind_var_occ = mkOccName tvName "k"

newMetaKindVar :: TcM TcKind
newMetaKindVar = do { uniq <- newUnique
		    ; details <- newMetaDetails TauTv
                    ; let kv = mkTcTyVar (mkKindName uniq) superKind details
		    ; return (mkTyVarTy kv) }

newMetaKindVars :: Int -> TcM [TcKind]
newMetaKindVars n = mapM (\ _ -> newMetaKindVar) (nOfThem n ())

mkKindSigVar :: Name -> KindVar
-- Use the specified name; don't clone it
mkKindSigVar n = mkTcTyVar n superKind (SkolemTv False)
\end{code}


%************************************************************************
%*									*
     Evidence variables; range over constraints we can abstract over
%*									*
%************************************************************************

\begin{code}
newEvVars :: TcThetaType -> TcM [EvVar]
newEvVars theta = mapM newEvVar theta

newWantedEvVar :: TcPredType -> TcM EvVar 
newWantedEvVar = newEvVar

newWantedEvVars :: TcThetaType -> TcM [EvVar] 
newWantedEvVars theta = mapM newWantedEvVar theta 

--------------

newEvVar :: TcPredType -> TcM EvVar
-- Creates new *rigid* variables for predicates
newEvVar ty = do { name <- newSysName (predTypeOccName ty) 
                 ; return (mkLocalId name ty) }

newEq :: TcType -> TcType -> TcM EvVar
newEq ty1 ty2
  = do { name <- newSysName (mkVarOccFS (fsLit "cobox"))
       ; return (mkLocalId name (mkTcEqPred ty1 ty2)) }

newDict :: Class -> [TcType] -> TcM DictId
newDict cls tys 
  = do { name <- newSysName (mkDictOcc (getOccName cls))
       ; return (mkLocalId name (mkClassPred cls tys)) }

predTypeOccName :: PredType -> OccName
predTypeOccName ty = case classifyPredType ty of
    ClassPred cls _ -> mkDictOcc (getOccName cls)
    EqPred _ _      -> mkVarOccFS (fsLit "cobox")
    TuplePred _     -> mkVarOccFS (fsLit "tup")
    IrredPred _     -> mkVarOccFS (fsLit "irred")
\end{code}


%************************************************************************
%*									*
	SkolemTvs (immutable)
%*									*
%************************************************************************

\begin{code}
tcInstType :: ([TyVar] -> TcM (TvSubst, [TcTyVar]))     -- How to instantiate the type variables
	   -> TcType 					-- Type to instantiate
	   -> TcM ([TcTyVar], TcThetaType, TcType)	-- Result
		-- (type vars (excl coercion vars), preds (incl equalities), rho)
tcInstType inst_tyvars ty
  = case tcSplitForAllTys ty of
	([],     rho) -> let	-- There may be overloading despite no type variables;
				-- 	(?x :: Int) => Int -> Int
			   (theta, tau) = tcSplitPhiTy rho
			 in
			 return ([], theta, tau)

	(tyvars, rho) -> do { (subst, tyvars') <- inst_tyvars tyvars
			    ; let (theta, tau) = tcSplitPhiTy (substTy subst rho)
			    ; return (tyvars', theta, tau) }

tcSkolDFunType :: Type -> TcM ([TcTyVar], TcThetaType, TcType)
-- Instantiate a type signature with skolem constants, but 
-- do *not* give them fresh names, because we want the name to
-- be in the type environment: it is lexically scoped.
tcSkolDFunType ty = tcInstType (\tvs -> return (tcSuperSkolTyVars tvs)) ty

tcSuperSkolTyVars :: [TyVar] -> (TvSubst, [TcTyVar])
-- Make skolem constants, but do *not* give them new names, as above
-- Moreover, make them "super skolems"; see comments with superSkolemTv
-- see Note [Kind substitution when instantiating]
-- Precondition: tyvars should be ordered (kind vars first)
tcSuperSkolTyVars = mapAccumL tcSuperSkolTyVar (mkTopTvSubst [])

tcSuperSkolTyVar :: TvSubst -> TyVar -> (TvSubst, TcTyVar)
tcSuperSkolTyVar subst tv
  = (extendTvSubst subst tv (mkTyVarTy new_tv), new_tv)
  where
    kind   = substTy subst (tyVarKind tv)
    new_tv = mkTcTyVar (tyVarName tv) kind superSkolemTv

tcInstSkolTyVar :: SrcSpan -> Bool -> TvSubst -> TyVar
                -> TcRnIf gbl lcl (TvSubst, TcTyVar)
-- Instantiate the tyvar, using 
--      * the occ-name and kind of the supplied tyvar, 
--      * the unique from the monad,
--      * the location either from the tyvar (skol_info = SigSkol)
--                     or from the monad (otherwise)
tcInstSkolTyVar loc overlappable subst tyvar
  = do  { uniq <- newUnique
        ; let new_name = mkInternalName uniq occ loc
              new_tv   = mkTcTyVar new_name kind (SkolemTv overlappable)
        ; return (extendTvSubst subst tyvar (mkTyVarTy new_tv), new_tv) }
  where
    old_name = tyVarName tyvar
    occ      = nameOccName old_name
    kind     = substTy subst (tyVarKind tyvar)

-- Wrappers
-- we need to be able to do this from outside the TcM monad:
tcInstSkolTyVarsLoc :: SrcSpan -> [TyVar] -> TcRnIf gbl lcl (TvSubst, [TcTyVar])
tcInstSkolTyVarsLoc loc = mapAccumLM (tcInstSkolTyVar loc False) (mkTopTvSubst [])

tcInstSkolTyVars :: [TyVar] -> TcM (TvSubst, [TcTyVar])
tcInstSkolTyVars = tcInstSkolTyVarsX (mkTopTvSubst [])

tcInstSuperSkolTyVars :: [TyVar] -> TcM [TcTyVar]
tcInstSuperSkolTyVars = fmap snd . tcInstSkolTyVars' True  (mkTopTvSubst [])

tcInstSkolTyVarsX, tcInstSuperSkolTyVarsX
  :: TvSubst -> [TyVar] -> TcM (TvSubst, [TcTyVar])
tcInstSkolTyVarsX      subst = tcInstSkolTyVars' False subst
tcInstSuperSkolTyVarsX subst = tcInstSkolTyVars' True  subst

tcInstSkolTyVars' :: Bool -> TvSubst -> [TyVar] -> TcM (TvSubst, [TcTyVar])
-- Precondition: tyvars should be ordered (kind vars first)
-- see Note [Kind substitution when instantiating]
tcInstSkolTyVars' isSuperSkol subst tvs
  = do { loc <- getSrcSpanM
       ; mapAccumLM (tcInstSkolTyVar loc isSuperSkol) subst tvs }

tcInstSkolType :: TcType -> TcM ([TcTyVar], TcThetaType, TcType)
-- Instantiate a type with fresh skolem constants
-- Binding location comes from the monad
tcInstSkolType ty = tcInstType tcInstSkolTyVars ty

tcInstSigTyVars :: [TyVar] -> TcM (TvSubst, [TcTyVar])
-- Make meta SigTv type variables for patten-bound scoped type varaibles
-- We use SigTvs for them, so that they can't unify with arbitrary types
-- Precondition: tyvars should be ordered (kind vars first)
-- see Note [Kind substitution when instantiating]
tcInstSigTyVars = mapAccumLM tcInstSigTyVar (mkTopTvSubst [])
	-- The tyvars are freshly made, by tcInstSigTyVar
        -- So mkTopTvSubst [] is ok

tcInstSigTyVar :: TvSubst -> TyVar -> TcM (TvSubst, TcTyVar)
tcInstSigTyVar subst tv
  = do { new_tv <- newSigTyVar (tyVarName tv) (substTy subst (tyVarKind tv))
       ; return (extendTvSubst subst tv (mkTyVarTy new_tv), new_tv) }

newSigTyVar :: Name -> Kind -> TcM TcTyVar
newSigTyVar name kind
  = do { uniq <- newUnique
       ; let name' = setNameUnique name uniq
                      -- Use the same OccName so that the tidy-er
                      -- doesn't gratuitously rename 'a' to 'a0' etc
       ; details <- newMetaDetails SigTv
       ; return (mkTcTyVar name' kind details) }

newMetaDetails :: MetaInfo -> TcM TcTyVarDetails
newMetaDetails info 
  = do { ref <- newMutVar Flexi
       ; untch <- getUntouchables
       ; return (MetaTv { mtv_info = info, mtv_ref = ref, mtv_untch = untch }) }
\end{code}

Note [Kind substitution when instantiating]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we instantiate a bunch of kind and type variables, first we
expect them to be sorted (kind variables first, then type variables).
Then we have to instantiate the kind variables, build a substitution
from old variables to the new variables, then instantiate the type
variables substituting the original kind.

Exemple: If we want to instantiate
  [(k1 :: BOX), (k2 :: BOX), (a :: k1 -> k2), (b :: k1)]
we want
  [(?k1 :: BOX), (?k2 :: BOX), (?a :: ?k1 -> ?k2), (?b :: ?k1)]
instead of the buggous
  [(?k1 :: BOX), (?k2 :: BOX), (?a :: k1 -> k2), (?b :: k1)]


%************************************************************************
%*									*
	MetaTvs (meta type variables; mutable)
%*									*
%************************************************************************

\begin{code}
newMetaTyVar :: MetaInfo -> Kind -> TcM TcTyVar
-- Make a new meta tyvar out of thin air
newMetaTyVar meta_info kind
  = do	{ uniq <- newUnique
        ; let name = mkTcTyVarName uniq s
              s = case meta_info of
                        PolyTv -> fsLit "s"
                        TauTv  -> fsLit "t"
                        SigTv  -> fsLit "a"
        ; details <- newMetaDetails meta_info
	; return (mkTcTyVar name kind details) }

cloneMetaTyVar :: TcTyVar -> TcM TcTyVar
cloneMetaTyVar tv
  = ASSERT( isTcTyVar tv )
    do	{ uniq <- newUnique
        ; ref  <- newMutVar Flexi
        ; let name'    = setNameUnique (tyVarName tv) uniq
              details' = case tcTyVarDetails tv of 
                           details@(MetaTv {}) -> details { mtv_ref = ref }
                           _ -> pprPanic "cloneMetaTyVar" (ppr tv)
        ; return (mkTcTyVar name' (tyVarKind tv) details') }

mkTcTyVarName :: Unique -> FastString -> Name
-- Make sure that fresh TcTyVar names finish with a digit
-- leaving the un-cluttered names free for user names
mkTcTyVarName uniq str = mkSysTvName uniq str

-- Works for both type and kind variables
readMetaTyVar :: TyVar -> TcM MetaDetails
readMetaTyVar tyvar = ASSERT2( isMetaTyVar tyvar, ppr tyvar )
		      readMutVar (metaTvRef tyvar)

isFilledMetaTyVar :: TyVar -> TcM Bool
-- True of a filled-in (Indirect) meta type variable
isFilledMetaTyVar tv
  | not (isTcTyVar tv) = return False
  | MetaTv { mtv_ref = ref } <- tcTyVarDetails tv
  = do 	{ details <- readMutVar ref
	; return (isIndirect details) }
  | otherwise = return False

isFlexiMetaTyVar :: TyVar -> TcM Bool
-- True of a un-filled-in (Flexi) meta type variable
isFlexiMetaTyVar tv
  | not (isTcTyVar tv) = return False
  | MetaTv { mtv_ref = ref } <- tcTyVarDetails tv
  = do 	{ details <- readMutVar ref
	; return (isFlexi details) }
  | otherwise = return False

--------------------
-- Works with both type and kind variables
writeMetaTyVar :: TcTyVar -> TcType -> TcM ()
-- Write into a currently-empty MetaTyVar

writeMetaTyVar tyvar ty
  | not debugIsOn 
  = writeMetaTyVarRef tyvar (metaTvRef tyvar) ty

-- Everything from here on only happens if DEBUG is on
  | not (isTcTyVar tyvar)
  = WARN( True, text "Writing to non-tc tyvar" <+> ppr tyvar )
    return ()

  | MetaTv { mtv_ref = ref } <- tcTyVarDetails tyvar
  = writeMetaTyVarRef tyvar ref ty

  | otherwise
  = WARN( True, text "Writing to non-meta tyvar" <+> ppr tyvar )
    return ()

--------------------
writeMetaTyVarRef :: TcTyVar -> TcRef MetaDetails -> TcType -> TcM ()
-- Here the tyvar is for error checking only; 
-- the ref cell must be for the same tyvar
writeMetaTyVarRef tyvar ref ty
  | not debugIsOn 
  = do { traceTc "writeMetaTyVar" (ppr tyvar <+> text ":=" <+> ppr ty)
       ; writeMutVar ref (Indirect ty) }

-- Everything from here on only happens if DEBUG is on
  | otherwise
  = do { meta_details <- readMutVar ref; 
       -- Zonk kinds to allow the error check to work
       ; zonked_tv_kind <- zonkTcKind tv_kind 
       ; zonked_ty_kind <- zonkTcKind ty_kind

       -- Check for double updates
       ; ASSERT2( isFlexi meta_details, 
                  hang (text "Double update of meta tyvar")
                   2 (ppr tyvar $$ ppr meta_details) )

         traceTc "writeMetaTyVar" (ppr tyvar <+> text ":=" <+> ppr ty)
       ; writeMutVar ref (Indirect ty) 
       ; when (   not (isPredTy tv_kind) 
                    -- Don't check kinds for updates to coercion variables
               && not (zonked_ty_kind `tcIsSubKind` zonked_tv_kind))
       $ WARN( True, hang (text "Ill-kinded update to meta tyvar")
                        2 (    ppr tyvar <+> text "::" <+> ppr tv_kind 
                           <+> text ":=" 
                           <+> ppr ty    <+> text "::" <+> ppr ty_kind) )
         (return ()) }
  where
    tv_kind = tyVarKind tyvar
    ty_kind = typeKind ty
\end{code}


%************************************************************************
%*									*
	MetaTvs: TauTvs
%*									*
%************************************************************************

\begin{code}
newFlexiTyVar :: Kind -> TcM TcTyVar
newFlexiTyVar kind = newMetaTyVar TauTv kind

newFlexiTyVarTy  :: Kind -> TcM TcType
newFlexiTyVarTy kind = do
    tc_tyvar <- newFlexiTyVar kind
    return (TyVarTy tc_tyvar)

newFlexiTyVarTys :: Int -> Kind -> TcM [TcType]
newFlexiTyVarTys n kind = mapM newFlexiTyVarTy (nOfThem n kind)

newPolyFlexiTyVarTy :: TcM TcType
newPolyFlexiTyVarTy = do { tv <- newMetaTyVar PolyTv liftedTypeKind
                         ; return (TyVarTy tv) }

tcInstTyVars :: [TKVar] -> TcM ([TcTyVar], [TcType], TvSubst)
-- Instantiate with META type variables
-- Note that this works for a sequence of kind and type
-- variables.  Eg    [ (k:BOX), (a:k->k) ]
--             Gives [ (k7:BOX), (a8:k7->k7) ]
tcInstTyVars tyvars = tcInstTyVarsX emptyTvSubst tyvars
    -- emptyTvSubst has an empty in-scope set, but that's fine here
    -- Since the tyvars are freshly made, they cannot possibly be
    -- captured by any existing for-alls.

tcInstTyVarsX :: TvSubst -> [TKVar] -> TcM ([TcTyVar], [TcType], TvSubst)
-- The "X" part is because of extending the substitution
tcInstTyVarsX subst tyvars =
  do { (subst', tyvars') <- mapAccumLM tcInstTyVarX subst tyvars
     ; return (tyvars', mkTyVarTys tyvars', subst') }

tcInstTyVarX :: TvSubst -> TKVar -> TcM (TvSubst, TcTyVar)
-- Make a new unification variable tyvar whose Name and Kind come from
-- an existing TyVar. We substitute kind variables in the kind.
tcInstTyVarX subst tyvar
  = do  { uniq <- newUnique
        ; details <- newMetaDetails TauTv
        ; let name   = mkSystemName uniq (getOccName tyvar)
              kind   = substTy subst (tyVarKind tyvar)
              new_tv = mkTcTyVar name kind details 
        ; return (extendTvSubst subst tyvar (mkTyVarTy new_tv), new_tv) }
\end{code}


%************************************************************************
%*									*
\subsection{Zonking -- the exernal interfaces}
%*									*
%************************************************************************

@tcGetGlobalTyVars@ returns a fully-zonked set of tyvars free in the environment.
To improve subsequent calls to the same function it writes the zonked set back into
the environment.

\begin{code}
tcGetGlobalTyVars :: TcM TcTyVarSet
tcGetGlobalTyVars
  = do { (TcLclEnv {tcl_tyvars = gtv_var}) <- getLclEnv
       ; gbl_tvs  <- readMutVar gtv_var
       ; gbl_tvs' <- zonkTyVarsAndFV gbl_tvs
       ; writeMutVar gtv_var gbl_tvs'
       ; return gbl_tvs' }
  where
\end{code}

-----------------  Type variables

\begin{code}
zonkTyVar :: TyVar -> TcM TcType
-- Works on TyVars and TcTyVars
zonkTyVar tv | isTcTyVar tv = zonkTcTyVar tv
             | otherwise    = return (mkTyVarTy tv)
   -- Hackily, when typechecking type and class decls
   -- we have TyVars in scopeadded (only) in 
   -- TcHsType.tcTyClTyVars, but it seems
   -- painful to make them into TcTyVars there

zonkTyVarsAndFV :: TyVarSet -> TcM TyVarSet
zonkTyVarsAndFV tyvars = tyVarsOfTypes <$> mapM zonkTyVar (varSetElems tyvars)

zonkTcTyVars :: [TcTyVar] -> TcM [TcType]
zonkTcTyVars tyvars = mapM zonkTcTyVar tyvars

-----------------  Types
zonkTyVarKind :: TyVar -> TcM TyVar
zonkTyVarKind tv = do { kind' <- zonkTcKind (tyVarKind tv)
                      ; return (setTyVarKind tv kind') }

zonkTcTypes :: [TcType] -> TcM [TcType]
zonkTcTypes tys = mapM zonkTcType tys

zonkTcThetaType :: TcThetaType -> TcM TcThetaType
zonkTcThetaType theta = mapM zonkTcPredType theta

zonkTcPredType :: TcPredType -> TcM TcPredType
zonkTcPredType = zonkTcType
\end{code}

-------------------  These ...ToType, ...ToKind versions
		     are used at the end of type checking

\begin{code}
defaultKindVarToStar :: TcTyVar -> TcM Kind
-- We have a meta-kind: unify it with '*'
defaultKindVarToStar kv 
  = do { ASSERT ( isKindVar kv && isMetaTyVar kv )
         writeMetaTyVar kv liftedTypeKind
       ; return liftedTypeKind }

zonkQuantifiedTyVars :: [TcTyVar] -> TcM [TcTyVar]
-- A kind variable k may occur *after* a tyvar mentioning k in its kind
zonkQuantifiedTyVars tyvars
  = do { let (kvs, tvs) = partition isKindVar tyvars
             (meta_kvs, skolem_kvs) = partition isMetaTyVar kvs

             -- In the non-PolyKinds case, default the kind variables
             -- to *, and zonk the tyvars as usual.  Notice that this
             -- may make zonkQuantifiedTyVars return a shorter list
             -- than it was passed, but that's ok
       ; poly_kinds <- xoptM Opt_PolyKinds
       ; qkvs <- if poly_kinds 
                 then return kvs
                 else WARN ( not (null skolem_kvs), ppr skolem_kvs )
                      do { mapM_ defaultKindVarToStar meta_kvs
                         ; return skolem_kvs }  -- Should be empty

       ; mapM zonkQuantifiedTyVar (qkvs ++ tvs) }
           -- Because of the order, any kind variables
           -- mentioned in the kinds of the type variables refer to
           -- the now-quantified versions

zonkQuantifiedTyVar :: TcTyVar -> TcM TcTyVar
-- The quantified type variables often include meta type variables
-- we want to freeze them into ordinary type variables, and
-- default their kind (e.g. from OpenTypeKind to TypeKind)
-- 			-- see notes with Kind.defaultKind
-- The meta tyvar is updated to point to the new skolem TyVar.  Now any 
-- bound occurences of the original type variable will get zonked to 
-- the immutable version.
--
-- We leave skolem TyVars alone; they are immutable.
--
-- This function is called on both kind and type variables,
-- but kind variables *only* if PolyKinds is on.
zonkQuantifiedTyVar tv
  = ASSERT2( isTcTyVar tv, ppr tv ) 
    case tcTyVarDetails tv of
      SkolemTv {} -> do { kind <- zonkTcKind (tyVarKind tv)
                        ; return $ setTyVarKind tv kind }
	-- It might be a skolem type variable, 
	-- for example from a user type signature

      MetaTv { mtv_ref = ref } ->
          do when debugIsOn $ do
                 -- [Sept 04] Check for non-empty.
                 -- See note [Silly Type Synonym]
                 cts <- readMutVar ref
                 case cts of
                     Flexi -> return ()
                     Indirect ty -> WARN( True, ppr tv $$ ppr ty )
                                    return ()
             skolemiseUnboundMetaTyVar tv vanillaSkolemTv
      _other -> pprPanic "zonkQuantifiedTyVar" (ppr tv) -- FlatSkol, RuntimeUnk

skolemiseUnboundMetaTyVar :: TcTyVar -> TcTyVarDetails -> TcM TyVar
-- We have a Meta tyvar with a ref-cell inside it
-- Skolemise it, including giving it a new Name, so that
--   we are totally out of Meta-tyvar-land
-- We create a skolem TyVar, not a regular TyVar
--   See Note [Zonking to Skolem]
skolemiseUnboundMetaTyVar tv details
  = ASSERT2( isMetaTyVar tv, ppr tv ) 
    do  { span <- getSrcSpanM    -- Get the location from "here"
                                 -- ie where we are generalising
        ; uniq <- newUnique      -- Remove it from TcMetaTyVar unique land
        ; kind <- zonkTcKind (tyVarKind tv)
        ; let final_kind = defaultKind kind
              final_name = mkInternalName uniq (getOccName tv) span
              final_tv   = mkTcTyVar final_name final_kind details

        ; writeMetaTyVar tv (mkTyVarTy final_tv)
        ; return final_tv }

skolemiseSigTv :: TcTyVar -> TcM TcTyVar
-- In TcBinds we create SigTvs for type signatures
-- but for singleton groups we want them to really be skolems
-- which do not unify with each other
skolemiseSigTv tv  
  = ASSERT2( isSigTyVar tv, ppr tv )
    do { writeMetaTyVarRef tv (metaTvRef tv) (mkTyVarTy skol_tv)
       ; return skol_tv }
  where
    skol_tv = setTcTyVarDetails tv (SkolemTv False)
\end{code}

\begin{code}
zonkImplication :: Implication -> TcM Implication
zonkImplication implic@(Implic { ic_untch  = untch
                               , ic_binds  = binds_var
                               , ic_skols  = skols
                               , ic_given  = given
                               , ic_wanted = wanted
                               , ic_info   = info })
  = do { skols'  <- mapM zonkTcTyVarBndr skols  -- Need to zonk their kinds!
                                                -- as Trac #7230 showed
       ; given'  <- mapM zonkEvVar given
       ; info'   <- zonkSkolemInfo info
       ; wanted' <- zonkWCRec binds_var untch wanted
       ; return (implic { ic_skols = skols'
                        , ic_given = given'
                        , ic_fsks  = []  -- Zonking removes all FlatSkol tyvars
                        , ic_wanted = wanted'
                        , ic_info = info' }) }

zonkEvVar :: EvVar -> TcM EvVar
zonkEvVar var = do { ty' <- zonkTcType (varType var)
                   ; return (setVarType var ty') }


zonkWC :: EvBindsVar -- May add new bindings for wanted family equalities in here
       -> WantedConstraints -> TcM WantedConstraints
zonkWC binds_var wc
  = do { untch <- getUntouchables
       ; zonkWCRec binds_var untch wc }

zonkWCRec :: EvBindsVar
          -> Untouchables
          -> WantedConstraints -> TcM WantedConstraints
zonkWCRec binds_var untch (WC { wc_flat = flat, wc_impl = implic, wc_insol = insol })
  = do { flat'   <- zonkFlats binds_var untch flat
       ; implic' <- mapBagM zonkImplication implic
       ; insol'  <- zonkCts insol -- No need to do the more elaborate zonkFlats thing
       ; return (WC { wc_flat = flat', wc_impl = implic', wc_insol = insol' }) }

zonkFlats :: EvBindsVar -> Untouchables -> Cts -> TcM Cts
-- This zonks and unflattens a bunch of flat constraints
-- See Note [Unflattening while zonking]
zonkFlats binds_var untch cts
  = do { -- See Note [How to unflatten]
         cts <- foldrBagM unflatten_one emptyCts cts
       ; zonkCts cts }
  where
    unflatten_one orig_ct cts
      = do { zct <- zonkCt orig_ct                -- First we need to fully zonk 
           ; mct <- try_zonk_fun_eq orig_ct zct   -- Then try to solve if family equation
           ; return $ maybe cts (`consBag` cts) mct }

    try_zonk_fun_eq orig_ct zct   -- See Note [How to unflatten]
      | EqPred ty_lhs ty_rhs <- classifyPredType (ctPred zct)
          -- NB: zonking de-classifies the constraint,
          --     so we can't look for CFunEqCan
      , Just tv <- getTyVar_maybe ty_rhs
      , ASSERT2( not (isFloatedTouchableMetaTyVar untch tv), ppr tv )
        isTouchableMetaTyVar untch tv
      , typeKind ty_lhs `tcIsSubKind` tyVarKind tv
      , not (tv `elemVarSet` tyVarsOfType ty_lhs)
--       , Just ty_lhs' <- occurCheck tv ty_lhs
      = ASSERT2( isWantedCt orig_ct, ppr orig_ct )
        ASSERT2( case tcSplitTyConApp_maybe ty_lhs of { Just (tc,_) -> isSynFamilyTyCon tc; _ -> False }, ppr orig_ct )
        do { writeMetaTyVar tv ty_lhs
           ; let evterm = EvCoercion (mkTcReflCo ty_lhs)
                 evvar  = ctev_evar (cc_ev zct)
           ; addTcEvBind binds_var evvar evterm
           ; traceTc "zonkFlats/unflattening" $
             vcat [ text "zct = " <+> ppr zct,
                    text "binds_var = " <+> ppr binds_var ]
           ; return Nothing }
      | otherwise
      = return (Just zct)
\end{code}

Note [Unflattening while zonking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A bunch of wanted constraints could contain wanted equations of the form
(F taus ~ alpha) where alpha is either an ordinary unification variable, or
a flatten unification variable.

These are ordinary wanted constraints and can/should be solved by
ordinary unification alpha := F taus. However the constraint solving
algorithm does not do that, as their 'inert' form is F taus ~ alpha.

Hence, we need an extra step to 'unflatten' these equations by
performing unification. This unification, if it happens at the end of
constraint solving, cannot produce any more interactions in the
constraint solver so it is safe to do it as the very very last step.

We choose therefore to do it during zonking, in the function
zonkFlats. This is in analgoy to the zonking of given flatten skolems
which are eliminated in favor of the underlying type that they are
equal to.

Note that, because we now have to affect *evidence* while zonking
(setting some evidence binds to identities), we have to pass to the
zonkWC function an evidence variable to collect all the extra
variables.

Note [How to unflatten]
~~~~~~~~~~~~~~~~~~~~~~~
How do we unflatten during zonking.  Consider a bunch of flat constraints.
Consider them one by one.  For each such constraint C
  * Zonk C (to apply current substitution)
  * If C is of form F tys ~ alpha, 
       where alpha is touchable
       and   alpha is not mentioned in tys
    then unify alpha := F tys
         and discard C

After processing all the flat constraints, zonk them again to propagate
the inforamtion from later ones to earlier ones.  Eg
  Start:  (F alpha ~ beta, G Int ~ alpha)
  Then we get beta := F alpha
              alpha := G Int
  but we must apply the second unification to the first constraint.


\begin{code}
zonkCts :: Cts -> TcM Cts
zonkCts = mapBagM zonkCt

zonkCt :: Ct -> TcM Ct
zonkCt ct@(CHoleCan { cc_ev = ev })
  = do { ev' <- zonkCtEvidence ev
       ; return $ ct { cc_ev = ev' } }
zonkCt ct
  = do { fl' <- zonkCtEvidence (cc_ev ct)
       ; return (CNonCanonical { cc_ev = fl'
                               , cc_loc = cc_loc ct }) }

zonkCtEvidence :: CtEvidence -> TcM CtEvidence
zonkCtEvidence ctev@(CtGiven { ctev_pred = pred }) 
  = do { pred' <- zonkTcType pred
       ; return (ctev { ctev_pred = pred'}) }
zonkCtEvidence ctev@(CtWanted { ctev_pred = pred })
  = do { pred' <- zonkTcType pred
       ; return (ctev { ctev_pred = pred' }) }
zonkCtEvidence ctev@(CtDerived { ctev_pred = pred })
  = do { pred' <- zonkTcType pred
       ; return (ctev { ctev_pred = pred' }) }

zonkSkolemInfo :: SkolemInfo -> TcM SkolemInfo
zonkSkolemInfo (SigSkol cx ty)  = do { ty' <- zonkTcType ty
                                     ; return (SigSkol cx ty') }
zonkSkolemInfo (InferSkol ntys) = do { ntys' <- mapM do_one ntys
                                     ; return (InferSkol ntys') }
  where
    do_one (n, ty) = do { ty' <- zonkTcType ty; return (n, ty') }
zonkSkolemInfo skol_info = return skol_info
\end{code}

Note [Silly Type Synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
	type C u a = u	-- Note 'a' unused

	foo :: (forall a. C u a -> C u a) -> u
	foo x = ...

	bar :: Num u => u
	bar = foo (\t -> t + t)

* From the (\t -> t+t) we get type  {Num d} =>  d -> d
  where d is fresh.

* Now unify with type of foo's arg, and we get:
	{Num (C d a)} =>  C d a -> C d a
  where a is fresh.

* Now abstract over the 'a', but float out the Num (C d a) constraint
  because it does not 'really' mention a.  (see exactTyVarsOfType)
  The arg to foo becomes
	\/\a -> \t -> t+t

* So we get a dict binding for Num (C d a), which is zonked to give
	a = ()
  [Note Sept 04: now that we are zonking quantified type variables
  on construction, the 'a' will be frozen as a regular tyvar on
  quantification, so the floated dict will still have type (C d a).
  Which renders this whole note moot; happily!]

* Then the \/\a abstraction has a zonked 'a' in it.

All very silly.   I think its harmless to ignore the problem.  We'll end up with
a \/\a in the final result but all the occurrences of a will be zonked to ()

Note [Zonking to Skolem]
~~~~~~~~~~~~~~~~~~~~~~~~
We used to zonk quantified type variables to regular TyVars.  However, this
leads to problems.  Consider this program from the regression test suite:

  eval :: Int -> String -> String -> String
  eval 0 root actual = evalRHS 0 root actual

  evalRHS :: Int -> a
  evalRHS 0 root actual = eval 0 root actual

It leads to the deferral of an equality (wrapped in an implication constraint)

  forall a. () => ((String -> String -> String) ~ a)

which is propagated up to the toplevel (see TcSimplify.tcSimplifyInferCheck).
In the meantime `a' is zonked and quantified to form `evalRHS's signature.
This has the *side effect* of also zonking the `a' in the deferred equality
(which at this point is being handed around wrapped in an implication
constraint).

Finally, the equality (with the zonked `a') will be handed back to the
simplifier by TcRnDriver.tcRnSrcDecls calling TcSimplify.tcSimplifyTop.
If we zonk `a' with a regular type variable, we will have this regular type
variable now floating around in the simplifier, which in many places assumes to
only see proper TcTyVars.

We can avoid this problem by zonking with a skolem.  The skolem is rigid
(which we require for a quantified variable), but is still a TcTyVar that the
simplifier knows how to deal with.


%************************************************************************
%*									*
\subsection{Zonking -- the main work-horses: zonkTcType, zonkTcTyVar}
%*									*
%*		For internal use only!					*
%*									*
%************************************************************************

\begin{code}
-- zonkId is used *during* typechecking just to zonk the Id's type
zonkId :: TcId -> TcM TcId
zonkId id
  = do { ty' <- zonkTcType (idType id)
       ; return (Id.setIdType id ty') }

-- For unbound, mutable tyvars, zonkType uses the function given to it
-- For tyvars bound at a for-all, zonkType zonks them to an immutable
--	type variable and zonks the kind too

zonkTcType :: TcType -> TcM TcType
zonkTcType ty
  = go ty
  where
    go (TyConApp tc tys) = do tys' <- mapM go tys
                              return (TyConApp tc tys')

    go (LitTy n)         = return (LitTy n)

    go (FunTy arg res)   = do arg' <- go arg
                              res' <- go res
                              return (FunTy arg' res')

    go (AppTy fun arg)   = do fun' <- go fun
                              arg' <- go arg
                              return (mkAppTy fun' arg')
		-- NB the mkAppTy; we might have instantiated a
		-- type variable to a type constructor, so we need
		-- to pull the TyConApp to the top.

	-- The two interesting cases!
    go (TyVarTy tyvar) | isTcTyVar tyvar = zonkTcTyVar tyvar
		       | otherwise	 = TyVarTy <$> updateTyVarKindM go tyvar
		-- Ordinary (non Tc) tyvars occur inside quantified types

    go (ForAllTy tv ty) = do { tv' <- zonkTcTyVarBndr tv
                             ; ty' <- go ty
                             ; return (ForAllTy tv' ty') }

zonkTcTyVarBndr :: TcTyVar -> TcM TcTyVar
-- A tyvar binder is never a unification variable (MetaTv),
-- rather it is always a skolems.  BUT it may have a kind 
-- that has not yet been zonked, and may include kind
-- unification variables.
zonkTcTyVarBndr tyvar
  = ASSERT2( isImmutableTyVar tyvar, ppr tyvar ) do
    updateTyVarKindM zonkTcType tyvar

zonkTcTyVar :: TcTyVar -> TcM TcType
-- Simply look through all Flexis
zonkTcTyVar tv
  = ASSERT2( isTcTyVar tv, ppr tv ) do
    case tcTyVarDetails tv of
      SkolemTv {}   -> zonk_kind_and_return
      RuntimeUnk {} -> zonk_kind_and_return
      FlatSkol ty   -> zonkTcType ty
      MetaTv { mtv_ref = ref }
         -> do { cts <- readMutVar ref
               ; case cts of
	            Flexi       -> zonk_kind_and_return
	            Indirect ty -> zonkTcType ty }
  where
    zonk_kind_and_return = do { z_tv <- zonkTyVarKind tv
                              ; return (TyVarTy z_tv) }
\end{code}



%************************************************************************
%*									*
			Zonking kinds
%*									*
%************************************************************************

\begin{code}
zonkTcKind :: TcKind -> TcM TcKind
zonkTcKind k = zonkTcType k
\end{code}
			
%************************************************************************
%*									*
\subsection{Checking a user type}
%*									*
%************************************************************************

When dealing with a user-written type, we first translate it from an HsType
to a Type, performing kind checking, and then check various things that should 
be true about it.  We don't want to perform these checks at the same time
as the initial translation because (a) they are unnecessary for interface-file
types and (b) when checking a mutually recursive group of type and class decls,
we can't "look" at the tycons/classes yet.  Also, the checks are are rather
diverse, and used to really mess up the other code.

One thing we check for is 'rank'.  

	Rank 0: 	monotypes (no foralls)
	Rank 1:		foralls at the front only, Rank 0 inside
	Rank 2:		foralls at the front, Rank 1 on left of fn arrow,

	basic ::= tyvar | T basic ... basic

	r2  ::= forall tvs. cxt => r2a
	r2a ::= r1 -> r2a | basic
	r1  ::= forall tvs. cxt => r0
	r0  ::= r0 -> r0 | basic
	
Another thing is to check that type synonyms are saturated. 
This might not necessarily show up in kind checking.
	type A i = i
	data T k = MkT (k Int)
	f :: T A	-- BAD!

	
\begin{code}
check_kind :: UserTypeCtxt -> TcType -> TcM ()
-- Check that the type's kind is acceptable for the context
check_kind ctxt ty
  | TySynCtxt {} <- ctxt
  = do { ck <- xoptM Opt_ConstraintKinds
       ; unless ck $
         checkTc (not (returnsConstraintKind actual_kind)) 
                 (constraintSynErr actual_kind) }

  | Just k <- expectedKindInCtxt ctxt
  = checkTc (tcIsSubKind actual_kind k) (kindErr actual_kind)

  | otherwise
  = return ()   -- Any kind will do
  where
    actual_kind = typeKind ty

-- Depending on the context, we might accept any kind (for instance, in a TH
-- splice), or only certain kinds (like in type signatures).
expectedKindInCtxt :: UserTypeCtxt -> Maybe Kind
expectedKindInCtxt (TySynCtxt _)  = Nothing -- Any kind will do
expectedKindInCtxt ThBrackCtxt    = Nothing
expectedKindInCtxt GhciCtxt       = Nothing
expectedKindInCtxt (ForSigCtxt _) = Just liftedTypeKind
expectedKindInCtxt InstDeclCtxt   = Just constraintKind
expectedKindInCtxt SpecInstCtxt   = Just constraintKind
expectedKindInCtxt _              = Just openTypeKind

checkValidType :: UserTypeCtxt -> Type -> TcM ()
-- Checks that the type is valid for the given context
-- Not used for instance decls; checkValidInstance instead
checkValidType ctxt ty 
  = do { traceTc "checkValidType" (ppr ty <+> text "::" <+> ppr (typeKind ty))
       ; rankn_flag  <- xoptM Opt_RankNTypes
       ; let gen_rank :: Rank -> Rank
             gen_rank r | rankn_flag = ArbitraryRank
	                | otherwise  = r

             rank1 = gen_rank r1
             rank0 = gen_rank r0

             r0 = rankZeroMonoType
             r1 = LimitedRank True r0

             rank
	       = case ctxt of
	     	 DefaultDeclCtxt-> MustBeMonoType
	     	 ResSigCtxt	-> MustBeMonoType
	     	 LamPatSigCtxt	-> rank0
	     	 BindPatSigCtxt	-> rank0
	     	 RuleSigCtxt _  -> rank1
	     	 TySynCtxt _    -> rank0

	     	 ExprSigCtxt 	-> rank1
	     	 FunSigCtxt _   -> rank1
	     	 InfSigCtxt _   -> ArbitraryRank	-- Inferred type
	     	 ConArgCtxt _   -> rank1 -- We are given the type of the entire
                                         -- constructor, hence rank 1

	     	 ForSigCtxt _	-> rank1
	     	 SpecInstCtxt   -> rank1
                 ThBrackCtxt    -> rank1
	     	 GhciCtxt       -> ArbitraryRank
                 _              -> panic "checkValidType"
                                          -- Can't happen; not used for *user* sigs

	-- Check the internal validity of the type itself
       ; check_type rank ty

	-- Check that the thing has kind Type, and is lifted if necessary
	-- Do this second, because we can't usefully take the kind of an 
	-- ill-formed type such as (a~Int)
       ; check_kind ctxt ty }

checkValidMonoType :: Type -> TcM ()
checkValidMonoType ty = check_mono_type MustBeMonoType ty
\end{code}

Note [Higher rank types]
~~~~~~~~~~~~~~~~~~~~~~~~
Technically 
	    Int -> forall a. a->a
is still a rank-1 type, but it's not Haskell 98 (Trac #5957).  So the
validity checker allow a forall after an arrow only if we allow it
before -- that is, with Rank2Types or RankNTypes

\begin{code}
data Rank = ArbitraryRank	  -- Any rank ok

          | LimitedRank   -- Note [Higher rank types]
                 Bool     -- Forall ok at top
                 Rank     -- Use for function arguments

          | MonoType SDoc   -- Monotype, with a suggestion of how it could be a polytype
  
          | MustBeMonoType  -- Monotype regardless of flags

rankZeroMonoType, tyConArgMonoType, synArgMonoType :: Rank
rankZeroMonoType = MonoType (ptext (sLit "Perhaps you intended to use -XRankNTypes or -XRank2Types"))
tyConArgMonoType = MonoType (ptext (sLit "Perhaps you intended to use -XImpredicativeTypes"))
synArgMonoType   = MonoType (ptext (sLit "Perhaps you intended to use -XLiberalTypeSynonyms"))

funArgResRank :: Rank -> (Rank, Rank)		  -- Function argument and result
funArgResRank (LimitedRank _ arg_rank) = (arg_rank, LimitedRank (forAllAllowed arg_rank) arg_rank)
funArgResRank other_rank               = (other_rank, other_rank)

forAllAllowed :: Rank -> Bool
forAllAllowed ArbitraryRank             = True
forAllAllowed (LimitedRank forall_ok _) = forall_ok
forAllAllowed _        	                = False

----------------------------------------
check_mono_type :: Rank -> KindOrType -> TcM ()	-- No foralls anywhere
				      		-- No unlifted types of any kind
check_mono_type rank ty
  | isKind ty = return ()  -- IA0_NOTE: Do we need to check kinds?
  | otherwise
   = do { check_type rank ty
	; checkTc (not (isUnLiftedType ty)) (unliftedArgErr ty) }

check_type :: Rank -> Type -> TcM ()
-- The args say what the *type context* requires, independent
-- of *flag* settings.  You test the flag settings at usage sites.
-- 
-- Rank is allowed rank for function args
-- Rank 0 means no for-alls anywhere

check_type rank ty
  | not (null tvs && null theta)
  = do	{ checkTc (forAllAllowed rank) (forAllTyErr rank ty)
		-- Reject e.g. (Maybe (?x::Int => Int)), 
		-- with a decent error message
	; check_valid_theta SigmaCtxt theta
	; check_type rank tau	-- Allow foralls to right of arrow
	; checkAmbiguity tvs theta (tyVarsOfType tau) }
  where
    (tvs, theta, tau) = tcSplitSigmaTy ty
   
check_type _ (TyVarTy _) = return ()

check_type rank (FunTy arg_ty res_ty)
  = do	{ check_type arg_rank arg_ty
	; check_type res_rank res_ty }
  where
    (arg_rank, res_rank) = funArgResRank rank

check_type rank (AppTy ty1 ty2)
  = do	{ check_arg_type rank ty1
	; check_arg_type rank ty2 }

check_type rank ty@(TyConApp tc tys)
  | isSynTyCon tc
  = do	{ 	-- Check that the synonym has enough args
		-- This applies equally to open and closed synonyms
	 	-- It's OK to have an *over-applied* type synonym
		--	data Tree a b = ...
		--	type Foo a = Tree [a]
		--	f :: Foo a b -> ...
 	  checkTc (tyConArity tc <= length tys) arity_msg

	-- See Note [Liberal type synonyms]
	; liberal <- xoptM Opt_LiberalTypeSynonyms
	; if not liberal || isSynFamilyTyCon tc then
		-- For H98 and synonym families, do check the type args
		mapM_ (check_mono_type synArgMonoType) tys

	  else	-- In the liberal case (only for closed syns), expand then check
	  case tcView ty of   
	     Just ty' -> check_type rank ty' 
	     Nothing  -> pprPanic "check_tau_type" (ppr ty)
    }
    
  | isUnboxedTupleTyCon tc
  = do	{ ub_tuples_allowed <- xoptM Opt_UnboxedTuples
	; checkTc ub_tuples_allowed ubx_tup_msg

	; impred <- xoptM Opt_ImpredicativeTypes	
	; let rank' = if impred then ArbitraryRank else tyConArgMonoType
		-- c.f. check_arg_type
		-- However, args are allowed to be unlifted, or
		-- more unboxed tuples, so can't use check_arg_ty
	; mapM_ (check_type rank') tys }

  | otherwise
  = mapM_ (check_arg_type rank) tys

  where
    n_args    = length tys
    tc_arity  = tyConArity tc

    arity_msg   = arityErr "Type synonym" (tyConName tc) tc_arity n_args
    ubx_tup_msg = ubxArgTyErr ty

check_type _ (LitTy {}) = return ()

check_type _ ty = pprPanic "check_type" (ppr ty)

----------------------------------------
check_arg_type :: Rank -> KindOrType -> TcM ()
-- The sort of type that can instantiate a type variable,
-- or be the argument of a type constructor.
-- Not an unboxed tuple, but now *can* be a forall (since impredicativity)
-- Other unboxed types are very occasionally allowed as type
-- arguments depending on the kind of the type constructor
-- 
-- For example, we want to reject things like:
--
--	instance Ord a => Ord (forall s. T s a)
-- and
--	g :: T s (forall b.b)
--
-- NB: unboxed tuples can have polymorphic or unboxed args.
--     This happens in the workers for functions returning
--     product types with polymorphic components.
--     But not in user code.
-- Anyway, they are dealt with by a special case in check_tau_type

check_arg_type rank ty
  | isKind ty = return ()  -- IA0_NOTE: Do we need to check a kind?
  | otherwise
  = do	{ impred <- xoptM Opt_ImpredicativeTypes
	; let rank' = case rank of 	    -- Predictive => must be monotype
	      	        MustBeMonoType     -> MustBeMonoType  -- Monotype, regardless
			_other | impred    -> ArbitraryRank
			       | otherwise -> tyConArgMonoType
			-- Make sure that MustBeMonoType is propagated, 
			-- so that we don't suggest -XImpredicativeTypes in
			--    (Ord (forall a.a)) => a -> a
			-- and so that if it Must be a monotype, we check that it is!

	; check_type rank' ty
	; checkTc (not (isUnLiftedType ty)) (unliftedArgErr ty) }
             -- NB the isUnLiftedType test also checks for 
             --    T State#
             -- where there is an illegal partial application of State# (which has
             -- kind * -> #); see Note [The kind invariant] in TypeRep

----------------------------------------
forAllTyErr :: Rank -> Type -> SDoc
forAllTyErr rank ty 
   = vcat [ hang (ptext (sLit "Illegal polymorphic or qualified type:")) 2 (ppr ty)
          , suggestion ]
  where
    suggestion = case rank of
    	       	   LimitedRank {} -> ptext (sLit "Perhaps you intended to use -XRankNTypes or -XRank2Types")
    	       	   MonoType d     -> d
		   _              -> empty      -- Polytype is always illegal

unliftedArgErr, ubxArgTyErr :: Type -> SDoc
unliftedArgErr  ty = sep [ptext (sLit "Illegal unlifted type:"), ppr ty]
ubxArgTyErr     ty = sep [ptext (sLit "Illegal unboxed tuple type as function argument:"), ppr ty]

kindErr :: Kind -> SDoc
kindErr kind = sep [ptext (sLit "Expecting an ordinary type, but found a type of kind"), ppr kind]
\end{code}

Note [Liberal type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If -XLiberalTypeSynonyms is on, expand closed type synonyms *before*
doing validity checking.  This allows us to instantiate a synonym defn
with a for-all type, or with a partially-applied type synonym.
	e.g.   type T a b = a
	       type S m   = m ()
	       f :: S (T Int)
Here, T is partially applied, so it's illegal in H98.  But if you
expand S first, then T we get just
	       f :: Int
which is fine.

IMPORTANT: suppose T is a type synonym.  Then we must do validity
checking on an appliation (T ty1 ty2)

	*either* before expansion (i.e. check ty1, ty2)
	*or* after expansion (i.e. expand T ty1 ty2, and then check)
	BUT NOT BOTH

If we do both, we get exponential behaviour!!

  data TIACons1 i r c = c i ::: r c
  type TIACons2 t x = TIACons1 t (TIACons1 t x)
  type TIACons3 t x = TIACons2 t (TIACons1 t x)
  type TIACons4 t x = TIACons2 t (TIACons2 t x)
  type TIACons7 t x = TIACons4 t (TIACons3 t x)


%************************************************************************
%*									*
\subsection{Checking a theta or source type}
%*									*
%************************************************************************

\begin{code}
checkValidTheta :: UserTypeCtxt -> ThetaType -> TcM ()
checkValidTheta ctxt theta 
  = addErrCtxt (checkThetaCtxt ctxt theta) (check_valid_theta ctxt theta)

-------------------------
check_valid_theta :: UserTypeCtxt -> [PredType] -> TcM ()
check_valid_theta _ []
  = return ()
check_valid_theta ctxt theta
  = do { dflags <- getDynFlags
       ; warnTc (wopt Opt_WarnDuplicateConstraints dflags &&
                 notNull dups) (dupPredWarn dups)
       ; mapM_ (check_pred_ty dflags ctxt) theta }
  where
    (_,dups) = removeDups cmpPred theta

-------------------------
check_pred_ty :: DynFlags -> UserTypeCtxt -> PredType -> TcM ()
-- Check the validity of a predicate in a signature
-- We look through any type synonyms; any constraint kinded
-- type synonyms have been checked at their definition site

check_pred_ty dflags ctxt pred
  | Just (tc,tys) <- tcSplitTyConApp_maybe pred
  = case () of 
      _ | Just cls <- tyConClass_maybe tc
        -> check_class_pred dflags ctxt cls tys

        | tc `hasKey` eqTyConKey
        , let [_, ty1, ty2] = tys
        -> check_eq_pred dflags ctxt ty1 ty2

        | isTupleTyCon tc
        -> check_tuple_pred dflags ctxt pred tys
  
        | otherwise   -- X t1 t2, where X is presumably a
                      -- type/data family returning ConstraintKind
        -> check_irred_pred dflags ctxt pred tys

  | (TyVarTy _, arg_tys) <- tcSplitAppTys pred
  = check_irred_pred dflags ctxt pred arg_tys

  | otherwise
  = badPred pred

badPred :: PredType -> TcM ()
badPred pred = failWithTc (ptext (sLit "Malformed predicate") <+> quotes (ppr pred))

check_class_pred :: DynFlags -> UserTypeCtxt -> Class -> [TcType] -> TcM ()
check_class_pred dflags ctxt cls tys
  = do {	-- Class predicates are valid in all contexts
       ; checkTc (arity == n_tys) arity_err

		-- Check the form of the argument types
       ; mapM_ checkValidMonoType tys
       ; checkTc (check_class_pred_tys dflags ctxt tys)
		 (predTyVarErr (mkClassPred cls tys) $$ how_to_allow)
       }
  where
    class_name = className cls
    arity      = classArity cls
    n_tys      = length tys
    arity_err  = arityErr "Class" class_name arity n_tys
    how_to_allow = parens (ptext (sLit "Use -XFlexibleContexts to permit this"))


check_eq_pred :: DynFlags -> UserTypeCtxt -> TcType -> TcType -> TcM ()
check_eq_pred dflags _ctxt ty1 ty2
  = do {	-- Equational constraints are valid in all contexts if type
		-- families are permitted
       ; checkTc (xopt Opt_TypeFamilies dflags || xopt Opt_GADTs dflags) 
                 (eqPredTyErr (mkEqPred ty1 ty2))

		-- Check the form of the argument types
       ; checkValidMonoType ty1
       ; checkValidMonoType ty2
       }

check_tuple_pred :: DynFlags -> UserTypeCtxt -> PredType -> [PredType] -> TcM ()
check_tuple_pred dflags ctxt pred ts
  = do { checkTc (xopt Opt_ConstraintKinds dflags)
                 (predTupleErr pred)
       ; mapM_ (check_pred_ty dflags ctxt) ts }
    -- This case will not normally be executed because 
    -- without -XConstraintKinds tuple types are only kind-checked as *

check_irred_pred :: DynFlags -> UserTypeCtxt -> PredType -> [TcType] -> TcM ()
check_irred_pred dflags ctxt pred arg_tys
    -- The predicate looks like (X t1 t2) or (x t1 t2) :: Constraint
    -- But X is not a synonym; that's been expanded already
    --
    -- Allowing irreducible predicates in class superclasses is somewhat dangerous
    -- because we can write:
    --
    --  type family Fooish x :: * -> Constraint
    --  type instance Fooish () = Foo
    --  class Fooish () a => Foo a where
    --
    -- This will cause the constraint simplifier to loop because every time we canonicalise a
    -- (Foo a) class constraint we add a (Fooish () a) constraint which will be immediately
    -- solved to add+canonicalise another (Foo a) constraint.
    --
    -- It is equally dangerous to allow them in instance heads because in that case the
    -- Paterson conditions may not detect duplication of a type variable or size change.
  = do { checkTc (xopt Opt_ConstraintKinds dflags)
                 (predIrredErr pred)
       ; mapM_ checkValidMonoType arg_tys
       ; unless (xopt Opt_UndecidableInstances dflags) $
                 -- Make sure it is OK to have an irred pred in this context
         checkTc (case ctxt of ClassSCCtxt _ -> False; InstDeclCtxt -> False; _ -> True)
                 (predIrredBadCtxtErr pred) }

-------------------------
check_class_pred_tys :: DynFlags -> UserTypeCtxt -> [KindOrType] -> Bool
check_class_pred_tys dflags ctxt kts
  = case ctxt of
	SpecInstCtxt -> True	-- {-# SPECIALISE instance Eq (T Int) #-} is fine
	InstDeclCtxt -> flexible_contexts || undecidable_ok || all tcIsTyVarTy tys
				-- Further checks on head and theta in
				-- checkInstTermination
	_             -> flexible_contexts || all tyvar_head tys
  where
    (_, tys) = span isKind kts  -- see Note [Kind polymorphic type classes]
    flexible_contexts = xopt Opt_FlexibleContexts dflags
    undecidable_ok = xopt Opt_UndecidableInstances dflags

-------------------------
tyvar_head :: Type -> Bool
tyvar_head ty			-- Haskell 98 allows predicates of form 
  | tcIsTyVarTy ty = True	-- 	C (a ty1 .. tyn)
  | otherwise			-- where a is a type variable
  = case tcSplitAppTy_maybe ty of
	Just (ty, _) -> tyvar_head ty
	Nothing	     -> False
\end{code}

Note [Kind polymorphic type classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MultiParam check:

    class C f where...   -- C :: forall k. k -> Constraint
    instance C Maybe where...

  The dictionary gets type [C * Maybe] even if it's not a MultiParam
  type class.

Flexibility check:

    class C f where...   -- C :: forall k. k -> Constraint
    data D a = D a
    instance C D where

  The dictionary gets type [C * (D *)]. IA0_TODO it should be
  generalized actually.

Note [The ambiguity check for type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
checkAmbiguity is a check on user-supplied type signatures.  It is
*purely* there to report functions that cannot possibly be called.  So for
example we want to reject:
   f :: C a => Int
The idea is there can be no legal calls to 'f' because every call will
give rise to an ambiguous constraint.  We could soundly omit the
ambiguity check on type signatures entirely, at the expense of
delaying ambiguity errors to call sites.

What about this, though?
   g :: C [a] => Int
Is every call to 'g' ambiguous?  After all, we might have
   intance C [a] where ...
at the call site.  So maybe that type is ok!  Indeed even f's
quintessentially ambiguous type might, just possibly be callable: 
with -XFlexibleInstances we could have
  instance C a where ...
and now a call could be legal after all!  (But only with  -XFlexibleInstances!)

What about things like this:
   class D a b | a -> b where ..
   h :: D Int b => Int 
The Int may well fix 'b' at the call site, so that signature should
not be rejected.  Moreover, using *visible* fundeps is too
conservative.  Consider
   class X a b where ...
   class D a b | a -> b where ...
   instance D a b => X [a] b where...
   h :: X a b => a -> a
Here h's type looks ambiguous in 'b', but here's a legal call:
   ...(h [True])...
That gives rise to a (X [Bool] beta) constraint, and using the
instance means we need (D Bool beta) and that fixes 'beta' via D's
fundep!

 So I think the only types we can reject as *definitely* ambiguous are ones like this
   f :: (Cambig, Cnonambig) => tau
where
  * 'Cambig', 'Cnonambig' are each a set of constraints.
  * fv(Cambig) does not intersect fv( Cnonambig => tau )
  * The constraints in 'Cambig' are all of form (C a b c) 
    where a,b,c are type variables
  * 'Cambig' is non-empty
  * '-XFlexibleInstances' is not on.

And that is what checkAmbiguity does.  See Trac #6134.


Side note: the ambiguity check is only used for *user* types, not for
types coming from inteface files.  The latter can legitimately have
ambiguous types. Example

   class S a where s :: a -> (Int,Int)
   instance S Char where s _ = (1,1)
   f:: S a => [a] -> Int -> (Int,Int)
   f (_::[a]) x = (a*x,b)
	where (a,b) = s (undefined::a)

Here the worker for f gets the type
	fw :: forall a. S a => Int -> (# Int, Int #)

Note [Implicit parameters and ambiguity] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Only a *class* predicate can give rise to ambiguity
An *implicit parameter* cannot.  For example:
	foo :: (?x :: [a]) => Int
	foo = length ?x
is fine.  The call site will suppply a particular 'x'

Furthermore, the type variables fixed by an implicit parameter
propagate to the others.  E.g.
	foo :: (Show a, ?x::[a]) => Int
	foo = show (?x++?x)
The type of foo looks ambiguous.  But it isn't, because at a call site
we might have
	let ?x = 5::Int in foo
and all is well.  In effect, implicit parameters are, well, parameters,
so we can take their type variables into account as part of the
"tau-tvs" stuff.  This is done in the function 'FunDeps.grow'.


\begin{code}
checkAmbiguity :: [TyVar] -> ThetaType -> TyVarSet -> TcM ()
-- Note [The ambiguity check for type signatures]
checkAmbiguity forall_tyvars theta tau_tyvars
  = do { flexible_instances <- xoptM Opt_FlexibleInstances
       ; unless flexible_instances $
         mapM_  ambigErr (filter is_ambig candidates) }
  where
	-- See Note [Implicit parameters and ambiguity] in TcSimplify
    is_candidate pred 
      | Just (_, tys) <- getClassPredTys_maybe pred
      , all isTyVarTy tys = True
      | otherwise         = False

    forall_tv_set = mkVarSet forall_tyvars
    (candidates, others) = partition is_candidate theta
    unambig_vars = growThetaTyVars theta (tau_tyvars `unionVarSet` tyVarsOfTypes others)

    is_ambig pred = (tyVarsOfType pred `minusVarSet` unambig_vars)
                    `intersectsVarSet` forall_tv_set

ambigErr :: PredType -> TcM ()
ambigErr pred
  = addErrTc $
    sep [ptext (sLit "Ambiguous constraint") <+> quotes (pprType pred),
	 nest 2 (ptext (sLit "At least one of the forall'd type variables mentioned by the constraint") $$
		 ptext (sLit "must be reachable from the type after the '=>'"))]
\end{code}

Note [Growing the tau-tvs using constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(growInstsTyVars insts tvs) is the result of extending the set 
    of tyvars tvs using all conceivable links from pred

E.g. tvs = {a}, preds = {H [a] b, K (b,Int) c, Eq e}
Then grow precs tvs = {a,b,c}

Note [Inheriting implicit parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:

	f x = (x::Int) + ?y

where f is *not* a top-level binding.
From the RHS of f we'll get the constraint (?y::Int).
There are two types we might infer for f:

	f :: Int -> Int

(so we get ?y from the context of f's definition), or

	f :: (?y::Int) => Int -> Int

At first you might think the first was better, becuase then
?y behaves like a free variable of the definition, rather than
having to be passed at each call site.  But of course, the WHOLE
IDEA is that ?y should be passed at each call site (that's what
dynamic binding means) so we'd better infer the second.

BOTTOM LINE: when *inferring types* you *must* quantify 
over implicit parameters. See the predicate isFreeWhenInferring.

\begin{code}
quantifyPred :: TyVarSet      -- Quantifying over these
	     -> PredType -> Bool	    -- True <=> quantify over this wanted
quantifyPred qtvs pred
  | isIPPred pred = True  -- Note [Inheriting implicit parameters]
  | otherwise	  = tyVarsOfType pred `intersectsVarSet` qtvs

growThetaTyVars :: TcThetaType -> TyVarSet -> TyVarSet
-- See Note [Growing the tau-tvs using constraints]
growThetaTyVars theta tvs
  | null theta = tvs
  | otherwise  = fixVarSet mk_next tvs
  where
    mk_next tvs = foldr grow_one tvs theta
    grow_one pred tvs = growPredTyVars pred tvs `unionVarSet` tvs

growPredTyVars :: TcPredType
               -> TyVarSet	-- The set to extend
	       -> TyVarSet	-- TyVars of the predicate if it intersects the set, 
growPredTyVars pred tvs 
   | isIPPred pred                   = pred_tvs   -- Always quantify over implicit parameers
   | pred_tvs `intersectsVarSet` tvs = pred_tvs
   | otherwise                       = emptyVarSet
  where
    pred_tvs = tyVarsOfType pred
\end{code}
    

\begin{code}
checkThetaCtxt :: UserTypeCtxt -> ThetaType -> SDoc
checkThetaCtxt ctxt theta
  = vcat [ptext (sLit "In the context:") <+> pprTheta theta,
	  ptext (sLit "While checking") <+> pprUserTypeCtxt ctxt ]

eqPredTyErr, predTyVarErr, predTupleErr, predIrredErr, predIrredBadCtxtErr :: PredType -> SDoc
eqPredTyErr  pred = ptext (sLit "Illegal equational constraint") <+> pprType pred
		    $$
		    parens (ptext (sLit "Use -XGADTs or -XTypeFamilies to permit this"))
predTyVarErr pred  = hang (ptext (sLit "Non type-variable argument"))
			2 (ptext (sLit "in the constraint:") <+> pprType pred)
predTupleErr pred  = hang (ptext (sLit "Illegal tuple constraint:") <+> pprType pred)
                        2 (parens (ptext (sLit "Use -XConstraintKinds to permit this")))
predIrredErr pred  = hang (ptext (sLit "Illegal constraint:") <+> pprType pred)
                        2 (parens (ptext (sLit "Use -XConstraintKinds to permit this")))
predIrredBadCtxtErr pred = hang (ptext (sLit "Illegal constraint") <+> quotes (pprType pred)
                                 <+> ptext (sLit "in a superclass/instance context")) 
                               2 (parens (ptext (sLit "Use -XUndecidableInstances to permit this")))

constraintSynErr :: Type -> SDoc
constraintSynErr kind = hang (ptext (sLit "Illegal constraint synonym of kind:") <+> quotes (ppr kind))
                           2 (parens (ptext (sLit "Use -XConstraintKinds to permit this")))

dupPredWarn :: [[PredType]] -> SDoc
dupPredWarn dups   = ptext (sLit "Duplicate constraint(s):") <+> pprWithCommas pprType (map head dups)

arityErr :: Outputable a => String -> a -> Int -> Int -> SDoc
arityErr kind name n m
  = hsep [ text kind, quotes (ppr name), ptext (sLit "should have"),
	   n_arguments <> comma, text "but has been given", 
           if m==0 then text "none" else int m]
    where
	n_arguments | n == 0 = ptext (sLit "no arguments")
		    | n == 1 = ptext (sLit "1 argument")
		    | True   = hsep [int n, ptext (sLit "arguments")]
\end{code}

%************************************************************************
%*									*
\subsection{Checking for a decent instance head type}
%*									*
%************************************************************************

@checkValidInstHead@ checks the type {\em and} its syntactic constraints:
it must normally look like: @instance Foo (Tycon a b c ...) ...@

The exceptions to this syntactic checking: (1)~if the @GlasgowExts@
flag is on, or (2)~the instance is imported (they must have been
compiled elsewhere). In these cases, we let them go through anyway.

We can also have instances for functions: @instance Foo (a -> b) ...@.

\begin{code}
checkValidInstHead :: UserTypeCtxt -> Class -> [Type] -> TcM ()
checkValidInstHead ctxt clas cls_args
  = do { dflags <- getDynFlags

           -- Check language restrictions; 
           -- but not for SPECIALISE isntance pragmas
       ; let ty_args = dropWhile isKind cls_args
       ; unless spec_inst_prag $
         do { checkTc (xopt Opt_TypeSynonymInstances dflags ||
                       all tcInstHeadTyNotSynonym ty_args)
                 (instTypeErr clas cls_args head_type_synonym_msg)
            ; checkTc (xopt Opt_FlexibleInstances dflags ||
                       all tcInstHeadTyAppAllTyVars ty_args)
                 (instTypeErr clas cls_args head_type_args_tyvars_msg)
            ; checkTc (xopt Opt_MultiParamTypeClasses dflags ||
                       isSingleton ty_args)  -- Only count type arguments
                 (instTypeErr clas cls_args head_one_type_msg) }

         -- May not contain type family applications
       ; mapM_ checkTyFamFreeness ty_args

       ; mapM_ checkValidMonoType ty_args
	-- For now, I only allow tau-types (not polytypes) in 
	-- the head of an instance decl.  
	-- 	E.g.  instance C (forall a. a->a) is rejected
	-- One could imagine generalising that, but I'm not sure
	-- what all the consequences might be
       }

  where
    spec_inst_prag = case ctxt of { SpecInstCtxt -> True; _ -> False }

    head_type_synonym_msg = parens (
                text "All instance types must be of the form (T t1 ... tn)" $$
                text "where T is not a synonym." $$
                text "Use -XTypeSynonymInstances if you want to disable this.")

    head_type_args_tyvars_msg = parens (vcat [
                text "All instance types must be of the form (T a1 ... an)",
                text "where a1 ... an are *distinct type variables*,",
                text "and each type variable appears at most once in the instance head.",
                text "Use -XFlexibleInstances if you want to disable this."])

    head_one_type_msg = parens (
                text "Only one type can be given in an instance head." $$
                text "Use -XMultiParamTypeClasses if you want to allow more.")

instTypeErr :: Class -> [Type] -> SDoc -> SDoc
instTypeErr cls tys msg
  = hang (ptext (sLit "Illegal instance declaration for") 
          <+> quotes (pprClassPred cls tys))
       2 msg
\end{code}

validDeivPred checks for OK 'deriving' context.  See Note [Exotic
derived instance contexts] in TcSimplify.  However the predicate is
here because it uses sizeTypes, fvTypes.

Also check for a bizarre corner case, when the derived instance decl 
would look like
    instance C a b => D (T a) where ...
Note that 'b' isn't a parameter of T.  This gives rise to all sorts of
problems; in particular, it's hard to compare solutions for equality
when finding the fixpoint, and that means the inferContext loop does
not converge.  See Trac #5287.

\begin{code}
validDerivPred :: TyVarSet -> PredType -> Bool
validDerivPred tv_set pred
  = case classifyPredType pred of
       ClassPred _ tys -> hasNoDups fvs 
                       && sizeTypes tys == length fvs
                       && all (`elemVarSet` tv_set) fvs
       TuplePred ps -> all (validDerivPred tv_set) ps
       _            -> True   -- Non-class predicates are ok
  where
    fvs = fvType pred
\end{code}


%************************************************************************
%*									*
\subsection{Checking instance for termination}
%*									*
%************************************************************************

\begin{code}
checkValidInstance :: UserTypeCtxt -> LHsType Name -> Type
                   -> TcM ([TyVar], ThetaType, Class, [Type])
checkValidInstance ctxt hs_type ty
  = do { let (tvs, theta, tau) = tcSplitSigmaTy ty
       ; case getClassPredTys_maybe tau of {
           Nothing          -> failWithTc (ptext (sLit "Malformed instance type")) ;
           Just (clas,inst_tys)  -> 
    do  { setSrcSpan head_loc (checkValidInstHead ctxt clas inst_tys)
        ; checkValidTheta ctxt theta

        -- The Termination and Coverate Conditions
	-- Check that instance inference will terminate (if we care)
	-- For Haskell 98 this will already have been done by checkValidTheta,
        -- but as we may be using other extensions we need to check.
        -- 
        -- Note that the Termination Condition is *more conservative* than 
        -- the checkAmbiguity test we do on other type signatures
        --   e.g.  Bar a => Bar Int is ambiguous, but it also fails
        --   the termination condition, because 'a' appears more often
        --   in the constraint than in the head
	; undecidable_ok <- xoptM Opt_UndecidableInstances
        ; unless undecidable_ok $
	  do { checkInstTermination inst_tys theta
	     ; checkTc (checkInstCoverage clas inst_tys)
                       (instTypeErr clas inst_tys msg) }
	  	  
        ; return (tvs, theta, clas, inst_tys) } } }
  where
    msg  = parens (vcat [ptext (sLit "the Coverage Condition fails for one of the functional dependencies;"),
			 undecidableMsg])

        -- The location of the "head" of the instance
    head_loc = case hs_type of
                 L _ (HsForAllTy _ _ _ (L loc _)) -> loc
                 L loc _                          -> loc
\end{code}

Note [Paterson conditions]
~~~~~~~~~~~~~~~~~~~~~~~~~~

Termination test: the so-called "Paterson conditions" (see Section 5 of
"Understanding functionsl dependencies via Constraint Handling Rules, 
JFP Jan 2007).

We check that each assertion in the context satisfies:
 (1) no variable has more occurrences in the assertion than in the head, and
 (2) the assertion has fewer constructors and variables (taken together
     and counting repetitions) than the head.
This is only needed with -fglasgow-exts, as Haskell 98 restrictions
(which have already been checked) guarantee termination. 

The underlying idea is that 

    for any ground substitution, each assertion in the
    context has fewer type constructors than the head.


\begin{code}
checkInstTermination :: [TcType] -> ThetaType -> TcM ()
checkInstTermination tys theta
  = mapM_ check theta
  where
   fvs  = fvTypes tys
   size = sizeTypes tys
   check pred 
      | not (null bad_tvs)
      = addErrTc (predUndecErr pred (nomoreMsg bad_tvs) $$ parens undecidableMsg)
      | sizePred pred >= size
      = addErrTc (predUndecErr pred smallerMsg $$ parens undecidableMsg)
      | otherwise
      = return ()
      where
        bad_tvs = filterOut isKindVar (fvType pred \\ fvs)
             -- Rightly or wrongly, we only check for
             -- excessive occurrences of *type* variables.
             -- e.g. type instance Demote {T k} a = T (Demote {k} (Any {k}))

predUndecErr :: PredType -> SDoc -> SDoc
predUndecErr pred msg = sep [msg,
			nest 2 (ptext (sLit "in the constraint:") <+> pprType pred)]

nomoreMsg :: [TcTyVar] -> SDoc
nomoreMsg tvs 
  = sep [ ptext (sLit "Variable") <> plural tvs <+> quotes (pprWithCommas ppr tvs) 
        , (if isSingleton tvs then ptext (sLit "occurs")
                                  else ptext (sLit "occur"))
          <+> ptext (sLit "more often than in the instance head") ]

smallerMsg, undecidableMsg :: SDoc
smallerMsg = ptext (sLit "Constraint is no smaller than the instance head")
undecidableMsg = ptext (sLit "Use -XUndecidableInstances to permit this")
\end{code}


%************************************************************************
%*									*
	Checking type instance well-formedness and termination
%*									*
%************************************************************************

\begin{code}
-- Check that a "type instance" is well-formed (which includes decidability
-- unless -XUndecidableInstances is given).
--
checkValidTyFamInst :: TyCon -> [TyVar] -> [Type] -> Type -> TcM ()
checkValidTyFamInst fam_tc tvs typats rhs
  = do { checkValidFamPats fam_tc tvs typats

         -- The right-hand side is a tau type
       ; checkValidMonoType rhs

         -- we have a decidable instance unless otherwise permitted
       ; undecidable_ok <- xoptM Opt_UndecidableInstances
       ; unless undecidable_ok $
	   mapM_ addErrTc (checkFamInstRhs typats (tcTyFamInsts rhs))
       }

-- Make sure that each type family application is 
--   (1) strictly smaller than the lhs,
--   (2) mentions no type variable more often than the lhs, and
--   (3) does not contain any further type family instances.
--
checkFamInstRhs :: [Type]                  -- lhs
             	-> [(TyCon, [Type])]       -- type family instances
             	-> [MsgDoc]
checkFamInstRhs lhsTys famInsts
  = mapCatMaybes check famInsts
  where
   size = sizeTypes lhsTys
   fvs  = fvTypes lhsTys
   check (tc, tys)
      | not (all isTyFamFree tys)
      = Just (famInstUndecErr famInst nestedMsg $$ parens undecidableMsg)
      | not (null bad_tvs)
      = Just (famInstUndecErr famInst (nomoreMsg bad_tvs) $$ parens undecidableMsg)
      | size <= sizeTypes tys
      = Just (famInstUndecErr famInst smallerAppMsg $$ parens undecidableMsg)
      | otherwise
      = Nothing
      where
        famInst = TyConApp tc tys
        bad_tvs = filterOut isKindVar (fvTypes tys \\ fvs)
             -- Rightly or wrongly, we only check for
             -- excessive occurrences of *type* variables.
             -- e.g. type instance Demote {T k} a = T (Demote {k} (Any {k}))

checkValidFamPats :: TyCon -> [TyVar] -> [Type] -> TcM ()
-- Patterns in a 'type instance' or 'data instance' decl should
-- a) contain no type family applications
--    (vanilla synonyms are fine, though)
-- b) properly bind all their free type variables
--    e.g. we disallow (Trac #7536)
--         type T a = Int
--         type instance F (T a) = a
checkValidFamPats fam_tc tvs ty_pats
  = do { mapM_ checkTyFamFreeness ty_pats
       ; let unbound_tvs = filterOut (`elemVarSet` exactTyVarsOfTypes ty_pats) tvs
       ; checkTc (null unbound_tvs) (famPatErr fam_tc unbound_tvs ty_pats) }

-- Ensure that no type family instances occur in a type.
--
checkTyFamFreeness :: Type -> TcM ()
checkTyFamFreeness ty
  = checkTc (isTyFamFree ty) $
    tyFamInstIllegalErr ty

-- Check that a type does not contain any type family applications.
--
isTyFamFree :: Type -> Bool
isTyFamFree = null . tcTyFamInsts

-- Error messages

tyFamInstIllegalErr :: Type -> SDoc
tyFamInstIllegalErr ty
  = hang (ptext (sLit "Illegal type synonym family application in instance") <> 
         colon) 2 $
      ppr ty

famInstUndecErr :: Type -> SDoc -> SDoc
famInstUndecErr ty msg 
  = sep [msg, 
         nest 2 (ptext (sLit "in the type family application:") <+> 
                 pprType ty)]

famPatErr :: TyCon -> [TyVar] -> [Type] -> SDoc
famPatErr fam_tc tvs pats
  = hang (ptext (sLit "Family instance purports to bind type variable") <> plural tvs
          <+> pprQuotedList tvs)
       2 (hang (ptext (sLit "but the real LHS (expanding synonyms) is:"))
             2 (pprTypeApp fam_tc (map expandTypeSynonyms pats) <+> ptext (sLit "= ...")))

nestedMsg, smallerAppMsg :: SDoc
nestedMsg     = ptext (sLit "Nested type family application")
smallerAppMsg = ptext (sLit "Application is no smaller than the instance head")
\end{code}


%************************************************************************
%*									*
\subsection{Auxiliary functions}
%*									*
%************************************************************************

\begin{code}
-- Free variables of a type, retaining repetitions, and expanding synonyms
fvType :: Type -> [TyVar]
fvType ty | Just exp_ty <- tcView ty = fvType exp_ty
fvType (TyVarTy tv)        = [tv]
fvType (TyConApp _ tys)    = fvTypes tys
fvType (LitTy {})          = []
fvType (FunTy arg res)     = fvType arg ++ fvType res
fvType (AppTy fun arg)     = fvType fun ++ fvType arg
fvType (ForAllTy tyvar ty) = filter (/= tyvar) (fvType ty)

fvTypes :: [Type] -> [TyVar]
fvTypes tys                = concat (map fvType tys)

sizeType :: Type -> Int
-- Size of a type: the number of variables and constructors
sizeType ty | Just exp_ty <- tcView ty = sizeType exp_ty
sizeType (TyVarTy {})      = 1
sizeType (TyConApp _ tys)  = sizeTypes tys + 1
sizeType (LitTy {})        = 1
sizeType (FunTy arg res)   = sizeType arg + sizeType res + 1
sizeType (AppTy fun arg)   = sizeType fun + sizeType arg
sizeType (ForAllTy _ ty)   = sizeType ty

sizeTypes :: [Type] -> Int
-- IA0_NOTE: Avoid kinds.
sizeTypes xs = sum (map sizeType tys)
  where tys = filter (not . isKind) xs

-- Size of a predicate
--
-- We are considering whether class constraints terminate.
-- Equality constraints and constraints for the implicit
-- parameter class always termiante so it is safe to say "size 0".
-- (Implicit parameter constraints always terminate because
-- there are no instances for them---they are only solved by
-- "local instances" in expressions).
-- See Trac #4200.
sizePred :: PredType -> Int
sizePred ty = goClass ty
  where
    goClass p | isIPPred p = 0
              | otherwise  = go (classifyPredType p)

    go (ClassPred _ tys') = sizeTypes tys'
    go (EqPred {})        = 0
    go (TuplePred ts)     = sum (map goClass ts)
    go (IrredPred ty)     = sizeType ty
\end{code}

Note [Paterson conditions on PredTypes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We are considering whether *class* constraints terminate
(see Note [Paterson conditions]). Precisely, the Paterson conditions
would have us check that "the constraint has fewer constructors and variables
(taken together and counting repetitions) than the head.".

However, we can be a bit more refined by looking at which kind of constraint
this actually is. There are two main tricks:

 1. It seems like it should be OK not to count the tuple type constructor
    for a PredType like (Show a, Eq a) :: Constraint, since we don't
    count the "implicit" tuple in the ThetaType itself.

    In fact, the Paterson test just checks *each component* of the top level
    ThetaType against the size bound, one at a time. By analogy, it should be
    OK to return the size of the *largest* tuple component as the size of the
    whole tuple.

 2. Once we get into an implicit parameter or equality we
    can't get back to a class constraint, so it's safe
    to say "size 0".  See Trac #4200.

NB: we don't want to detect PredTypes in sizeType (and then call 
sizePred on them), or we might get an infinite loop if that PredType
is irreducible. See Trac #5581.
