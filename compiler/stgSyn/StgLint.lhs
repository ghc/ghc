%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[StgLint]{A ``lint'' pass to check for Stg correctness}

\begin{code}
{-# LANGUAGE CPP #-}

module StgLint ( lintStgBindings ) where

import StgSyn

import Bag              ( Bag, emptyBag, isEmptyBag, snocBag, bagToList )
import Id               ( Id, idType, isLocalId )
import VarSet
import DataCon
import CoreSyn          ( AltCon(..) )
import PrimOp           ( primOpType )
import Literal          ( literalType )
import Maybes
import Name             ( getSrcLoc )
import ErrUtils         ( MsgDoc, Severity(..), mkLocMessage )
import TypeRep
import Type
import TyCon
import Util 
import SrcLoc
import Outputable
import FastString
import Control.Applicative ( Applicative(..) )
import Control.Monad
import Data.Function

#include "HsVersions.h"
\end{code}

Checks for
        (a) *some* type errors
        (b) locally-defined variables used but not defined


Note: unless -dverbose-stg is on, display of lint errors will result
in "panic: bOGUS_LVs".

WARNING:
~~~~~~~~

This module has suffered bit-rot; it is likely to yield lint errors
for Stg code that is currently perfectly acceptable for code
generation.  Solution: don't use it!  (KSW 2000-05).


%************************************************************************
%*                                                                      *
\subsection{``lint'' for various constructs}
%*                                                                      *
%************************************************************************

@lintStgBindings@ is the top-level interface function.

\begin{code}
lintStgBindings :: String -> [StgBinding] -> [StgBinding]

lintStgBindings whodunnit binds
  = {-# SCC "StgLint" #-}
    case (initL (lint_binds binds)) of
      Nothing  -> binds
      Just msg -> pprPanic "" (vcat [
                        ptext (sLit "*** Stg Lint ErrMsgs: in") <+>
                              text whodunnit <+> ptext (sLit "***"),
                        msg,
                        ptext (sLit "*** Offending Program ***"),
                        pprStgBindings binds,
                        ptext (sLit "*** End of Offense ***")])
  where
    lint_binds :: [StgBinding] -> LintM ()

    lint_binds [] = return ()
    lint_binds (bind:binds) = do
        binders <- lintStgBinds bind
        addInScopeVars binders $
            lint_binds binds
\end{code}


\begin{code}
lintStgArg :: StgArg -> LintM (Maybe Type)
lintStgArg (StgLitArg lit) = return (Just (literalType lit))
lintStgArg (StgVarArg v)   = lintStgVar v

lintStgVar :: Id -> LintM (Maybe Kind)
lintStgVar v = do checkInScope v
                  return (Just (idType v))
\end{code}

\begin{code}
lintStgBinds :: StgBinding -> LintM [Id] -- Returns the binders
lintStgBinds (StgNonRec binder rhs) = do
    lint_binds_help (binder,rhs)
    return [binder]

lintStgBinds (StgRec pairs)
  = addInScopeVars binders $ do
        mapM_ lint_binds_help pairs
        return binders
  where
    binders = [b | (b,_) <- pairs]

lint_binds_help :: (Id, StgRhs) -> LintM ()
lint_binds_help (binder, rhs)
  = addLoc (RhsOf binder) $ do
        -- Check the rhs
        _maybe_rhs_ty <- lintStgRhs rhs

        -- Check binder doesn't have unlifted type
        checkL (not (isUnLiftedType binder_ty))
               (mkUnLiftedTyMsg binder rhs)

        -- Check match to RHS type
        -- Actually we *can't* check the RHS type, because
        -- unsafeCoerce means it really might not match at all
        -- notably;  eg x::Int = (error @Bool "urk") |> unsafeCoerce...
        -- case maybe_rhs_ty of
        --  Nothing     -> return ()
        --    Just rhs_ty -> checkTys binder_ty
        --                          rhs_ty
        ---                         (mkRhsMsg binder rhs_ty)

        return ()
  where
    binder_ty = idType binder
\end{code}

\begin{code}
lintStgRhs :: StgRhs -> LintM (Maybe Type)   -- Just ty => type is exact

lintStgRhs (StgRhsClosure _ _ _ _ _ [] expr)
  = lintStgExpr expr

lintStgRhs (StgRhsClosure _ _ _ _ _ binders expr)
  = addLoc (LambdaBodyOf binders) $
      addInScopeVars binders $ runMaybeT $ do
        body_ty <- MaybeT $ lintStgExpr expr
        return (mkFunTys (map idType binders) body_ty)

lintStgRhs (StgRhsCon _ con args) = runMaybeT $ do
    arg_tys <- mapM (MaybeT . lintStgArg) args
    MaybeT $ checkFunApp con_ty arg_tys (mkRhsConMsg con_ty arg_tys)
  where
    con_ty = dataConRepType con
\end{code}

\begin{code}
lintStgExpr :: StgExpr -> LintM (Maybe Type) -- Just ty => type is exact

lintStgExpr (StgLit l) = return (Just (literalType l))

lintStgExpr e@(StgApp fun args) = runMaybeT $ do
    fun_ty <- MaybeT $ lintStgVar fun
    arg_tys <- mapM (MaybeT . lintStgArg) args
    MaybeT $ checkFunApp fun_ty arg_tys (mkFunAppMsg fun_ty arg_tys e)

lintStgExpr e@(StgConApp con args) = runMaybeT $ do
    arg_tys <- mapM (MaybeT . lintStgArg) args
    MaybeT $ checkFunApp con_ty arg_tys (mkFunAppMsg con_ty arg_tys e)
  where
    con_ty = dataConRepType con

lintStgExpr e@(StgOpApp (StgPrimOp op) args _) = runMaybeT $ do
    arg_tys <- mapM (MaybeT . lintStgArg) args
    MaybeT $ checkFunApp op_ty arg_tys (mkFunAppMsg op_ty arg_tys e)
  where
    op_ty = primOpType op

lintStgExpr (StgOpApp _ args res_ty) = runMaybeT $ do
        -- We don't have enough type information to check
        -- the application for StgFCallOp and StgPrimCallOp; ToDo
    _maybe_arg_tys <- mapM (MaybeT . lintStgArg) args
    return res_ty

lintStgExpr (StgLam bndrs _) = do
    addErrL (ptext (sLit "Unexpected StgLam") <+> ppr bndrs)
    return Nothing

lintStgExpr (StgLet binds body) = do
    binders <- lintStgBinds binds
    addLoc (BodyOfLetRec binders) $
      addInScopeVars binders $
        lintStgExpr body

lintStgExpr (StgLetNoEscape _ _ binds body) = do
    binders <- lintStgBinds binds
    addLoc (BodyOfLetRec binders) $
      addInScopeVars binders $
        lintStgExpr body

lintStgExpr (StgSCC _ _ _ expr) = lintStgExpr expr

lintStgExpr (StgCase scrut _ _ bndr _ alts_type alts) = runMaybeT $ do
    _ <- MaybeT $ lintStgExpr scrut

    in_scope <- MaybeT $ liftM Just $
     case alts_type of
        AlgAlt tc    -> check_bndr tc >> return True
        PrimAlt tc   -> check_bndr tc >> return True
        UbxTupAlt _  -> return False -- Binder is always dead in this case
        PolyAlt      -> return True

    MaybeT $ addInScopeVars [bndr | in_scope] $
             lintStgAlts alts scrut_ty
  where
    scrut_ty          = idType bndr
    UnaryRep scrut_rep = repType scrut_ty -- Not used if scrutinee is unboxed tuple
    check_bndr tc = case tyConAppTyCon_maybe scrut_rep of
                        Just bndr_tc -> checkL (tc == bndr_tc) bad_bndr
                        Nothing      -> addErrL bad_bndr
                  where
                     bad_bndr = mkDefltMsg bndr tc

lintStgExpr e = pprPanic "lintStgExpr" (ppr e)

lintStgAlts :: [StgAlt]
            -> Type               -- Type of scrutinee
            -> LintM (Maybe Type) -- Just ty => type is accurage

lintStgAlts alts scrut_ty = do
    maybe_result_tys <- mapM (lintAlt scrut_ty) alts

    -- Check the result types
    case catMaybes (maybe_result_tys) of
      []             -> return Nothing

      (first_ty:_tys) -> do -- mapM_ check tys
                           return (Just first_ty)
        where
          -- check ty = checkTys first_ty ty (mkCaseAltMsg alts)
          -- We can't check that the alternatives have the
          -- same type, because they don't, with unsafeCoerce#

lintAlt :: Type -> (AltCon, [Id], [Bool], StgExpr) -> LintM (Maybe Type)
lintAlt _ (DEFAULT, _, _, rhs)
 = lintStgExpr rhs

lintAlt scrut_ty (LitAlt lit, _, _, rhs) = do
   checkTys (literalType lit) scrut_ty (mkAltMsg1 scrut_ty)
   lintStgExpr rhs

lintAlt scrut_ty (DataAlt con, args, _, rhs) = do
    case splitTyConApp_maybe scrut_ty of
      Just (tycon, tys_applied) | isAlgTyCon tycon &&
                                  not (isNewTyCon tycon) -> do
         let
           cons    = tyConDataCons tycon
           arg_tys = dataConInstArgTys con tys_applied
                -- This does not work for existential constructors

         checkL (con `elem` cons) (mkAlgAltMsg2 scrut_ty con)
         checkL (length args == dataConRepArity con) (mkAlgAltMsg3 con args)
         when (isVanillaDataCon con) $
           mapM_ check (zipEqual "lintAlgAlt:stg" arg_tys args)
         return ()
      _ ->
         addErrL (mkAltMsg1 scrut_ty)

    addInScopeVars args $
         lintStgExpr rhs
  where
    check (ty, arg) = checkTys ty (idType arg) (mkAlgAltMsg4 ty arg)

    -- elem: yes, the elem-list here can sometimes be long-ish,
    -- but as it's use-once, probably not worth doing anything different
    -- We give it its own copy, so it isn't overloaded.
    elem _ []       = False
    elem x (y:ys)   = x==y || elem x ys
\end{code}


%************************************************************************
%*                                                                      *
\subsection[lint-monad]{The Lint monad}
%*                                                                      *
%************************************************************************

\begin{code}
newtype LintM a = LintM
    { unLintM :: [LintLocInfo]      -- Locations
              -> IdSet              -- Local vars in scope
              -> Bag MsgDoc        -- Error messages so far
              -> (a, Bag MsgDoc)   -- Result and error messages (if any)
    }

data LintLocInfo
  = RhsOf Id            -- The variable bound
  | LambdaBodyOf [Id]   -- The lambda-binder
  | BodyOfLetRec [Id]   -- One of the binders

dumpLoc :: LintLocInfo -> (SrcSpan, SDoc)
dumpLoc (RhsOf v) =
  (srcLocSpan (getSrcLoc v), ptext (sLit " [RHS of ") <> pp_binders [v] <> char ']' )
dumpLoc (LambdaBodyOf bs) =
  (srcLocSpan (getSrcLoc (head bs)), ptext (sLit " [in body of lambda with binders ") <> pp_binders bs <> char ']' )

dumpLoc (BodyOfLetRec bs) =
  (srcLocSpan (getSrcLoc (head bs)), ptext (sLit " [in body of letrec with binders ") <> pp_binders bs <> char ']' )


pp_binders :: [Id] -> SDoc
pp_binders bs
  = sep (punctuate comma (map pp_binder bs))
  where
    pp_binder b
      = hsep [ppr b, dcolon, ppr (idType b)]
\end{code}

\begin{code}
initL :: LintM a -> Maybe MsgDoc
initL (LintM m)
  = case (m [] emptyVarSet emptyBag) of { (_, errs) ->
    if isEmptyBag errs then
        Nothing
    else
        Just (vcat (punctuate blankLine (bagToList errs)))
    }

instance Functor LintM where
      fmap = liftM

instance Applicative LintM where
      pure = return
      (<*>) = ap

instance Monad LintM where
    return a = LintM $ \_loc _scope errs -> (a, errs)
    (>>=) = thenL
    (>>)  = thenL_

thenL :: LintM a -> (a -> LintM b) -> LintM b
thenL m k = LintM $ \loc scope errs
  -> case unLintM m loc scope errs of
      (r, errs') -> unLintM (k r) loc scope errs'

thenL_ :: LintM a -> LintM b -> LintM b
thenL_ m k = LintM $ \loc scope errs
  -> case unLintM m loc scope errs of
      (_, errs') -> unLintM k loc scope errs'
\end{code}

\begin{code}
checkL :: Bool -> MsgDoc -> LintM ()
checkL True  _   = return ()
checkL False msg = addErrL msg

addErrL :: MsgDoc -> LintM ()
addErrL msg = LintM $ \loc _scope errs -> ((), addErr errs msg loc)

addErr :: Bag MsgDoc -> MsgDoc -> [LintLocInfo] -> Bag MsgDoc
addErr errs_so_far msg locs
  = errs_so_far `snocBag` mk_msg locs
  where
    mk_msg (loc:_) = let (l,hdr) = dumpLoc loc
                     in  mkLocMessage SevWarning l (hdr $$ msg)
    mk_msg []      = msg

addLoc :: LintLocInfo -> LintM a -> LintM a
addLoc extra_loc m = LintM $ \loc scope errs
   -> unLintM m (extra_loc:loc) scope errs

addInScopeVars :: [Id] -> LintM a -> LintM a
addInScopeVars ids m = LintM $ \loc scope errs
 -> -- We check if these "new" ids are already
    -- in scope, i.e., we have *shadowing* going on.
    -- For now, it's just a "trace"; we may make
    -- a real error out of it...
    let
        new_set = mkVarSet ids
    in
--  After adding -fliberate-case, Simon decided he likes shadowed
--  names after all.  WDP 94/07
--  (if isEmptyVarSet shadowed
--  then id
--  else pprTrace "Shadowed vars:" (ppr (varSetElems shadowed))) $
    unLintM m loc (scope `unionVarSet` new_set) errs
\end{code}

Checking function applications: we only check that the type has the
right *number* of arrows, we don't actually compare the types.  This
is because we can't expect the types to be equal - the type
applications and type lambdas that we use to calculate accurate types
have long since disappeared.

\begin{code}
checkFunApp :: Type                 -- The function type
            -> [Type]               -- The arg type(s)
            -> MsgDoc              -- Error message
            -> LintM (Maybe Type)   -- Just ty => result type is accurate

checkFunApp fun_ty arg_tys msg
 = do { case mb_msg of
          Just msg -> addErrL msg
          Nothing  -> return ()
      ; return mb_ty }
 where
  (mb_ty, mb_msg) = cfa True fun_ty arg_tys

  cfa :: Bool -> Type -> [Type] -> (Maybe Type          -- Accurate result?
                                   , Maybe MsgDoc)      -- Errors?

  cfa accurate fun_ty []      -- Args have run out; that's fine
      = (if accurate then Just fun_ty else Nothing, Nothing)

  cfa accurate fun_ty arg_tys@(arg_ty':arg_tys')   
      | Just (arg_ty, res_ty) <- splitFunTy_maybe fun_ty
      = if accurate && not (arg_ty `stgEqType` arg_ty') 
        then (Nothing, Just msg)       -- Arg type mismatch
        else cfa accurate res_ty arg_tys'

      | Just (_, fun_ty') <- splitForAllTy_maybe fun_ty
      = cfa False fun_ty' arg_tys

      | Just (tc,tc_args) <- splitTyConApp_maybe fun_ty
      , isNewTyCon tc
      = if length tc_args < tyConArity tc 
        then WARN( True, text "cfa: unsaturated newtype" <+> ppr fun_ty $$ msg )
             (Nothing, Nothing)   -- This is odd, but I've seen it
        else cfa False (newTyConInstRhs tc tc_args) arg_tys

      | Just tc <- tyConAppTyCon_maybe fun_ty
      , not (isSynFamilyTyCon tc)       -- Definite error
      = (Nothing, Just msg)             -- Too many args

      | otherwise
      = (Nothing, Nothing)
\end{code}

\begin{code}
stgEqType :: Type -> Type -> Bool
-- Compare types, but crudely because we have discarded
-- both casts and type applications, so types might look
-- different but be the same.  So reply "True" if in doubt.
-- "False" means that the types are definitely different.
--
-- Fundamentally this is a losing battle because of unsafeCoerce

stgEqType orig_ty1 orig_ty2 
  = gos (repType orig_ty1) (repType orig_ty2)
  where
    gos :: RepType -> RepType -> Bool
    gos (UbxTupleRep tys1) (UbxTupleRep tys2)
      = equalLength tys1 tys2 && and (zipWith go tys1 tys2)
    gos (UnaryRep ty1) (UnaryRep ty2) = go ty1 ty2
    gos _ _ = False

    go :: UnaryType -> UnaryType -> Bool
    go ty1 ty2
      | Just (tc1, tc_args1) <- splitTyConApp_maybe ty1
      , Just (tc2, tc_args2) <- splitTyConApp_maybe ty2
      , let res = if tc1 == tc2 
                  then equalLength tc_args1 tc_args2 && and (zipWith (gos `on` repType) tc_args1 tc_args2)
                  else  -- TyCons don't match; but don't bleat if either is a 
                        -- family TyCon because a coercion might have made it 
                        -- equal to something else
                    (isFamilyTyCon tc1 || isFamilyTyCon tc2)
      = if res then True
        else 
        pprTrace "stgEqType: unequal" (vcat [ppr ty1, ppr ty2]) 
        False

      | otherwise = True  -- Conservatively say "fine".  
                          -- Type variables in particular

checkInScope :: Id -> LintM ()
checkInScope id = LintM $ \loc scope errs
 -> if isLocalId id && not (id `elemVarSet` scope) then
        ((), addErr errs (hsep [ppr id, ptext (sLit "is out of scope")]) loc)
    else
        ((), errs)

checkTys :: Type -> Type -> MsgDoc -> LintM ()
checkTys ty1 ty2 msg = LintM $ \loc _scope errs
  -> if (ty1 `stgEqType` ty2)
     then ((), errs)
     else ((), addErr errs msg loc)
\end{code}

\begin{code}
_mkCaseAltMsg :: [StgAlt] -> MsgDoc
_mkCaseAltMsg _alts
  = ($$) (text "In some case alternatives, type of alternatives not all same:")
            (empty) -- LATER: ppr alts

mkDefltMsg :: Id -> TyCon -> MsgDoc
mkDefltMsg bndr tc
  = ($$) (ptext (sLit "Binder of a case expression doesn't match type of scrutinee:"))
         (ppr bndr $$ ppr (idType bndr) $$ ppr tc)

mkFunAppMsg :: Type -> [Type] -> StgExpr -> MsgDoc
mkFunAppMsg fun_ty arg_tys expr
  = vcat [text "In a function application, function type doesn't match arg types:",
              hang (ptext (sLit "Function type:")) 4 (ppr fun_ty),
              hang (ptext (sLit "Arg types:")) 4 (vcat (map (ppr) arg_tys)),
              hang (ptext (sLit "Expression:")) 4 (ppr expr)]

mkRhsConMsg :: Type -> [Type] -> MsgDoc
mkRhsConMsg fun_ty arg_tys
  = vcat [text "In a RHS constructor application, con type doesn't match arg types:",
              hang (ptext (sLit "Constructor type:")) 4 (ppr fun_ty),
              hang (ptext (sLit "Arg types:")) 4 (vcat (map (ppr) arg_tys))]

mkAltMsg1 :: Type -> MsgDoc
mkAltMsg1 ty
  = ($$) (text "In a case expression, type of scrutinee does not match patterns")
         (ppr ty)

mkAlgAltMsg2 :: Type -> DataCon -> MsgDoc
mkAlgAltMsg2 ty con
  = vcat [
        text "In some algebraic case alternative, constructor is not a constructor of scrutinee type:",
        ppr ty,
        ppr con
    ]

mkAlgAltMsg3 :: DataCon -> [Id] -> MsgDoc
mkAlgAltMsg3 con alts
  = vcat [
        text "In some algebraic case alternative, number of arguments doesn't match constructor:",
        ppr con,
        ppr alts
    ]

mkAlgAltMsg4 :: Type -> Id -> MsgDoc
mkAlgAltMsg4 ty arg
  = vcat [
        text "In some algebraic case alternative, type of argument doesn't match data constructor:",
        ppr ty,
        ppr arg
    ]

_mkRhsMsg :: Id -> Type -> MsgDoc
_mkRhsMsg binder ty
  = vcat [hsep [ptext (sLit "The type of this binder doesn't match the type of its RHS:"),
                     ppr binder],
              hsep [ptext (sLit "Binder's type:"), ppr (idType binder)],
              hsep [ptext (sLit "Rhs type:"), ppr ty]
             ]

mkUnLiftedTyMsg :: Id -> StgRhs -> SDoc
mkUnLiftedTyMsg binder rhs
  = (ptext (sLit "Let(rec) binder") <+> quotes (ppr binder) <+>
     ptext (sLit "has unlifted type") <+> quotes (ppr (idType binder)))
    $$
    (ptext (sLit "RHS:") <+> ppr rhs)
\end{code}
