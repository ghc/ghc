{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section[StgLint]{A ``lint'' pass to check for Stg correctness}
-}

{-# LANGUAGE CPP #-}

module StgLint ( lintStgTopBindings ) where

import GhcPrelude

import StgSyn

import Bag              ( Bag, emptyBag, isEmptyBag, snocBag, bagToList )
import Id               ( Id, idType, isLocalId, isJoinId )
import VarSet
import DataCon
import CoreSyn          ( AltCon(..) )
import PrimOp           ( primOpType )
import Literal          ( literalType )
import Maybes
import Name             ( getSrcLoc )
import ErrUtils         ( MsgDoc, Severity(..), mkLocMessage )
import Type
import RepType
import TyCon
import Util
import SrcLoc
import Outputable
import Control.Monad

#include "HsVersions.h"

{-
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


************************************************************************
*                                                                      *
\subsection{``lint'' for various constructs}
*                                                                      *
************************************************************************

@lintStgTopBindings@ is the top-level interface function.
-}

lintStgTopBindings :: Bool  -- ^ have we run Unarise yet?
                   -> String -> [StgTopBinding] -> [StgTopBinding]

lintStgTopBindings unarised whodunnit binds
  = {-# SCC "StgLint" #-}
    case (initL unarised (lint_binds binds)) of
      Nothing  -> binds
      Just msg -> pprPanic "" (vcat [
                        text "*** Stg Lint ErrMsgs: in" <+>
                              text whodunnit <+> text "***",
                        msg,
                        text "*** Offending Program ***",
                        pprStgTopBindings binds,
                        text "*** End of Offense ***"])
  where
    lint_binds :: [StgTopBinding] -> LintM ()

    lint_binds [] = return ()
    lint_binds (bind:binds) = do
        binders <- lint_bind bind
        addInScopeVars binders $
            lint_binds binds

    lint_bind (StgTopLifted bind) = lintStgBinds bind
    lint_bind (StgTopStringLit v _) = return [v]

lintStgArg :: StgArg -> LintM (Maybe Type)
lintStgArg (StgLitArg lit) = return (Just (literalType lit))
lintStgArg (StgVarArg v)   = lintStgVar v

lintStgVar :: Id -> LintM (Maybe Kind)
lintStgVar v = do checkInScope v
                  return (Just (idType v))

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
        checkL (isJoinId binder || not (isUnliftedType binder_ty))
               (mkUnliftedTyMsg binder rhs)

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

lintStgRhs :: StgRhs -> LintM (Maybe Type)   -- Just ty => type is exact

lintStgRhs (StgRhsClosure _ _ _ _ [] expr)
  = lintStgExpr expr

lintStgRhs (StgRhsClosure _ _ _ _ binders expr)
  = addLoc (LambdaBodyOf binders) $
      addInScopeVars binders $ runMaybeT $ do
        body_ty <- MaybeT $ lintStgExpr expr
        return (mkFunTys (map idType binders) body_ty)

lintStgRhs rhs@(StgRhsCon _ con args) = do
    -- TODO: Check arg_tys
    when (isUnboxedTupleCon con || isUnboxedSumCon con) $
      addErrL (text "StgRhsCon is an unboxed tuple or sum application" $$
               ppr rhs)
    runMaybeT $ do
      arg_tys <- mapM (MaybeT . lintStgArg) args
      MaybeT $ checkFunApp con_ty arg_tys (mkRhsConMsg con_ty arg_tys)
  where
    con_ty = dataConRepType con

lintStgExpr :: StgExpr -> LintM (Maybe Type) -- Just ty => type is exact

lintStgExpr (StgLit l) = return (Just (literalType l))

lintStgExpr e@(StgApp fun args) = runMaybeT $ do
    fun_ty <- MaybeT $ lintStgVar fun
    arg_tys <- mapM (MaybeT . lintStgArg) args
    MaybeT $ checkFunApp fun_ty arg_tys (mkFunAppMsg fun_ty arg_tys e)

lintStgExpr e@(StgConApp con args _arg_tys) = runMaybeT $ do
    -- TODO: Check arg_tys
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
    addErrL (text "Unexpected StgLam" <+> ppr bndrs)
    return Nothing

lintStgExpr (StgLet binds body) = do
    binders <- lintStgBinds binds
    addLoc (BodyOfLetRec binders) $
      addInScopeVars binders $
        lintStgExpr body

lintStgExpr (StgLetNoEscape binds body) = do
    binders <- lintStgBinds binds
    addLoc (BodyOfLetRec binders) $
      addInScopeVars binders $
        lintStgExpr body

lintStgExpr (StgTick _ expr) = lintStgExpr expr

lintStgExpr (StgCase scrut bndr alts_type alts) = runMaybeT $ do
    _ <- MaybeT $ lintStgExpr scrut

    lf <- liftMaybeT getLintFlags
    in_scope <- MaybeT $ liftM Just $
     case alts_type of
        AlgAlt tc     -> check_bndr (tyConPrimRep tc) >> return True
        PrimAlt rep   -> check_bndr [rep]             >> return True
        -- Case binders of unboxed tuple or unboxed sum type always dead
        -- after the unariser has run. See Note [Post-unarisation invariants].
        MultiValAlt _
          | lf_unarised lf -> return False
          | otherwise      -> return True
        PolyAlt       -> return True

    MaybeT $ addInScopeVars [bndr | in_scope] $
             lintStgAlts alts scrut_ty
  where
    scrut_ty        = idType bndr
    scrut_reps      = typePrimRep scrut_ty
    check_bndr reps = checkL (scrut_reps == reps) bad_bndr
                  where
                     bad_bndr = mkDefltMsg bndr reps

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

lintAlt :: Type -> (AltCon, [Id], StgExpr) -> LintM (Maybe Type)
lintAlt _ (DEFAULT, _, rhs)
 = lintStgExpr rhs

lintAlt scrut_ty (LitAlt lit, _, rhs) = do
   checkTys (literalType lit) scrut_ty (mkAltMsg1 scrut_ty)
   lintStgExpr rhs

lintAlt scrut_ty (DataAlt con, args, rhs) = do
    case splitTyConApp_maybe scrut_ty of
      Just (tycon, tys_applied) | isAlgTyCon tycon &&
                                  not (isNewTyCon tycon) -> do
         let
           cons    = tyConDataCons tycon
           arg_tys = dataConInstArgTys con tys_applied
                -- This does not work for existential constructors

         checkL (con `elem` cons) (mkAlgAltMsg2 scrut_ty con)
         checkL (args `lengthIs` dataConRepArity con) (mkAlgAltMsg3 con args)
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

{-
************************************************************************
*                                                                      *
\subsection[lint-monad]{The Lint monad}
*                                                                      *
************************************************************************
-}

newtype LintM a = LintM
    { unLintM :: LintFlags
              -> [LintLocInfo]      -- Locations
              -> IdSet              -- Local vars in scope
              -> Bag MsgDoc        -- Error messages so far
              -> (a, Bag MsgDoc)   -- Result and error messages (if any)
    }

data LintFlags = LintFlags { lf_unarised :: !Bool
                             -- ^ have we run the unariser yet?
                           }

data LintLocInfo
  = RhsOf Id            -- The variable bound
  | LambdaBodyOf [Id]   -- The lambda-binder
  | BodyOfLetRec [Id]   -- One of the binders

dumpLoc :: LintLocInfo -> (SrcSpan, SDoc)
dumpLoc (RhsOf v) =
  (srcLocSpan (getSrcLoc v), text " [RHS of " <> pp_binders [v] <> char ']' )
dumpLoc (LambdaBodyOf bs) =
  (srcLocSpan (getSrcLoc (head bs)), text " [in body of lambda with binders " <> pp_binders bs <> char ']' )

dumpLoc (BodyOfLetRec bs) =
  (srcLocSpan (getSrcLoc (head bs)), text " [in body of letrec with binders " <> pp_binders bs <> char ']' )


pp_binders :: [Id] -> SDoc
pp_binders bs
  = sep (punctuate comma (map pp_binder bs))
  where
    pp_binder b
      = hsep [ppr b, dcolon, ppr (idType b)]

initL :: Bool -> LintM a -> Maybe MsgDoc
initL unarised (LintM m)
  = case (m lf [] emptyVarSet emptyBag) of { (_, errs) ->
    if isEmptyBag errs then
        Nothing
    else
        Just (vcat (punctuate blankLine (bagToList errs)))
    }
  where
    lf = LintFlags unarised

instance Functor LintM where
      fmap = liftM

instance Applicative LintM where
      pure a = LintM $ \_lf _loc _scope errs -> (a, errs)
      (<*>) = ap
      (*>)  = thenL_

instance Monad LintM where
    (>>=) = thenL
    (>>)  = (*>)

thenL :: LintM a -> (a -> LintM b) -> LintM b
thenL m k = LintM $ \lf loc scope errs
  -> case unLintM m lf loc scope errs of
      (r, errs') -> unLintM (k r) lf loc scope errs'

thenL_ :: LintM a -> LintM b -> LintM b
thenL_ m k = LintM $ \lf loc scope errs
  -> case unLintM m lf loc scope errs of
      (_, errs') -> unLintM k lf loc scope errs'

checkL :: Bool -> MsgDoc -> LintM ()
checkL True  _   = return ()
checkL False msg = addErrL msg

addErrL :: MsgDoc -> LintM ()
addErrL msg = LintM $ \_lf loc _scope errs -> ((), addErr errs msg loc)

addErr :: Bag MsgDoc -> MsgDoc -> [LintLocInfo] -> Bag MsgDoc
addErr errs_so_far msg locs
  = errs_so_far `snocBag` mk_msg locs
  where
    mk_msg (loc:_) = let (l,hdr) = dumpLoc loc
                     in  mkLocMessage SevWarning l (hdr $$ msg)
    mk_msg []      = msg

addLoc :: LintLocInfo -> LintM a -> LintM a
addLoc extra_loc m = LintM $ \lf loc scope errs
   -> unLintM m lf (extra_loc:loc) scope errs

addInScopeVars :: [Id] -> LintM a -> LintM a
addInScopeVars ids m = LintM $ \lf loc scope errs
 -> let
        new_set = mkVarSet ids
    in unLintM m lf loc (scope `unionVarSet` new_set) errs

getLintFlags :: LintM LintFlags
getLintFlags = LintM $ \lf _loc _scope errs -> (lf, errs)

{-
Checking function applications: we only check that the type has the
right *number* of arrows, we don't actually compare the types.  This
is because we can't expect the types to be equal - the type
applications and type lambdas that we use to calculate accurate types
have long since disappeared.
-}

checkFunApp :: Type                 -- The function type
            -> [Type]               -- The arg type(s)
            -> MsgDoc               -- Error message
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
      = if tc_args `lengthLessThan` tyConArity tc
        then WARN( True, text "cfa: unsaturated newtype" <+> ppr fun_ty $$ msg )
             (Nothing, Nothing)   -- This is odd, but I've seen it
        else cfa False (newTyConInstRhs tc tc_args) arg_tys

      | Just tc <- tyConAppTyCon_maybe fun_ty
      , not (isTypeFamilyTyCon tc)      -- Definite error
      = (Nothing, Just msg)             -- Too many args

      | otherwise
      = (Nothing, Nothing)

-- | "Compare" types. We used to try a crude comparison of the type themselves,
-- but this is essentially impossible in STG as we have discarded both casts
-- and type applications, so types might look different but be the same. Now we
-- simply compare their runtime representations. See #14120.
stgEqType :: Type -> Type -> Bool
stgEqType ty1 ty2
  = reps1 == reps2
  where
    reps1 = typePrimRep ty1
    reps2 = typePrimRep ty2

checkInScope :: Id -> LintM ()
checkInScope id = LintM $ \_lf loc scope errs
 -> if isLocalId id && not (id `elemVarSet` scope) then
        ((), addErr errs (hsep [ppr id, dcolon, ppr (idType id),
                                text "is out of scope"]) loc)
    else
        ((), errs)

checkTys :: Type -> Type -> MsgDoc -> LintM ()
checkTys ty1 ty2 msg = LintM $ \_lf loc _scope errs
  -> if (ty1 `stgEqType` ty2)
     then ((), errs)
     else ((), addErr errs msg loc)

_mkCaseAltMsg :: [StgAlt] -> MsgDoc
_mkCaseAltMsg _alts
  = ($$) (text "In some case alternatives, type of alternatives not all same:")
            (Outputable.empty) -- LATER: ppr alts

mkDefltMsg :: Id -> [PrimRep] -> MsgDoc
mkDefltMsg bndr reps
  = ($$) (text "Binder of a case expression doesn't match representation of scrutinee:")
         (ppr bndr $$ ppr (idType bndr) $$ ppr reps)

mkFunAppMsg :: Type -> [Type] -> StgExpr -> MsgDoc
mkFunAppMsg fun_ty arg_tys expr
  = vcat [text "In a function application, function type doesn't match arg types:",
              hang (text "Function type:") 4 (ppr fun_ty),
              hang (text "Arg types:") 4 (vcat (map (ppr) arg_tys)),
              hang (text "Expression:") 4 (ppr expr)]

mkRhsConMsg :: Type -> [Type] -> MsgDoc
mkRhsConMsg fun_ty arg_tys
  = vcat [text "In a RHS constructor application, con type doesn't match arg types:",
              hang (text "Constructor type:") 4 (ppr fun_ty),
              hang (text "Arg types:") 4 (vcat (map (ppr) arg_tys))]

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
        ppr con <+> parens (text "arity" <+> ppr (dataConRepArity con)),
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
  = vcat [hsep [text "The type of this binder doesn't match the type of its RHS:",
                     ppr binder],
              hsep [text "Binder's type:", ppr (idType binder)],
              hsep [text "Rhs type:", ppr ty]
             ]

mkUnliftedTyMsg :: Id -> StgRhs -> SDoc
mkUnliftedTyMsg binder rhs
  = (text "Let(rec) binder" <+> quotes (ppr binder) <+>
     text "has unlifted type" <+> quotes (ppr (idType binder)))
    $$
    (text "RHS:" <+> ppr rhs)
