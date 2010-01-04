%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[StgLint]{A ``lint'' pass to check for Stg correctness}

\begin{code}
module StgLint ( lintStgBindings ) where

import StgSyn

import Bag              ( Bag, emptyBag, isEmptyBag, snocBag, bagToList )
import Id               ( Id, idType, isLocalId )
import VarSet
import DataCon          ( DataCon, dataConInstArgTys, dataConRepType )
import CoreSyn          ( AltCon(..) )
import PrimOp           ( primOpType )
import Literal          ( literalType )
import Maybes
import Name             ( getSrcLoc )
import ErrUtils         ( Message, mkLocMessage )
import TypeRep
import Type             ( mkFunTys, splitFunTy_maybe, splitTyConApp_maybe,
                          isUnLiftedType, isTyVarTy, dropForAlls
                        )
import TyCon            ( isAlgTyCon, isNewTyCon, tyConDataCons )
import Util             ( zipEqual, equalLength )
import SrcLoc
import Outputable
import FastString
import Control.Monad
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
lintStgArg a               = pprPanic "lintStgArg" (ppr a)

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
        maybe_rhs_ty <- lintStgRhs rhs

        -- Check binder doesn't have unlifted type
        checkL (not (isUnLiftedType binder_ty))
               (mkUnLiftedTyMsg binder rhs)

        -- Check match to RHS type
        case maybe_rhs_ty of
          Nothing     -> return ()
          Just rhs_ty -> checkTys binder_ty
                                  rhs_ty
                                  (mkRhsMsg binder rhs_ty)

        return ()
  where
    binder_ty = idType binder
\end{code}

\begin{code}
lintStgRhs :: StgRhs -> LintM (Maybe Type)

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
lintStgExpr :: StgExpr -> LintM (Maybe Type) -- Nothing if error found

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

lintStgExpr (StgOpApp (StgFCallOp _ _) args res_ty) = runMaybeT $ do
        -- We don't have enough type information to check
        -- the application; ToDo
    _maybe_arg_tys <- mapM (MaybeT . lintStgArg) args
    return res_ty

lintStgExpr e@(StgOpApp (StgPrimOp op) args _) = runMaybeT $ do
    arg_tys <- mapM (MaybeT . lintStgArg) args
    MaybeT $ checkFunApp op_ty arg_tys (mkFunAppMsg op_ty arg_tys e)
  where
    op_ty = primOpType op

lintStgExpr (StgLam _ bndrs _) = do
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

lintStgExpr (StgSCC _ expr) = lintStgExpr expr

lintStgExpr e@(StgCase scrut _ _ bndr _ alts_type alts) = runMaybeT $ do
    _ <- MaybeT $ lintStgExpr scrut

    MaybeT $ liftM Just $
     case alts_type of
        AlgAlt tc    -> check_bndr tc
        PrimAlt tc   -> check_bndr tc
        UbxTupAlt tc -> check_bndr tc
        PolyAlt      -> return ()

    MaybeT $ do
        -- we only allow case of tail-call or primop.
     case scrut of
        StgApp _ _     -> return ()
        StgConApp _ _  -> return ()
        StgOpApp _ _ _ -> return ()
        _              -> addErrL (mkCaseOfCaseMsg e)

     addInScopeVars [bndr] $
        lintStgAlts alts scrut_ty
  where
    scrut_ty      = idType bndr
    bad_bndr      = mkDefltMsg bndr
    check_bndr tc = case splitTyConApp_maybe scrut_ty of
                        Just (bndr_tc, _) -> checkL (tc == bndr_tc) bad_bndr
                        Nothing           -> addErrL bad_bndr

lintStgExpr e = pprPanic "lintStgExpr" (ppr e)

lintStgAlts :: [StgAlt]
            -> Type               -- Type of scrutinee
            -> LintM (Maybe Type) -- Type of alternatives

lintStgAlts alts scrut_ty = do
    maybe_result_tys <- mapM (lintAlt scrut_ty) alts

    -- Check the result types
    case catMaybes (maybe_result_tys) of
      []             -> return Nothing

      (first_ty:tys) -> do mapM_ check tys
                           return (Just first_ty)
        where
          check ty = checkTys first_ty ty (mkCaseAltMsg alts)

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
                -- This almost certainly does not work for existential constructors

         checkL (con `elem` cons) (mkAlgAltMsg2 scrut_ty con)
         checkL (equalLength arg_tys args) (mkAlgAltMsg3 con args)
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
              -> Bag Message        -- Error messages so far
              -> (a, Bag Message)   -- Result and error messages (if any)
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
initL :: LintM a -> Maybe Message
initL (LintM m)
  = case (m [] emptyVarSet emptyBag) of { (_, errs) ->
    if isEmptyBag errs then
        Nothing
    else
        Just (vcat (punctuate blankLine (bagToList errs)))
    }

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
checkL :: Bool -> Message -> LintM ()
checkL True  _   = return ()
checkL False msg = addErrL msg

addErrL :: Message -> LintM ()
addErrL msg = LintM $ \loc _scope errs -> ((), addErr errs msg loc)

addErr :: Bag Message -> Message -> [LintLocInfo] -> Bag Message
addErr errs_so_far msg locs
  = errs_so_far `snocBag` mk_msg locs
  where
    mk_msg (loc:_) = let (l,hdr) = dumpLoc loc
                     in  mkLocMessage l (hdr $$ msg)
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
            -> Message              -- Error messgae
            -> LintM (Maybe Type)   -- The result type

checkFunApp fun_ty arg_tys msg = LintM checkFunApp'
 where
  checkFunApp' loc _scope errs
   = cfa fun_ty arg_tys
   where
    cfa fun_ty []      -- Args have run out; that's fine
      = (Just fun_ty, errs)

    cfa fun_ty (_:arg_tys)   
      | Just (_arg_ty, res_ty) <- splitFunTy_maybe (dropForAlls fun_ty)
      = cfa res_ty arg_tys

      | isTyVarTy fun_ty      -- Expected arg tys ran out first;
      = (Just fun_ty, errs)   -- first see if fun_ty is a tyvar template;
                              -- otherwise, maybe fun_ty is a
                              -- dictionary type which is actually a function?
      | otherwise
      = (Nothing, addErr errs msg loc)     -- Too many args
\end{code}

\begin{code}
checkInScope :: Id -> LintM ()
checkInScope id = LintM $ \loc scope errs
 -> if isLocalId id && not (id `elemVarSet` scope) then
        ((), addErr errs (hsep [ppr id, ptext (sLit "is out of scope")]) loc)
    else
        ((), errs)

checkTys :: Type -> Type -> Message -> LintM ()
checkTys _ty1 _ty2 _msg = LintM $ \_loc _scope errs
 -> -- if (ty1 == ty2) then
    ((), errs)
    -- else ((), addErr errs msg loc)
\end{code}

\begin{code}
mkCaseAltMsg :: [StgAlt] -> Message
mkCaseAltMsg _alts
  = ($$) (text "In some case alternatives, type of alternatives not all same:")
            (empty) -- LATER: ppr alts

mkDefltMsg :: Id -> Message
mkDefltMsg _bndr
  = ($$) (ptext (sLit "Binder of a case expression doesn't match type of scrutinee:"))
            (panic "mkDefltMsg")

mkFunAppMsg :: Type -> [Type] -> StgExpr -> Message
mkFunAppMsg fun_ty arg_tys expr
  = vcat [text "In a function application, function type doesn't match arg types:",
              hang (ptext (sLit "Function type:")) 4 (ppr fun_ty),
              hang (ptext (sLit "Arg types:")) 4 (vcat (map (ppr) arg_tys)),
              hang (ptext (sLit "Expression:")) 4 (ppr expr)]

mkRhsConMsg :: Type -> [Type] -> Message
mkRhsConMsg fun_ty arg_tys
  = vcat [text "In a RHS constructor application, con type doesn't match arg types:",
              hang (ptext (sLit "Constructor type:")) 4 (ppr fun_ty),
              hang (ptext (sLit "Arg types:")) 4 (vcat (map (ppr) arg_tys))]

mkAltMsg1 :: Type -> Message
mkAltMsg1 ty
  = ($$) (text "In a case expression, type of scrutinee does not match patterns")
         (ppr ty)

mkAlgAltMsg2 :: Type -> DataCon -> Message
mkAlgAltMsg2 ty con
  = vcat [
        text "In some algebraic case alternative, constructor is not a constructor of scrutinee type:",
        ppr ty,
        ppr con
    ]

mkAlgAltMsg3 :: DataCon -> [Id] -> Message
mkAlgAltMsg3 con alts
  = vcat [
        text "In some algebraic case alternative, number of arguments doesn't match constructor:",
        ppr con,
        ppr alts
    ]

mkAlgAltMsg4 :: Type -> Id -> Message
mkAlgAltMsg4 ty arg
  = vcat [
        text "In some algebraic case alternative, type of argument doesn't match data constructor:",
        ppr ty,
        ppr arg
    ]

mkCaseOfCaseMsg :: StgExpr -> Message
mkCaseOfCaseMsg e
  = text "Case of non-tail-call:" $$ ppr e

mkRhsMsg :: Id -> Type -> Message
mkRhsMsg binder ty
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
