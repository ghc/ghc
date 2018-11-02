{- |
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

A lint pass to check basic STG invariants:

- Variables should be defined before used.

- Let bindings should not have unboxed types (unboxed bindings should only
  appear in case), except when they're join points (see Note [CoreSyn let/app
  invariant] and #14117).

- If linting after unarisation, invariants listed in Note [Post-unarisation
  invariants].

Because we don't have types and coercions in STG we can't really check types
here.

Some history:

StgLint used to check types, but it never worked and so it was disabled in 2000
with this note:

    WARNING:
    ~~~~~~~~

    This module has suffered bit-rot; it is likely to yield lint errors
    for Stg code that is currently perfectly acceptable for code
    generation.  Solution: don't use it!  (KSW 2000-05).

Since then there were some attempts at enabling it again, as summarised in
#14787. It's finally decided that we remove all type checking and only look for
basic properties listed above.
-}

module StgLint ( lintStgTopBindings ) where

import GhcPrelude

import StgSyn

import DynFlags
import Bag              ( Bag, emptyBag, isEmptyBag, snocBag, bagToList )
import Id               ( Id, idType, isLocalId, isJoinId )
import VarSet
import DataCon
import CoreSyn          ( AltCon(..) )
import Name             ( getSrcLoc )
import ErrUtils         ( MsgDoc, Severity(..), mkLocMessage )
import Type
import RepType
import SrcLoc
import Outputable
import qualified ErrUtils as Err
import Control.Applicative ((<|>))
import Control.Monad

lintStgTopBindings :: DynFlags
                   -> Bool   -- ^ have we run Unarise yet?
                   -> String -- ^ who produced the STG?
                   -> [StgTopBinding]
                   -> IO ()

lintStgTopBindings dflags unarised whodunnit binds
  = {-# SCC "StgLint" #-}
    case initL unarised (lint_binds binds) of
      Nothing  ->
        return ()
      Just msg -> do
        putLogMsg dflags NoReason Err.SevDump noSrcSpan
          (defaultDumpStyle dflags)
          (vcat [ text "*** Stg Lint ErrMsgs: in" <+>
                        text whodunnit <+> text "***",
                  msg,
                  text "*** Offending Program ***",
                  pprStgTopBindings binds,
                  text "*** End of Offense ***"])
        Err.ghcExit dflags 1
  where
    lint_binds :: [StgTopBinding] -> LintM ()

    lint_binds [] = return ()
    lint_binds (bind:binds) = do
        binders <- lint_bind bind
        addInScopeVars binders $
            lint_binds binds

    lint_bind (StgTopLifted bind) = lintStgBinds bind
    lint_bind (StgTopStringLit v _) = return [v]

lintStgArg :: StgArg -> LintM ()
lintStgArg (StgLitArg _) = return ()
lintStgArg (StgVarArg v) = lintStgVar v

lintStgVar :: Id -> LintM ()
lintStgVar id = checkInScope id

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
        lintStgRhs rhs
        -- Check binder doesn't have unlifted type or it's a join point
        checkL (isJoinId binder || not (isUnliftedType (idType binder)))
               (mkUnliftedTyMsg binder rhs)

lintStgRhs :: StgRhs -> LintM ()

lintStgRhs (StgRhsClosure _ _ _ _ [] expr)
  = lintStgExpr expr

lintStgRhs (StgRhsClosure _ _ _ _ binders expr)
  = addLoc (LambdaBodyOf binders) $
      addInScopeVars binders $
        lintStgExpr expr

lintStgRhs rhs@(StgRhsCon _ con args) = do
    when (isUnboxedTupleCon con || isUnboxedSumCon con) $
      addErrL (text "StgRhsCon is an unboxed tuple or sum application" $$
               ppr rhs)
    mapM_ lintStgArg args
    mapM_ checkPostUnariseConArg args

lintStgExpr :: StgExpr -> LintM ()

lintStgExpr (StgLit _) = return ()

lintStgExpr (StgApp fun args) = do
    lintStgVar fun
    mapM_ lintStgArg args

lintStgExpr app@(StgConApp con args _arg_tys) = do
    -- unboxed sums should vanish during unarise
    lf <- getLintFlags
    when (lf_unarised lf && isUnboxedSumCon con) $
      addErrL (text "Unboxed sum after unarise:" $$
               ppr app)
    mapM_ lintStgArg args
    mapM_ checkPostUnariseConArg args

lintStgExpr (StgOpApp _ args _) =
    mapM_ lintStgArg args

lintStgExpr lam@(StgLam _ _) =
    addErrL (text "Unexpected StgLam" <+> ppr lam)

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

lintStgExpr (StgCase scrut bndr alts_type alts) = do
    lintStgExpr scrut

    lf <- getLintFlags
    let in_scope = stgCaseBndrInScope alts_type (lf_unarised lf)

    addInScopeVars [bndr | in_scope] (mapM_ lintAlt alts)

lintAlt :: (AltCon, [Id], StgExpr) -> LintM ()

lintAlt (DEFAULT, _, rhs) =
    lintStgExpr rhs

lintAlt (LitAlt _, _, rhs) =
    lintStgExpr rhs

lintAlt (DataAlt _, bndrs, rhs) = do
    mapM_ checkPostUnariseBndr bndrs
    addInScopeVars bndrs (lintStgExpr rhs)

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

-- Case alts shouldn't have unboxed sum, unboxed tuple, or void binders.
checkPostUnariseBndr :: Id -> LintM ()
checkPostUnariseBndr bndr = do
    lf <- getLintFlags
    when (lf_unarised lf) $
      forM_ (checkPostUnariseId bndr) $ \unexpected ->
        addErrL $
          text "After unarisation, binder " <>
          ppr bndr <> text " has " <> text unexpected <> text " type " <>
          ppr (idType bndr)

-- Arguments shouldn't have sum, tuple, or void types.
checkPostUnariseConArg :: StgArg -> LintM ()
checkPostUnariseConArg arg = case arg of
    StgLitArg _ ->
      return ()
    StgVarArg id -> do
      lf <- getLintFlags
      when (lf_unarised lf) $
        forM_ (checkPostUnariseId id) $ \unexpected ->
          addErrL $
            text "After unarisation, arg " <>
            ppr id <> text " has " <> text unexpected <> text " type " <>
            ppr (idType id)

-- Post-unarisation args and case alt binders should not have unboxed tuple,
-- unboxed sum, or void types. Return what the binder is if it is one of these.
checkPostUnariseId :: Id -> Maybe String
checkPostUnariseId id =
    let
      id_ty = idType id
      is_sum, is_tuple, is_void :: Maybe String
      is_sum = guard (isUnboxedSumType id_ty) >> return "unboxed sum"
      is_tuple = guard (isUnboxedTupleType id_ty) >> return "unboxed tuple"
      is_void = guard (isVoidTy id_ty) >> return "void"
    in
      is_sum <|> is_tuple <|> is_void

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

checkInScope :: Id -> LintM ()
checkInScope id = LintM $ \_lf loc scope errs
 -> if isLocalId id && not (id `elemVarSet` scope) then
        ((), addErr errs (hsep [ppr id, dcolon, ppr (idType id),
                                text "is out of scope"]) loc)
    else
        ((), errs)

mkUnliftedTyMsg :: Id -> StgRhs -> SDoc
mkUnliftedTyMsg binder rhs
  = (text "Let(rec) binder" <+> quotes (ppr binder) <+>
     text "has unlifted type" <+> quotes (ppr (idType binder)))
    $$
    (text "RHS:" <+> ppr rhs)
