{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Desugaring arrow commands
-}

{-# LANGUAGE CPP #-}

module GHC.HsToCore.Arrows ( dsProcExpr ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Hs   hiding (collectPatBinders, collectPatsBinders,
                        collectLStmtsBinders, collectLStmtBinders,
                        collectStmtBinders, pprParendExpr )
import qualified GHC.Hs.Utils as HsUtils

-- NB: The desugarer, which straddles the source and Core worlds, sometimes
--     needs to see source types (newtypes etc), and sometimes not
--     So WATCH OUT; check each use of split*Ty functions.
-- Sigh.  This is a pain.

import {-# SOURCE #-} GHC.HsToCore.Expr ( dsExpr, dsLExpr, dsLExprNoLP, dsLocalBinds,
                                          dsSyntaxExpr )

import Data.List
import Data.Traversable (for)
import GHC.Builtin.Types
import GHC.Builtin.Types.Arrows
import GHC.Core
import GHC.Core.FVs
import GHC.Core.Make
import GHC.Core.Utils
import GHC.HsToCore.Arrows.Expr
import GHC.HsToCore.Binds (dsHsWrapper)
import GHC.HsToCore.Match
import GHC.HsToCore.Monad
import GHC.HsToCore.Utils
import GHC.Tc.Types.Evidence
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Zonk
import GHC.Types.Basic
import GHC.Types.Id
import GHC.Types.SrcLoc
import GHC.Types.Unique.DSet
import GHC.Types.Var.Set
import Outputable
import Util

{- Note [Desugaring arrow notation: the basics]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unlike monadic do notation, which has a straightforward desugaring,
desugaring arrow notation is much more involved. Its mechanism is
similar in spirit to lambda lifting: we translate what appear to be
direct lexical references into a pipeline of independent functions
that thread along an explicit environment.

For example, consider the following expression:

    proc (a, b, c) -> do { d <- f -< b; g -< (c, a + d) }

If the arrow at play here were (->), that would be simply equivalent to:

    \(a, b, c) -> let { d = f b } in g (c, a + d)

However, in general, we do not have the luxury of anything like lambda
or let expressions for an arbitrary arrow. Instead, we are restricted
to the following combinators:

    arr   :: Arrow p => (a -> b) -> p a b
    (>>>) :: Arrow p => p a b -> p b c -> p a c
    first :: Arrow p => p a b -> p (a, c) (b, c)

This demands an indirect and not-entirely-obvious translation:

        arr (\(a, b, c) -> (b, (a, c)))
    >>> first f
    >>> arr (\(d, (a, c)) -> (c, a + d))
    >>> g

How do we get there from the original program? Here’s the key idea.
When we desugar sequencing of the form

    proc pat1 -> do { pat2 <- cmd1; cmd2 }

we can determine the set of variables bound by pat1 that are used in
cmd1 and cmd2. In the example above, b is used in `f -< b`, while a
and c are used in `g -< (c, a + d)`. We can build an arrow that
partitions the values into the left and right sides of a pair:

        arr (\(a, b, c) -> (b, (a, c)))

Now we can pass the b variable to f while passing a and c through
using first:

        arr (\(a, b, c) -> (b, (a, c)))
    >>> first f

We can now repeat this processes for each statement in the do block.
This amounts to threading a tuple through the arrow pipeline
containing the lexical environment of any arrow-local variables.

Note [The command environment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As described in Note [Desugaring arrow notation: the basics], we
thread the lexical environment through the desugared arrow pipeline
using a tuple. We thread a set of ids through the desugaring process,
AllEnvIds, which holds all the arrow-local variables currently in
scope. For example, if we have

    proc (a, b, c) -> { (d, e) <- cmd1; cmd2 }

then AllEnvIds will contain {a, b, c} while desugaring cmd1 and
{a, b, c, d} while desugaring cmd2.

It would be quite inefficient to actually pass around the *entire*
local environment through the whole computation, since most variables
will only be used in certain branches. For that reason, we also return
the set of arrow-local variables that were actually used by each
command, UsedEnvIds.

Each desugared command expects its needed UsedEnvIds to be supplied
(in order) in a tuple passed to the arrow. For example, the command

    f -< (a, b + c)

will desugar to

    arr (\(a, b, c) -> (a, b + c)) >>> f

and it is the responsibility of the caller to place the desugared
expression in a context where the environment has the appropriate shape.

Note that the input tuple may contain additional elements from the
current command stack; see Note [The command stack].

Note [The command stack]
~~~~~~~~~~~~~~~~~~~~~~~~
Alongside the command environment, the tuple threaded through a
desugared arrow computation also includes the command stack. While the
contents of the command environment are implicitly determined by the
set of variables currently in scope, the command stack is explicitly
manipulated in the source program.

Most commands do not interact with the stack at all, they just pass it
along. The commands that do care about the stack are:

    * arrow application       f -< e   /   f -<< e
    * command application     cmd e
    * command abstraction     \pat -> cmd
    * control operators       (| cmd e1 ... en |)

Command application and command abstraction push and pop values onto
or from the stack, respectively. In arrow application, any values
currently on the stack are implicitly tupled with the input argument.
For example, the command

    ((f -< e1) e2) e3

is precisely equivalent to the command

    f -< (e1, e2, e3)

and the same idea applies to -<<. This on its own is not especially
useful, but the main reason the stack exists is for control operators.
Control operators use the stack to both receive arguments from the
current environment and to pass arguments to their sub-commands. For
example, we might have an operator like the following:

    traverseA :: (ArrowChoice p, Traversable t)
              => p (e, a) b -> p (e, t a) (t b)
    traverseA f = ...

Using this as a control operator allows a command to be passed to
traverseA as an “argument”, which can refer to variables in the local
command environment:

    proc (a, b) -> do
      cs <- f -< a
      (| traverseA (\c -> f -< b + c) |) cs

By applying the second command to cs, it is pushed onto the stack, and
it is tupled with the lexical environment (which in this case contains
only b) and passed to traverseA. The desugared expression is roughly:

    first f >>> arr (\(cs, b) -> (b, cs))
      >>> traverseA (arr (\(b, c) -> b + c) >>> f))

Any values currently on the stack that aren’t consumed by the current
command are threaded alongside the other values in the environment
tuple (see mkEnvStackTup). -}

type AllEnvIds  = IdSet    -- see Note [The command environment]
type UsedEnvIds = DIdSet   -- see Note [The command environment]
type StackIds   = [Id]     -- see Note [The command stack]

----------------------------------------------

-- | > mkStackTupTy [y1::s1, ..., ym::sm]
--   >   ==> ArrowStackTup '[s1, ..., sm]
mkStackTupTy :: StackIds -> Type
mkStackTupTy stk_ids
  = arrowStackTupTy $ mkPromotedListTy liftedTypeKind $ map idType stk_ids

-- | > mkStackTup [y1::s1, ..., ym::sm]
--   >   ==> (y1, ..., ym) :: ArrowStackTup '[s1, ..., sm]
mkStackTup :: StackIds -> CoreExpr
mkStackTup stk_ids
  = mkCastDs (mkCoreTup $ map Var stk_ids)
             (mkTcSubCo $ mkTcSymCo $ mkArrowStackTupCo $ map idType stk_ids)

-- | > mkEnvStackTupTy {x1::t1, ..., xn::tn} [y1::s1, ..., ym::sm]
--   >   ==> ArrowEnvTup (t1, ..., tn) '[s1, ..., sm]
mkEnvStackTupTy :: UsedEnvIds -> StackIds -> Type
mkEnvStackTupTy env_ids stk_ids = arrowEnvTupTy env_ty stk_ty
  where
    env_ty = mkBigCoreVarTupTy $ dVarSetElems env_ids
    stk_ty = mkPromotedListTy liftedTypeKind $ map idType stk_ids

-- | Given a list of environment ids @x1::t1 ... xn::tn@ and stack ids
-- @y1::s1 ... ym::sm@, builds an expression of the shape:
--
-- > ((x1, ..., xn), y1, ..., ym)
-- >   :: ArrowEnvTup (t1, ..., tn) '[s1, ..., sm]
--
-- See Note [The command environment] and Note [The command stack].
mkEnvStackTup :: UsedEnvIds -> StackIds -> CoreExpr
mkEnvStackTup env_ids stk_ids
  = mkCastDs stk_expr (mkTcSubCo $ mkTcSymCo $ mkArrowEnvTupCo env_ty stk_tys)
  where
    stk_expr = mkCoreTup (env_expr : map Var stk_ids)
    stk_tys  = map idType stk_ids

    env_expr = mkCastDs env_tup (mkTcSymCo $ mkArrowEnvCo env_ty)
    env_ty   = mkBigCoreVarTupTy $ dVarSetElems env_ids
    env_tup  = mkBigCoreVarTup $ dVarSetElems env_ids

-- | Wraps the given expression in a lambda that matches on an argument
-- of type @ArrowEnvTup@ and brings its bindings into scope:
--
-- > \(ArrowEnv (x1, ..., xn), y1, ..., ym) -> body
--
-- See Note [The command environment] and Note [The command stack].
matchEnvStackTup :: UsedEnvIds -> StackIds -> CoreExpr -> DsM CoreExpr
matchEnvStackTup env_ids stk_ids body = do
  -- We need temporary ids at all of the following types:
  --   arg_id  :: ArrowEnvTup (t1, ..., tn) (s1, ..., sn)
  --   arg_id' :: (ArrowEnv (t1, ..., tn), s1, ..., sn)
  --   env_id  :: ArrowEnv (t1, ..., tn)
  --   env_id' :: (t1, ..., tn)
  -- arg_id' and env_id' aren’t used, but they are needed as case binders.
  arg_id  <- newSysLocalDs $ arrowEnvTupTy env_ty stk_ty
  arg_id' <- newSysLocalDs $ mkBoxedTupleTy (arrowEnvTy env_ty : stk_tys)
  env_id  <- newSysLocalDs $ arrowEnvTy env_ty
  env_id' <- newSysLocalDs $ env_ty

  let arg_expr = mkCastDs (Var arg_id) (mkTcSubCo $ mkArrowEnvTupCo env_ty stk_tys)
      env_expr = mkCastDs (Var env_id) (mkArrowEnvCo env_ty)

  inner_uniqs <- newUniqueSupply
  outer_uniqs <- newUniqueSupply
      -- case (arg_id `cast` ...) of arg_id' { (env_id, y1, ..., ym) -> ... }
  let outer_case = mkTupleCase outer_uniqs (env_id:stk_ids) inner_case arg_id' arg_expr
      -- case (env_id `cast` ...) of env_id' { (x1, ..., xn) -> ... }
      inner_case = mkTupleCase inner_uniqs (dVarSetElems env_ids) body env_id' env_expr

  pure $ Lam arg_id outer_case
  where
    env_ty  = mkBigCoreTupTy $ map idType $ dVarSetElems env_ids
    stk_ty  = mkPromotedListTy liftedTypeKind stk_tys
    stk_tys = map idType stk_ids

----------------------------------------------

-- | Desugars a @proc@ expression (aka arrow abstraction);
-- see Note [Desugaring arrow notation: the basics].
dsProcExpr :: LPat GhcTc -> LHsCmdTop GhcTc -> DsM CoreExpr
dsProcExpr pat (L _ (HsCmdTop (CmdTopTc _ res_ty stx_table) cmd)) = do
  let env_ids = mkVarSet $ collectPatBinders pat
  (core_cmd, used_ids) <- dsLCmd env_ids [] res_ty cmd

  let env_ty = mkEnvStackTupTy used_ids []
      env_expr = mkEnvStackTup used_ids []
  scrut_id <- selectSimpleMatchVarL pat
  fail_expr <- mkFailExpr ProcExpr env_ty
  lam_body <- matchSimply (Var scrut_id) ProcExpr pat env_expr fail_expr

  let pat_ty = hsLPatType pat
  dsArrExpr stx_table $ mkPremapArr pat_ty (Lam scrut_id lam_body) core_cmd

dsLCmd :: AllEnvIds -> StackIds -> Type
       -> LHsCmd GhcTc -> DsM (DsTArrExpr, UsedEnvIds)
dsLCmd env_ids stk_ids res_ty cmd
  = dsCmd env_ids stk_ids res_ty (unLoc cmd)

-- see Note [Desugaring arrow notation: the basics]
dsCmd :: AllEnvIds     -- ^ see Note [The command environment]
      -> StackIds      -- ^ see Note [The command stack]
      -> Type          -- ^ return type of the command
      -> HsCmd GhcTc   -- ^ command to desugar
      -> DsM (DsTArrExpr, UsedEnvIds)

-- --------------------------------------------------------
-- arrow application (first order)
--
-- desugar[[ D, s, e1 -< e2 ]] = arr (\(D', s) -> (e2', s)) >>> e1'
--   where e1' = desugar[[ e1 ]]
--         e2' = desugar[[ e2 ]]
--         D'  = D ∩ fvs(e2')

dsCmd env_ids stk_ids res_ty (HsCmdArrApp _ arrow arg HsFirstOrderApp _) = do
  core_arrow <- dsLExprNoLP arrow
  core_arg <- dsLExprNoLP arg

  let used_ids = exprFreeIdsDSet core_arg `uniqDSetIntersectUniqSet` env_ids
  arg_id <- newSysLocalDs $ exprType core_arg
  lam <- matchEnvStackTup used_ids stk_ids $
           mkLetNonRec arg_id core_arg $ mkStackTup (arg_id:stk_ids)

  let env_ty = mkEnvStackTupTy used_ids stk_ids
      stk_ty = mkStackTupTy (arg_id:stk_ids)
  pure (mkPremapArr env_ty lam $ mkExprArr stk_ty res_ty core_arrow, used_ids)

-- --------------------------------------------------------
-- arrow application (higher order)
--
-- desugar[[ D, s, e1 -<< e2 ]] = arr (\(D', s) -> (e1', (e2', s))) >>> app
--   where e1' = desugar[[ e1 ]]
--         e2' = desugar[[ e2 ]]
--         D'  = D ∩ fvs(e1', e2')

dsCmd env_ids stk_ids res_ty
      (HsCmdArrApp arrow_ty arrow arg HsHigherOrderApp _) = do
  core_arrow <- dsLExprNoLP arrow
  core_arg <- dsLExprNoLP arg

  let used_ids = exprFreeIdsDSet core_arrow
                 `unionDVarSet` exprFreeIdsDSet core_arg
                 `uniqDSetIntersectUniqSet` env_ids
  arg_id <- newSysLocalDs $ exprType core_arg
  lam <- matchEnvStackTup used_ids stk_ids $
           mkLetNonRec arg_id core_arg $
           mkCoreTup [core_arrow, mkStackTup (arg_id:stk_ids)]

  let env_ty = mkEnvStackTupTy used_ids stk_ids
      stk_ty = mkStackTupTy (arg_id:stk_ids)
  pure (mkPremapArr env_ty lam $ mkAppArr arrow_ty stk_ty res_ty, used_ids)

-- --------------------------------------------------------
-- command application
--
-- desugar[[ D1, s, cmd e ]] = arr (\(D3, s) -> (D2, e1, s)) >>> e2
--   where e1     = desugar[[ e ]]
--         e2, D2 = desugar[[ D1, typeOf(e1):s, cmd ]]
--         D3     = D2 ∪ (D ∩ fvs(e1))

dsCmd env_ids stk_ids res_ty (HsCmdApp _ cmd arg) = do
  core_arg <- dsLExprNoLP arg
  let arg_ty = exprType core_arg
  stk_id <- newSysLocalDs arg_ty
  (core_cmd, used_ids) <- dsLCmd env_ids (stk_id:stk_ids) res_ty cmd

  let used_ids' = used_ids `unionDVarSet`
                  (exprFreeIdsDSet core_arg `uniqDSetIntersectUniqSet` env_ids)
  lam <- matchEnvStackTup used_ids' stk_ids $
           mkLetNonRec stk_id core_arg $
           mkEnvStackTup used_ids (stk_id:stk_ids)

  let in_ty = mkEnvStackTupTy used_ids' stk_ids
  pure (mkPremapArr in_ty lam core_cmd, used_ids')

-- --------------------------------------------------------
-- case, lambda, and lambda case commands

-- see Note [Desugaring case commands]
dsCmd env_ids stk_ids res_ty (HsCmdCase _ scrut matches) = do
  core_scrut <- dsLExpr scrut
  ([scrut_id], core_case, core_cmd, used_ids)
    <- dsCmdMatch env_ids stk_ids res_ty CaseAlt (Just scrut) matches

  let used_ids' = used_ids `unionDVarSet`
                  (exprFreeIdsDSet core_scrut `dVarSetIntersectVarSet` env_ids)
  lam <- matchEnvStackTup used_ids' stk_ids $ bindNonRec scrut_id core_scrut core_case

  let env_ty = mkEnvStackTupTy used_ids' stk_ids
  pure (mkPremapArr env_ty lam core_cmd, used_ids')

-- see Note [Desugaring case commands]
dsCmd env_ids (stk_id:stk_ids) res_ty (HsCmdLamCase _ matches) = do
  ([scrut_id], core_case, core_cmd, used_ids)
    <- dsCmdMatch env_ids stk_ids res_ty CaseAlt Nothing matches

  lam <- matchEnvStackTup used_ids [stk_id] $ wrapBind scrut_id stk_id core_case
  let env_ty = mkEnvStackTupTy used_ids [stk_id]
  pure (mkPremapArr env_ty lam core_cmd, used_ids)

dsCmd env_ids stk_ids res_ty cmd@(HsCmdLam _ matches) = do
  MASSERT2( matchGroupArity matches <= length stk_ids, ppr stk_ids $$ ppr cmd )
  let (lam_ids, stk_ids') = splitAt (matchGroupArity matches) stk_ids
  (scrut_ids, core_case, core_cmd, used_ids)
    <- dsCmdMatch env_ids stk_ids' res_ty LambdaExpr Nothing matches

  let env_ty = mkEnvStackTupTy used_ids stk_ids
  lam <- matchEnvStackTup used_ids stk_ids $
           wrapBinds (zip scrut_ids lam_ids) core_case
  pure (mkPremapArr env_ty lam core_cmd, used_ids)

-- --------------------------------------------------------
-- if commands
--
-- Desugaring if commands is a lot like desugaring case commands (see
-- Note [Desugaring case commands]), but significantly simpler, since
-- there are only ever exactly two branches. So we just desugar them
-- directly rather than use dsCmdMatch.
--
-- desugar[[ D1, s, if e then cmd1 else cmd2 ]]
--   = arr (\(D4) -> if e' then Left (D2) else Right (D3)) >>> (e1 ||| e2)
--   where
--     e'     = desugar[[ D1, e ]]
--     e1, D2 = desugar[[ D1, s, cmd1 ]]
--     e2, D3 = desugar[[ D1, s, cmd2 ]]
--     D4     = (fvs(e') ∩ D1) ∪ D2 ∪ D3

dsCmd env_ids stk_ids res_ty (HsCmdIf _ mb_fun cond then_cmd else_cmd) = do
  core_cond <- dsLExpr cond
  (core_then, used_ids_then) <- dsLCmd env_ids stk_ids res_ty then_cmd
  (core_else, used_ids_else) <- dsLCmd env_ids stk_ids res_ty else_cmd

  let then_ty   = mkEnvStackTupTy used_ids_then stk_ids
      else_ty   = mkEnvStackTupTy used_ids_else stk_ids
      then_expr = mkLeftExpr then_ty else_ty $ mkEnvStackTup used_ids_then stk_ids
      else_expr = mkRightExpr then_ty else_ty $ mkEnvStackTup used_ids_else stk_ids

  core_if <- case mb_fun of
    NoSyntaxExprTc -> pure $ mkIfThenElse core_cond then_expr else_expr
    _ -> dsSyntaxExpr mb_fun [core_cond, then_expr, else_expr]

  let used_ids = exprFreeIdsDSet core_cond
                 `dVarSetIntersectVarSet` env_ids
                 `unionDVarSet` used_ids_then
                 `unionDVarSet` used_ids_else
  lam <- matchEnvStackTup used_ids stk_ids core_if

  let env_ty = mkEnvStackTupTy used_ids stk_ids
  pure (mkPremapArr env_ty lam (mkChoiceArr core_then core_else), used_ids)

-- --------------------------------------------------------
-- control operators

dsCmd env_ids stk_ids res_ty (HsCmdArrForm _ op _ _ args) = do
  core_op <- dsLExpr op

  core_args_with_ids <- traverse ds_arg args
  let used_ids = unionDVarSets $ map get_used_ids core_args_with_ids
      tup_ty   = mkBigCoreVarTupTy $ dVarSetElems used_ids
  core_args <- traverse (trim_env used_ids) core_args_with_ids

  let core_op_app = mkApps core_op (Type tup_ty : core_args)
      env_ty      = mkEnvStackTupTy used_ids stk_ids
  pure (mkExprArr env_ty res_ty core_op_app, used_ids)
  where
    -- Desugars an argument to a control operator. It returns it with
    -- a whole bunch of extra information, since we need that to pass
    -- to trim_env (and we need to desugar all the args first so we
    -- can build all_used_ids).
    ds_arg :: LHsCmdTop GhcTc
           -> DsM (CmdSyntaxTable GhcTc, UsedEnvIds, StackIds, DsTArrExpr)
    ds_arg (L _ (HsCmdTop (CmdTopTc stk_ty res_ty stx_table) cmd)) = do
      stk_ids <- newSysLocalsDs $ extractPromotedList stk_ty
      (core_cmd, used_ids) <- dsLCmd env_ids stk_ids res_ty cmd
      pure (stx_table, used_ids, stk_ids, core_cmd)

    get_used_ids (_, used_ids, _, _) = used_ids

    -- “Trims” a command’s input environment to a minimal subset. This
    -- is necessary for control operators, since the operator will
    -- pass the same environment to all of its argument commands, so
    -- if we have something like
    --
    --   (| blah (f -< a) (g -< (b, c)) |)
    --
    -- we will pass the environment {a, b, c} to blah. trim_env will
    -- then further restrict the environment to just {a} and {b, c}
    -- for the first and second commands, respectively, since those
    -- are the environments they expect.
    trim_env :: UsedEnvIds
             -> (CmdSyntaxTable GhcTc, UsedEnvIds, StackIds, DsTArrExpr)
             -> DsM CoreExpr
    trim_env all_used_ids (stx_table, used_ids, stk_ids, core_arg)
      -- We’re guaranteed to hit this case if the control operator
      -- only takes one argument, which is relatively common, so we
      -- avoid the redundant mapping explicitly.
      | all_used_ids == used_ids = dsArrExpr stx_table core_arg
      | otherwise = do
          let full_ty = mkEnvStackTupTy all_used_ids stk_ids
          lam <- matchEnvStackTup all_used_ids stk_ids $
                   mkEnvStackTup used_ids stk_ids
          dsArrExpr stx_table $ mkPremapArr full_ty lam core_arg

-- --------------------------------------------------------
-- let commands

dsCmd env_ids stk_ids res_ty (HsCmdLet _ binds cmd) = do
  let env_ids' = env_ids `extendVarSetList` collectLocalBinders (unLoc binds)
  (core_cmd, used_ids) <- dsLCmd env_ids' stk_ids res_ty cmd
  core_binds <- dsLocalBinds binds $ mkEnvStackTup used_ids stk_ids

  let used_ids' = used_ids `unionDVarSet` exprFreeIdsDSet core_binds
                           `dVarSetIntersectVarSet` env_ids
  lam <- matchEnvStackTup used_ids' stk_ids core_binds
  let env_ty = mkEnvStackTupTy used_ids' stk_ids
  pure (mkPremapArr env_ty lam core_cmd, used_ids')

-- --------------------------------------------------------
-- miscellaneous other commands

dsCmd env_ids [] res_ty (HsCmdDo _ (L _ stmts))
  = dsCmdStmts env_ids (DsDoStmts res_ty) stmts

dsCmd env_ids stk_ids res_ty (HsCmdPar _ cmd)
  = dsLCmd env_ids stk_ids res_ty cmd

dsCmd env_ids stk_ids res_ty (XCmd (HsWrap wrap cmd)) = do
  (core_cmd, used_ids) <- dsCmd env_ids stk_ids res_ty cmd
  core_wrap <- dsHsWrapper wrap
  pure (mkWrapArr core_wrap core_cmd, used_ids)

dsCmd _ stk_ids _ cmd = pprPanic "dsCmd" $ vcat
  [ text "stk_ids =" <+> ppr stk_ids
  , text "cmd =" <+> ppr cmd ]

-- --------------------------------------------------------

data DsCmdStmtsMode
  -- | We’re desugaring statements from a do block, so we expect them
  -- to end in a 'LastStmt' with a final command.
  = DsDoStmts  { stmts_res_ty :: Type }

  -- | We’re desugaring a rec block, so we don’t have a 'LastStmt' at
  -- the end. Therefore, we keep around some extra information from
  -- the original 'RecStmt' so we can synthesize a command expression
  -- when we reach the end of the block.
  | DsRecStmts { stmts_res_ty :: Type
               , stmts_rec_ids :: [Id]
               , stmts_later_ids :: [Id]
               , stmts_rec_rets :: [HsExpr GhcTc]
               , stmts_later_rets :: [HsExpr GhcTc] }

dsCmdStmts :: AllEnvIds          -- ^ see Note [The command environment]
           -> DsCmdStmtsMode     -- ^ are we desugaring do or rec?
           -> [CmdLStmt GhcTc]   -- ^ statements to desugar
           -> DsM (DsTArrExpr, UsedEnvIds)

dsCmdStmts env_ids (DsDoStmts res_ty) [L _ (LastStmt _ cmd _ _)]
  = dsLCmd env_ids [] res_ty cmd

dsCmdStmts env_ids mode (L _ stmt : stmts) = do
  (core_stmt, used_ids_stmt, bound_ids) <- dsCmdStmt env_ids stmt
  let env_ids' = env_ids `extendVarSetList` dVarSetElems bound_ids
  (core_stmts, used_ids_stmts) <- dsCmdStmts env_ids' mode stmts

  let passthru_ids = used_ids_stmts `minusDVarSet` bound_ids
      passthru_ty  = mkEnvStackTupTy passthru_ids []
      all_used_ids = used_ids_stmt `unionDVarSet` passthru_ids
  before_lam <- matchEnvStackTup all_used_ids [] $
                  mkCoreTup [ mkEnvStackTup used_ids_stmt []
                            , mkEnvStackTup passthru_ids [] ]

  let bound_ty = mkEnvStackTupTy bound_ids []
  bound_id <- newSysLocalDs bound_ty
  bound_lam <- matchEnvStackTup bound_ids [] $ mkEnvStackTup used_ids_stmts []
  passthru_lam <- matchEnvStackTup passthru_ids [] $
                    mkCoreApps bound_lam [Var bound_id]

  let after_ty = mkBoxedTupleTy [bound_ty, passthru_ty]
  after_id    <- newSysLocalDs after_ty
  passthru_id <- newSysLocalDs passthru_ty
  let after_lam = Lam after_id $ coreCasePair after_id bound_id passthru_id $
                    mkCoreApps passthru_lam [Var passthru_id]

  let in_ty      = mkEnvStackTupTy all_used_ids []
      before_cmd = mkPremapArr in_ty before_lam $ mkFirstArr passthru_ty core_stmt
      after_cmd  = mkPremapArr after_ty after_lam core_stmts
  pure (mkComposeArr before_cmd after_cmd, all_used_ids)

dsCmdStmts env_ids DsRecStmts { stmts_res_ty = res_ty
                              , stmts_rec_ids = rec_ids
                              , stmts_later_ids = later_ids
                              , stmts_rec_rets = rec_rets
                              , stmts_later_rets = later_rets
                              } [] = do
  let rec_ids' = mkDVarSet $ map lookup_in_env rec_ids
      later_ids' = mkDVarSet $ map lookup_in_env later_ids
      all_ids = rec_ids' `unionDVarSet` later_ids'
  core_rec_rets <- traverse dsExpr rec_rets
  core_later_rets <- traverse dsExpr later_rets
  res_lam <- matchEnvStackTup all_ids [] $
               mkCoreLets (zipWith NonRec later_ids core_later_rets) $
               mkCoreLets (zipWith NonRec rec_ids core_rec_rets) $
               mkCoreTup [ mkEnvStackTup (mkDVarSet later_ids) []
                         , mkEnvStackTup (mkDVarSet rec_ids) [] ]
  pure (mkArrArr (mkEnvStackTupTy all_ids []) res_ty res_lam, all_ids)
  where
    lookup_in_env id
      | Just id' <- lookupVarSet env_ids id = id'
      | otherwise = pprPanic "dsCmdStmts.lookup_in_env: not in scope" $ vcat
                      [ text "id =" <+> ppr id
                      , text "env_ids =" <+> ppr env_ids ]

dsCmdStmts _ _ stmts = pprPanic "dsCmdStmts" $ ppr stmts


dsCmdStmt :: AllEnvIds
          -> CmdStmt GhcTc     -- ^ the statement to desugar
          -> DsM (DsTArrExpr,  -- the desugared statement
                  UsedEnvIds,  -- the env ids used by this statement
                  DIdSet)      -- the ids bound by this statement

dsCmdStmt env_ids (LetStmt _ binds) = do
  let bound_ids = mkDVarSet $ collectLocalBinders $ unLoc binds
  core_binds <- dsLocalBinds binds $ mkEnvStackTup bound_ids []

  let used_ids = exprFreeIdsDSet core_binds `dVarSetIntersectVarSet` env_ids
      in_ty    = mkEnvStackTupTy used_ids []
      out_ty   = mkEnvStackTupTy bound_ids []
  lam <- matchEnvStackTup used_ids [] core_binds
  pure (mkArrArr in_ty out_ty lam, used_ids, bound_ids)

dsCmdStmt env_ids (BodyStmt res_ty cmd _ _) = do
  (core_cmd, used_ids) <- dsLCmd env_ids [] res_ty cmd
  res_id <- newSysLocalDs res_ty
  let ignore_arr = mkArrArr res_ty (mkEnvStackTupTy emptyDVarSet []) $
                     Lam res_id $ mkEnvStackTup emptyDVarSet []
  pure (mkComposeArr core_cmd ignore_arr, used_ids, emptyDVarSet)

dsCmdStmt env_ids (BindStmt _ pat cmd) = do
  let pat_ty = hsLPatType pat
  (core_cmd, used_ids) <- dsLCmd env_ids [] pat_ty cmd

  let pat_ids  = mkDVarSet $ collectPatBinders pat
      out_ty   = mkEnvStackTupTy pat_ids []
      out_expr = mkEnvStackTup pat_ids []
  pat_id <- selectSimpleMatchVarL pat
  fail_expr <- mkFailExpr (StmtCtxt DoExpr) out_ty
  core_match <- matchSimply (Var pat_id) (StmtCtxt DoExpr) pat out_expr fail_expr

  let match_arr = mkArrArr pat_ty out_ty $ Lam pat_id core_match
  pure (mkComposeArr core_cmd match_arr, used_ids, pat_ids)

dsCmdStmt env_ids
          RecStmt { recS_stmts = stmts
                  , recS_rec_ids = rec_ids
                  , recS_later_ids = later_ids
                  , recS_ext = RecStmtTc { recS_rec_rets = rec_rets
                                         , recS_later_rets = later_rets } } = do
  let rec_env_ids  = env_ids `extendVarSetList` rec_ids
      rec_id_set   = mkDVarSet rec_ids
      later_id_set = mkDVarSet later_ids

      rec_res_ty = mkBoxedTupleTy [ mkEnvStackTupTy later_id_set []
                                  , mkEnvStackTupTy rec_id_set [] ]
      rec_mode = DsRecStmts { stmts_res_ty = rec_res_ty
                            , stmts_rec_ids = rec_ids
                            , stmts_later_ids = later_ids
                            , stmts_rec_rets = rec_rets
                            , stmts_later_rets = later_rets }
  (core_stmts, used_ids) <- dsCmdStmts rec_env_ids rec_mode stmts

  let used_env_ids = used_ids `dVarSetIntersectVarSet` env_ids
      loop_env_ty  = mkEnvStackTupTy used_env_ids []
      feedback_ty  = mkEnvStackTupTy rec_id_set []
      loop_in_ty   = mkBoxedTupleTy [ loop_env_ty, feedback_ty ]

  loop_in_id   <- newSysLocalDs loop_in_ty
  loop_env_id  <- newSysLocalDs loop_env_ty
  feedback_id  <- newSysLocalDs feedback_ty

  loop_env_lam <- matchEnvStackTup used_env_ids [] $ mkEnvStackTup used_ids []
  feedback_lam <- matchEnvStackTup rec_id_set [] $
                    mkCoreApps loop_env_lam [Var loop_env_id]
  let loop_in_lam = Lam loop_in_id $
                      coreCasePair loop_in_id loop_env_id feedback_id $
                      mkCoreApps feedback_lam [Var feedback_id]

      body_expr = mkPremapArr loop_in_ty loop_in_lam core_stmts
  pure (mkLoopArr body_expr, used_env_ids, later_id_set)

dsCmdStmt _ stmt = pprPanic "dsCmdStmt" $ ppr stmt

-- -------------------------------------------------------------------

{- Note [Desugaring case commands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Case commands are particularly complicated to desugar. A case command
looks just like a case expression, except the RHSs are commands:

    case e0 of { pat1 -> cmd1; pat2 -> cmd2; pat3 -> cmd3 }

The only way for an arrow to branch is to use the (|||) operator, so
we can’t place the desugared commands directly inside the desugared
case expression. Instead, we have to do the branching indirectly. We
replace each RHSs of the case expression with an environment tuple
(see Note [The command environment]), then wrap it in some number of
Left/Right constructors before passing the result to the tree of
desugared commands:

    arr (\env0 -> case e0 of { pat1 -> Left (Left env1);
                               pat2 -> Left (Right env2);
                               pat3 -> Right env3 })
      >>> ((cmd1 ||| cmd2) ||| cmd3)

Doing this transformation is a little involved, since we want to take
care to build a *balanced* tree of commands. A naïve desugaring might
yield a simple, right-associated chain of (|||)s, like this:

    cmd1 ||| (cmd2 ||| (cmd3 ||| (cmd4 ||| cmd5)))

But this means selecting a branch will be linear in the number of
alternatives (unless the optimizer manages to improve things, and it
might not be able to). By generating a balanced tree, we ensure
selecting a branch is only logarithmic in the number of alternatives:

    ((cmd1 ||| cmd2) ||| cmd3) ||| (cmd4 ||| cmd5)

The dsCmdMatch function performs all this tricky rearranging, then
eventually calls GHC.HsToCore.Match.matchWrapper to desugar the
patterns and guards themselves. -}

-- Analogous to GHC.HsToCore.Match.matchWrapper, but also does the
-- extra work needed to rearrange the desugared commands;
-- see Note [Desugaring case commands] for the details.
dsCmdMatch
  :: AllEnvIds
  -> StackIds -- ^ may be non-empty if this is a lambda command
  -> Type -- ^ the return type of each command
  -> HsMatchContext GhcRn
  -> Maybe (LHsExpr GhcTc)
  -- ^ the scrutinee, if this is a case expression (otherwise Nothing)
  -> MatchGroup GhcTc (LHsCmd GhcTc)
  -> DsM ([Id],       -- ids that should be bound to the scrutinees
          CoreExpr,   -- the desugared case expression
          DsTArrExpr, -- a desugared tree of commands, with (|||) at the nodes
          UsedEnvIds) -- env ids used by the desugared case expression
dsCmdMatch env_ids stk_ids res_ty match_ctxt mb_scrut
           MG { mg_alts = L l alts
              , mg_ext = MatchGroupTc scrut_tys _
              , mg_origin = origin } = do
  -- Step 1: Desugar the commands in the leaves of the `case` alternatives,
  -- then collect them into a balanced binary tree, with (|||) at the nodes.
  -- (See mk_choice for the details of how we build the tree.)
  core_leaves <- concat <$> traverse ds_leaves alts
  let (core_cmd, sum_ty, core_rhss) = foldb mk_choice $ map mk_branch core_leaves

  -- Step 2: Replace the leaves of the `case` alternatives with the
  -- Left-/Right-wrapped environment tuples we built in mk_choice, then
  -- desugar the reconstructed matches.
  let ([], alts') = mapAccumL replace_match_leaves core_rhss alts
      match_group = MG { mg_alts = L l alts'
                       , mg_ext = MatchGroupTc scrut_tys sum_ty
                       , mg_origin = origin }
  (scrut_ids, core_case) <- matchWrapperWith pure match_ctxt mb_scrut match_group

  -- Step 3: Gather up the env ids used in the desugared case expression
  -- and return all the results.
  let used_ids = exprFreeIdsDSet core_case `dVarSetIntersectVarSet` env_ids
  pure (scrut_ids, core_case, core_cmd, used_ids)
  where
    -- Desugars the commands in the RHSs of the given case alternative.
    ds_leaves :: LMatch GhcTc (LHsCmd GhcTc) -> DsM [(DsTArrExpr, UsedEnvIds)]
    ds_leaves (L _ Match { m_pats = pats
                         , m_grhss = GRHSs _ grhss (L _ binds) }) = do
      let env_ids' = env_ids `extendVarSetList` collectPatsBinders pats
                             `extendVarSetList` collectLocalBinders binds
      for grhss $ \(L _ (GRHS _ stmts cmd)) -> do
        let env_ids'' = env_ids' `extendVarSetList` collectLStmtsBinders stmts
        dsLCmd env_ids'' stk_ids res_ty cmd

    -- Replaces the RHSs of the given case alternative with the given
    -- replacements, and returns any remaining replacements that weren’t
    -- used. Panics if there aren’t enough replacements.
    replace_match_leaves :: [a] -> LMatch GhcTc b -> ([a], LMatch GhcTc a)
    replace_match_leaves rhss (L l match@Match { m_grhss = GRHSs x grhss binds })
      = let (rhss', grhss') = mapAccumL replace_grhs_leaves rhss grhss
        in (rhss', L l match { m_ext = noExtField
                             , m_grhss = GRHSs x grhss' binds })

    -- Like replace_match_leaves, but for a single GRHS.
    replace_grhs_leaves :: [a] -> LGRHS GhcTc b -> ([a], LGRHS GhcTc a)
    replace_grhs_leaves (rhs:rhss) (L l (GRHS x stmts _))
      = (rhss, L l (GRHS x stmts rhs))
    replace_grhs_leaves [] _ = panic "replace_grhs_leaves []"

    -- Builds a branch that can be fed to mk_choice from a desugared leaf.
    mk_branch :: (DsTArrExpr, UsedEnvIds) -> (DsTArrExpr, Type, [CoreExpr])
    mk_branch (core_cmd, used_ids)
      = ( core_cmd
        , mkEnvStackTupTy used_ids stk_ids
        , [mkEnvStackTup used_ids stk_ids] )

    -- Builds a branch of the case tree, as described in
    -- Note [Desugaring case commands]. Each argument is a tuple of:
    --
    --   1. A tree of desugared commands accumulated so far.
    --   2. The input type of the tree of desugared commands.
    --   3. The RHSs of the branches of the case expression, which build
    --      environment tuples wrapped in Left or Right constructors.
    --
    -- The result combines the desugared commands with (|||), the input
    -- types with Either, and wraps the branches with Left or Right.
    -- For example:
    --
    --   mk_choice (cmd1 ||| cmd2, Either (a, b) c, [Left (w, x), Right y])
    --             (cmd3,          Maybe d,         [Just z])
    --
    --     ==> ( (cmd1 ||| cmd2) ||| cmd3
    --         , Either (Either (a, b) c) (Maybe d)
    --         , [ Left (Left (w, x))
    --           , Left (Right y)
    --           , Right (Just z)
    --           ] )
    mk_choice :: (DsTArrExpr, Type, [CoreExpr])
              -> (DsTArrExpr, Type, [CoreExpr])
              -> (DsTArrExpr, Type, [CoreExpr])
    mk_choice (cmd1, ty1, rhss1) (cmd2, ty2, rhss2)
      = (cmd3, ty3, rhss3)
      where
        cmd3  = mkChoiceArr cmd1 cmd2
        ty3   = mkEitherTy ty1 ty2
        rhss3 = map (mkLeftExpr ty1 ty2) rhss1
             ++ map (mkRightExpr ty1 ty2) rhss2

    -- Balanced fold of a non-empty list.
    foldb :: (a -> a -> a) -> [a] -> a
    foldb _ []  = panic "dsCmdCase.foldb []"
    foldb _ [x] = x
    foldb f xs  = foldb f (fold_pairs xs)
      where
        fold_pairs [] = []
        fold_pairs [x] = [x]
        fold_pairs (x1:x2:xs) = f x1 x2:fold_pairs xs

-- -------------------------------------------------------------------
-- miscellaneous helper functions

mkFailExpr :: HsMatchContext GhcRn -> Type -> DsM CoreExpr
mkFailExpr ctxt ty
  = mkErrorAppDs pAT_ERROR_ID ty (matchContextErrString ctxt)

coreCasePair :: Id -> Id -> Id -> CoreExpr -> CoreExpr
coreCasePair scrut_var var1 var2 body
  = Case (Var scrut_var) scrut_var (exprType body)
         [(DataAlt (tupleDataCon Boxed 2), [var1, var2], body)]

{-
Note [Dictionary binders in ConPatOut] See also same Note in GHC.Hs.Utils
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The following functions to collect value variables from patterns are
copied from GHC.Hs.Utils, with one change: we also collect the dictionary
bindings (pat_binds) from ConPatOut.  We need them for cases like

h :: Arrow a => Int -> a (Int,Int) Int
h x = proc (y,z) -> case compare x y of
                GT -> returnA -< z+x

The type checker turns the case into

                case compare x y of
                  GT { p77 = plusInt } -> returnA -< p77 z x

Here p77 is a local binding for the (+) operation.

See comments in GHC.Hs.Utils for why the other version does not include
these bindings.
-}

collectPatBinders :: LPat GhcTc -> [Id]
collectPatBinders pat = collectl pat []

collectPatsBinders :: [LPat GhcTc] -> [Id]
collectPatsBinders pats = foldr collectl [] pats

---------------------
collectl :: LPat GhcTc -> [Id] -> [Id]
-- See Note [Dictionary binders in ConPatOut]
collectl (L _ pat) bndrs
  = go pat
  where
    go (VarPat _ (L _ var))       = var : bndrs
    go (WildPat _)                = bndrs
    go (LazyPat _ pat)            = collectl pat bndrs
    go (BangPat _ pat)            = collectl pat bndrs
    go (AsPat _ (L _ a) pat)      = a : collectl pat bndrs
    go (ParPat _ pat)             = collectl pat bndrs

    go (ListPat _ pats)           = foldr collectl bndrs pats
    go (TuplePat _ pats _)        = foldr collectl bndrs pats
    go (SumPat _ pat _ _)         = collectl pat bndrs

    go (ConPatIn _ ps)            = foldr collectl bndrs (hsConPatArgs ps)
    go (ConPatOut {pat_args=ps, pat_binds=ds}) =
                                    collectEvBinders ds
                                    ++ foldr collectl bndrs (hsConPatArgs ps)
    go (LitPat _ _)               = bndrs
    go (NPat {})                  = bndrs
    go (NPlusKPat _ (L _ n) _ _ _ _) = n : bndrs

    go (SigPat _ pat _)           = collectl pat bndrs
    go (CoPat _ _ pat _)          = collectl (noLoc pat) bndrs
    go (ViewPat _ _ pat)          = collectl pat bndrs
    go p@(SplicePat {})           = pprPanic "collectl/go" (ppr p)

collectEvBinders :: TcEvBinds -> [Id]
collectEvBinders (EvBinds bs)   = foldr add_ev_bndr [] bs
collectEvBinders (TcEvBinds {}) = panic "ToDo: collectEvBinders"

add_ev_bndr :: EvBind -> [Id] -> [Id]
add_ev_bndr (EvBind { eb_lhs = b }) bs | isId b    = b:bs
                                       | otherwise = bs
  -- A worry: what about coercion variable binders??

collectLStmtsBinders :: [LStmt GhcTc body] -> [Id]
collectLStmtsBinders = concatMap collectLStmtBinders

collectLStmtBinders :: LStmt GhcTc body -> [Id]
collectLStmtBinders = collectStmtBinders . unLoc

collectStmtBinders :: Stmt GhcTc body -> [Id]
collectStmtBinders (RecStmt { recS_later_ids = later_ids }) = later_ids
collectStmtBinders stmt = HsUtils.collectStmtBinders stmt
