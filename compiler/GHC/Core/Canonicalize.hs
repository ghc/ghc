-- | Canonicalizing local binder names in Core dumps.
--
-- See Note [Canonicalizing local binders for dumps].
module GHC.Core.Canonicalize (
        canonicalizeBindsForDump,
        canonicalizeRulesForDump
    ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.TyCo.Rep  ( Type, Coercion )
import GHC.Core.TyCo.Tidy ( tidyType, tidyCo )

import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Types.Tickish
import GHC.Types.Var
import GHC.Types.Var.Env

import GHC.Data.Maybe
import GHC.Utils.Monad ( mapAccumLM )
import GHC.Utils.Monad.State.Strict

import Data.Char ( chr, ord )

{- Note [Canonicalizing local binders for dumps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The names GHC invents for local binders (`go1`, `ds`, `eta`, `lvl`, ...) are
unstable: unrelated compiler changes perturb them, even with
-dsuppress-uniques, which makes golden tests on -ddump-simpl output fail
spuriously and makes Core hard to diff across compiler versions (#27176).

With -dcanonicalize-local-binds we rename, *in dump output only*, every
nested binder whose Name is internal (isInternalName, which includes System
names) to a deterministic canonical name.  The program that is linted and
compiled is completely unaffected; the renaming happens lazily inside the
dump document in GHC.Core.Lint.dumpPassResult, the chokepoint through which
all Core-pass dumps go.

The scheme:

* Names are <prefix><letters>, where the letters count A..Z, AA, AB, ...
  (bijective base 26).  Letters are the only varying dimension; canonical
  names never end in a digit, so they cannot be confused with ordinary
  tidied names like `x1`.

* The prefix encodes the binder's role, with one independent counter per
  role:
    - cj: join points (let/letrec binders satisfying isJoinId)
    - cw: case binders (the binder after `of`)
    - ct: type and coercion variable binders
    - cb: every other term binder (lambda args, non-join lets,
          alt pattern binders, rule binders)
  Separate counters confine churn: adding a let binder cannot renumber join
  points, a new type lambda cannot renumber term binders, and so on.  Known
  sensitivity: when a binder changes role (canonically, the occurrence
  analyser turning a let into a join point) its prefix changes and the two
  affected sequences are renumbered; such a flip is always visible in the
  dump anyway (`let` becomes `join`, calls become `jump`).

* All counters restart at A for each top-level CoreBind (per NonRec, per
  whole Rec group), so a change in one top-level binding never renames
  binders in another.

* Within one binding (a top-level pair or a nested let), names are
  allocated visible-first: the binder itself and its RHS before the
  unfolding template and attached RULES, even though IdInfo prints *above*
  the RHS.  Under -dsuppress-all the IdInfo is invisible, and allocating it
  first would let an unfolding-only change renumber the visible RHS.  The
  cosmetic cost when IdInfo is shown is that the `Unf=` block contains
  higher-numbered names than the RHS below it.

* Each renamed binder keeps its Unique, its OccName namespace, its
  IdDetails (join points keep printing as `join`/`jump`) and its IdInfo
  (modulo the rewriting of unfolding templates and attached rules), so the
  output composes with and without -dsuppress-uniques and other -dsuppress
  flags.

* Top-level binders keep their Names, even internal ones (e.g. floated-out
  `lvl` bindings): renaming them would obscure which top-level definitions
  the dump talks about.  Only their IdInfo is rewritten, since unfolding
  templates and attached rules contain nested binders.

* Occurrences of renamed variables inside types and coercions are
  substituted (via tidyType/tidyCo), but binders occurring *inside* type or
  coercion structure (e.g. under a forall) are not given canonical names
  and consume no counter.  Hence changes confined to type or coercion
  structure -- say, in coercion optimization -- can never shift canonical
  names.

* We do not avoid clashes with user-written names: a source variable
  spelled `cbA` collides textually with a canonical name.  Uniques still
  disambiguate, unless they are suppressed too.  Renaming source names away
  from the collision would defeat the purpose of stable output.
-}

-- | The role of a binder, determining the prefix of its canonical name.
-- See Note [Canonicalizing local binders for dumps].
data BndrRole
  = JoinBndr   -- ^ join points: @cj@
  | CaseBndr   -- ^ case binders: @cw@
  | TyCoBndr   -- ^ type and coercion variables: @ct@
  | TermBndr   -- ^ all other term binders: @cb@

-- | One name counter per 'BndrRole'.
data CanonCounters = CC
  { cc_join :: !Int
  , cc_case :: !Int
  , cc_tyco :: !Int
  , cc_term :: !Int
  }

startCounters :: CanonCounters
startCounters = CC 0 0 0 0

type CanonM = State CanonCounters

-- | The substitution for occurrences of renamed binders.  Passed as a plain
-- argument (not in 'CanonM'): the counters must thread across sibling
-- branches, the substitution must not escape the binder's scope.
type CanonEnv = VarEnv Var

-- | Allocate the next canonical OccName string for the given role.
takeCanonOccString :: BndrRole -> CanonM String
takeCanonOccString role = state $ \cc -> case role of
  JoinBndr -> ("cj" ++ bijBase26 (cc_join cc), cc { cc_join = cc_join cc + 1 })
  CaseBndr -> ("cw" ++ bijBase26 (cc_case cc), cc { cc_case = cc_case cc + 1 })
  TyCoBndr -> ("ct" ++ bijBase26 (cc_tyco cc), cc { cc_tyco = cc_tyco cc + 1 })
  TermBndr -> ("cb" ++ bijBase26 (cc_term cc), cc { cc_term = cc_term cc + 1 })

-- | Bijective base-26 numbering with digits A..Z:
-- @0 -> "A"@, ..., @25 -> "Z"@, @26 -> "AA"@, @27 -> "AB"@, ...
bijBase26 :: Int -> String
bijBase26 n0 = go n0 ""
  where
    go n acc
      | q == 0    = acc'
      | otherwise = go (q - 1) acc'
      where
        (q, r) = n `quotRem` 26
        acc'   = chr (ord 'A' + r) : acc

{-
************************************************************************
*                                                                      *
                Top level
*                                                                      *
************************************************************************
-}

canonicalizeBindsForDump :: CoreProgram -> CoreProgram
canonicalizeBindsForDump binds = map canonTopBind binds

-- | Canonicalize the rules in the standalone \"local rules for imported
-- ids\" list of a dump.  Each rule gets a fresh set of counters.
canonicalizeRulesForDump :: [CoreRule] -> [CoreRule]
canonicalizeRulesForDump rules
  = map (\r -> evalState (canonRule emptyVarEnv r) startCounters) rules

-- All counters restart per top-level bind.  Top-level binder Names are
-- kept; only their IdInfo is rewritten, after the (visible) RHSs.
-- See Note [Canonicalizing local binders for dumps].
canonTopBind :: CoreBind -> CoreBind
canonTopBind bind = evalState (go bind) startCounters
  where
    go (NonRec b rhs) = do
      rhs' <- canonExpr emptyVarEnv rhs
      b'   <- canonIdInfo emptyVarEnv b
      return (NonRec b' rhs')
    go (Rec prs) = do
      let (bs, rhss) = unzip prs
      rhss' <- mapM (canonExpr emptyVarEnv) rhss
      bs'   <- mapM (canonIdInfo emptyVarEnv) bs
      return (Rec (bs' `zip` rhss'))

{-
************************************************************************
*                                                                      *
                Expressions
*                                                                      *
************************************************************************
-}

canonExpr :: CanonEnv -> CoreExpr -> CanonM CoreExpr
canonExpr env expr = case expr of
  Var v       -> return (Var (canonVarOcc env v))
  Lit lit     -> return (Lit lit)
  Type ty     -> return (Type (canonType env ty))
  Coercion co -> return (Coercion (canonCo env co))
  App f a     -> App <$> canonExpr env f <*> canonExpr env a
  Tick t e    -> Tick (canonTickish env t) <$> canonExpr env e
  Cast e co   -> do
    e' <- canonExpr env e
    return (Cast e' (canonCo env co))
  Lam b e     -> do
    (env', b') <- canonBndr env (nonLetBndrRole b) b
    Lam b' <$> canonExpr env' e
  Let bind body -> do
    (env', bind') <- canonBind env bind
    Let bind' <$> canonExpr env' body
  Case scrut b ty alts -> do
    scrut'     <- canonExpr env scrut
    (env', b') <- canonBndr env CaseBndr b
    alts'      <- mapM (canonAlt env') alts
    return (Case scrut' b' (canonType env ty) alts')

canonAlt :: CanonEnv -> CoreAlt -> CanonM CoreAlt
canonAlt env (Alt con bs rhs) = do
  (env', bs') <- canonBndrs env bs
  Alt con bs' <$> canonExpr env' rhs

canonBind :: CanonEnv -> CoreBind -> CanonM (CanonEnv, CoreBind)
canonBind env (NonRec b rhs) = do
  -- Binder first (it prints first), then the RHS, then the IdInfo
  -- (visible-first; see Note [Canonicalizing local binders for dumps]).
  -- This handles type-lets too: see Note [Substituting type-lets]
  -- in GHC.Core.Lint.SubstTypeLets.
  (env', b1) <- canonBndr env (letBndrRole b) b
  rhs'       <- canonExpr env rhs
  b2         <- canonIdInfo env' b1
  return (env', NonRec b2 rhs')
canonBind env (Rec prs) = do
  let (bs, rhss) = unzip prs
  (env', bs1) <- mapAccumLM (\e b -> canonBndr e (letBndrRole b) b) env bs
  rhss'       <- mapM (canonExpr env') rhss
  bs2         <- mapM (canonIdInfo env') bs1
  return (env', Rec (bs2 `zip` rhss'))

canonTickish :: CanonEnv -> CoreTickish -> CoreTickish
canonTickish env (Breakpoint ext bid ids)
  = Breakpoint ext bid (map (canonVarOcc env) ids)
canonTickish _ other_tickish = other_tickish

canonRule :: CanonEnv -> CoreRule -> CanonM CoreRule
canonRule _ rule@(BuiltinRule {}) = return rule
canonRule env rule@(Rule { ru_bndrs = bndrs, ru_args = args, ru_rhs = rhs }) = do
  -- ru_fn and ru_rough refer to top-level things; leave them alone.
  (env', bndrs') <- canonBndrs env bndrs
  args' <- mapM (canonExpr env') args
  rhs'  <- canonExpr env' rhs
  return (rule { ru_bndrs = bndrs', ru_args = args', ru_rhs = rhs' })

{-
************************************************************************
*                                                                      *
                Binders
*                                                                      *
************************************************************************
-}

-- | Role of a lambda, alt or rule binder.
nonLetBndrRole :: Var -> BndrRole
nonLetBndrRole v
  | isTyCoVar v = TyCoBndr
  | otherwise   = TermBndr

-- | Role of a let(rec) binder.
letBndrRole :: Var -> BndrRole
letBndrRole v
  | isTyCoVar v = TyCoBndr   -- type-lets
  | isJoinId v  = JoinBndr
  | otherwise   = TermBndr

-- | Rename a nested binder (iff its Name is internal), substitute its type,
-- and extend the substitution.  Does /not/ touch the unfolding or rules in
-- its IdInfo; that is 'canonIdInfo', called separately so that those names
-- are allocated after the visible RHS.
canonBndr :: CanonEnv -> BndrRole -> Var -> CanonM (CanonEnv, Var)
canonBndr env role v = do
  v1 <- if isInternalName (getName v)
          then do
            occ_str <- takeCanonOccString role
            let occ = mkOccName (occNameSpace (getOccName v)) occ_str
            -- setVarName preserves IdDetails and IdInfo
            return (setVarName v (mkInternalName (varUnique v) occ noSrcSpan))
          else return v
  let v2 | isTyCoVar v1 = updateVarType        (canonType env) v1
         | otherwise    = updateIdTypeAndMult  (canonType env) v1
  return (extendVarEnv env v v2, v2)

canonBndrs :: CanonEnv -> [Var] -> CanonM (CanonEnv, [Var])
canonBndrs env vs = mapAccumLM (\e v -> canonBndr e (nonLetBndrRole v) v) env vs

-- | Rewrite the parts of a binder's IdInfo that embed Core: the unfolding
-- template and any attached rules.  The binders stored in the 'CanonEnv'
-- for occurrences don't carry this rewriting; only the Name matters there.
canonIdInfo :: CanonEnv -> Var -> CanonM Var
canonIdInfo env v
  | isId v = do
      unf' <- canonUnfolding env (realIdUnfolding v)
      let RuleInfo rules fvs = idSpecialisation v
      rules' <- mapM (canonRule env) rules
      return (v `setIdUnfolding` unf'
                `setIdSpecialisation` RuleInfo rules' fvs)
  | otherwise = return v

canonUnfolding :: CanonEnv -> Unfolding -> CanonM Unfolding
canonUnfolding env unf = case unf of
  CoreUnfolding { uf_tmpl = tmpl } -> do
    -- All CoreUnfoldings, not just stable ones: Tmpl= is printed for any.
    tmpl' <- canonExpr env tmpl
    return (unf { uf_tmpl = tmpl' })
  DFunUnfolding { df_bndrs = bndrs, df_args = args } -> do
    (env', bndrs') <- canonBndrs env bndrs
    args' <- mapM (canonExpr env') args
    return (unf { df_bndrs = bndrs', df_args = args' })
  _ -> return unf

{-
************************************************************************
*                                                                      *
                Occurrences, types, coercions
*                                                                      *
************************************************************************
-}

canonVarOcc :: CanonEnv -> Var -> Var
canonVarOcc env v = lookupVarEnv env v `orElse` v

-- Substitute occurrences of renamed binders in types and coercions.
-- Binders inside type/coercion structure are not canonicalized and consume
-- no counter; see Note [Canonicalizing local binders for dumps].
canonType :: CanonEnv -> Type -> Type
canonType env ty = tidyType (emptyTidyOccEnv, env) ty

canonCo :: CanonEnv -> Coercion -> Coercion
canonCo env co = tidyCo (emptyTidyOccEnv, env) co
