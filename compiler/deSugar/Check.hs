{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>

Pattern Matching Coverage Checking.
-}

{-# LANGUAGE CPP, GADTs, DataKinds, KindSignatures #-}

module Check (
        -- Checking and printing
        checkSingle, checkMatches, isAnyPmCheckEnabled,

        -- See Note [Type and Term Equality Propagation]
        genCaseTmCs1, genCaseTmCs2
    ) where

#include "HsVersions.h"

import TmOracle

import DynFlags
import HsSyn
import TcHsSyn
import Id
import ConLike
import DataCon
import Name
import FamInstEnv
import TysWiredIn
import TyCon
import SrcLoc
import Util
import Outputable
import FastString

import DsMonad    -- DsM, initTcDsForSolver, getDictsDs
import TcSimplify -- tcCheckSatisfiability
import TcType     -- toTcType, toTcTypeBag
import Bag
import ErrUtils
import MonadUtils -- MonadIO
import Var        -- EvVar
import Type
import UniqSupply
import DsGRHSs    -- isTrueLHsExpr

import Data.List     -- find
import Data.Maybe    -- isNothing, isJust, fromJust
import Control.Monad -- liftM3, forM
import Coercion
import TcEvidence
import IOEnv

{-
This module checks pattern matches for:
\begin{enumerate}
  \item Equations that are redundant
  \item Equations with inaccessible right-hand-side
  \item Exhaustiveness
\end{enumerate}

The algorithm is based on the paper:

  "GADTs Meet Their Match:
     Pattern-matching Warnings That Account for GADTs, Guards, and Laziness"

    http://people.cs.kuleuven.be/~george.karachalias/papers/p424-karachalias.pdf

%************************************************************************
%*                                                                      *
                     Pattern Match Check Types
%*                                                                      *
%************************************************************************
-}

type PmM a = DsM a

data PatTy = PAT | VA -- Used only as a kind, to index PmPat

-- The *arity* of a PatVec [p1,..,pn] is
-- the number of p1..pn that are not Guards

data PmPat :: PatTy -> * where
  PmCon  :: { pm_con_con     :: DataCon
            , pm_con_arg_tys :: [Type]
            , pm_con_tvs     :: [TyVar]
            , pm_con_dicts   :: [EvVar]
            , pm_con_args    :: [PmPat t] } -> PmPat t
            -- For PmCon arguments' meaning see @ConPatOut@ in hsSyn/HsPat.hs
  PmVar  :: { pm_var_id   :: Id    } -> PmPat t
  PmLit  :: { pm_lit_lit  :: PmLit } -> PmPat t -- See Note [Literals in PmPat]
  PmNLit :: { pm_lit_id   :: Id
            , pm_lit_not  :: [PmLit] } -> PmPat 'VA
  PmGrd  :: { pm_grd_pv   :: PatVec
            , pm_grd_expr :: PmExpr  } -> PmPat 'PAT

-- data T a where
--     MkT :: forall p q. (Eq p, Ord q) => p -> q -> T [p]
-- or  MkT :: forall p q r. (Eq p, Ord q, [p] ~ r) => p -> q -> T r

type Pattern = PmPat 'PAT -- ^ Patterns
type ValAbs  = PmPat 'VA  -- ^ Value Abstractions

type PatVec = [Pattern]             -- ^ Pattern Vectors
data ValVec = ValVec [ValAbs] Delta -- ^ Value Vector Abstractions

-- | Term and type constraints to accompany each value vector abstraction.
-- For efficiency, we store the term oracle state instead of the term
-- constraints. TODO: Do the same for the type constraints?
data Delta = MkDelta { delta_ty_cs :: Bag EvVar
                     , delta_tm_cs :: TmState }

type ValSetAbs = [ValVec]  -- ^ Value Set Abstractions
type Uncovered = ValSetAbs

-- Instead of keeping the whole sets in memory, we keep a boolean for both the
-- covered and the divergent set (we store the uncovered set though, since we
-- want to print it). For both the covered and the divergent we have:
--
--   True <=> The set is non-empty
--
-- hence:
--  C = True             ==> Useful clause (no warning)
--  C = False, D = True  ==> Clause with inaccessible RHS
--  C = False, D = False ==> Redundant clause
type Triple = (Bool, Uncovered, Bool)

-- | Pattern check result
--
-- * Redundant clauses
-- * Not-covered clauses
-- * Clauses with inaccessible RHS
type PmResult = ([Located [LPat Id]], Uncovered, [Located [LPat Id]])

{-
%************************************************************************
%*                                                                      *
       Entry points to the checker: checkSingle and checkMatches
%*                                                                      *
%************************************************************************
-}

-- | Check a single pattern binding (let)
checkSingle :: DynFlags -> DsMatchContext -> Id -> Pat Id -> DsM ()
checkSingle dflags ctxt@(DsMatchContext _ locn) var p = do
  mb_pm_res <- tryM (checkSingle' locn var p)
  case mb_pm_res of
    Left  _   -> warnPmIters dflags ctxt
    Right res -> dsPmWarn dflags ctxt res

-- | Check a single pattern binding (let)
checkSingle' :: SrcSpan -> Id -> Pat Id -> DsM PmResult
checkSingle' locn var p = do
  resetPmIterDs -- set the iter-no to zero
  fam_insts <- dsGetFamInstEnvs
  clause    <- translatePat fam_insts p
  missing   <- mkInitialUncovered [var]
  (cs,us,ds) <- runMany (pmcheckI clause []) missing -- no guards
  return $ case (cs,ds) of
    (True,  _    ) -> ([], us, []) -- useful
    (False, False) -> ( m, us, []) -- redundant
    (False, True ) -> ([], us,  m) -- inaccessible rhs
  where m = [L locn [L locn p]]

-- | Check a matchgroup (case, functions, etc.)
checkMatches :: DynFlags -> DsMatchContext
             -> [Id] -> [LMatch Id (LHsExpr Id)] -> DsM ()
checkMatches dflags ctxt vars matches = do
  mb_pm_res <- tryM (checkMatches' vars matches)
  case mb_pm_res of
    Left  _   -> warnPmIters dflags ctxt
    Right res -> dsPmWarn dflags ctxt res

-- | Check a matchgroup (case, functions, etc.)
checkMatches' :: [Id] -> [LMatch Id (LHsExpr Id)] -> DsM PmResult
checkMatches' vars matches
  | null matches = return ([], [], [])
  | otherwise = do
      resetPmIterDs -- set the iter-no to zero
      missing    <- mkInitialUncovered vars
      (rs,us,ds) <- go matches missing
      return (map hsLMatchToLPats rs, us, map hsLMatchToLPats ds)
  where
    go []     missing = return ([], missing, [])
    go (m:ms) missing = do
      fam_insts          <- dsGetFamInstEnvs
      (clause, guards)   <- translateMatch fam_insts m
      (cs, missing', ds) <- runMany (pmcheckI clause guards) missing
      (rs, final_u, is)  <- go ms missing'
      return $ case (cs, ds) of
        (True,  _    ) -> (  rs, final_u,   is) -- useful
        (False, False) -> (m:rs, final_u,   is) -- redundant
        (False, True ) -> (  rs, final_u, m:is) -- inaccessible

    hsLMatchToLPats :: LMatch id body -> Located [LPat id]
    hsLMatchToLPats (L l (Match _ pats _ _)) = L l pats

{-
%************************************************************************
%*                                                                      *
              Transform source syntax to *our* syntax
%*                                                                      *
%************************************************************************
-}

-- -----------------------------------------------------------------------
-- * Utilities

nullaryConPattern :: DataCon -> Pattern
-- Nullary data constructor and nullary type constructor
nullaryConPattern con =
  PmCon { pm_con_con = con, pm_con_arg_tys = []
        , pm_con_tvs = [], pm_con_dicts = [], pm_con_args = [] }
{-# INLINE nullaryConPattern #-}

truePattern :: Pattern
truePattern = nullaryConPattern trueDataCon
{-# INLINE truePattern #-}

-- | A fake guard pattern (True <- _) used to represent cases we cannot handle
fake_pat :: Pattern
fake_pat = PmGrd { pm_grd_pv   = [truePattern]
                 , pm_grd_expr = PmExprOther EWildPat }
{-# INLINE fake_pat #-}

-- | Check whether a guard pattern is generated by the checker (unhandled)
isFakeGuard :: [Pattern] -> PmExpr -> Bool
isFakeGuard [PmCon { pm_con_con = c }] (PmExprOther EWildPat)
  | c == trueDataCon = True
  | otherwise        = False
isFakeGuard _pats _e = False

-- | Generate a `canFail` pattern vector of a specific type
mkCanFailPmPat :: Type -> PmM PatVec
mkCanFailPmPat ty = do
  var <- mkPmVar ty
  return [var, fake_pat]

vanillaConPattern :: DataCon -> [Type] -> PatVec -> Pattern
-- ADT constructor pattern => no existentials, no local constraints
vanillaConPattern con arg_tys args =
  PmCon { pm_con_con = con, pm_con_arg_tys = arg_tys
        , pm_con_tvs = [], pm_con_dicts = [], pm_con_args = args }
{-# INLINE vanillaConPattern #-}

-- | Create an empty list pattern of a given type
nilPattern :: Type -> Pattern
nilPattern ty =
  PmCon { pm_con_con = nilDataCon, pm_con_arg_tys = [ty]
        , pm_con_tvs = [], pm_con_dicts = []
        , pm_con_args = [] }
{-# INLINE nilPattern #-}

mkListPatVec :: Type -> PatVec -> PatVec -> PatVec
mkListPatVec ty xs ys = [PmCon { pm_con_con = consDataCon
                               , pm_con_arg_tys = [ty]
                               , pm_con_tvs = [], pm_con_dicts = []
                               , pm_con_args = xs++ys }]
{-# INLINE mkListPatVec #-}

-- | Create a (non-overloaded) literal pattern
mkLitPattern :: HsLit -> Pattern
mkLitPattern lit = PmLit { pm_lit_lit = PmSLit lit }
{-# INLINE mkLitPattern #-}

-- -----------------------------------------------------------------------
-- * Transform (Pat Id) into of (PmPat Id)

translatePat :: FamInstEnvs -> Pat Id -> PmM PatVec
translatePat fam_insts pat = case pat of
  WildPat ty  -> mkPmVars [ty]
  VarPat  id  -> return [PmVar (unLoc id)]
  ParPat p    -> translatePat fam_insts (unLoc p)
  LazyPat _   -> mkPmVars [hsPatType pat] -- like a variable

  -- ignore strictness annotations for now
  BangPat p   -> translatePat fam_insts (unLoc p)

  AsPat lid p -> do
     -- Note [Translating As Patterns]
    ps <- translatePat fam_insts (unLoc p)
    let [e] = map vaToPmExpr (coercePatVec ps)
        g   = PmGrd [PmVar (unLoc lid)] e
    return (ps ++ [g])

  SigPatOut p _ty -> translatePat fam_insts (unLoc p)

  -- See Note [Translate CoPats]
  CoPat wrapper p ty
    | isIdHsWrapper wrapper                   -> translatePat fam_insts p
    | WpCast co <-  wrapper, isReflexiveCo co -> translatePat fam_insts p
    | otherwise -> do
        ps      <- translatePat fam_insts p
        (xp,xe) <- mkPmId2Forms ty
        let g = mkGuard ps (HsWrap wrapper (unLoc xe))
        return [xp,g]

  -- (n + k)  ===>   x (True <- x >= k) (n <- x-k)
  NPlusKPat (L _ _n) _k1 _k2 _ge _minus ty -> mkCanFailPmPat ty

  -- (fun -> pat)   ===>   x (pat <- fun x)
  ViewPat lexpr lpat arg_ty -> do
    ps <- translatePat fam_insts (unLoc lpat)
    -- See Note [Guards and Approximation]
    case all cantFailPattern ps of
      True  -> do
        (xp,xe) <- mkPmId2Forms arg_ty
        let g = mkGuard ps (HsApp lexpr xe)
        return [xp,g]
      False -> mkCanFailPmPat arg_ty

  -- list
  ListPat ps ty Nothing -> do
    foldr (mkListPatVec ty) [nilPattern ty]
      <$> translatePatVec fam_insts (map unLoc ps)

  -- overloaded list
  ListPat lpats elem_ty (Just (pat_ty, _to_list))
    | Just e_ty <- splitListTyConApp_maybe pat_ty
    , (_, norm_elem_ty) <- normaliseType fam_insts Nominal elem_ty
         -- elem_ty is frequently something like
         -- `Item [Int]`, but we prefer `Int`
    , norm_elem_ty `eqType` e_ty ->
        -- We have to ensure that the element types are exactly the same.
        -- Otherwise, one may give an instance IsList [Int] (more specific than
        -- the default IsList [a]) with a different implementation for `toList'
        translatePat fam_insts (ListPat lpats e_ty Nothing)
      -- See Note [Guards and Approximation]
    | otherwise -> mkCanFailPmPat pat_ty

  ConPatOut { pat_con = L _ (PatSynCon _) } ->
    -- Pattern synonyms have a "matcher"
    -- (see Note [Pattern synonym representation] in PatSyn.hs
    -- We should be able to transform (P x y)
    -- to   v (Just (x, y) <- matchP v (\x y -> Just (x,y)) Nothing
    -- That is, a combination of a variable pattern and a guard
    -- But there are complications with GADTs etc, and this isn't done yet
    mkCanFailPmPat (hsPatType pat)

  ConPatOut { pat_con     = L _ (RealDataCon con)
            , pat_arg_tys = arg_tys
            , pat_tvs     = ex_tvs
            , pat_dicts   = dicts
            , pat_args    = ps } -> do
    args <- translateConPatVec fam_insts arg_tys ex_tvs con ps
    return [PmCon { pm_con_con     = con
                  , pm_con_arg_tys = arg_tys
                  , pm_con_tvs     = ex_tvs
                  , pm_con_dicts   = dicts
                  , pm_con_args    = args }]

  NPat (L _ ol) mb_neg _eq ty -> translateNPat fam_insts ol mb_neg ty

  LitPat lit
      -- If it is a string then convert it to a list of characters
    | HsString src s <- lit ->
        foldr (mkListPatVec charTy) [nilPattern charTy] <$>
          translatePatVec fam_insts (map (LitPat . HsChar src) (unpackFS s))
    | otherwise -> return [mkLitPattern lit]

  PArrPat ps ty -> do
    tidy_ps <- translatePatVec fam_insts (map unLoc ps)
    let fake_con = parrFakeCon (length ps)
    return [vanillaConPattern fake_con [ty] (concat tidy_ps)]

  TuplePat ps boxity tys -> do
    tidy_ps <- translatePatVec fam_insts (map unLoc ps)
    let tuple_con = tupleDataCon boxity (length ps)
    return [vanillaConPattern tuple_con tys (concat tidy_ps)]

  -- --------------------------------------------------------------------------
  -- Not supposed to happen
  ConPatIn  {} -> panic "Check.translatePat: ConPatIn"
  SplicePat {} -> panic "Check.translatePat: SplicePat"
  SigPatIn  {} -> panic "Check.translatePat: SigPatIn"

-- | Translate an overloaded literal (see `tidyNPat' in deSugar/MatchLit.hs)
translateNPat :: FamInstEnvs
              -> HsOverLit Id -> Maybe (SyntaxExpr Id) -> Type -> PmM PatVec
translateNPat fam_insts (OverLit val False _ ty) mb_neg outer_ty
  | not type_change, isStringTy ty, HsIsString src s <- val, Nothing <- mb_neg
  = translatePat fam_insts (LitPat (HsString src s))
  | not type_change, isIntTy    ty, HsIntegral src i <- val
  = translatePat fam_insts (mk_num_lit HsInt src i)
  | not type_change, isWordTy   ty, HsIntegral src i <- val
  = translatePat fam_insts (mk_num_lit HsWordPrim src i)
  where
    type_change = not (outer_ty `eqType` ty)
    mk_num_lit c src i = LitPat $ case mb_neg of
      Nothing -> c src i
      Just _  -> c src (-i)
translateNPat _ ol mb_neg _
  = return [PmLit { pm_lit_lit = PmOLit (isJust mb_neg) ol }]

-- | Translate a list of patterns (Note: each pattern is translated
-- to a pattern vector but we do not concatenate the results).
translatePatVec :: FamInstEnvs -> [Pat Id] -> PmM [PatVec]
translatePatVec fam_insts pats = mapM (translatePat fam_insts) pats

-- | Translate a constructor pattern
translateConPatVec :: FamInstEnvs -> [Type] -> [TyVar]
                   -> DataCon -> HsConPatDetails Id -> PmM PatVec
translateConPatVec fam_insts _univ_tys _ex_tvs _ (PrefixCon ps)
  = concat <$> translatePatVec fam_insts (map unLoc ps)
translateConPatVec fam_insts _univ_tys _ex_tvs _ (InfixCon p1 p2)
  = concat <$> translatePatVec fam_insts (map unLoc [p1,p2])
translateConPatVec fam_insts  univ_tys  ex_tvs c (RecCon (HsRecFields fs _))
    -- Nothing matched. Make up some fresh term variables
  | null fs        = mkPmVars arg_tys
    -- The data constructor was not defined using record syntax. For the
    -- pattern to be in record syntax it should be empty (e.g. Just {}).
    -- So just like the previous case.
  | null orig_lbls = ASSERT(null matched_lbls) mkPmVars arg_tys
    -- Some of the fields appear, in the original order (there may be holes).
    -- Generate a simple constructor pattern and make up fresh variables for
    -- the rest of the fields
  | matched_lbls `subsetOf` orig_lbls
  = ASSERT(length orig_lbls == length arg_tys)
      let translateOne (lbl, ty) = case lookup lbl matched_pats of
            Just p  -> translatePat fam_insts p
            Nothing -> mkPmVars [ty]
      in  concatMapM translateOne (zip orig_lbls arg_tys)
    -- The fields that appear are not in the correct order. Make up fresh
    -- variables for all fields and add guards after matching, to force the
    -- evaluation in the correct order.
  | otherwise = do
      arg_var_pats    <- mkPmVars arg_tys
      translated_pats <- forM matched_pats $ \(x,pat) -> do
        pvec <- translatePat fam_insts pat
        return (x, pvec)

      let zipped = zip orig_lbls [ x | PmVar x <- arg_var_pats ]
          guards = map (\(name,pvec) -> case lookup name zipped of
                            Just x  -> PmGrd pvec (PmExprVar (idName x))
                            Nothing -> panic "translateConPatVec: lookup")
                       translated_pats

      return (arg_var_pats ++ guards)
  where
    -- The actual argument types (instantiated)
    arg_tys = dataConInstOrigArgTys c (univ_tys ++ mkTyVarTys ex_tvs)

    -- Some label information
    orig_lbls    = map flSelector $ dataConFieldLabels c
    matched_pats = [ (getName (unLoc (hsRecFieldId x)), unLoc (hsRecFieldArg x))
                   | L _ x <- fs]
    matched_lbls = [ name | (name, _pat) <- matched_pats ]

    subsetOf :: Eq a => [a] -> [a] -> Bool
    subsetOf []     _  = True
    subsetOf (_:_)  [] = False
    subsetOf (x:xs) (y:ys)
      | x == y    = subsetOf    xs  ys
      | otherwise = subsetOf (x:xs) ys

-- Translate a single match
translateMatch :: FamInstEnvs -> LMatch Id (LHsExpr Id) -> PmM (PatVec,[PatVec])
translateMatch fam_insts (L _ (Match _ lpats _ grhss)) = do
  pats'   <- concat <$> translatePatVec fam_insts pats
  guards' <- mapM (translateGuards fam_insts) guards
  return (pats', guards')
  where
    extractGuards :: LGRHS Id (LHsExpr Id) -> [GuardStmt Id]
    extractGuards (L _ (GRHS gs _)) = map unLoc gs

    pats   = map unLoc lpats
    guards = map extractGuards (grhssGRHSs grhss)

-- -----------------------------------------------------------------------
-- * Transform source guards (GuardStmt Id) to PmPats (Pattern)

-- | Translate a list of guard statements to a pattern vector
translateGuards :: FamInstEnvs -> [GuardStmt Id] -> PmM PatVec
translateGuards fam_insts guards = do
  all_guards <- concat <$> mapM (translateGuard fam_insts) guards
  return (replace_unhandled all_guards)
  -- It should have been (return all_guards) but it is too expressive.
  -- Since the term oracle does not handle all constraints we generate,
  -- we (hackily) replace all constraints the oracle cannot handle with a
  -- single one (we need to know if there is a possibility of falure).
  -- See Note [Guards and Approximation] for all guard-related approximations
  -- we implement.
  where
    replace_unhandled :: PatVec -> PatVec
    replace_unhandled gv
      | any_unhandled gv = fake_pat : [ p | p <- gv, shouldKeep p ]
      | otherwise        = gv

    any_unhandled :: PatVec -> Bool
    any_unhandled gv = any (not . shouldKeep) gv

    shouldKeep :: Pattern -> Bool
    shouldKeep p
      | PmVar {} <- p      = True
      | PmCon {} <- p      = length (allConstructors (pm_con_con p)) == 1
                             && all shouldKeep (pm_con_args p)
    shouldKeep (PmGrd pv e)
      | all shouldKeep pv  = True
      | isNotPmExprOther e = True  -- expensive but we want it
    shouldKeep _other_pat  = False -- let the rest..

-- | Check whether a pattern can fail to match
cantFailPattern :: Pattern -> Bool
cantFailPattern p
  | PmVar {} <- p = True
  | PmCon {} <- p = length (allConstructors (pm_con_con p)) == 1
                    && all cantFailPattern (pm_con_args p)
cantFailPattern (PmGrd pv _e)
                  = all cantFailPattern pv
cantFailPattern _ = False

-- | Translate a guard statement to Pattern
translateGuard :: FamInstEnvs -> GuardStmt Id -> PmM PatVec
translateGuard fam_insts guard = case guard of
  BodyStmt   e _ _ _ -> translateBoolGuard e
  LetStmt      binds -> translateLet (unLoc binds)
  BindStmt p e _ _ _ -> translateBind fam_insts p e
  LastStmt        {} -> panic "translateGuard LastStmt"
  ParStmt         {} -> panic "translateGuard ParStmt"
  TransStmt       {} -> panic "translateGuard TransStmt"
  RecStmt         {} -> panic "translateGuard RecStmt"
  ApplicativeStmt {} -> panic "translateGuard ApplicativeLastStmt"

-- | Translate let-bindings
translateLet :: HsLocalBinds Id -> PmM PatVec
translateLet _binds = return []

-- | Translate a pattern guard
translateBind :: FamInstEnvs -> LPat Id -> LHsExpr Id -> PmM PatVec
translateBind fam_insts (L _ p) e = do
  ps <- translatePat fam_insts p
  return [mkGuard ps (unLoc e)]

-- | Translate a boolean guard
translateBoolGuard :: LHsExpr Id -> PmM PatVec
translateBoolGuard e
  | isJust (isTrueLHsExpr e) = return []
    -- The formal thing to do would be to generate (True <- True)
    -- but it is trivial to solve so instead we give back an empty
    -- PatVec for efficiency
  | otherwise = return [mkGuard [truePattern] (unLoc e)]

{- Note [Guards and Approximation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Even if the algorithm is really expressive, the term oracle we use is not.
Hence, several features are not translated *properly* but we approximate.
The list includes:

1. View Patterns
----------------
A view pattern @(f -> p)@ should be translated to @x (p <- f x)@. The term
oracle does not handle function applications so we know that the generated
constraints will not be handled at the end. Hence, we distinguish between two
cases:
  a) Pattern @p@ cannot fail. Then this is just a binding and we do the *right
     thing*.
  b) Pattern @p@ can fail. This means that when checking the guard, we will
     generate several cases, with no useful information. E.g.:

       h (f -> [a,b]) = ...
       h x ([a,b] <- f x) = ...

       uncovered set = { [x |> { False ~ (f x ~ [])            }]
                       , [x |> { False ~ (f x ~ (t1:[]))       }]
                       , [x |> { False ~ (f x ~ (t1:t2:t3:t4)) }] }

     So we have two problems:
       1) Since we do not print the constraints in the general case (they may
          be too many), the warning will look like this:

            Pattern match(es) are non-exhaustive
            In an equation for `h':
                Patterns not matched:
                    _
                    _
                    _
          Which is not short and not more useful than a single underscore.
       2) The size of the uncovered set increases a lot, without gaining more
          expressivity in our warnings.

     Hence, in this case, we replace the guard @([a,b] <- f x)@ with a *dummy*
     @fake_pat@: @True <- _@. That is, we record that there is a possibility
     of failure but we minimize it to a True/False. This generates a single
     warning and much smaller uncovered sets.

2. Overloaded Lists
-------------------
An overloaded list @[...]@ should be translated to @x ([...] <- toList x)@. The
problem is exactly like above, as its solution. For future reference, the code
below is the *right thing to do*:

   ListPat lpats elem_ty (Just (pat_ty, to_list))
     otherwise -> do
       (xp, xe) <- mkPmId2Forms pat_ty
       ps       <- translatePatVec (map unLoc lpats)
       let pats = foldr (mkListPatVec elem_ty) [nilPattern elem_ty] ps
           g    = mkGuard pats (HsApp (noLoc to_list) xe)
       return [xp,g]

3. Overloaded Literals
----------------------
The case with literals is a bit different. a literal @l@ should be translated
to @x (True <- x == from l)@. Since we want to have better warnings for
overloaded literals as it is a very common feature, we treat them differently.
They are mainly covered in Note [Undecidable Equality on Overloaded Literals]
in PmExpr.

4. N+K Patterns & Pattern Synonyms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An n+k pattern (n+k) should be translated to @x (True <- x >= k) (n <- x-k)@.
Since the only pattern of the three that causes failure is guard @(n <- x-k)@,
and has two possible outcomes. Hence, there is no benefit in using a dummy and
we implement the proper thing. Pattern synonyms are simply not implemented yet.
Hence, to be conservative, we generate a dummy pattern, assuming that the
pattern can fail.

5. Actual Guards
----------------
During translation, boolean guards and pattern guards are translated properly.
Let bindings though are omitted by function @translateLet@. Since they are lazy
bindings, we do not actually want to generate a (strict) equality (like we do
in the pattern bind case). Hence, we safely drop them.

Additionally, top-level guard translation (performed by @translateGuards@)
replaces guards that cannot be reasoned about (like the ones we described in
1-4) with a single @fake_pat@ to record the possibility of failure to match.

Note [Translate CoPats]
~~~~~~~~~~~~~~~~~~~~~~~
The pattern match checker did not know how to handle coerced patterns `CoPat`
efficiently, which gave rise to #11276. The original approach translated
`CoPat`s:

    pat |> co    ===>    x (pat <- (e |> co))

Instead, we now check whether the coercion is a hole or if it is just refl, in
which case we can drop it. Unfortunately, data families generate useful
coercions so guards are still generated in these cases and checking data
families is not really efficient.

%************************************************************************
%*                                                                      *
                 Utilities for Pattern Match Checking
%*                                                                      *
%************************************************************************
-}

-- ----------------------------------------------------------------------------
-- * Basic utilities

-- | Get the type out of a PmPat. For guard patterns (ps <- e) we use the type
-- of the first (or the single -WHEREVER IT IS- valid to use?) pattern
pmPatType :: PmPat p -> Type
pmPatType (PmCon { pm_con_con = con, pm_con_arg_tys = tys })
  = mkTyConApp (dataConTyCon con) tys
pmPatType (PmVar  { pm_var_id  = x }) = idType x
pmPatType (PmLit  { pm_lit_lit = l }) = pmLitType l
pmPatType (PmNLit { pm_lit_id  = x }) = idType x
pmPatType (PmGrd  { pm_grd_pv  = pv })
  = ASSERT(patVecArity pv == 1) (pmPatType p)
  where Just p = find ((==1) . patternArity) pv

-- | Generate a value abstraction for a given constructor (generate
-- fresh variables of the appropriate type for arguments)
mkOneConFull :: Id -> DataCon -> PmM (ValAbs, ComplexEq, Bag EvVar)
--  *  x :: T tys, where T is an algebraic data type
--     NB: in the case of a data familiy, T is the *representation* TyCon
--     e.g.   data instance T (a,b) = T1 a b
--       leads to
--            data TPair a b = T1 a b  -- The "representation" type
--       It is TPair, not T, that is given to mkOneConFull
--
--  * 'con' K is a constructor of data type T
--
-- After instantiating the universal tyvars of K we get
--          K tys :: forall bs. Q => s1 .. sn -> T tys
--
-- Results: ValAbs:          K (y1::s1) .. (yn::sn)
--          ComplexEq:       x ~ K y1..yn
--          [EvVar]:         Q
mkOneConFull x con = do
  let -- res_ty == TyConApp (dataConTyCon cabs_con) cabs_arg_tys
      res_ty  = idType x
      (univ_tvs, ex_tvs, eq_spec, thetas, arg_tys, _) = dataConFullSig con
      data_tc = dataConTyCon con   -- The representation TyCon
      tc_args = case splitTyConApp_maybe res_ty of
                  Just (tc, tys) -> ASSERT( tc == data_tc ) tys
                  Nothing -> pprPanic "mkOneConFull: Not TyConApp:" (ppr res_ty)
      subst1  = zipTvSubst univ_tvs tc_args

  (subst, ex_tvs') <- cloneTyVarBndrs subst1 ex_tvs <$> getUniqueSupplyM

  -- Fresh term variables (VAs) as arguments to the constructor
  arguments <-  mapM mkPmVar (substTys subst arg_tys)
  -- All constraints bound by the constructor (alpha-renamed)
  let theta_cs = substTheta subst (eqSpecPreds eq_spec ++ thetas)
  evvars <- mapM (nameType "pm") theta_cs
  let con_abs  = PmCon { pm_con_con     = con
                       , pm_con_arg_tys = tc_args
                       , pm_con_tvs     = ex_tvs'
                       , pm_con_dicts   = evvars
                       , pm_con_args    = arguments }
  return (con_abs, (PmExprVar (idName x), vaToPmExpr con_abs), listToBag evvars)

-- ----------------------------------------------------------------------------
-- * More smart constructors and fresh variable generation

-- | Create a guard pattern
mkGuard :: PatVec -> HsExpr Id -> Pattern
mkGuard pv e
  | all cantFailPattern pv = PmGrd pv expr
  | PmExprOther {} <- expr = fake_pat
  | otherwise              = PmGrd pv expr
  where
    expr = hsExprToPmExpr e

-- | Create a term equality of the form: `(False ~ (x ~ lit))`
mkNegEq :: Id -> PmLit -> ComplexEq
mkNegEq x l = (falsePmExpr, PmExprVar (idName x) `PmExprEq` PmExprLit l)
{-# INLINE mkNegEq #-}

-- | Create a term equality of the form: `(x ~ lit)`
mkPosEq :: Id -> PmLit -> ComplexEq
mkPosEq x l = (PmExprVar (idName x), PmExprLit l)
{-# INLINE mkPosEq #-}

-- | Generate a variable pattern of a given type
mkPmVar :: Type -> PmM (PmPat p)
mkPmVar ty = PmVar <$> mkPmId ty
{-# INLINE mkPmVar #-}

-- | Generate many variable patterns, given a list of types
mkPmVars :: [Type] -> PmM PatVec
mkPmVars tys = mapM mkPmVar tys
{-# INLINE mkPmVars #-}

-- | Generate a fresh `Id` of a given type
mkPmId :: Type -> PmM Id
mkPmId ty = getUniqueM >>= \unique ->
  let occname = mkVarOccFS (fsLit (show unique))
      name    = mkInternalName unique occname noSrcSpan
  in  return (mkLocalId name ty)

-- | Generate a fresh term variable of a given and return it in two forms:
-- * A variable pattern
-- * A variable expression
mkPmId2Forms :: Type -> PmM (Pattern, LHsExpr Id)
mkPmId2Forms ty = do
  x <- mkPmId ty
  return (PmVar x, noLoc (HsVar (noLoc x)))

-- ----------------------------------------------------------------------------
-- * Converting between Value Abstractions, Patterns and PmExpr

-- | Convert a value abstraction an expression
vaToPmExpr :: ValAbs -> PmExpr
vaToPmExpr (PmCon  { pm_con_con = c, pm_con_args = ps })
  = PmExprCon c (map vaToPmExpr ps)
vaToPmExpr (PmVar  { pm_var_id  = x }) = PmExprVar (idName x)
vaToPmExpr (PmLit  { pm_lit_lit = l }) = PmExprLit l
vaToPmExpr (PmNLit { pm_lit_id  = x }) = PmExprVar (idName x)

-- | Convert a pattern vector to a list of value abstractions by dropping the
-- guards (See Note [Translating As Patterns])
coercePatVec :: PatVec -> [ValAbs]
coercePatVec pv = concatMap coercePmPat pv

-- | Convert a pattern to a list of value abstractions (will be either an empty
-- list if the pattern is a guard pattern, or a singleton list in all other
-- cases) by dropping the guards (See Note [Translating As Patterns])
coercePmPat :: Pattern -> [ValAbs]
coercePmPat (PmVar { pm_var_id  = x }) = [PmVar { pm_var_id  = x }]
coercePmPat (PmLit { pm_lit_lit = l }) = [PmLit { pm_lit_lit = l }]
coercePmPat (PmCon { pm_con_con = con, pm_con_arg_tys = arg_tys
                   , pm_con_tvs = tvs, pm_con_dicts = dicts
                   , pm_con_args = args })
  = [PmCon { pm_con_con  = con, pm_con_arg_tys = arg_tys
           , pm_con_tvs  = tvs, pm_con_dicts = dicts
           , pm_con_args = coercePatVec args }]
coercePmPat (PmGrd {}) = [] -- drop the guards

-- | Get all constructors in the family (including given)
allConstructors :: DataCon -> [DataCon]
allConstructors = tyConDataCons . dataConTyCon

-- -----------------------------------------------------------------------
-- * Types and constraints

newEvVar :: Name -> Type -> EvVar
newEvVar name ty = mkLocalId name (toTcType ty)

nameType :: String -> Type -> PmM EvVar
nameType name ty = do
  unique <- getUniqueM
  let occname = mkVarOccFS (fsLit (name++"_"++show unique))
      idname  = mkInternalName unique occname noSrcSpan
  return (newEvVar idname ty)

{-
%************************************************************************
%*                                                                      *
                              The type oracle
%*                                                                      *
%************************************************************************
-}

-- | Check whether a set of type constraints is satisfiable.
tyOracle :: Bag EvVar -> PmM Bool
tyOracle evs
  = do { ((_warns, errs), res) <- initTcDsForSolver $ tcCheckSatisfiability evs
       ; case res of
            Just sat -> return sat
            Nothing  -> pprPanic "tyOracle" (vcat $ pprErrMsgBagWithLoc errs) }

{-
%************************************************************************
%*                                                                      *
                             Sanity Checks
%*                                                                      *
%************************************************************************
-}

-- | The arity of a pattern/pattern vector is the
-- number of top-level patterns that are not guards
type PmArity = Int

-- | Compute the arity of a pattern vector
patVecArity :: PatVec -> PmArity
patVecArity = sum . map patternArity

-- | Compute the arity of a pattern
patternArity :: Pattern -> PmArity
patternArity (PmGrd {}) = 0
patternArity _other_pat = 1

{-
%************************************************************************
%*                                                                      *
            Heart of the algorithm: Function pmcheck
%*                                                                      *
%************************************************************************

Main functions are:

* mkInitialUncovered :: [Id] -> PmM Uncovered

  Generates the initial uncovered set. Term and type constraints in scope
  are checked, if they are inconsistent, the set is empty, otherwise, the
  set contains only a vector of variables with the constraints in scope.

* pmcheck :: PatVec -> [PatVec] -> ValVec -> PmM Triple

  Checks redundancy, coverage and inaccessibility, using auxilary functions
  `pmcheckGuards` and `pmcheckHd`. Mainly handles the guard case which is
  common in all three checks (see paper) and calls `pmcheckGuards` when the
  whole clause is checked, or `pmcheckHd` when the pattern vector does not
  start with a guard.

* pmcheckGuards :: [PatVec] -> ValVec -> PmM Triple

  Processes the guards.

* pmcheckHd :: Pattern -> PatVec -> [PatVec]
          -> ValAbs -> ValVec -> PmM Triple

  Worker: This function implements functions `covered`, `uncovered` and
  `divergent` from the paper at once. Slightly different from the paper because
  it does not even produce the covered and uncovered sets. Since we only care
  about whether a clause covers SOMETHING or if it may forces ANY argument, we
  only store a boolean in both cases, for efficiency.
-}

-- | Lift a pattern matching action from a single value vector abstration to a
-- value set abstraction, but calling it on every vector and the combining the
-- results.
runMany :: (ValVec -> PmM Triple) -> (Uncovered -> PmM Triple)
runMany pm us = mapAndUnzip3M pm us >>= \(css, uss, dss) ->
                  return (or css, concat uss, or dss)
{-# INLINE runMany #-}

-- | Generate the initial uncovered set. It initializes the
-- delta with all term and type constraints in scope.
mkInitialUncovered :: [Id] -> PmM Uncovered
mkInitialUncovered vars = do
  ty_cs  <- getDictsDs
  tm_cs  <- map toComplex . bagToList <$> getTmCsDs
  sat_ty <- tyOracle ty_cs
  return $ case (sat_ty, tmOracle initialTmState tm_cs) of
    (True, Just tm_state) -> [ValVec patterns (MkDelta ty_cs tm_state)]
    -- If any of the term/type constraints are non
    -- satisfiable, the initial uncovered set is empty
    _non_satisfiable      -> []
  where
    patterns  = map PmVar vars

-- | Increase the counter for elapsed algorithm iterations, check that the
-- limit is not exceeded and call `pmcheck`
pmcheckI :: PatVec -> [PatVec] -> ValVec -> PmM Triple
pmcheckI ps guards vva = incrCheckPmIterDs >> pmcheck ps guards vva
{-# INLINE pmcheckI #-}

-- | Increase the counter for elapsed algorithm iterations, check that the
-- limit is not exceeded and call `pmcheckGuards`
pmcheckGuardsI :: [PatVec] -> ValVec -> PmM Triple
pmcheckGuardsI gvs vva = incrCheckPmIterDs >> pmcheckGuards gvs vva
{-# INLINE pmcheckGuardsI #-}

-- | Increase the counter for elapsed algorithm iterations, check that the
-- limit is not exceeded and call `pmcheckHd`
pmcheckHdI :: Pattern -> PatVec -> [PatVec] -> ValAbs -> ValVec -> PmM Triple
pmcheckHdI p ps guards va vva = incrCheckPmIterDs >>
                                  pmcheckHd p ps guards va vva
{-# INLINE pmcheckHdI #-}

-- | Matching function: Check simultaneously a clause (takes separately the
-- patterns and the list of guards) for exhaustiveness, redundancy and
-- inaccessibility.
pmcheck :: PatVec -> [PatVec] -> ValVec -> PmM Triple
pmcheck [] guards vva@(ValVec [] _)
  | null guards = return (True, [], False)
  | otherwise   = pmcheckGuardsI guards vva

-- Guard
pmcheck (p@(PmGrd pv e) : ps) guards vva@(ValVec vas delta)
    -- short-circuit if the guard pattern is useless.
    -- we just have two possible outcomes: fail here or match and recurse
    -- none of the two contains any useful information about the failure
    -- though. So just have these two cases but do not do all the boilerplate
  | isFakeGuard pv e = forces . mkCons vva <$> pmcheckI ps guards vva
  | otherwise = do
      y <- mkPmId (pmPatType p)
      let tm_state = extendSubst y e (delta_tm_cs delta)
          delta'   = delta { delta_tm_cs = tm_state }
      utail <$> pmcheckI (pv ++ ps) guards (ValVec (PmVar y : vas) delta')

pmcheck [] _ (ValVec (_:_) _) = panic "pmcheck: nil-cons"
pmcheck (_:_) _ (ValVec [] _) = panic "pmcheck: cons-nil"

pmcheck (p:ps) guards (ValVec (va:vva) delta)
  = pmcheckHdI p ps guards va (ValVec vva delta)

-- | Check the list of guards
pmcheckGuards :: [PatVec] -> ValVec -> PmM Triple
pmcheckGuards []       vva = return (False, [vva], False)
pmcheckGuards (gv:gvs) vva = do
  (cs,  vsa,  ds ) <- pmcheckI gv [] vva
  (css, vsas, dss) <- runMany (pmcheckGuardsI gvs) vsa
  return (cs || css, vsas, ds || dss)

-- | Worker function: Implements all cases described in the paper for all three
-- functions (`covered`, `uncovered` and `divergent`) apart from the `Guard`
-- cases which are handled by `pmcheck`
pmcheckHd :: Pattern -> PatVec -> [PatVec] -> ValAbs -> ValVec -> PmM Triple

-- Var
pmcheckHd (PmVar x) ps guards va (ValVec vva delta)
  | Just tm_state <- solveOneEq (delta_tm_cs delta)
                                (PmExprVar (idName x), vaToPmExpr va)
  = ucon va <$> pmcheckI ps guards (ValVec vva (delta {delta_tm_cs = tm_state}))
  | otherwise = return (False, [], False)

-- ConCon
pmcheckHd ( p@(PmCon {pm_con_con = c1, pm_con_args = args1})) ps guards
          (va@(PmCon {pm_con_con = c2, pm_con_args = args2})) (ValVec vva delta)
  | c1 /= c2  = return (False, [ValVec (va:vva) delta], False)
  | otherwise = kcon c1 (pm_con_arg_tys p) (pm_con_tvs p) (pm_con_dicts p)
                <$> pmcheckI (args1 ++ ps) guards (ValVec (args2 ++ vva) delta)

-- LitLit
pmcheckHd (PmLit l1) ps guards (va@(PmLit l2)) vva = case eqPmLit l1 l2 of
  True  -> ucon va <$> pmcheckI ps guards vva
  False -> return $ ucon va (False, [vva], False)

-- ConVar
pmcheckHd (p@(PmCon { pm_con_con = con })) ps guards
          (PmVar x) (ValVec vva delta) = do
  cons_cs  <- mapM (mkOneConFull x) (allConstructors con)
  inst_vsa <- flip concatMapM cons_cs $ \(va, tm_ct, ty_cs) -> do
    let ty_state = ty_cs `unionBags` delta_ty_cs delta -- not actually a state
    sat_ty <- if isEmptyBag ty_cs then return True
                                  else tyOracle ty_state
    return $ case (sat_ty, solveOneEq (delta_tm_cs delta) tm_ct) of
      (True, Just tm_state) -> [ValVec (va:vva) (MkDelta ty_state tm_state)]
      _ty_or_tm_failed      -> []

  force_if (canDiverge (idName x) (delta_tm_cs delta)) <$>
    runMany (pmcheckI (p:ps) guards) inst_vsa

-- LitVar
pmcheckHd (p@(PmLit l)) ps guards (PmVar x) (ValVec vva delta)
  = force_if (canDiverge (idName x) (delta_tm_cs delta)) <$>
      mkUnion non_matched <$>
        case solveOneEq (delta_tm_cs delta) (mkPosEq x l) of
          Just tm_state -> pmcheckHdI p ps guards (PmLit l) $
                             ValVec vva (delta {delta_tm_cs = tm_state})
          Nothing       -> return (False, [], False)
  where
    us | Just tm_state <- solveOneEq (delta_tm_cs delta) (mkNegEq x l)
       = [ValVec (PmNLit x [l] : vva) (delta { delta_tm_cs = tm_state })]
       | otherwise = []

    non_matched = (False, us, False)

-- LitNLit
pmcheckHd (p@(PmLit l)) ps guards
          (PmNLit { pm_lit_id = x, pm_lit_not = lits }) (ValVec vva delta)
  | all (not . eqPmLit l) lits
  , Just tm_state <- solveOneEq (delta_tm_cs delta) (mkPosEq x l)
    -- Both guards check the same so it would be sufficient to have only
    -- the second one. Nevertheless, it is much cheaper to check whether
    -- the literal is in the list so we check it first, to avoid calling
    -- the term oracle (`solveOneEq`) if possible
  = mkUnion non_matched <$>
      pmcheckHdI p ps guards (PmLit l)
                (ValVec vva (delta { delta_tm_cs = tm_state }))
  | otherwise = return non_matched
  where
    us | Just tm_state <- solveOneEq (delta_tm_cs delta) (mkNegEq x l)
       = [ValVec (PmNLit x (l:lits) : vva) (delta { delta_tm_cs = tm_state })]
       | otherwise = []

    non_matched = (False, us, False)

-- ----------------------------------------------------------------------------
-- The following three can happen only in cases like #322 where constructors
-- and overloaded literals appear in the same match. The general strategy is
-- to replace the literal (positive/negative) by a variable and recurse. The
-- fact that the variable is equal to the literal is recorded in `delta` so
-- no information is lost

-- LitCon
pmcheckHd (PmLit l) ps guards (va@(PmCon {})) (ValVec vva delta)
  = do y <- mkPmId (pmPatType va)
       let tm_state = extendSubst y (PmExprLit l) (delta_tm_cs delta)
           delta'   = delta { delta_tm_cs = tm_state }
       pmcheckHdI (PmVar y) ps guards va (ValVec vva delta')

-- ConLit
pmcheckHd (p@(PmCon {})) ps guards (PmLit l) (ValVec vva delta)
  = do y <- mkPmId (pmPatType p)
       let tm_state = extendSubst y (PmExprLit l) (delta_tm_cs delta)
           delta'   = delta { delta_tm_cs = tm_state }
       pmcheckHdI p ps guards (PmVar y) (ValVec vva delta')

-- ConNLit
pmcheckHd (p@(PmCon {})) ps guards (PmNLit { pm_lit_id = x }) vva
  = pmcheckHdI p ps guards (PmVar x) vva

-- Impossible: handled by pmcheck
pmcheckHd (PmGrd {}) _ _ _ _ = panic "pmcheckHd: Guard"

-- ----------------------------------------------------------------------------
-- * Utilities for main checking

-- | Take the tail of all value vector abstractions in the uncovered set
utail :: Triple -> Triple
utail (cs, vsa, ds) = (cs, vsa', ds)
  where vsa' = [ ValVec vva delta | ValVec (_:vva) delta <- vsa ]

-- | Prepend a value abstraction to all value vector abstractions in the
-- uncovered set
ucon :: ValAbs -> Triple -> Triple
ucon va (cs, vsa, ds) = (cs, vsa', ds)
  where vsa' = [ ValVec (va:vva) delta | ValVec vva delta <- vsa ]

-- | Given a data constructor of arity `a` and an uncovered set containing
-- value vector abstractions of length `(a+n)`, pass the first `n` value
-- abstractions to the constructor (Hence, the resulting value vector
-- abstractions will have length `n+1`)
kcon :: DataCon -> [Type] -> [TyVar] -> [EvVar] -> Triple -> Triple
kcon con arg_tys ex_tvs dicts (cs, vsa, ds)
  = (cs, [ ValVec (va:vva) delta
         | ValVec vva' delta <- vsa
         , let (args, vva) = splitAt n vva'
         , let va = PmCon { pm_con_con     = con
                          , pm_con_arg_tys = arg_tys
                          , pm_con_tvs     = ex_tvs
                          , pm_con_dicts   = dicts
                          , pm_con_args    = args } ]
       , ds)
  where n = dataConSourceArity con

-- | Get the union of two covered, uncovered and divergent value set
-- abstractions. Since the covered and divergent sets are represented by a
-- boolean, union means computing the logical or (at least one of the two is
-- non-empty).
mkUnion :: Triple -> Triple -> Triple
mkUnion (cs1, vsa1, ds1) (cs2, vsa2, ds2)
  = (cs1 || cs2, vsa1 ++ vsa2, ds1 || ds2)

-- | Add a value vector abstraction to a value set abstraction (uncovered).
mkCons :: ValVec -> Triple -> Triple
mkCons vva (cs, vsa, ds) = (cs, vva:vsa, ds)

-- | Set the divergent set to not empty
forces :: Triple -> Triple
forces (cs, us, _) = (cs, us, True)

-- | Set the divergent set to non-empty if the flag is `True`
force_if :: Bool -> Triple -> Triple
force_if True  (cs,us,_) = (cs,us,True)
force_if False triple    = triple

-- ----------------------------------------------------------------------------
-- * Propagation of term constraints inwards when checking nested matches

{- Note [Type and Term Equality Propagation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When checking a match it would be great to have all type and term information
available so we can get more precise results. For this reason we have functions
`addDictsDs' and `addTmCsDs' in DsMonad that store in the environment type and
term constraints (respectively) as we go deeper.

The type constraints we propagate inwards are collected by `collectEvVarsPats'
in HsPat.hs. This handles bug #4139 ( see example
  https://ghc.haskell.org/trac/ghc/attachment/ticket/4139/GADTbug.hs )
where this is needed.

For term equalities we do less, we just generate equalities for HsCase. For
example we accurately give 2 redundancy warnings for the marked cases:

f :: [a] -> Bool
f x = case x of

  []    -> case x of        -- brings (x ~ []) in scope
             []    -> True
             (_:_) -> False -- can't happen

  (_:_) -> case x of        -- brings (x ~ (_:_)) in scope
             (_:_) -> True
             []    -> False -- can't happen

Functions `genCaseTmCs1' and `genCaseTmCs2' are responsible for generating
these constraints.
-}

-- | Generate equalities when checking a case expression:
--     case x of { p1 -> e1; ... pn -> en }
-- When we go deeper to check e.g. e1 we record two equalities:
-- (x ~ y), where y is the initial uncovered when checking (p1; .. ; pn)
-- and (x ~ p1).
genCaseTmCs2 :: Maybe (LHsExpr Id) -- Scrutinee
             -> [Pat Id]           -- LHS       (should have length 1)
             -> [Id]               -- MatchVars (should have length 1)
             -> DsM (Bag SimpleEq)
genCaseTmCs2 Nothing _ _ = return emptyBag
genCaseTmCs2 (Just scr) [p] [var] = do
  fam_insts <- dsGetFamInstEnvs
  [e] <- map vaToPmExpr . coercePatVec <$> translatePat fam_insts p
  let scr_e = lhsExprToPmExpr scr
  return $ listToBag [(var, e), (var, scr_e)]
genCaseTmCs2 _ _ _ = panic "genCaseTmCs2: HsCase"

-- | Generate a simple equality when checking a case expression:
--     case x of { matches }
-- When checking matches we record that (x ~ y) where y is the initial
-- uncovered. All matches will have to satisfy this equality.
genCaseTmCs1 :: Maybe (LHsExpr Id) -> [Id] -> Bag SimpleEq
genCaseTmCs1 Nothing     _    = emptyBag
genCaseTmCs1 (Just scr) [var] = unitBag (var, lhsExprToPmExpr scr)
genCaseTmCs1 _ _              = panic "genCaseTmCs1: HsCase"

{- Note [Literals in PmPat]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Instead of translating a literal to a variable accompanied with a guard, we
treat them like constructor patterns. The following example from
"./libraries/base/GHC/IO/Encoding.hs" shows why:

mkTextEncoding' :: CodingFailureMode -> String -> IO TextEncoding
mkTextEncoding' cfm enc = case [toUpper c | c <- enc, c /= '-'] of
    "UTF8"    -> return $ UTF8.mkUTF8 cfm
    "UTF16"   -> return $ UTF16.mkUTF16 cfm
    "UTF16LE" -> return $ UTF16.mkUTF16le cfm
    ...

Each clause gets translated to a list of variables with an equal number of
guards. For every guard we generate two cases (equals True/equals False) which
means that we generate 2^n cases to feed the oracle with, where n is the sum of
the length of all strings that appear in the patterns. For this particular
example this means over 2^40 cases. Instead, by representing them like with
constructor we get the following:
  1. We exploit the common prefix with our representation of VSAs
  2. We prune immediately non-reachable cases
     (e.g. False == (x == "U"), True == (x == "U"))

Note [Translating As Patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Instead of translating x@p as:  x (p <- x)
we instead translate it as:     p (x <- coercePattern p)
for performance reasons. For example:

  f x@True  = 1
  f y@False = 2

Gives the following with the first translation:

  x |> {x == False, x == y, y == True}

If we use the second translation we get an empty set, independently of the
oracle. Since the pattern `p' may contain guard patterns though, it cannot be
used as an expression. That's why we call `coercePatVec' to drop the guard and
`vaToPmExpr' to transform the value abstraction to an expression in the
guard pattern (value abstractions are a subset of expressions). We keep the
guards in the first pattern `p' though.


%************************************************************************
%*                                                                      *
      Pretty printing of exhaustiveness/redundancy check warnings
%*                                                                      *
%************************************************************************
-}

-- | Check whether any part of pattern match checking is enabled (does not
-- matter whether it is the redundancy check or the exhaustiveness check).
isAnyPmCheckEnabled :: DynFlags -> DsMatchContext -> Bool
isAnyPmCheckEnabled dflags (DsMatchContext kind _loc)
  = wopt Opt_WarnOverlappingPatterns dflags || exhaustive dflags kind

instance Outputable ValVec where
  ppr (ValVec vva delta)
    = let (residual_eqs, subst) = wrapUpTmState (delta_tm_cs delta)
          vector                = substInValAbs subst vva
      in  ppr_uncovered (vector, residual_eqs)

-- | Apply a term substitution to a value vector abstraction. All VAs are
-- transformed to PmExpr (used only before pretty printing).
substInValAbs :: PmVarEnv -> [ValAbs] -> [PmExpr]
substInValAbs subst = map (exprDeepLookup subst . vaToPmExpr)

-- | Wrap up the term oracle's state once solving is complete. Drop any
-- information about unhandled constraints (involving HsExprs) and flatten
-- (height 1) the substitution.
wrapUpTmState :: TmState -> ([ComplexEq], PmVarEnv)
wrapUpTmState (residual, (_, subst)) = (residual, flattenPmVarEnv subst)

-- | Issue all the warnings (coverage, exhaustiveness, inaccessibility)
dsPmWarn :: DynFlags -> DsMatchContext -> PmResult -> DsM ()
dsPmWarn dflags ctx@(DsMatchContext kind loc) pm_result
  = when (flag_i || flag_u) $ do
      let exists_r = flag_i && notNull redundant
          exists_i = flag_i && notNull inaccessible
          exists_u = flag_u && notNull uncovered
      when exists_r $ forM_ redundant $ \(L l q) -> do
        putSrcSpanDs l (warnDs (Reason Opt_WarnOverlappingPatterns)
                               (pprEqn q "is redundant"))
      when exists_i $ forM_ inaccessible $ \(L l q) -> do
        putSrcSpanDs l (warnDs (Reason Opt_WarnOverlappingPatterns)
                               (pprEqn q "has inaccessible right hand side"))
      when exists_u $
        putSrcSpanDs loc (warnDs flag_u_reason (pprEqns uncovered))
  where
    (redundant, uncovered, inaccessible) = pm_result

    flag_i = wopt Opt_WarnOverlappingPatterns dflags
    flag_u = exhaustive dflags kind
    flag_u_reason = maybe NoReason Reason (exhaustiveWarningFlag kind)

    maxPatterns = maxUncoveredPatterns dflags

    -- Print a single clause (for redundant/with-inaccessible-rhs)
    pprEqn q txt = pp_context True ctx (text txt) $ \f -> ppr_eqn f kind q

    -- Print several clauses (for uncovered clauses)
    pprEqns qs = pp_context False ctx (text "are non-exhaustive") $ \_ ->
      case qs of -- See #11245
           [ValVec [] _]
                    -> text "Guards do not cover entire pattern space"
           _missing -> let us = map ppr qs
                       in  hang (text "Patterns not matched:") 4
                                (vcat (take maxPatterns us)
                                 $$ dots maxPatterns us)

-- | Issue a warning when the predefined number of iterations is exceeded
-- for the pattern match checker
warnPmIters :: DynFlags -> DsMatchContext -> PmM ()
warnPmIters dflags (DsMatchContext kind loc)
  = when (flag_i || flag_u) $ do
      iters <- maxPmCheckIterations <$> getDynFlags
      putSrcSpanDs loc (warnDs NoReason (msg iters))
  where
    ctxt   = pprMatchContext kind
    msg is = fsep [ text "Pattern match checker exceeded"
                  , parens (ppr is), text "iterations in", ctxt <> dot
                  , text "(Use -fmax-pmcheck-iterations=n"
                  , text "to set the maximun number of iterations to n)" ]

    flag_i = wopt Opt_WarnOverlappingPatterns dflags
    flag_u = exhaustive dflags kind

dots :: Int -> [a] -> SDoc
dots maxPatterns qs
    | qs `lengthExceeds` maxPatterns = text "..."
    | otherwise                      = empty

-- | Check whether the exhaustiveness checker should run (exhaustiveness only)
exhaustive :: DynFlags -> HsMatchContext id -> Bool
exhaustive  dflags = maybe False (`wopt` dflags) . exhaustiveWarningFlag

-- | Denotes whether an exhaustiveness check is supported, and if so,
-- via which 'WarningFlag' it's controlled.
-- Returns 'Nothing' if check is not supported.
exhaustiveWarningFlag :: HsMatchContext id -> Maybe WarningFlag
exhaustiveWarningFlag (FunRhs {})   = Just Opt_WarnIncompletePatterns
exhaustiveWarningFlag CaseAlt       = Just Opt_WarnIncompletePatterns
exhaustiveWarningFlag IfAlt         = Nothing
exhaustiveWarningFlag LambdaExpr    = Just Opt_WarnIncompleteUniPatterns
exhaustiveWarningFlag PatBindRhs    = Just Opt_WarnIncompleteUniPatterns
exhaustiveWarningFlag ProcExpr      = Just Opt_WarnIncompleteUniPatterns
exhaustiveWarningFlag RecUpd        = Just Opt_WarnIncompletePatternsRecUpd
exhaustiveWarningFlag ThPatSplice   = Nothing
exhaustiveWarningFlag PatSyn        = Nothing
exhaustiveWarningFlag ThPatQuote    = Nothing
exhaustiveWarningFlag (StmtCtxt {}) = Nothing -- Don't warn about incomplete patterns
                                       -- in list comprehensions, pattern guards
                                       -- etc. They are often *supposed* to be
                                       -- incomplete

-- True <==> singular
pp_context :: Bool -> DsMatchContext -> SDoc -> ((SDoc -> SDoc) -> SDoc) -> SDoc
pp_context singular (DsMatchContext kind _loc) msg rest_of_msg_fun
  = vcat [text txt <+> msg,
          sep [ text "In" <+> ppr_match <> char ':'
              , nest 4 (rest_of_msg_fun pref)]]
  where
    txt | singular  = "Pattern match"
        | otherwise = "Pattern match(es)"

    (ppr_match, pref)
        = case kind of
             FunRhs (L _ fun) _ -> (pprMatchContext kind,
                                    \ pp -> ppr fun <+> pp)
             _                  -> (pprMatchContext kind, \ pp -> pp)

ppr_pats :: HsMatchContext Name -> [Pat Id] -> SDoc
ppr_pats kind pats
  = sep [sep (map ppr pats), matchSeparator kind, text "..."]

ppr_eqn :: (SDoc -> SDoc) -> HsMatchContext Name -> [LPat Id] -> SDoc
ppr_eqn prefixF kind eqn = prefixF (ppr_pats kind (map unLoc eqn))

ppr_constraint :: (SDoc,[PmLit]) -> SDoc
ppr_constraint (var, lits) = var <+> text "is not one of"
                                 <+> braces (pprWithCommas ppr lits)

ppr_uncovered :: ([PmExpr], [ComplexEq]) -> SDoc
ppr_uncovered (expr_vec, complex)
  | null cs   = fsep vec -- there are no literal constraints
  | otherwise = hang (fsep vec) 4 $
                  text "where" <+> vcat (map ppr_constraint cs)
  where
    sdoc_vec = mapM pprPmExprWithParens expr_vec
    (vec,cs) = runPmPprM sdoc_vec (filterComplex complex)

{- Note [Representation of Term Equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the paper, term constraints always take the form (x ~ e). Of course, a more
general constraint of the form (e1 ~ e1) can always be transformed to an
equivalent set of the former constraints, by introducing a fresh, intermediate
variable: { y ~ e1, y ~ e1 }. Yet, implementing this representation gave rise
to #11160 (incredibly bad performance for literal pattern matching). Two are
the main sources of this problem (the actual problem is how these two interact
with each other):

1. Pattern matching on literals generates twice as many constraints as needed.
   Consider the following (tests/ghci/should_run/ghcirun004):

    foo :: Int -> Int
    foo 1    = 0
    ...
    foo 5000 = 4999

   The covered and uncovered set *should* look like:
     U0 = { x |> {} }

     C1  = { 1  |> { x ~ 1 } }
     U1  = { x  |> { False ~ (x ~ 1) } }
     ...
     C10 = { 10 |> { False ~ (x ~ 1), .., False ~ (x ~ 9), x ~ 10 } }
     U10 = { x  |> { False ~ (x ~ 1), .., False ~ (x ~ 9), False ~ (x ~ 10) } }
     ...

     If we replace { False ~ (x ~ 1) } with { y ~ False, y ~ (x ~ 1) }
     we get twice as many constraints. Also note that half of them are just the
     substitution [x |-> False].

2. The term oracle (`tmOracle` in deSugar/TmOracle) uses equalities of the form
   (x ~ e) as substitutions [x |-> e]. More specifically, function
   `extendSubstAndSolve` applies such substitutions in the residual constraints
   and partitions them in the affected and non-affected ones, which are the new
   worklist. Essentially, this gives quadradic behaviour on the number of the
   residual constraints. (This would not be the case if the term oracle used
   mutable variables but, since we use it to handle disjunctions on value set
   abstractions (`Union` case), we chose a pure, incremental interface).

Now the problem becomes apparent (e.g. for clause 300):
  * Set U300 contains 300 substituting constraints [y_i |-> False] and 300
    constraints that we know that will not reduce (stay in the worklist).
  * To check for consistency, we apply the substituting constraints ONE BY ONE
    (since `tmOracle` is called incrementally, it does not have all of them
    available at once). Hence, we go through the (non-progressing) constraints
    over and over, achieving over-quadradic behaviour.

If instead we allow constraints of the form (e ~ e),
  * All uncovered sets Ui contain no substituting constraints and i
    non-progressing constraints of the form (False ~ (x ~ lit)) so the oracle
    behaves linearly.
  * All covered sets Ci contain exactly (i-1) non-progressing constraints and
    a single substituting constraint. So the term oracle goes through the
    constraints only once.

The performance improvement becomes even more important when more arguments are
involved.
-}
