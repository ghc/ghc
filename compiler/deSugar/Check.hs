{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>

Pattern Matching Coverage Checking.
-}

{-# LANGUAGE CPP, GADTs, DataKinds, KindSignatures #-}

module Check (
        -- Actual check and pretty printing
        checkSingle, checkMatches, dsPmWarn,

        -- Check for exponential explosion due to guards
        computeNoGuards,
        isAnyPmCheckEnabled,
        warnManyGuards,
        maximum_failing_guards,

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

{-
This module checks pattern matches for:
\begin{enumerate}
  \item Equations that are redundant
  \item Equations with inaccessible right-hand-side
  \item Exhaustiveness
\end{enumerate}

The algorithm used is described in the paper:

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

data PmConstraint = TmConstraint PmExpr PmExpr -- ^ Term equalities: e ~ e
                    -- See Note [Representation of Term Equalities]
                  | TyConstraint [EvVar]   -- ^ Type equalities
                  | BtConstraint Id        -- ^ Strictness constraints: x ~ _|_

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
  PmNLit :: { pm_lit_id :: Id
            , pm_lit_not :: [PmLit] } -> PmPat 'VA
  PmGrd  :: { pm_grd_pv   :: PatVec
            , pm_grd_expr :: PmExpr } -> PmPat 'PAT

-- data T a where
--     MkT :: forall p q. (Eq p, Ord q) => p -> q -> T [p]
-- or  MkT :: forall p q r. (Eq p, Ord q, [p] ~ r) => p -> q -> T r

type Pattern = PmPat 'PAT -- ^ Patterns
type ValAbs  = PmPat 'VA  -- ^ Value Abstractions

type PatVec    = [Pattern] -- Pattern Vectors
type ValVecAbs = [ValAbs]  -- Value Vector Abstractions

data ValSetAbs   -- Reprsents a set of value vector abstractions
                 -- Notionally each value vector abstraction is a triple:
                 --   (Gamma |- us |> Delta)
                 -- where 'us'    is a ValueVec
                 --       'Delta' is a constraint
  -- INVARIANT VsaInvariant: an empty ValSetAbs is always represented by Empty
  -- INVARIANT VsaArity: the number of Cons's in any path to a leaf is the same
  -- The *arity* of a ValSetAbs is the number of Cons's in any path to a leaf
  = Empty                               -- ^ {}
  | Union ValSetAbs ValSetAbs           -- ^ S1 u S2
  | Singleton                           -- ^ { |- empty |> empty }
  | Constraint [PmConstraint] ValSetAbs -- ^ Extend Delta
  | Cons ValAbs ValSetAbs               -- ^ map (ucon u) vs

-- | Pattern check result
--
-- * redundant clauses
-- * clauses with inaccessible RHS
-- * missing
type PmResult = ( [[LPat Id]]
                , [[LPat Id]]
                , [([PmExpr], [ComplexEq])] )

{-
%************************************************************************
%*                                                                      *
       Entry points to the checker: checkSingle and checkMatches
%*                                                                      *
%************************************************************************
-}

-- | Check a single pattern binding (let)
checkSingle :: Id -> Pat Id -> DsM PmResult
checkSingle var p = do
  let lp = [noLoc p]
  vec <- liftUs (translatePat p)
  vsa <- initial_uncovered [var]
  (c,d,us') <- patVectProc False (vec,[]) vsa -- no guards
  us <- pruneVSA us'
  return $ case (c,d) of
    (True,  _)     -> ([],   [],   us)
    (False, True)  -> ([],   [lp], us)
    (False, False) -> ([lp], [],   us)

-- | Check a matchgroup (case, functions, etc.)
checkMatches :: Bool -> [Id] -> [LMatch Id (LHsExpr Id)] -> DsM PmResult
checkMatches oversimplify vars matches
  | null matches = return ([],[],[])
  | otherwise    = do
      missing    <- initial_uncovered vars
      (rs,is,us) <- go matches missing
      return (map hsLMatchPats rs, map hsLMatchPats is, us)
  where
    go [] missing = do
      missing' <- pruneVSA missing
      return ([], [], missing')

    go (m:ms) missing = do
      clause        <- liftUs (translateMatch m)
      (c,  d,  us ) <- patVectProc oversimplify clause missing
      (rs, is, us') <- go ms us
      return $ case (c,d) of
        (True,  _)     -> (  rs,   is, us')
        (False, True)  -> (  rs, m:is, us')
        (False, False) -> (m:rs,   is, us')

-- | Generate the initial uncovered set. It initializes the
-- delta with all term and type constraints in scope.
initial_uncovered :: [Id] -> DsM ValSetAbs
initial_uncovered vars = do
  cs <- getCsPmM
  let vsa = foldr Cons Singleton (map PmVar vars)
  return $ if null cs then vsa
                      else mkConstraint cs vsa

-- | Collect all term and type constraints from the local environment
getCsPmM :: DsM [PmConstraint]
getCsPmM = do
  ty_cs <- bagToList <$> getDictsDs
  tm_cs <- map simpleToTmCs . bagToList <$> getTmCsDs
  return $ if null ty_cs
    then tm_cs
    else TyConstraint ty_cs : tm_cs
  where
    simpleToTmCs :: (Id, PmExpr) -> PmConstraint
    simpleToTmCs (x,e) = TmConstraint (PmExprVar x) e

-- | Total number of guards in a translated match that could fail.
noFailingGuards :: [(PatVec,[PatVec])] -> Int
noFailingGuards clauses = sum [ countPatVecs gvs | (_, gvs) <- clauses ]
  where
    countPatVec  gv  = length [ () | p <- gv, not (cantFailPattern p) ]
    countPatVecs gvs = sum [ countPatVec gv | gv <- gvs ]

computeNoGuards :: [LMatch Id (LHsExpr Id)] -> PmM Int
computeNoGuards matches = do
  matches' <- mapM (liftUs . translateMatch) matches
  return (noFailingGuards matches')

maximum_failing_guards :: Int
maximum_failing_guards = 20 -- Find a better number

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

truePattern :: Pattern
truePattern = nullaryConPattern trueDataCon

-- | A fake guard pattern (True <- _) used to represent cases we cannot handle
fake_pat :: Pattern
fake_pat = PmGrd { pm_grd_pv   = [truePattern]
                 , pm_grd_expr = PmExprOther EWildPat }

vanillaConPattern :: DataCon -> [Type] -> PatVec -> Pattern
-- ADT constructor pattern => no existentials, no local constraints
vanillaConPattern con arg_tys args =
  PmCon { pm_con_con = con, pm_con_arg_tys = arg_tys
        , pm_con_tvs = [], pm_con_dicts = [], pm_con_args = args }

nilPattern :: Type -> Pattern
nilPattern ty =
  PmCon { pm_con_con = nilDataCon, pm_con_arg_tys = [ty]
        , pm_con_tvs = [], pm_con_dicts = []
        , pm_con_args = [] }

mkListPatVec :: Type -> PatVec -> PatVec -> PatVec
mkListPatVec ty xs ys = [PmCon { pm_con_con = consDataCon
                               , pm_con_arg_tys = [ty]
                               , pm_con_tvs = [], pm_con_dicts = []
                               , pm_con_args = xs++ys }]

mkLitPattern :: HsLit -> Pattern
mkLitPattern lit = PmLit { pm_lit_lit = PmSLit lit }

-- -----------------------------------------------------------------------
-- * Transform (Pat Id) into of (PmPat Id)

translatePat :: Pat Id -> UniqSM PatVec
translatePat pat = case pat of
  WildPat ty  -> mkPmVarsSM [ty]
  VarPat  id  -> return [PmVar (unLoc id)]
  ParPat p    -> translatePat (unLoc p)
  LazyPat _   -> mkPmVarsSM [hsPatType pat] -- like a variable

  -- ignore strictness annotations for now
  BangPat p   -> translatePat (unLoc p)

  AsPat lid p -> do
     -- Note [Translating As Patterns]
    ps <- translatePat (unLoc p)
    let [e] = map valAbsToPmExpr (coercePatVec ps)
        g   = PmGrd [PmVar (unLoc lid)] e
    return (ps ++ [g])

  SigPatOut p _ty -> translatePat (unLoc p)

  -- See Note [Translate CoPats]
  CoPat wrapper p ty
    | isIdHsWrapper wrapper                   -> translatePat p
    | WpCast co <-  wrapper, isReflexiveCo co -> translatePat p
    | otherwise -> do
        ps      <- translatePat p
        (xp,xe) <- mkPmId2FormsSM ty
        let g = mkGuard ps (HsWrap wrapper (unLoc xe))
        return [xp,g]

  -- (n + k)  ===>   x (True <- x >= k) (n <- x-k)
  NPlusKPat (L _ n) k ge minus -> do
    (xp, xe) <- mkPmId2FormsSM (idType n)
    let ke = L (getLoc k) (HsOverLit (unLoc k))
        g1 = mkGuard [truePattern] (OpApp xe (noLoc ge)    no_fixity ke)
        g2 = mkGuard [PmVar n]     (OpApp xe (noLoc minus) no_fixity ke)
    return [xp, g1, g2]

  -- (fun -> pat)   ===>   x (pat <- fun x)
  ViewPat lexpr lpat arg_ty -> do
    ps <- translatePat (unLoc lpat)
    -- See Note [Guards and Approximation]
    case all cantFailPattern ps of
      True  -> do
        (xp,xe) <- mkPmId2FormsSM arg_ty
        let g = mkGuard ps (HsApp lexpr xe)
        return [xp,g]
      False -> do
        var <- mkPmVarSM arg_ty
        return [var, fake_pat]

  -- list
  ListPat ps ty Nothing -> do
    foldr (mkListPatVec ty) [nilPattern ty] <$> translatePatVec (map unLoc ps)

  -- overloaded list
  ListPat lpats elem_ty (Just (pat_ty, _to_list))
    | Just e_ty <- splitListTyConApp_maybe pat_ty, elem_ty `eqType` e_ty ->
        -- We have to ensure that the element types are exactly the same.
        -- Otherwise, one may give an instance IsList [Int] (more specific than
        -- the default IsList [a]) with a different implementation for `toList'
        translatePat (ListPat lpats e_ty Nothing)
    | otherwise -> do
        -- See Note [Guards and Approximation]
        var <- mkPmVarSM pat_ty
        return [var, fake_pat]

  ConPatOut { pat_con = L _ (PatSynCon _) } -> do
    -- Pattern synonyms have a "matcher"
    -- (see Note [Pattern synonym representation] in PatSyn.hs
    -- We should be able to transform (P x y)
    -- to   v (Just (x, y) <- matchP v (\x y -> Just (x,y)) Nothing
    -- That is, a combination of a variable pattern and a guard
    -- But there are complications with GADTs etc, and this isn't done yet
    var <- mkPmVarSM (hsPatType pat)
    return [var,fake_pat]

  ConPatOut { pat_con     = L _ (RealDataCon con)
            , pat_arg_tys = arg_tys
            , pat_tvs     = ex_tvs
            , pat_dicts   = dicts
            , pat_args    = ps } -> do
    args <- translateConPatVec arg_tys ex_tvs con ps
    return [PmCon { pm_con_con     = con
                  , pm_con_arg_tys = arg_tys
                  , pm_con_tvs     = ex_tvs
                  , pm_con_dicts   = dicts
                  , pm_con_args    = args }]

  NPat (L _ ol) mb_neg _eq -> translateNPat ol mb_neg

  LitPat lit
      -- If it is a string then convert it to a list of characters
    | HsString src s <- lit ->
        foldr (mkListPatVec charTy) [nilPattern charTy] <$>
          translatePatVec (map (LitPat . HsChar src) (unpackFS s))
    | otherwise -> return [mkLitPattern lit]

  PArrPat ps ty -> do
    tidy_ps <- translatePatVec (map unLoc ps)
    let fake_con = parrFakeCon (length ps)
    return [vanillaConPattern fake_con [ty] (concat tidy_ps)]

  TuplePat ps boxity tys -> do
    tidy_ps <- translatePatVec (map unLoc ps)
    let tuple_con = tupleDataCon boxity (length ps)
    return [vanillaConPattern tuple_con tys (concat tidy_ps)]

  -- --------------------------------------------------------------------------
  -- Not supposed to happen
  ConPatIn  {} -> panic "Check.translatePat: ConPatIn"
  SplicePat {} -> panic "Check.translatePat: SplicePat"
  SigPatIn  {} -> panic "Check.translatePat: SigPatIn"

-- | Translate an overloaded literal (see `tidyNPat' in deSugar/MatchLit.hs)
translateNPat :: HsOverLit Id -> Maybe (SyntaxExpr Id) -> UniqSM PatVec
translateNPat (OverLit val False _ ty) mb_neg
  | isStringTy ty, HsIsString src s <- val, Nothing <- mb_neg
  = translatePat (LitPat (HsString src s))
  | isIntTy    ty, HsIntegral src i <- val
  = translatePat (mk_num_lit HsInt src i)
  | isWordTy   ty, HsIntegral src i <- val
  = translatePat (mk_num_lit HsWordPrim src i)
  where
    mk_num_lit c src i = LitPat $ case mb_neg of
      Nothing -> c src i
      Just _  -> c src (-i)
translateNPat ol mb_neg
  = return [PmLit { pm_lit_lit = PmOLit (isJust mb_neg) ol }]

-- | Translate a list of patterns (Note: each pattern is translated
-- to a pattern vector but we do not concatenate the results).
translatePatVec :: [Pat Id] -> UniqSM [PatVec]
translatePatVec pats = mapM translatePat pats

translateConPatVec :: [Type] -> [TyVar]
                   -> DataCon -> HsConPatDetails Id -> UniqSM PatVec
translateConPatVec _univ_tys _ex_tvs _ (PrefixCon ps)
  = concat <$> translatePatVec (map unLoc ps)
translateConPatVec _univ_tys _ex_tvs _ (InfixCon p1 p2)
  = concat <$> translatePatVec (map unLoc [p1,p2])
translateConPatVec  univ_tys  ex_tvs c (RecCon (HsRecFields fs _))
    -- Nothing matched. Make up some fresh term variables
  | null fs        = mkPmVarsSM arg_tys
    -- The data constructor was not defined using record syntax. For the
    -- pattern to be in record syntax it should be empty (e.g. Just {}).
    -- So just like the previous case.
  | null orig_lbls = ASSERT(null matched_lbls) mkPmVarsSM arg_tys
    -- Some of the fields appear, in the original order (there may be holes).
    -- Generate a simple constructor pattern and make up fresh variables for
    -- the rest of the fields
  | matched_lbls `subsetOf` orig_lbls
  = ASSERT(length orig_lbls == length arg_tys)
      let translateOne (lbl, ty) = case lookup lbl matched_pats of
            Just p  -> translatePat p
            Nothing -> mkPmVarsSM [ty]
      in  concatMapM translateOne (zip orig_lbls arg_tys)
    -- The fields that appear are not in the correct order. Make up fresh
    -- variables for all fields and add guards after matching, to force the
    -- evaluation in the correct order.
  | otherwise = do
      arg_var_pats    <- mkPmVarsSM arg_tys
      translated_pats <- forM matched_pats $ \(x,pat) -> do
        pvec <- translatePat pat
        return (x, pvec)

      let zipped = zip orig_lbls [ x | PmVar x <- arg_var_pats ]
          guards = map (\(name,pvec) -> case lookup name zipped of
                            Just x  -> PmGrd pvec (PmExprVar x)
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

translateMatch :: LMatch Id (LHsExpr Id) -> UniqSM (PatVec,[PatVec])
translateMatch (L _ (Match _ lpats _ grhss)) = do
  pats'   <- concat <$> translatePatVec pats
  guards' <- mapM translateGuards guards
  return (pats', guards')
  where
    extractGuards :: LGRHS Id (LHsExpr Id) -> [GuardStmt Id]
    extractGuards (L _ (GRHS gs _)) = map unLoc gs

    pats   = map unLoc lpats
    guards = map extractGuards (grhssGRHSs grhss)

-- -----------------------------------------------------------------------
-- * Transform source guards (GuardStmt Id) to PmPats (Pattern)

-- | Translate a list of guard statements to a pattern vector
translateGuards :: [GuardStmt Id] -> UniqSM PatVec
translateGuards guards = do
  all_guards <- concat <$> mapM translateGuard guards
  return (replace_unhandled all_guards)
  -- It should have been (return $ all_guards) but it is too expressive.
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
translateGuard :: GuardStmt Id -> UniqSM PatVec
translateGuard (BodyStmt   e _ _ _) = translateBoolGuard e
translateGuard (LetStmt      binds) = translateLet (unLoc binds)
translateGuard (BindStmt   p e _ _) = translateBind p e
translateGuard (LastStmt        {}) = panic "translateGuard LastStmt"
translateGuard (ParStmt         {}) = panic "translateGuard ParStmt"
translateGuard (TransStmt       {}) = panic "translateGuard TransStmt"
translateGuard (RecStmt         {}) = panic "translateGuard RecStmt"
translateGuard (ApplicativeStmt {}) = panic "translateGuard ApplicativeLastStmt"

-- | Translate let-bindings
translateLet :: HsLocalBinds Id -> UniqSM PatVec
translateLet _binds = return []

-- | Translate a pattern guard
translateBind :: LPat Id -> LHsExpr Id -> UniqSM PatVec
translateBind (L _ p) e = do
  ps <- translatePat p
  return [mkGuard ps (unLoc e)]

-- | Translate a boolean guard
translateBoolGuard :: LHsExpr Id -> UniqSM PatVec
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
       (xp, xe) <- mkPmId2FormsSM pat_ty
       ps       <- translatePatVec (map unLoc lpats)
       let pats = foldr (mkListPatVec elem_ty) [nilPattern elem_ty] ps
           g    = mkGuard pats (HsApp (noLoc to_list) xe)
       return [xp,g]

3. Overloaded Literals
----------------------
The case with literals is a bit different. a literal @l@ should be translated
to @x (True <- x == from l)@. Since we want to have better warnings for
overloaded literals as it is a very common feature, we treat them differently.
They are mainly covered in Note [Undecidable Equality on Overloaded Literals].

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
                    Main Pattern Matching Check
%*                                                                      *
%************************************************************************
-}

-- ----------------------------------------------------------------------------
-- * Process a vector

-- Covered, Uncovered, Divergent
process_guards :: UniqSupply -> Bool -> [PatVec]
               -> (ValSetAbs, ValSetAbs, ValSetAbs)
process_guards _us _oversimplify [] = (Singleton, Empty, Empty) -- No guard
process_guards  us  oversimplify gs
  -- If we have a list of guards but one of them is empty (True by default)
  -- then we know that it is exhaustive (just a shortcut)
  | any null gs  = (Singleton, Empty, Singleton)
  -- If the user wants the same behaviour (almost no expressivity about guards)
  | oversimplify = go us Singleton [[fake_pat]] -- to signal failure
  -- If the user want the full reasoning (may be non-performant)
  | otherwise    = go us Singleton gs
  where
    go _usupply missing []       = (Empty, missing, Empty)
    go  usupply missing (gv:gvs) = (mkUnion cs css, uss, mkUnion ds dss)
      where
        (us1, us2, us3, us4) = splitUniqSupply4 usupply

        cs = covered   us1 Singleton gv missing
        us = uncovered us2 Empty     gv missing
        ds = divergent us3 Empty     gv missing

        (css, uss, dss) = go us4 us gvs

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

mkOneConFull :: Id -> UniqSupply -> DataCon -> (ValAbs, [PmConstraint])
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
--          [PmConstraint]:  Q, x ~ K y1..yn

mkOneConFull x usupply con = (con_abs, constraints)
  where

    (usupply1, usupply2, usupply3) = splitUniqSupply3 usupply

    res_ty = idType x -- res_ty == TyConApp (dataConTyCon cabs_con) cabs_arg_tys
    (univ_tvs, ex_tvs, eq_spec, thetas, arg_tys, _) = dataConFullSig con
    data_tc = dataConTyCon con   -- The representation TyCon
    tc_args = case splitTyConApp_maybe res_ty of
                 Just (tc, tys) -> ASSERT( tc == data_tc ) tys
                 Nothing -> pprPanic "mkOneConFull: Not TyConApp:" (ppr res_ty)

    subst1  = zipTopTCvSubst univ_tvs tc_args

    (subst, ex_tvs') = cloneTyVarBndrs subst1 ex_tvs usupply1

    -- Fresh term variables (VAs) as arguments to the constructor
    arguments  = mkConVars usupply2 (substTys subst arg_tys)
    -- All constraints bound by the constructor (alpha-renamed)
    theta_cs   = substTheta subst (eqSpecPreds eq_spec ++ thetas)
    evvars     = zipWith (nameType "pm") (listSplitUniqSupply usupply3) theta_cs
    con_abs    = PmCon { pm_con_con     = con
                       , pm_con_arg_tys = tc_args
                       , pm_con_tvs     = ex_tvs'
                       , pm_con_dicts   = evvars
                       , pm_con_args    = arguments }

    constraints -- term and type constraints
      | null evvars = [ TmConstraint (PmExprVar x) (valAbsToPmExpr con_abs) ]
      | otherwise   = [ TmConstraint (PmExprVar x) (valAbsToPmExpr con_abs)
                      , TyConstraint evvars ]

mkConVars :: UniqSupply -> [Type] -> [ValAbs] -- ys, fresh with the given type
mkConVars us tys = zipWith mkPmVar (listSplitUniqSupply us) tys

tailVSA :: ValSetAbs -> ValSetAbs
tailVSA Empty               = Empty
tailVSA Singleton           = panic "tailVSA: Singleton"
tailVSA (Union vsa1 vsa2)   = tailVSA vsa1 `mkUnion` tailVSA vsa2
tailVSA (Constraint cs vsa) = cs `mkConstraint` tailVSA vsa
tailVSA (Cons _ vsa)        = vsa -- actual work

wrapK :: DataCon -> [Type] -> [TyVar] -> [EvVar] -> ValSetAbs -> ValSetAbs
wrapK con arg_tys ex_tvs dicts = go (dataConSourceArity con) emptylist
  where
    go :: Int -> DList ValAbs -> ValSetAbs -> ValSetAbs
    go _ _    Empty = Empty
    go 0 args vsa   = PmCon { pm_con_con  = con, pm_con_arg_tys = arg_tys
                            , pm_con_tvs  = ex_tvs, pm_con_dicts = dicts
                            , pm_con_args = toList args } `mkCons` vsa
    go _ _    Singleton           = panic "wrapK: Singleton"
    go n args (Cons vs vsa)       = go (n-1) (args `snoc` vs) vsa
    go n args (Constraint cs vsa) = cs `mkConstraint` go n args vsa
    go n args (Union vsa1 vsa2)   = go n args vsa1 `mkUnion` go n args vsa2

newtype DList a = DL { unDL :: [a] -> [a] }

toList :: DList a -> [a]
toList = ($[]) . unDL
{-# INLINE toList #-}

emptylist :: DList a
emptylist = DL id
{-# INLINE emptylist #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x:))
{-# INLINE snoc #-}

-- ----------------------------------------------------------------------------
-- * Smart Value Set Abstraction Constructors
-- (NB: An empty value set can only be represented as `Empty')

-- | The smart constructor for Constraint (maintains VsaInvariant)
mkConstraint :: [PmConstraint] -> ValSetAbs -> ValSetAbs
mkConstraint _cs Empty                = Empty
mkConstraint cs1 (Constraint cs2 vsa) = Constraint (cs1++cs2) vsa
mkConstraint cs  other_vsa            = Constraint cs other_vsa

-- | The smart constructor for Union (maintains VsaInvariant)
mkUnion :: ValSetAbs -> ValSetAbs -> ValSetAbs
mkUnion Empty vsa = vsa
mkUnion vsa Empty = vsa
mkUnion vsa1 vsa2 = Union vsa1 vsa2

-- | The smart constructor for Cons (maintains VsaInvariant)
mkCons :: ValAbs -> ValSetAbs -> ValSetAbs
mkCons _ Empty = Empty
mkCons va vsa  = Cons va vsa

-- ----------------------------------------------------------------------------
-- * More smart constructors and fresh variable generation

mkGuard :: PatVec -> HsExpr Id -> Pattern
mkGuard pv e = PmGrd pv (hsExprToPmExpr e)

mkPmVar :: UniqSupply -> Type -> PmPat p
mkPmVar usupply ty = PmVar (mkPmId usupply ty)

mkPmVarSM :: Type -> UniqSM Pattern
mkPmVarSM ty = flip mkPmVar ty <$> getUniqueSupplyM

mkPmVarsSM :: [Type] -> UniqSM PatVec
mkPmVarsSM tys = mapM mkPmVarSM tys

mkPmId :: UniqSupply -> Type -> Id
mkPmId usupply ty = mkLocalId name ty
  where
    unique  = uniqFromSupply usupply
    occname = mkVarOccFS (fsLit (show unique))
    name    = mkInternalName unique occname noSrcSpan

mkPmId2FormsSM :: Type -> UniqSM (Pattern, LHsExpr Id)
mkPmId2FormsSM ty = do
  us <- getUniqueSupplyM
  let x = mkPmId us ty
  return (PmVar x, noLoc (HsVar (noLoc x)))

-- ----------------------------------------------------------------------------
-- * Converting between Value Abstractions, Patterns and PmExpr

valAbsToPmExpr :: ValAbs -> PmExpr
valAbsToPmExpr (PmCon  { pm_con_con = c, pm_con_args = ps })
  = PmExprCon c (map valAbsToPmExpr ps)
valAbsToPmExpr (PmVar  { pm_var_id  = x }) = PmExprVar x
valAbsToPmExpr (PmLit  { pm_lit_lit = l }) = PmExprLit l
valAbsToPmExpr (PmNLit { pm_lit_id  = x }) = PmExprVar x

-- Convert a pattern vector to a value list abstraction by dropping the guards
-- recursively (See Note [Translating As Patterns])
coercePatVec :: PatVec -> ValVecAbs
coercePatVec pv = concatMap coercePmPat pv

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

no_fixity :: a -- TODO: Can we retrieve the fixity from the operator name?
no_fixity = panic "Check: no fixity"

-- Get all constructors in the family (including given)
allConstructors :: DataCon -> [DataCon]
allConstructors = tyConDataCons . dataConTyCon

-- -----------------------------------------------------------------------
-- * Types and constraints

newEvVar :: Name -> Type -> EvVar
newEvVar name ty = mkLocalId name (toTcType ty)

nameType :: String -> UniqSupply -> Type -> EvVar
nameType name usupply ty = newEvVar idname ty
  where
    unique  = uniqFromSupply usupply
    occname = mkVarOccFS (fsLit (name++"_"++show unique))
    idname  = mkInternalName unique occname noSrcSpan

-- | Partition the constraints to type cs, term cs and forced variables
splitConstraints :: [PmConstraint] -> ([EvVar], [(PmExpr, PmExpr)], Maybe Id)
splitConstraints [] = ([],[],Nothing)
splitConstraints (c : rest)
  = case c of
      TyConstraint cs    -> (cs ++ ty_cs, tm_cs, bot_cs)
      TmConstraint e1 e2 -> (ty_cs, (e1,e2):tm_cs, bot_cs)
      BtConstraint cs    -> ASSERT(isNothing bot_cs) -- NB: Only one x ~ _|_
                                  (ty_cs, tm_cs, Just cs)
  where
    (ty_cs, tm_cs, bot_cs) = splitConstraints rest

{-
%************************************************************************
%*                                                                      *
                              The oracles
%*                                                                      *
%************************************************************************
-}

-- | Check whether at least a path in a value set
-- abstraction has satisfiable constraints.
anySatVSA :: ValSetAbs -> PmM Bool
anySatVSA vsa = notNull <$> pruneVSABound 1 vsa

pruneVSA :: ValSetAbs -> PmM [([PmExpr], [ComplexEq])]
-- Prune a Value Set abstraction, keeping only as many as we are going to print
-- plus one more. We need the additional one to be able to print "..." when the
-- uncovered are too many.
pruneVSA vsa = pruneVSABound (maximum_output+1) vsa

-- | Apply a term substitution to a value vector abstraction. All VAs are
-- transformed to PmExpr (used only before pretty printing).
substInValVecAbs :: PmVarEnv -> ValVecAbs -> [PmExpr]
substInValVecAbs subst = map (exprDeepLookup subst . valAbsToPmExpr)

mergeBotCs :: Maybe Id -> Maybe Id -> Maybe Id
mergeBotCs (Just x) Nothing  = Just x
mergeBotCs Nothing  (Just y) = Just y
mergeBotCs Nothing  Nothing  = Nothing
mergeBotCs (Just _) (Just _) = panic "mergeBotCs: two (x ~ _|_) constraints"

-- | Wrap up the term oracle's state once solving is complete. Drop any
-- information about unhandled constraints (involving HsExprs) and flatten
-- (height 1) the substitution.
wrapUpTmState :: TmState -> ([ComplexEq], PmVarEnv)
wrapUpTmState (residual, (_, subst)) = (residual, flattenPmVarEnv subst)

-- | Prune all paths in a value set abstraction with inconsistent constraints.
-- Returns only `n' value vector abstractions, when `n' is given as an argument.
pruneVSABound :: Int -> ValSetAbs -> PmM [([PmExpr], [ComplexEq])]
pruneVSABound n v = go n init_cs emptylist v
  where
    init_cs :: ([EvVar], TmState, Maybe Id)
    init_cs = ([], initialTmState, Nothing)

    go :: Int -> ([EvVar], TmState, Maybe Id) -> DList ValAbs
       -> ValSetAbs -> PmM [([PmExpr], [ComplexEq])]
    go n all_cs@(ty_cs, tm_env, bot_ct) vec in_vsa
      | n <= 0    = return [] -- no need to keep going
      | otherwise = case in_vsa of
          Empty -> return []
          Union vsa1 vsa2 -> do
            vecs1 <- go n                  all_cs vec vsa1
            vecs2 <- go (n - length vecs1) all_cs vec vsa2
            return (vecs1 ++ vecs2)
          Singleton -> do
            -- TODO: Provide an incremental interface for the type oracle
            sat <- tyOracle (listToBag ty_cs)
            return $ case sat of
              True  -> let (residual_eqs, subst) = wrapUpTmState tm_env
                           vector = substInValVecAbs subst (toList vec)
                       in  [(vector, residual_eqs)]
              False -> []

          Constraint cs vsa -> case splitConstraints cs of
            (new_ty_cs, new_tm_cs, new_bot_ct) ->
              case tmOracle tm_env new_tm_cs of
                Just new_tm_env ->
                  let bot = mergeBotCs new_bot_ct bot_ct
                      ans = case bot of
                              Nothing -> True                    -- covered
                              Just b  -> canDiverge b new_tm_env -- divergent
                  in  case ans of
                        True  -> go n (new_ty_cs++ty_cs,new_tm_env,bot) vec vsa
                        False -> return []
                Nothing -> return []
          Cons va vsa -> go n all_cs (vec `snoc` va) vsa

-- | This variable shows the maximum number of lines of output generated for
-- warnings. It will limit the number of patterns/equations displayed to
-- maximum_output. (TODO: add command-line option?)
maximum_output :: Int
maximum_output = 4

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

type PmArity = Int

patVecArity :: PatVec -> PmArity
patVecArity = sum . map patternArity

patternArity :: Pattern -> PmArity
patternArity (PmGrd {}) = 0
patternArity _other_pat = 1

{-
%************************************************************************
%*                                                                      *
            Heart of the algorithm: Function patVectProc
%*                                                                      *
%************************************************************************
-}

-- | Process a single vector
patVectProc :: Bool -> (PatVec, [PatVec]) -> ValSetAbs
            -> PmM (Bool, Bool, ValSetAbs)
patVectProc oversimplify (vec,gvs) vsa = do
  us <- getUniqueSupplyM
  let (c_def, u_def, d_def) = process_guards us oversimplify gvs
  (usC, usU, usD) <- getUniqueSupplyM3
  mb_c <- anySatVSA (covered   usC c_def vec vsa)
  mb_d <- anySatVSA (divergent usD d_def vec vsa)
  let vsa' = uncovered usU u_def vec vsa
  return (mb_c, mb_d, vsa')

-- | Covered, Uncovered, Divergent
covered, uncovered, divergent :: UniqSupply -> ValSetAbs
                              -> PatVec -> ValSetAbs -> ValSetAbs
covered   us gvsa vec vsa = pmTraverse us gvsa cMatcher vec vsa
uncovered us gvsa vec vsa = pmTraverse us gvsa uMatcher vec vsa
divergent us gvsa vec vsa = pmTraverse us gvsa dMatcher vec vsa

-- ----------------------------------------------------------------------------
-- * Generic traversal function
--
-- | Because we represent Value Set Abstractions as a different datatype, more
-- cases than the ones described in the paper appear. Since they are the same
-- for all three functions (covered, uncovered, divergent), function
-- `pmTraverse' handles these cases (`pmTraverse' also takes care of the
-- Guard-Case since it is common for all). The actual work is done by functions
-- `cMatcher', `uMatcher' and `dMatcher' below.

pmTraverse :: UniqSupply
           -> ValSetAbs -- gvsa
           -> PmMatcher -- what to do
           -> PatVec
           -> ValSetAbs
           -> ValSetAbs
pmTraverse _us _gvsa _rec _vec Empty      = Empty
pmTraverse _us  gvsa _rec []   Singleton  = gvsa
pmTraverse _us _gvsa _rec []   (Cons _ _) = panic "pmTraverse: cons"
pmTraverse  us  gvsa  rec vec  (Union vsa1 vsa2)
  = mkUnion (pmTraverse us1 gvsa rec vec vsa1)
            (pmTraverse us2 gvsa rec vec vsa2)
  where (us1, us2) = splitUniqSupply us
pmTraverse us gvsa rec vec (Constraint cs vsa)
  = mkConstraint cs (pmTraverse us gvsa rec vec vsa)
pmTraverse us gvsa rec (p:ps) vsa
  | PmGrd pv e <- p
  = -- Guard Case
    let (us1, us2) = splitUniqSupply us
        y  = mkPmId us1 (pmPatType p)
        cs = [TmConstraint (PmExprVar y) e]
    in  mkConstraint cs $ tailVSA $
          pmTraverse us2 gvsa rec (pv++ps) (PmVar y `mkCons` vsa)

  -- Constructor/Variable/Literal Case
  | Cons va vsa <- vsa = rec us gvsa p ps va vsa
  -- Impossible: length mismatch for ValSetAbs and PatVec
  | otherwise = panic "pmTraverse: singleton" -- can't be anything else

type PmMatcher =  UniqSupply
               -> ValSetAbs
               -> Pattern -> PatVec    -- Vector head and tail
               -> ValAbs  -> ValSetAbs -- VSA    head and tail
               -> ValSetAbs

cMatcher, uMatcher, dMatcher :: PmMatcher

-- cMatcher
-- ----------------------------------------------------------------------------

-- CVar
cMatcher us gvsa (PmVar x) ps va vsa
  = va `mkCons` (cs `mkConstraint` covered us gvsa ps vsa)
  where cs = [TmConstraint (PmExprVar x) (valAbsToPmExpr va)]

-- CLitCon
cMatcher us gvsa (PmLit l) ps (va@(PmCon {})) vsa
  = va `mkCons` (cs `mkConstraint` covered us gvsa ps vsa)
  where cs = [ TmConstraint (PmExprLit l) (valAbsToPmExpr va) ]

-- CConLit
cMatcher us gvsa (p@(PmCon {})) ps (PmLit l) vsa
  = cMatcher us3 gvsa p ps con_abs (mkConstraint cs vsa)
  where
    (us1, us2, us3)   = splitUniqSupply3 us
    y                 = mkPmId us1 (pmPatType p)
    (con_abs, all_cs) = mkOneConFull y us2 (pm_con_con p)
    cs = TmConstraint (PmExprVar y) (PmExprLit l) : all_cs

-- CConNLit
cMatcher us gvsa (p@(PmCon { pm_con_con = con })) ps
                 (PmNLit { pm_lit_id = x }) vsa
  = cMatcher us2 gvsa p ps con_abs (mkConstraint all_cs vsa)
  where
    (us1, us2)        = splitUniqSupply us
    (con_abs, all_cs) = mkOneConFull x us1 con

-- CConCon
cMatcher us gvsa (p@(PmCon { pm_con_con = c1, pm_con_args = args1 })) ps
                    (PmCon { pm_con_con = c2, pm_con_args = args2 }) vsa
  | c1 /= c2  = Empty
  | otherwise = wrapK c1 (pm_con_arg_tys p)
                         (pm_con_tvs     p)
                         (pm_con_dicts   p)
                         (covered us gvsa (args1 ++ ps)
                                          (foldr mkCons vsa args2))

-- CLitLit
cMatcher us gvsa (PmLit l1) ps (va@(PmLit l2)) vsa = case eqPmLit l1 l2 of
  -- See Note [Undecidable Equality for Overloaded Literals]
  True  -> va `mkCons` covered us gvsa ps vsa -- match
  False -> Empty                              -- mismatch

-- CConVar
cMatcher us gvsa (p@(PmCon { pm_con_con = con })) ps (PmVar x) vsa
  = cMatcher us2 gvsa p ps con_abs (mkConstraint all_cs vsa)
  where
    (us1, us2)        = splitUniqSupply us
    (con_abs, all_cs) = mkOneConFull x us1 con

-- CLitVar
cMatcher us gvsa (p@(PmLit l)) ps (PmVar x) vsa
  = cMatcher us gvsa p ps lit_abs (mkConstraint cs vsa)
  where
    lit_abs = PmLit l
    cs      = [TmConstraint (PmExprVar x) (PmExprLit l)]

-- CLitNLit
cMatcher us gvsa (p@(PmLit l)) ps
                 (PmNLit { pm_lit_id = x, pm_lit_not = lits }) vsa
  | all (not . eqPmLit l) lits
  = cMatcher us gvsa p ps lit_abs (mkConstraint cs vsa)
  | otherwise = Empty
  where
    lit_abs = PmLit l
    cs      = [TmConstraint (PmExprVar x) (PmExprLit l)]

-- Impossible: handled by pmTraverse
cMatcher _ _ (PmGrd {}) _ _ _ = panic "Check.cMatcher: Guard"

-- uMatcher
-- ----------------------------------------------------------------------------

-- UVar
uMatcher us gvsa (PmVar x) ps va vsa
  = va `mkCons` (cs `mkConstraint` uncovered us gvsa ps vsa)
  where cs = [TmConstraint (PmExprVar x) (valAbsToPmExpr va)]

-- ULitCon
uMatcher us gvsa (PmLit l) ps (va@(PmCon {})) vsa
  = uMatcher us2 gvsa (PmVar y) ps va (mkConstraint cs vsa)
  where
    (us1, us2) = splitUniqSupply us
    y  = mkPmId us1 (pmPatType va)
    cs = [TmConstraint (PmExprVar y) (PmExprLit l)]

-- UConLit
uMatcher us gvsa (p@(PmCon {})) ps (PmLit l) vsa
  = uMatcher us2 gvsa p ps (PmVar y) (mkConstraint cs vsa)
  where
    (us1, us2) = splitUniqSupply us
    y  = mkPmId us1 (pmPatType p)
    cs = [TmConstraint (PmExprVar y) (PmExprLit l)]

-- UConNLit
uMatcher us gvsa (p@(PmCon {})) ps (PmNLit { pm_lit_id = x }) vsa
  = uMatcher us gvsa p ps (PmVar x) vsa

-- UConCon
uMatcher us gvsa ( p@(PmCon { pm_con_con = c1, pm_con_args = args1 })) ps
                 (va@(PmCon { pm_con_con = c2, pm_con_args = args2 })) vsa
  | c1 /= c2  = va `mkCons` vsa
  | otherwise = wrapK c1 (pm_con_arg_tys p)
                         (pm_con_tvs     p)
                         (pm_con_dicts   p)
                         (uncovered us gvsa (args1 ++ ps)
                                            (foldr mkCons vsa args2))

-- ULitLit
uMatcher us gvsa (PmLit l1) ps (va@(PmLit l2)) vsa = case eqPmLit l1 l2 of
  -- See Note [Undecidable Equality for Overloaded Literals]
  True  -> va `mkCons` uncovered us gvsa ps vsa -- match
  False -> va `mkCons` vsa                      -- mismatch

-- UConVar
uMatcher us gvsa (p@(PmCon { pm_con_con = con })) ps (PmVar x) vsa
  = uncovered us2 gvsa (p : ps) inst_vsa
  where
    (us1, us2) = splitUniqSupply us

    -- Unfold the variable to all possible constructor patterns
    cons_cs  = zipWith (mkOneConFull x) (listSplitUniqSupply us1)
                                        (allConstructors con)
    add_one (va,cs) valset = mkUnion valset (va `mkCons` mkConstraint cs vsa)
    inst_vsa = foldr add_one Empty cons_cs -- instantiated vsa [x mapsto K_j ys]

-- ULitVar
uMatcher us gvsa (p@(PmLit l)) ps (PmVar x) vsa
  = mkUnion (uMatcher us gvsa p ps (PmLit l) (mkConstraint match_cs vsa))
            (non_match_cs `mkConstraint` (PmNLit x [l] `mkCons` vsa))
  where
    match_cs     = [ TmConstraint (PmExprVar x) (PmExprLit l)]
   -- See Note [Representation of Term Equalities]
    non_match_cs = [ TmConstraint falsePmExpr
                                  (PmExprEq (PmExprVar x) (PmExprLit l)) ]

-- ULitNLit
uMatcher us gvsa (p@(PmLit l)) ps
                 (va@(PmNLit { pm_lit_id = x, pm_lit_not = lits })) vsa
  | all (not . eqPmLit l) lits
  = mkUnion (uMatcher us gvsa p ps (PmLit l) (mkConstraint match_cs vsa))
            (non_match_cs `mkConstraint` (PmNLit x (l:lits) `mkCons` vsa))
  | otherwise = va `mkCons` vsa
  where
    match_cs     = [ TmConstraint (PmExprVar x) (PmExprLit l)]
   -- See Note [Representation of Term Equalities]
    non_match_cs = [ TmConstraint falsePmExpr
                                  (PmExprEq (PmExprVar x) (PmExprLit l)) ]

-- Impossible: handled by pmTraverse
uMatcher _ _ (PmGrd {}) _ _ _ = panic "Check.uMatcher: Guard"

-- dMatcher
-- ----------------------------------------------------------------------------

-- DVar
dMatcher us gvsa (PmVar x) ps va vsa
  = va `mkCons` (cs `mkConstraint` divergent us gvsa ps vsa)
  where cs = [TmConstraint (PmExprVar x) (valAbsToPmExpr va)]

-- DLitCon
dMatcher us gvsa (PmLit l) ps (va@(PmCon {})) vsa
  = va `mkCons` (cs `mkConstraint` divergent us gvsa ps vsa)
  where cs = [ TmConstraint (PmExprLit l) (valAbsToPmExpr va) ]

-- DConLit
dMatcher us gvsa (p@(PmCon { pm_con_con = con })) ps (PmLit l) vsa
  = dMatcher us3 gvsa p ps con_abs (mkConstraint cs vsa)
  where
    (us1, us2, us3)   = splitUniqSupply3 us
    y                 = mkPmId us1 (pmPatType p)
    (con_abs, all_cs) = mkOneConFull y us2 con
    cs = TmConstraint (PmExprVar y) (PmExprLit l) : all_cs

-- DConNLit
dMatcher us gvsa (p@(PmCon { pm_con_con = con })) ps
                 (PmNLit { pm_lit_id = x }) vsa
  = dMatcher us2 gvsa p ps con_abs (mkConstraint all_cs vsa)
  where
    (us1, us2)        = splitUniqSupply us
    (con_abs, all_cs) = mkOneConFull x us1 con

-- DConCon
dMatcher us gvsa (p@(PmCon { pm_con_con = c1, pm_con_args = args1 })) ps
                    (PmCon { pm_con_con = c2, pm_con_args = args2 }) vsa
  | c1 /= c2  = Empty
  | otherwise = wrapK c1 (pm_con_arg_tys p)
                         (pm_con_tvs     p)
                         (pm_con_dicts   p)
                         (divergent us gvsa (args1 ++ ps)
                                            (foldr mkCons vsa args2))

-- DLitLit
dMatcher us gvsa (PmLit l1) ps (va@(PmLit l2)) vsa = case eqPmLit l1 l2 of
  -- See Note [Undecidable Equality for Overloaded Literals]
  True  -> va `mkCons` divergent us gvsa ps vsa -- match
  False -> Empty                                -- mismatch

-- DConVar
dMatcher us gvsa (p@(PmCon { pm_con_con = con })) ps (PmVar x) vsa
  = mkUnion (PmVar x `mkCons` mkConstraint [BtConstraint x] vsa)
            (dMatcher us2 gvsa p ps con_abs (mkConstraint all_cs vsa))
  where
    (us1, us2)        = splitUniqSupply us
    (con_abs, all_cs) = mkOneConFull x us1 con

-- DLitVar
dMatcher us gvsa (PmLit l) ps (PmVar x) vsa
  = mkUnion (PmVar x `mkCons` mkConstraint [BtConstraint x] vsa)
            (dMatcher us gvsa (PmLit l) ps (PmLit l) (mkConstraint cs vsa))
  where
    cs = [TmConstraint (PmExprVar x) (PmExprLit l)]

-- DLitNLit
dMatcher us gvsa (p@(PmLit l)) ps
                 (PmNLit { pm_lit_id = x, pm_lit_not = lits }) vsa
  | all (not . eqPmLit l) lits
  = dMatcher us gvsa p ps lit_abs (mkConstraint cs vsa)
  | otherwise = Empty
  where
    lit_abs = PmLit l
    cs      = [TmConstraint (PmExprVar x) (PmExprLit l)]

-- Impossible: handled by pmTraverse
dMatcher _ _ (PmGrd {}) _ _ _ = panic "Check.dMatcher: Guard"

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
genCaseTmCs2 (Just scr) [p] [var] = liftUs $ do
  [e] <- map valAbsToPmExpr . coercePatVec <$> translatePat p
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
`valAbsToPmExpr' to transform the value abstraction to an expression in the
guard pattern (value abstractions are a subset of expressions). We keep the
guards in the first pattern `p' though.
-}

{-
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

-- | Issue a warning if the guards are too many and the checker gives up
warnManyGuards :: DsMatchContext -> DsM ()
warnManyGuards (DsMatchContext kind loc)
  = putSrcSpanDs loc $ warnDs $ vcat
      [ sep [ ptext (sLit "Too many guards in") <+> pprMatchContext kind
            , ptext (sLit "Guard checking has been over-simplified") ]
      , parens (ptext (sLit "Use:") <+> (opt_1 $$ opt_2)) ]
  where
    opt_1 = hang (ptext (sLit "-Wno-too-many-guards")) 2 $
      ptext (sLit "to suppress this warning")
    opt_2 = hang (ptext (sLit "-ffull-guard-reasoning")) 2 $ vcat
      [ ptext (sLit "to run the full checker (may increase")
      , ptext (sLit "compilation time and memory consumption)") ]

dsPmWarn :: DynFlags -> DsMatchContext -> DsM PmResult -> DsM ()
dsPmWarn dflags ctx@(DsMatchContext kind loc) mPmResult
  = when (flag_i || flag_u) $ do
      (redundant, inaccessible, uncovered) <- mPmResult
      let exists_r = flag_i && notNull redundant
          exists_i = flag_i && notNull inaccessible
          exists_u = flag_u && notNull uncovered
      when exists_r $ putSrcSpanDs loc (warnDs (pprEqns  redundant    rmsg))
      when exists_i $ putSrcSpanDs loc (warnDs (pprEqns  inaccessible imsg))
      when exists_u $ putSrcSpanDs loc (warnDs (pprEqnsU uncovered))
  where
    flag_i = wopt Opt_WarnOverlappingPatterns dflags
    flag_u = exhaustive dflags kind

    rmsg = "are redundant"
    imsg = "have inaccessible right hand side"

    pprEqns qs text = pp_context ctx (ptext (sLit text)) $ \f ->
      vcat (map (ppr_eqn f kind) (take maximum_output qs)) $$ dots qs

    pprEqnsU qs = pp_context ctx (ptext (sLit "are non-exhaustive")) $ \_ ->
      case qs of -- See #11245
           [([],_)] -> ptext (sLit "Guards do not cover entire pattern space")
           _missing -> let us = map ppr_uncovered qs
                       in  hang (ptext (sLit "Patterns not matched:")) 4
                                (vcat (take maximum_output us) $$ dots us)

dots :: [a] -> SDoc
dots qs | qs `lengthExceeds` maximum_output = ptext (sLit "...")
        | otherwise                         = empty

exhaustive :: DynFlags -> HsMatchContext id -> Bool
exhaustive  dflags (FunRhs {})   = wopt Opt_WarnIncompletePatterns dflags
exhaustive  dflags CaseAlt       = wopt Opt_WarnIncompletePatterns dflags
exhaustive _dflags IfAlt         = False
exhaustive  dflags LambdaExpr    = wopt Opt_WarnIncompleteUniPatterns dflags
exhaustive  dflags PatBindRhs    = wopt Opt_WarnIncompleteUniPatterns dflags
exhaustive  dflags ProcExpr      = wopt Opt_WarnIncompleteUniPatterns dflags
exhaustive  dflags RecUpd        = wopt Opt_WarnIncompletePatternsRecUpd dflags
exhaustive _dflags ThPatSplice   = False
exhaustive _dflags PatSyn        = False
exhaustive _dflags ThPatQuote    = False
exhaustive _dflags (StmtCtxt {}) = False -- Don't warn about incomplete patterns
                                       -- in list comprehensions, pattern guards
                                       -- etc. They are often *supposed* to be
                                       -- incomplete

pp_context :: DsMatchContext -> SDoc -> ((SDoc -> SDoc) -> SDoc) -> SDoc
pp_context (DsMatchContext kind _loc) msg rest_of_msg_fun
  = vcat [ptext (sLit "Pattern match(es)") <+> msg,
          sep [ ptext (sLit "In") <+> ppr_match <> char ':'
              , nest 4 (rest_of_msg_fun pref)]]
  where
    (ppr_match, pref)
        = case kind of
             FunRhs fun -> (pprMatchContext kind, \ pp -> ppr fun <+> pp)
             _          -> (pprMatchContext kind, \ pp -> pp)

ppr_pats :: HsMatchContext Name -> [Pat Id] -> SDoc
ppr_pats kind pats
  = sep [sep (map ppr pats), matchSeparator kind, ptext (sLit "...")]

ppr_eqn :: (SDoc -> SDoc) -> HsMatchContext Name -> [LPat Id] -> SDoc
ppr_eqn prefixF kind eqn = prefixF (ppr_pats kind (map unLoc eqn))

ppr_constraint :: (SDoc,[PmLit]) -> SDoc
ppr_constraint (var, lits) = var <+> ptext (sLit "is not one of")
                                 <+> braces (pprWithCommas ppr lits)

ppr_uncovered :: ([PmExpr], [ComplexEq]) -> SDoc
ppr_uncovered (expr_vec, complex)
  | null cs   = fsep vec -- there are no literal constraints
  | otherwise = hang (fsep vec) 4 $
                  ptext (sLit "where") <+> vcat (map ppr_constraint cs)
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
