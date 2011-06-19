{-# LANGUAGE PatternGuards #-}
module TcTypeNats ( canonicalNum, NumericsResult(..) ) where

import TcSMonad   ( TcS, Xi
                  , newCoVar, setCoBind
                  , newDictVar, setDictBind
                  , getWantedLoc
                  , tcsLookupTyCon, tcsLookupClass
                  , CanonicalCt (..), CanonicalCts
                  , mkFrozenError
                  , traceTcS
                  , WorkList, unionWorkList, emptyWorkList
                  , workListFromEqs, workListFromNonEqs
                  , isCFunEqCan_Maybe, isCTyEqCan
                  )
import TcRnTypes  ( CtFlavor(..) )
import TcCanonical (mkCanonicals)
import HsBinds    (EvTerm(..))
import Class      ( Class, className, classTyCon )
import Type       ( tcView, mkTyConApp, mkNumberTy, isNumberTy
                  , eqType, cmpType, pprType
                  )
import TypeRep    (Type(..))
import TyCon      (TyCon, tyConName)
import Var        (EvVar)
import Coercion   ( mkUnsafeCo )
import Outputable
import PrelNames  ( lessThanEqualClassName
                  , addTyFamName, mulTyFamName, expTyFamName
                  )
import Bag        (bagToList, unionManyBags, unionBags, partitionBag)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList, isJust)
import Data.List  (nub, partition, sort)
import Control.Monad (msum, mplus, zipWithM, (<=<), guard)

import Debug.Trace
import Unique(getUnique)
import Type(getTyVar)


data Term = Var Xi | Num Integer (Maybe Xi)
data Op   = Add | Mul | Exp deriving Eq
data Prop = EqFun Op Term Term Term
          | Leq Term Term
          | Eq Term Term

commutative :: Op -> Bool
commutative op = op == Add || op == Mul

associative :: Op -> Bool
associative op = op == Add || op == Mul

num :: Integer -> Term
num n = Num n Nothing

opFun :: Op -> (Integer -> Integer -> Integer)
opFun x = case x of
            Add -> (+)
            Mul -> (*)
            Exp -> (^)


instance Eq Term where
  Var x   == Var y    = eqType x y
  Num x _ == Num y _  = x == y
  _       == _        = False

instance Ord Term where
  compare (Num x _) (Num y _) = compare x y
  compare (Num _ _) (Var _)   = LT
  compare (Var x)   (Var y)   = cmpType x y
  compare (Var _)   (Num _ _) = GT


--------------------------------------------------------------------------------
-- Interface with the type checker
--------------------------------------------------------------------------------

data NumericsResult = NumericsResult
  { numNewWork :: WorkList
  , numInert   :: Maybe CanonicalCts   -- Nothing for "no change"
  , numNext    :: Maybe CanonicalCt
  }

-- We keep the original type in numeric constants to preserve type synonyms.
toTerm :: Xi -> Term
toTerm xi = case mplus (isNumberTy xi) (isNumberTy =<< tcView xi) of
              Just n -> Num n (Just xi)
              _      -> Var xi

fromTerm :: Term -> Xi
fromTerm (Num n mb) = fromMaybe (mkNumberTy n) mb
fromTerm (Var xi)   = xi

toProp :: CanonicalCt -> Prop
toProp (CDictCan { cc_class = c, cc_tyargs = [xi1,xi2] })
  | className c == lessThanEqualClassName = Leq (toTerm xi1) (toTerm xi2)

toProp (CFunEqCan { cc_fun = tc, cc_tyargs = [xi11,xi12], cc_rhs = xi2 })
  | tyConName tc == addTyFamName = EqFun Add t1 t2 t3
  | tyConName tc == mulTyFamName = EqFun Mul t1 t2 t3
  | tyConName tc == expTyFamName = EqFun Exp t1 t2 t3

    where t1 = toTerm xi11
          t2 = toTerm xi12
          t3 = toTerm xi2

toProp p = panic $
  "[TcTypeNats.toProp] Unexpected CanonicalCt: " ++ showSDoc (ppr p)

-- XXX: Not doing anything, just trying to get things to compile.
canonicalNum :: CanonicalCts -> CanonicalCts -> CanonicalCts -> CanonicalCt ->
                TcS NumericsResult
canonicalNum given derived wanted prop =
  case cc_flavor prop of
    Wanted {}   -> solveNumWanted given derived wanted prop
    Derived {}  -> addNumDerived  given derived wanted prop
    Given {}    -> addNumGiven    given derived wanted prop

{- Disables the solver.
  return NumericsResult { numInert    = Nothing
                        , numNewWork  = emptyWorkList
                        , numNext     = Just prop
                        }
-}




solveNumWanted :: CanonicalCts -> CanonicalCts -> CanonicalCts -> CanonicalCt ->
                TcS NumericsResult
solveNumWanted given derived wanted prop =
  do let asmps = map toProp $ bagToList $ unionManyBags [given,derived,wanted]
         goal  = toProp prop

     numTrace "solveNumWanted" (vmany asmps <+> text "|-" <+> ppr goal)

     case solve asmps goal of

       Simplified sgs ->
         do numTrace "Simplified to" (vmany sgs)
            defineDummy (cc_id prop) =<< fromProp goal
            evs   <- mapM (newSubGoal <=< fromProp) sgs
            goals <- mkCanonicals (cc_flavor prop) evs
            return NumericsResult
              { numNext = Nothing, numInert = Nothing, numNewWork = goals }

       -- XXX: The new wanted might imply some of the existing wanteds...
       Improved is ->
         do numTrace "Improved by" (vmany is)
            evs   <- mapM (newSubGoal <=< fromProp) is
            goals <- mkCanonicals (cc_flavor prop) evs
            return NumericsResult
              { numNext = Just prop, numInert = Nothing, numNewWork = goals }

       Impossible -> impossible prop


-- XXX: Need to understand derived work better.
addNumDerived :: CanonicalCts -> CanonicalCts -> CanonicalCts -> CanonicalCt ->
              TcS NumericsResult
addNumDerived given derived wanted prop =
  do let asmps = map toProp $ bagToList given
         goal  = toProp prop

     numTrace "addNumDerived" (vmany asmps <+> text "|-" <+> ppr goal)

     case solve asmps goal of

       Simplified sgs ->
         do numTrace "Simplified to" (vmany sgs)
            defineDummy (cc_id prop) =<< fromProp goal
            evs   <- mapM (newSubGoal <=< fromProp) sgs
            goals <- mkCanonicals (cc_flavor prop) evs
            return NumericsResult
              { numNext = Nothing, numInert = Nothing, numNewWork = goals }

       -- XXX: watch out for cycles because of the wanteds being restarted:
       -- W => D && D => W, we could solve W by W
       -- if W <=> D, then we should simplify W to D, not make it derived.
       Improved is ->
         do numTrace "Improved by" (vmany is)
            evs   <- mapM (newSubGoal <=< fromProp) is
            goals <- mkCanonicals (Derived (getWantedLoc prop)) evs
            return NumericsResult
              { numNext = Just prop, numInert = Just (unionBags given derived)
              , numNewWork = unionWorkList (reconsider wanted) goals }

       Impossible -> impossible prop


addNumGiven :: CanonicalCts -> CanonicalCts -> CanonicalCts
            -> CanonicalCt -> TcS NumericsResult
addNumGiven given derived wanted prop =
  do let asmps = map toProp (bagToList given)
         goal  = toProp prop

     numTrace "addNumGiven" (vmany asmps <+> text " /\\ " <+> ppr goal)
     case solve asmps goal of

       Simplified sgs ->
         do numTrace "Simplified to" (vmany sgs)
            evs   <- mapM (newFact <=< fromProp) sgs
            facts <- mkCanonicals (cc_flavor prop) evs
            return NumericsResult
              { numNext = Nothing, numInert = Nothing, numNewWork = facts }

       Improved is ->
         do numTrace "Improved by" (vmany is)
            evs <- mapM (newFact <=< fromProp) is
            facts <- mkCanonicals (cc_flavor prop) evs
            return NumericsResult
              { numNext = Just prop, numInert = Just (unionBags given derived)
              , numNewWork = unionWorkList (reconsider wanted) facts }

       Impossible -> impossible prop


reconsider :: CanonicalCts -> WorkList
reconsider cs = let (eqs,non) = partitionBag isEq cs
                in unionWorkList (workListFromEqs eqs) (workListFromNonEqs non)
  where isEq c = isCTyEqCan c || isJust (isCFunEqCan_Maybe c)


impossible :: CanonicalCt -> TcS NumericsResult
impossible c =
  do numTrace "Impossible" empty
     let err = mkFrozenError (cc_flavor c) (cc_id c)
     return NumericsResult
       { numNext = Just err, numInert = Nothing, numNewWork = emptyWorkList }



data CvtProp  = CvtClass Class [Type]
              | CvtCo Type Type

fromProp :: Prop -> TcS CvtProp
fromProp (Leq t1 t2) =
  do cl <- tcsLookupClass lessThanEqualClassName
     return (CvtClass cl [ fromTerm t1, fromTerm t2 ])

fromProp (Eq t1 t2) = return $ CvtCo (fromTerm t1) (fromTerm t2)

fromProp (EqFun op t1 t2 t3) =
  do tc <- tcsLookupTyCon $ case op of
                              Add -> addTyFamName
                              Mul -> mulTyFamName
                              Exp -> expTyFamName
     return $ CvtCo (mkTyConApp tc [fromTerm t1, fromTerm t2]) (fromTerm t3)


newSubGoal :: CvtProp -> TcS EvVar
newSubGoal (CvtClass c ts) = newDictVar c ts
newSubGoal (CvtCo t1 t2)   = newCoVar t1 t2

newFact :: CvtProp -> TcS EvVar
newFact prop =
  do d <- newSubGoal prop
     defineDummy d prop
     return d


-- If we decided that we want to generate evidence terms,
-- here we would set the evidence properly.  For now, we skip this
-- step because evidence terms are not used for anything, and they
-- get quite large, at least, if we start with a small set of axioms.
defineDummy :: EvVar -> CvtProp -> TcS ()
defineDummy d (CvtClass c ts) =
  setDictBind d $ EvAxiom "<=" $ mkTyConApp (classTyCon c) ts

defineDummy c (CvtCo t1 t2) =
  setCoBind c (mkUnsafeCo t1 t2)




--------------------------------------------------------------------------------
-- The Solver
--------------------------------------------------------------------------------


data Result = Impossible            -- We know that the goal cannot be solved.
            | Simplified [Prop]     -- We reformulated the goal.
            | Improved   [Prop]     -- We learned some new facts.

solve :: [Prop] -> Prop -> Result
solve asmps (Leq a b) =
  let ps = propsToOrd asmps
  in case isLeq ps a b of
       True  -> Simplified []
       False ->
         case improveLeq ps a b of
           Nothing -> Impossible
           Just ps -> Improved ps

solve asmps prop =
  case solveWanted1 prop of
    Just sgs -> Simplified sgs
    Nothing ->
      case msum $ zipWith solveWanted2 asmps (repeat prop) of
        Just sgs -> Simplified sgs
        Nothing
          | solveAC asmps prop  -> Simplified []
          | byDistr asmps prop  -> Simplified []
          | otherwise ->
            case improve1 prop of
              Nothing   -> Impossible
              Just eqs  ->
                case zipWithM improve2 asmps (repeat prop) of
                  Nothing   -> Impossible
                  Just eqs1 -> Improved (concat (eqs : eqs1))




improve1 :: Prop -> Maybe [Prop]
improve1 prop =
  case prop of

    EqFun Add (Num m _) (Num n _) t         -> Just [ Eq (num (m+n)) t ]
    EqFun Add (Num 0 _) s         t         -> Just [ Eq s t ]
    EqFun Add (Num m _) s         (Num n _)
      | m <= n                              -> Just [ Eq (num (n-m)) s ]
      | otherwise                           -> Nothing
    EqFun Add r s (Num 0 _)             -> Just [ Eq (num 0) r, Eq (num 0) s ]
    EqFun Add r         s         t
      | r == t                              -> Just [ Eq (num 0) s ]
      | s == t                              -> Just [ Eq (num 0) r ]

    EqFun Mul (Num m _) (Num n _) t -> Just [ Eq (num (m*n)) t ]
    EqFun Mul (Num 0 _) _         t -> Just [ Eq (num 0) t ]
    EqFun Mul (Num 1 _) s         t -> Just [ Eq s t ]
    EqFun Mul (Num _ _) s         t
      | s == t                      -> Just [ Eq (num 0) s ]

    EqFun Mul r         s (Num 1 _) -> Just [ Eq (num 1) r, Eq (num 1) s ]
    EqFun Mul (Num m _) s (Num n _)
      | Just a <- divide n m        -> Just [ Eq (num a) s ]
      | otherwise                   -> Nothing


    EqFun Exp (Num m _) (Num n _) t -> Just [ Eq (num (m ^ n)) t ]

    EqFun Exp (Num 1 _) _         t -> Just [ Eq (num 1) t ]
    EqFun Exp (Num _ _) s t
      | s == t                      -> Nothing
    EqFun Exp (Num m _) s (Num n _) -> do a <- descreteLog m n
                                          return [ Eq (num a) s ]

    EqFun Exp _ (Num 0 _) t         -> Just [ Eq (num 1) t ]
    EqFun Exp r (Num 1 _) t         -> Just [ Eq r t ]
    EqFun Exp r (Num m _) (Num n _) -> do a <- descreteRoot m n
                                          return [ Eq (num a) r ]

    _                               -> Just []

improve2 :: Prop -> Prop -> Maybe [ Prop ]
improve2 asmp prop =
  case asmp of

    EqFun Add a1 b1 c1 ->
      case prop of
        EqFun Add a2 b2 c2
          | a1 == a2 && b1 == b2  -> Just [ Eq c1 c2 ]
          | a1 == b2 && b1 == a2  -> Just [ Eq c1 c2 ]
          | c1 == c2 && a1 == a2  -> Just [ Eq b1 b2 ]
          | c1 == c2 && a1 == b2  -> Just [ Eq b1 a2 ]
          | c1 == c2 && b1 == b2  -> Just [ Eq a1 a2 ]
          | c1 == c2 && b1 == a2  -> Just [ Eq a1 b2 ]
        _ -> Just []


    EqFun Mul a1 b1 c1 ->
      case prop of
        EqFun Mul a2 b2 c2
          | a1 == a2 && b1 == b2  -> Just [ Eq c1 c2 ]
          | a1 == b2 && b1 == a2  -> Just [ Eq c1 c2 ]
          | c1 == c2 && b1 == b2, Num m _ <- a1, Num n _ <- a2, m /= n
                                  -> Just [ Eq (num 0) b1, Eq (num 0) c1 ]
        _ -> Just []


    _ -> Just []


solveWanted1 :: Prop -> Maybe [ Prop ]
solveWanted1 prop =
  case prop of

    EqFun Add (Num m _) (Num n _) (Num mn _)  | m + n == mn -> Just []
    EqFun Add (Num 0 _) s         t           | s == t      -> Just []
    EqFun Add r         s         t           | r == s      ->
                                                Just [ EqFun Mul (num 2) r t ]

    EqFun Mul (Num m _) (Num n _) (Num mn _)  | m * n == mn -> Just []
    EqFun Mul (Num 0 _) _         (Num 0 _)                 -> Just []
    EqFun Mul (Num 1 _) s         t           | s == t      -> Just []
    EqFun Mul r         s         t           | r == s      ->
                                                Just [ EqFun Exp r (num 2) t ]

    -- Simple normalization of commutative operators
    EqFun op  r@(Var _) s@(Num _ _) t         | commutative op ->
                                                Just [ EqFun op s r t ]

    EqFun Exp (Num m _) (Num n _) (Num mn _)  | m ^ n == mn -> Just []
    EqFun Exp (Num 1 _) _         (Num 1 _)                 -> Just []
    EqFun Exp _         (Num 0 _) (Num 1 _)                 -> Just []
    EqFun Exp r         (Num 1 _) t           | r == t      -> Just []
    EqFun Exp r         (Num _ _) t           | r == t      ->
                                                Just  [Leq r (num 1)]

    _ -> Nothing


solveWanted2 :: Prop -> Prop -> Maybe [Prop]
solveWanted2 asmp prop =
  case (asmp, prop) of

    (EqFun op1 r1 s1 t1, EqFun op2 r2 s2 t2)
      | op1 == op2 && t1 == t2 &&
      (  r1 == r2 && s1 == s2
      || commutative op1 && r1 == s2 && s1 == r2
      )                         -> Just []

    (EqFun Add (Num m _) b c1, EqFun Add (Num n _) d c2)
      | c1 == c2 -> if m >= n then Just [ EqFun Add (num (m - n)) b d ]
                              else Just [ EqFun Add (num (n - m)) d b ]

    (EqFun Mul (Num m _) b c1, EqFun Mul (Num n _) d c2)
      | c1 == c2, Just x <- divide m n -> Just [ EqFun Mul (num x) b d ]
      | c1 == c2, Just x <- divide n m -> Just [ EqFun Mul (num x) d b ]

    -- hm:  m * b = c |- c + b = d <=> (m + 1) * b = d
    (EqFun Mul (Num m _) s1 t1, EqFun Add r2 s2 t2)
      | t1 == r2 && s1 == s2    -> Just [ EqFun Mul (num (m + 1)) s1 t2 ]
      | t1 == s2 && s1 == r2    -> Just [ EqFun Mul (num (m + 1)) r2 t2 ]

    _                           -> Nothing


--------------------------------------------------------------------------------
-- Reasoning about ordering.
--------------------------------------------------------------------------------

-- This function assumes that the assumptions are acyclic.
isLeq :: [(Term, Term)] -> Term -> Term -> Bool
isLeq _ (Num 0 _) _         = True
isLeq _ (Num m _) (Num n _) = m <= n
isLeq _ a b | a == b        = True
isLeq ps (Num m _) a        = or [ isLeq ps b a  | (Num x _, b) <- ps, m <= x ]
isLeq ps a (Num m _)        = or [ isLeq ps a b  | (b, Num x _) <- ps, x <= m ]
isLeq ps a b                = or [ isLeq ps c b  | (a',c)       <- ps, a == a' ]

isGt :: [(Term, Term)] -> Term -> Term -> Bool
isGt _ (Num m _) (Num n _)  = m > n
isGt ps (Num m _) a         = (m > 0) && isLeq ps a (num (m - 1))
isGt ps a (Num m _)         = isLeq ps (num (m + 1)) a
isGt _ _ _                  = False

improveLeq :: [(Term,Term)] -> Term -> Term -> Maybe [Prop]
improveLeq ps a b | isLeq ps b a  = Just [Eq a b]
                  | isGt ps a b   = Nothing
                  | otherwise     = Just []

-- Ordering constraints derived from numeric predicates.
-- We do not consider equlities because they should be substituted away.
propsToOrd :: [Prop] -> [(Term,Term)]
propsToOrd props = loop (step [] unconditional)
  where
  loop ps = let new = filter (`notElem` ps) (step ps conditional)
            in if null new then ps else loop (new ++ ps)

  step ps = nub . concatMap (toOrd ps)

  isConditional (EqFun op _ _ _)  = op == Mul || op == Exp
  isConditional _                 = False

  (conditional,unconditional)     = partition isConditional props

  toOrd _ (Leq a b) = [(a,b)]
  toOrd _ (Eq _ _)  = []      -- Would lead to a cycle, should be subst. away
  toOrd ps (EqFun op a b c) =
    case op of
      Add               -> [(a,c),(b,c)]

      Mul               -> (guard (isLeq ps (num 1) a) >> return (b,c)) ++
                           (guard (isLeq ps (num 1) b) >> return (a,c))
      Exp
        | Num 0 _ <- a  -> [(c, num 1)]
        | a == c        -> [(a, num 1)]
        | otherwise     -> (guard (isLeq ps (num 2) a) >> return (b,c)) ++
                           (guard (isLeq ps (num 1) b) >> return (a,c))

--------------------------------------------------------------------------------
-- Associative and Commutative Operators
--------------------------------------------------------------------------------

-- XXX: It'd be nice to go back to a "small-step" style solve.
-- If we can get it to work, it is likely to be simpler,
-- and it would be way easier to provide proofs.


-- XXX: recursion may non-terminate: x * y = x
-- XXX: does not do improvements



solveAC :: [Prop] -> Prop -> Bool
solveAC ps (EqFun op x y z)
  | commutative op && associative op =
      (xs_ys === z) || or [ add as xs_ys === r | (as,r) <- cancelCands z ]

  where
  xs_ys         = add (sums x) (sums y)

  candidates c  = [ (a,b) | (a,b,c') <- asmps, c == c' ]

  -- (xs,e) `elem` cancelCands g    ==> xs + g = e
  cancelCands :: Term -> [([[Term]],Term)]
  cancelCands g = do (a,b,c) <- asmps
                     let us = filter (all mayCancel)
                            $ case () of
                                _ | a == g    -> sums b
                                  | b == g    -> sums a
                                  | otherwise -> [ ]
                     guard (not (null us))
                     (us,c) : [ (add us vs, e) | (vs,e) <- cancelCands c ]


  mayCancel x   = case op of
                    Add -> True
                    Mul -> isLeq ordProps (num 1) x
                    Exp -> False

  -- xs `elem` sums a   ==>   sum xs = a
  sums a        = [a] : [ p | (b,c) <- candidates a, p <- add (sums b) (sums c)]

  add :: [Terms] -> [Terms] -> [Terms]
  add as bs     = [ doOp2 (opFun op) u v | u <- as, v <- bs ]

  (===) :: [[Term]] -> Term -> Bool
  as === b      = any (`elem` sums b) as

  -- Facts in a more convenient from
  asmps         = [ (a,b,c) | EqFun op' a b c <- ps, op == op' ]
  ordProps      = propsToOrd ps


solveAC _ _ = False




-- Combining sequences of terms with the same operation.
-- We preserve a normal form with at most 1 constant and variables sorted.
-- Examples:
--  (1 + c + y) + (2 + b + z) = 3 + b + c + y + z
--  (2 * c * y) * (3 * b * z) = 6 * b * c * y * z

type Terms = [Term]

doOp2 :: (Integer -> Integer -> Integer) -> Terms -> Terms -> Terms
doOp2 op (Num m _ : as) (Num n _ : bs) = num (op m n) : merge as bs
doOp2 _ xs ys                          = merge xs ys

merge :: Ord a => [a] -> [a] -> [a]
merge xs@(a:as) ys@(b:bs)
  | a <= b    = a : merge as ys
  | otherwise = b : merge xs bs
merge [] ys = ys
merge xs [] = xs


-- XXX: This duplicates a lot of work done by AC.
byDistr :: [Prop] -> Prop -> Bool
byDistr ps (EqFun op x y z)
  -- (a * b + a * c) = x |- a * (b + c) = x
  | op == Mul = let zs = sumsFor z
                    ps = sumProds x y
                in -- pNumTrace "Distr *" (text "zs:" <+> vmany zs <> comma <+> text "ps:" <+> vmany ps) $
                   any (`elem` zs) ps

  -- a * (b + c) = x |- a * b + a * c = x
  | op == Add = let as = sumsFor x
                    bs = sumsFor y
                    cs = [ doOp2 (opFun op) a b | a <- as, b <- bs ]
                    ds = do (a,b,z') <- prods
                            guard (z == z')
                            sumProds a b
                in -- pNumTrace "Distr +" (text "cs:" <+> vmany cs <> comma <+> text "ds:" <+> vmany ds) $
                   any (`elem` cs) ds

  where
  sums      = [ (a,b,c) | EqFun Add a b c <- ps ]
  prods     = [ (a,b,c) | EqFun Mul a b c <- ps ]
  sumsFor  :: Term -> [[Term]]
  sumsFor c = [c] : do (a,b,c') <- sums
                       guard (c == c')
                       u <- sumsFor a
                       v <- sumsFor b
                       return (doOp2 (opFun Mul) u v)

  -- This is very ad-hock.
  prod (Num 1 _, a) = Just a
  prod (a, Num 1 _) = Just a
  prod (a,b)  = listToMaybe [ c | (a',b',c) <- prods, a == a', b == b' ]

  sumProds u v  = do s1 <- sumsFor u
                     s2 <- sumsFor v
                     maybeToList $
                       do s3 <- mapM prod [ (a,b) | a <- s1, b <- s2 ]
                          return (sort s3)
byDistr _ _ = False




--------------------------------------------------------------------------------
-- Descrete Math
--------------------------------------------------------------------------------

descreteRoot :: Integer -> Integer -> Maybe Integer
descreteRoot root num = search 0 num
  where
  search from to = let x = from + div (to - from) 2
                       a = x ^ root
                   in case compare a num of
                        EQ              -> Just x
                        LT | x /= from  -> search x to
                        GT | x /= to    -> search from x
                        _               -> Nothing

descreteLog :: Integer -> Integer -> Maybe Integer
descreteLog _    0   = Just 0
descreteLog base num | base == num  = Just 1
descreteLog base num = case divMod num base of
                         (x,0) -> fmap (1+) (descreteLog base x)
                         _     -> Nothing

divide :: Integer -> Integer -> Maybe Integer
divide _ 0  = Nothing
divide x y  = case divMod x y of
                (a,0) -> Just a
                _     -> Nothing


--------------------------------------------------------------------------------
-- Debugging
--------------------------------------------------------------------------------

numTrace :: String -> SDoc -> TcS ()
numTrace x y = traceTcS ("[numerics] " ++ x) y

pNumTrace :: String -> SDoc -> a -> a
pNumTrace x y = trace ("[numerics] " ++ x ++ " " ++ showSDoc y)

vmany :: Outputable a => [a] -> SDoc
vmany xs = braces $ hcat $ punctuate comma $ map ppr xs

instance Outputable Term where
  ppr (Var xi)  = pprType xi <> un
    where un = brackets $ text $ show $ getUnique (getTyVar "numerics dbg" xi)
  ppr (Num n _) = integer n

instance Outputable Prop where
  ppr (EqFun op t1 t2 t3) = ppr t1 <+> ppr op <+> ppr t2 <+> char '~' <+> ppr t3
  ppr (Leq t1 t2)         = ppr t1 <+> text "<=" <+> ppr t2
  ppr (Eq t1 t2)          = ppr t1 <+> char '~'  <+> ppr t2

instance Outputable Op where
  ppr op  = case op of
              Add -> char '+'
              Mul -> char '*'
              Exp -> char '^'

