{-# LANGUAGE PatternGuards #-}
module TcTypeNats (solveNumWanted, addNumGiven) where

import TcSMonad   ( TcS, Xi
                  , newWantedCoVar
                  , setWantedCoBind, setDictBind, newDictVar
                  , tcsLookupTyCon, tcsLookupClass
                  , CanonicalCt (..), CanonicalCts
                  , traceTcS
                  )
import TcCanonical (mkCanonicals)
import HsBinds    (EvTerm(..))
import Class      ( Class, className, classTyCon )
import Type       ( tcView, mkTyConApp, mkNumberTy, isNumberTy
                  , tcEqType )
import TypeRep    (Type(..))
import TyCon      (TyCon, tyConName)
import Var        (EvVar)
import Coercion   ( mkUnsafeCoercion )
import Outputable
import PrelNames  ( lessThanEqualClassName
                  , addTyFamName, mulTyFamName, expTyFamName
                  )
import Bag        (bagToList, emptyBag, unionBags)
import Data.Maybe (fromMaybe)
import Control.Monad (msum, mplus, zipWithM, (<=<))



data Term = Var Xi | Num Integer (Maybe Xi)
data Op   = Add | Mul | Exp deriving Eq
data Prop = EqFun Op Term Term Term
          | Leq Term Term
          | Eq Term Term

commutative :: Op -> Bool
commutative op = op == Add || op == Mul

num :: Integer -> Term
num n = Num n Nothing

instance Eq Term where
  Var x   == Var y    = tcEqType x y
  Num x _ == Num y _  = x == y
  _       == _        = False


--------------------------------------------------------------------------------
-- Interface with the type checker
--------------------------------------------------------------------------------


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


solveNumWanted :: CanonicalCts -> CanonicalCts -> CanonicalCt ->
                TcS (Maybe CanonicalCt, CanonicalCts)
solveNumWanted given wanted prop =
  do let asmps = map toProp $ bagToList $ unionBags given wanted
         goal  = toProp prop

     numTrace "solveNumWanted" (vmany asmps <+> text "|-" <+> ppr goal)

     case solve asmps goal of

       Simplified sgs ->
         do numTrace "Simplified to" (vmany sgs)
            defineDummy (cc_id prop) =<< fromProp goal
            evs   <- mapM (newSubGoal <=< fromProp) sgs
            goals <- mkCanonicals (cc_flavor prop) evs
            return (Nothing, goals)

       -- XXX: The new wanted might imply some of the existing wanteds...
       Improved is ->
         do numTrace "Improved by" (vmany is)
            evs   <- mapM (newSubGoal <=< fromProp) is
            goals <- mkCanonicals (cc_flavor prop) evs
            return (Just prop, goals)

       -- XXX: Report an error
       Impossible ->
         do numTrace "Impssoble" empty
            return (Just prop, emptyBag)


addNumGiven :: CanonicalCts -> CanonicalCts -> CanonicalCt ->
              TcS (Maybe CanonicalCt, CanonicalCts, CanonicalCts)
addNumGiven given wanted prop =
  do let asmps = map toProp $ bagToList $ unionBags given wanted
         goal  = toProp prop

     numTrace "addNumGiven" (vmany asmps <+> text " /\\ " <+> ppr goal)
     case solve asmps goal of

       Simplified sgs ->
         do numTrace "Simplified to" (vmany sgs)
            evs   <- mapM (newFact <=< fromProp) sgs
            facts <- mkCanonicals (cc_flavor prop) evs
            return (Nothing, unionBags given wanted, facts)

       Improved is ->
         do numTrace "Improved by" (vmany is)
            evs <- mapM (newFact <=< fromProp) is
            facts <- mkCanonicals (cc_flavor prop) evs
            return (Just prop, given, unionBags wanted facts)

       -- XXX: report error
       Impossible ->
         do numTrace "Impossible" empty
            return (Just prop, unionBags given wanted, emptyBag)



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
newSubGoal (CvtCo t1 t2)   = newWantedCoVar t1 t2

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
  setWantedCoBind c $ mkUnsafeCoercion t1 t2




--------------------------------------------------------------------------------
-- The Solver
--------------------------------------------------------------------------------


data Result = Impossible            -- We know that the goal cannot be solved.
            | Simplified [Prop]     -- We reformulated the goal.
            | Improved   [Prop]     -- We learned some new facts.

solve :: [Prop] -> Prop -> Result
solve asmps prop =
  case solveWanted1 prop of
    Just sgs -> Simplified sgs
    Nothing ->
      case msum $ zipWith solveWanted2 asmps (repeat prop) of
        Just sgs -> Simplified sgs
        Nothing  ->
          case improve1 prop of
            Nothing   -> Impossible
            Just eqs  ->
              case zipWithM improve2 asmps (repeat prop) of
                Nothing   -> Impossible
                Just eqs1 -> Improved (concat (eqs : eqs1))




improve1 :: Prop -> Maybe [Prop]
improve1 prop =
  case prop of

    Leq s t@(Num 0 _)                -> Just [ Eq s t ]


    EqFun Add (Num m _) (Num n _) t         -> Just [ Eq (num (m+n)) t ]
    EqFun Add (Num 0 _) s         t         -> Just [ Eq s t ]
    EqFun Add (Num m _) s         (Num n _)
      | m <= n                              -> Just [ Eq (num (n-m)) s ]
      | otherwise                           -> Nothing
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

    Leq (Num m _) (Num n _) | m <= n      -> Just []
    Leq (Num 0 _) _                       -> Just []
    Leq s t                 | s == t      -> Just []

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

    (Leq s1 t1, Leq s2 t2)
      | s1 == s2 && t1 == t2    -> Just []

    (Leq (Num m _) t1, Leq (Num n _) t2)
      | t1 == t2 && m >= n      -> Just []

    (Leq s1 (Num m _), Leq s2 (Num n _))
      | s1 == s2 && m <= n      -> Just []

    (EqFun Add r1 s1 t1, Leq r2 t2)
      | t1 == t2 && r1 == r2    -> Just []
      | t1 == t2 && s1 == r2    -> Just []

    -- XXX: others, transitivity

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

vmany :: Outputable a => [a] -> SDoc
vmany xs = braces $ hcat $ punctuate comma $ map ppr xs

instance Outputable Term where
  ppr (Var xi)  = ppr xi
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

