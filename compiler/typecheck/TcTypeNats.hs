module TcTypeNats
  ( typeNatTyCons
  , typeNatCoAxiomRules
  , TcBuiltInSynFamily(..)
  ) where

import Type
import Pair
import TcType     ( TcType )
import TyCon      ( TyCon, SynTyConRhs(..), mkSynTyCon, TyConParent(..)  )
import Coercion   ( Role(..) )
import TcRnTypes  ( Xi )
import TcEvidence ( mkTcAxiomRuleCo, TcCoercion )
import CoAxiom    ( CoAxiomRule(..) )
import Name       ( Name, BuiltInSyntax(..) )
import TysWiredIn ( typeNatKind, mkWiredInTyConName
                  , promotedBoolTyCon
                  , promotedFalseDataCon, promotedTrueDataCon
                  )
import TysPrim    ( tyVarList, mkArrowKinds )
import PrelNames  ( gHC_TYPELITS
                  , typeNatAddTyFamNameKey
                  , typeNatMulTyFamNameKey
                  , typeNatExpTyFamNameKey
                  , typeNatLeqTyFamNameKey
                  )
import FamInst    ( TcBuiltInSynFamily(..) )
import FastString ( FastString, fsLit )
import qualified Data.Map as Map
import Data.Maybe ( isJust )

{-------------------------------------------------------------------------------
Built-in type constructors for functions on type-lelve nats
-}

typeNatTyCons :: [TyCon]
typeNatTyCons =
  [ typeNatAddTyCon
  , typeNatMulTyCon
  , typeNatExpTyCon
  , typeNatLeqTyCon
  ]

typeNatAddTyCon :: TyCon
typeNatAddTyCon = mkTypeNatFunTyCon2 name
  TcBuiltInSynFamily
    { sfMatchFam      = matchFamAdd
    , sfInteractTop   = interactTopAdd
    , sfInteractInert = interactInertAdd
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "+")
            typeNatAddTyFamNameKey typeNatAddTyCon

typeNatMulTyCon :: TyCon
typeNatMulTyCon = mkTypeNatFunTyCon2 name
  TcBuiltInSynFamily
    { sfMatchFam      = matchFamMul
    , sfInteractTop   = interactTopMul
    , sfInteractInert = interactInertMul
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "*")
            typeNatMulTyFamNameKey typeNatMulTyCon

typeNatExpTyCon :: TyCon
typeNatExpTyCon = mkTypeNatFunTyCon2 name
  TcBuiltInSynFamily
    { sfMatchFam      = matchFamExp
    , sfInteractTop   = interactTopExp
    , sfInteractInert = interactInertExp
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "^")
                typeNatExpTyFamNameKey typeNatExpTyCon

typeNatLeqTyCon :: TyCon
typeNatLeqTyCon =
  mkSynTyCon name
    (mkArrowKinds [ typeNatKind, typeNatKind ] boolKind)
    (take 2 $ tyVarList typeNatKind)
    [Nominal,Nominal]
    (BuiltInSynFamTyCon ops)
    NoParentTyCon

  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "<=?")
                typeNatLeqTyFamNameKey typeNatLeqTyCon
  ops = TcBuiltInSynFamily
    { sfMatchFam      = matchFamLeq
    , sfInteractTop   = interactTopLeq
    , sfInteractInert = interactInertLeq
    }


-- Make a binary built-in constructor of kind: Nat -> Nat -> Nat
mkTypeNatFunTyCon2 :: Name -> TcBuiltInSynFamily -> TyCon
mkTypeNatFunTyCon2 op tcb =
  mkSynTyCon op
    (mkArrowKinds [ typeNatKind, typeNatKind ] typeNatKind)
    (take 2 $ tyVarList typeNatKind)
    [Nominal,Nominal]
    (BuiltInSynFamTyCon tcb)
    NoParentTyCon




{-------------------------------------------------------------------------------
Built-in rules axioms
-------------------------------------------------------------------------------}

-- If you add additional rules, please remember to add them to
-- `typeNatCoAxiomRules` also.
axAddDef
  , axMulDef
  , axExpDef
  , axLeqDef
  , axAdd0L
  , axAdd0R
  , axMul0L
  , axMul0R
  , axMul1L
  , axMul1R
  , axExp1L
  , axExp0R
  , axExp1R
  , axLeqRefl
  , axLeq0L
  :: CoAxiomRule

axAddDef = mkBinAxiom "AddDef" typeNatAddTyCon $
              \x y -> num (x + y)

axMulDef = mkBinAxiom "MulDef" typeNatMulTyCon $ 
              \x y -> num (x * y)

axExpDef = mkBinAxiom "ExpDef" typeNatExpTyCon $
              \x y -> num (x ^ y)

axLeqDef = mkBinAxiom "LeqDef" typeNatLeqTyCon $
              \x y -> bool (x <= y)

axAdd0L     = mkAxiom1 "Add0L"    $ \t -> (num 0 .+. t) === t
axAdd0R     = mkAxiom1 "Add0R"    $ \t -> (t .+. num 0) === t
axMul0L     = mkAxiom1 "Mul0L"    $ \t -> (num 0 .*. t) === num 0
axMul0R     = mkAxiom1 "Mul0R"    $ \t -> (t .*. num 0) === num 0
axMul1L     = mkAxiom1 "Mul1L"    $ \t -> (num 1 .*. t) === t
axMul1R     = mkAxiom1 "Mul1R"    $ \t -> (t .*. num 1) === t
axExp1L     = mkAxiom1 "Exp1L"    $ \t -> (num 1 .^. t) === num 1
axExp0R     = mkAxiom1 "Exp0R"    $ \t -> (t .^. num 0) === num 1
axExp1R     = mkAxiom1 "Exp1R"    $ \t -> (t .^. num 1) === t
axLeqRefl   = mkAxiom1 "LeqRefl"  $ \t -> (t <== t) === bool True
axLeq0L     = mkAxiom1 "Leq0L"    $ \t -> (num 0 <== t) === bool True

typeNatCoAxiomRules :: Map.Map FastString CoAxiomRule
typeNatCoAxiomRules = Map.fromList $ map (\x -> (coaxrName x, x))
  [ axAddDef
  , axMulDef
  , axExpDef
  , axLeqDef
  , axAdd0L
  , axAdd0R
  , axMul0L
  , axMul0R
  , axMul1L
  , axMul1R
  , axExp1L
  , axExp0R
  , axExp1R
  , axLeqRefl
  , axLeq0L
  ]



{-------------------------------------------------------------------------------
Various utilities for making axioms and types
-------------------------------------------------------------------------------}

(.+.) :: Type -> Type -> Type
s .+. t = mkTyConApp typeNatAddTyCon [s,t]

(.*.) :: Type -> Type -> Type
s .*. t = mkTyConApp typeNatMulTyCon [s,t]

(.^.) :: Type -> Type -> Type
s .^. t = mkTyConApp typeNatExpTyCon [s,t]

(<==) :: Type -> Type -> Type
s <== t = mkTyConApp typeNatLeqTyCon [s,t]

(===) :: Type -> Type -> Pair Type
x === y = Pair x y

num :: Integer -> Type
num = mkNumLitTy

boolKind :: Kind
boolKind = mkTyConApp promotedBoolTyCon []

bool :: Bool -> Type
bool b = if b then mkTyConApp promotedTrueDataCon []
              else mkTyConApp promotedFalseDataCon []

isBoolLitTy :: Type -> Maybe Bool
isBoolLitTy tc =
  do (tc,[]) <- splitTyConApp_maybe tc
     case () of
       _ | tc == promotedFalseDataCon -> return False
         | tc == promotedTrueDataCon  -> return True
         | otherwise                   -> Nothing

known :: (Integer -> Bool) -> TcType -> Bool
known p x = case isNumLitTy x of
              Just a  -> p a
              Nothing -> False




-- For the definitional axioms
mkBinAxiom :: String -> TyCon ->
              (Integer -> Integer -> Type) -> CoAxiomRule
mkBinAxiom str tc f =
  CoAxiomRule
    { coaxrName      = fsLit str
    , coaxrTypeArity = 2
    , coaxrAsmpRoles = []
    , coaxrRole      = Nominal
    , coaxrProves    = \ts cs ->
        case (ts,cs) of
          ([s,t],[]) -> do x <- isNumLitTy s
                           y <- isNumLitTy t
                           return (mkTyConApp tc [s,t] === f x y)
          _ -> Nothing
    }

mkAxiom1 :: String -> (Type -> Pair Type) -> CoAxiomRule
mkAxiom1 str f =
  CoAxiomRule
    { coaxrName      = fsLit str
    , coaxrTypeArity = 1
    , coaxrAsmpRoles = []
    , coaxrRole      = Nominal
    , coaxrProves    = \ts cs ->
        case (ts,cs) of
          ([s],[]) -> return (f s)
          _        -> Nothing
    }


{-------------------------------------------------------------------------------
Evaluation
-------------------------------------------------------------------------------}

matchFamAdd :: [Type] -> Maybe (TcCoercion, TcType)
matchFamAdd [s,t]
  | Just 0 <- mbX = Just (mkTcAxiomRuleCo axAdd0L [t] [], t)
  | Just 0 <- mbY = Just (mkTcAxiomRuleCo axAdd0R [s] [], s)
  | Just x <- mbX, Just y <- mbY =
    Just (mkTcAxiomRuleCo axAddDef [s,t] [], num (x + y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamAdd _ = Nothing

matchFamMul :: [Xi] -> Maybe (TcCoercion, Xi)
matchFamMul [s,t]
  | Just 0 <- mbX = Just (mkTcAxiomRuleCo axMul0L [t] [], num 0)
  | Just 0 <- mbY = Just (mkTcAxiomRuleCo axMul0R [s] [], num 0)
  | Just 1 <- mbX = Just (mkTcAxiomRuleCo axMul1L [t] [], t)
  | Just 1 <- mbY = Just (mkTcAxiomRuleCo axMul1R [s] [], s)
  | Just x <- mbX, Just y <- mbY =
    Just (mkTcAxiomRuleCo axMulDef [s,t] [], num (x * y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamMul _ = Nothing

matchFamExp :: [Xi] -> Maybe (TcCoercion, Xi)
matchFamExp [s,t]
  | Just 0 <- mbY = Just (mkTcAxiomRuleCo axExp0R [s] [], num 1)
  | Just 1 <- mbX = Just (mkTcAxiomRuleCo axExp1L [t] [], num 1)
  | Just 1 <- mbY = Just (mkTcAxiomRuleCo axExp1R [s] [], s)
  | Just x <- mbX, Just y <- mbY =
    Just (mkTcAxiomRuleCo axExpDef [s,t] [], num (x ^ y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamExp _ = Nothing

matchFamLeq :: [Xi] -> Maybe (TcCoercion, Xi)
matchFamLeq [s,t]
  | Just 0 <- mbX = Just (mkTcAxiomRuleCo axLeq0L [t] [], bool True)
  | Just x <- mbX, Just y <- mbY =
    Just (mkTcAxiomRuleCo axLeqDef [s,t] [], bool (x <= y))
  | eqType s t  = Just (mkTcAxiomRuleCo axLeqRefl [s] [], bool True)
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamLeq _ = Nothing

{-------------------------------------------------------------------------------
Interact with axioms
-------------------------------------------------------------------------------}

interactTopAdd :: [Xi] -> Xi -> [Pair Type]
interactTopAdd [s,t] r
  | Just 0 <- mbZ = [ s === num 0, t === num 0 ]
  | Just x <- mbX, Just z <- mbZ, Just y <- minus z x = [t === num y]
  | Just y <- mbY, Just z <- mbZ, Just x <- minus z y = [s === num x]
  where
  mbX = isNumLitTy s
  mbY = isNumLitTy t
  mbZ = isNumLitTy r
interactTopAdd _ _ = []

interactTopMul :: [Xi] -> Xi -> [Pair Type]
interactTopMul [s,t] r
  | Just 1 <- mbZ = [ s === num 1, t === num 1 ]
  | Just x <- mbX, Just z <- mbZ, Just y <- divide z x = [t === num y]
  | Just y <- mbY, Just z <- mbZ, Just x <- divide z y = [s === num x]
  where
  mbX = isNumLitTy s
  mbY = isNumLitTy t
  mbZ = isNumLitTy r
interactTopMul _ _ = []

interactTopExp :: [Xi] -> Xi -> [Pair Type]
interactTopExp [s,t] r
  | Just 0 <- mbZ = [ s === num 0 ]
  | Just x <- mbX, Just z <- mbZ, Just y <- logExact  z x = [t === num y]
  | Just y <- mbY, Just z <- mbZ, Just x <- rootExact z y = [s === num x]
  where
  mbX = isNumLitTy s
  mbY = isNumLitTy t
  mbZ = isNumLitTy r
interactTopExp _ _ = []

interactTopLeq :: [Xi] -> Xi -> [Pair Type]
interactTopLeq [s,t] r
  | Just 0 <- mbY, Just True <- mbZ = [ s === num 0 ]
  where
  mbY = isNumLitTy t
  mbZ = isBoolLitTy r
interactTopLeq _ _ = []



{-------------------------------------------------------------------------------
Interacton with inerts
-------------------------------------------------------------------------------}

interactInertAdd :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertAdd [x1,y1] z1 [x2,y2] z2
  | sameZ && eqType x1 x2         = [ y1 === y2 ]
  | sameZ && eqType y1 y2         = [ x1 === x2 ]
  where sameZ = eqType z1 z2
interactInertAdd _ _ _ _ = []

interactInertMul :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertMul [x1,y1] z1 [x2,y2] z2
  | sameZ && known (/= 0) x1 && eqType x1 x2 = [ y1 === y2 ]
  | sameZ && known (/= 0) y1 && eqType y1 y2 = [ x1 === x2 ]
  where sameZ   = eqType z1 z2

interactInertMul _ _ _ _ = []

interactInertExp :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertExp [x1,y1] z1 [x2,y2] z2
  | sameZ && known (> 1) x1 && eqType x1 x2 = [ y1 === y2 ]
  | sameZ && known (> 0) y1 && eqType y1 y2 = [ x1 === x2 ]
  where sameZ = eqType z1 z2

interactInertExp _ _ _ _ = []


interactInertLeq :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertLeq [x1,y1] z1 [x2,y2] z2
  | bothTrue && eqType x1 y2 && eqType y1 x2 = [ x1 === y1 ]
  | bothTrue && eqType y1 x2                 = [ (x1 <== y2) === bool True ]
  | bothTrue && eqType y2 x1                 = [ (x2 <== y1) === bool True ]
  where bothTrue = isJust $ do True <- isBoolLitTy z1
                               True <- isBoolLitTy z2
                               return ()

interactInertLeq _ _ _ _ = []




{- -----------------------------------------------------------------------------
These inverse functions are used for simplifying propositions using
concrete natural numbers.
----------------------------------------------------------------------------- -}

-- | Subtract two natural numbers.
minus :: Integer -> Integer -> Maybe Integer
minus x y = if x >= y then Just (x - y) else Nothing

-- | Compute the exact logarithm of a natural number.
-- The logarithm base is the second argument.
logExact :: Integer -> Integer -> Maybe Integer
logExact x y = do (z,True) <- genLog x y
                  return z


-- | Divide two natural numbers.
divide :: Integer -> Integer -> Maybe Integer
divide _ 0  = Nothing
divide x y  = case divMod x y of
                (a,0) -> Just a
                _     -> Nothing

-- | Compute the exact root of a natural number.
-- The second argument specifies which root we are computing.
rootExact :: Integer -> Integer -> Maybe Integer
rootExact x y = do (z,True) <- genRoot x y
                   return z



{- | Compute the the n-th root of a natural number, rounded down to
the closest natural number.  The boolean indicates if the result
is exact (i.e., True means no rounding was done, False means rounded down).
The second argument specifies which root we are computing. -}
genRoot :: Integer -> Integer -> Maybe (Integer, Bool)
genRoot _  0    = Nothing
genRoot x0 1    = Just (x0, True)
genRoot x0 root = Just (search 0 (x0+1))
  where
  search from to = let x = from + div (to - from) 2
                       a = x ^ root
                   in case compare a x0 of
                        EQ              -> (x, True)
                        LT | x /= from  -> search x to
                           | otherwise  -> (from, False)
                        GT | x /= to    -> search from x
                           | otherwise  -> (from, False)

{- | Compute the logarithm of a number in the given base, rounded down to the
closest integer.  The boolean indicates if we the result is exact
(i.e., True means no rounding happened, False means we rounded down).
The logarithm base is the second argument. -}
genLog :: Integer -> Integer -> Maybe (Integer, Bool)
genLog x 0    = if x == 1 then Just (0, True) else Nothing
genLog _ 1    = Nothing
genLog 0 _    = Nothing
genLog x base = Just (exactLoop 0 x)
  where
  exactLoop s i
    | i == 1     = (s,True)
    | i < base   = (s,False)
    | otherwise  =
        let s1 = s + 1
        in s1 `seq` case divMod i base of
                      (j,r)
                        | r == 0    -> exactLoop s1 j
                        | otherwise -> (underLoop s1 j, False)

  underLoop s i
    | i < base  = s
    | otherwise = let s1 = s + 1 in s1 `seq` underLoop s1 (div i base)







