module TcTypeNats
  ( typeNatTyCons
  , typeNatCoAxiomRules
  , BuiltInSynFamily(..)
  , ExternalSolver(..), ExtSolRes(..), newExternalSolver
  ) where

import Type
import Pair
import TcType     ( TcType, tcEqType )
import TyCon      ( TyCon, SynTyConRhs(..), mkSynTyCon, TyConParent(..)  )
import Coercion   ( Role(..) )
import TcRnTypes  ( Xi, Ct(..) )
import CoAxiom    ( CoAxiomRule(..), BuiltInSynFamily(..) )
import Name       ( Name, BuiltInSyntax(..), nameOccName, nameUnique )
import OccName    ( occNameString )
import Var        ( tyVarName, tyVarKind )
import TysWiredIn ( typeNatKind, typeSymbolKind, typeNatKindCon
                  , mkWiredInTyConName
                  , promotedBoolTyCon
                  , promotedFalseDataCon, promotedTrueDataCon
                  , promotedOrderingTyCon
                  , promotedLTDataCon
                  , promotedEQDataCon
                  , promotedGTDataCon
                  )
import TysPrim    ( tyVarList, mkArrowKinds )
import PrelNames  ( gHC_TYPELITS
                  , typeNatAddTyFamNameKey
                  , typeNatMulTyFamNameKey
                  , typeNatExpTyFamNameKey
                  , typeNatLeqTyFamNameKey
                  , typeNatSubTyFamNameKey
                  , typeNatCmpTyFamNameKey
                  , typeSymbolCmpTyFamNameKey
                  )
import Panic      (panic)
import FastString ( FastString, fsLit )
import UniqFM     ( UniqFM,  emptyUFM, unitUFM, plusUFM, eltsUFM )
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Maybe ( isJust )
import           Data.Char  ( isSpace )
import           Data.List ( unfoldr, mapAccumL, foldl' )
import           Data.IORef ( newIORef, atomicModifyIORef', modifyIORef' )
import           Control.Monad (forever)
import           Control.Concurrent ( forkIO )
import qualified Control.Exception as X
import           System.Process ( runInteractiveProcess, waitForProcess )
import           System.IO ( hPutStrLn, hFlush, hGetContents, hGetLine )

{-------------------------------------------------------------------------------
Built-in type constructors for functions on type-lelve nats
-}

typeNatTyCons :: [TyCon]
typeNatTyCons =
  [ typeNatAddTyCon
  , typeNatMulTyCon
  , typeNatExpTyCon
  , typeNatLeqTyCon
  , typeNatSubTyCon
  , typeNatCmpTyCon
  , typeSymbolCmpTyCon
  ]

typeNatAddTyCon :: TyCon
typeNatAddTyCon = mkTypeNatFunTyCon2 name
  BuiltInSynFamily
    { sfMatchFam      = matchFamAdd
    , sfInteractTop   = interactTopAdd
    , sfInteractInert = interactInertAdd
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "+")
            typeNatAddTyFamNameKey typeNatAddTyCon

typeNatSubTyCon :: TyCon
typeNatSubTyCon = mkTypeNatFunTyCon2 name
  BuiltInSynFamily
    { sfMatchFam      = matchFamSub
    , sfInteractTop   = interactTopSub
    , sfInteractInert = interactInertSub
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "-")
            typeNatSubTyFamNameKey typeNatSubTyCon

typeNatMulTyCon :: TyCon
typeNatMulTyCon = mkTypeNatFunTyCon2 name
  BuiltInSynFamily
    { sfMatchFam      = matchFamMul
    , sfInteractTop   = interactTopMul
    , sfInteractInert = interactInertMul
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "*")
            typeNatMulTyFamNameKey typeNatMulTyCon

typeNatExpTyCon :: TyCon
typeNatExpTyCon = mkTypeNatFunTyCon2 name
  BuiltInSynFamily
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
  ops = BuiltInSynFamily
    { sfMatchFam      = matchFamLeq
    , sfInteractTop   = interactTopLeq
    , sfInteractInert = interactInertLeq
    }

typeNatCmpTyCon :: TyCon
typeNatCmpTyCon =
  mkSynTyCon name
    (mkArrowKinds [ typeNatKind, typeNatKind ] orderingKind)
    (take 2 $ tyVarList typeNatKind)
    [Nominal,Nominal]
    (BuiltInSynFamTyCon ops)
    NoParentTyCon

  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "CmpNat")
                typeNatCmpTyFamNameKey typeNatCmpTyCon
  ops = BuiltInSynFamily
    { sfMatchFam      = matchFamCmpNat
    , sfInteractTop   = interactTopCmpNat
    , sfInteractInert = \_ _ _ _ -> []
    }

typeSymbolCmpTyCon :: TyCon
typeSymbolCmpTyCon =
  mkSynTyCon name
    (mkArrowKinds [ typeSymbolKind, typeSymbolKind ] orderingKind)
    (take 2 $ tyVarList typeSymbolKind)
    [Nominal,Nominal]
    (BuiltInSynFamTyCon ops)
    NoParentTyCon

  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "CmpSymbol")
                typeSymbolCmpTyFamNameKey typeSymbolCmpTyCon
  ops = BuiltInSynFamily
    { sfMatchFam      = matchFamCmpSymbol
    , sfInteractTop   = interactTopCmpSymbol
    , sfInteractInert = \_ _ _ _ -> []
    }





-- Make a binary built-in constructor of kind: Nat -> Nat -> Nat
mkTypeNatFunTyCon2 :: Name -> BuiltInSynFamily -> TyCon
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
  , axCmpNatDef
  , axCmpSymbolDef
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
  , axCmpNatRefl
  , axCmpSymbolRefl
  , axLeq0L
  , axSubDef
  , axSub0R
  :: CoAxiomRule

axAddDef = mkBinAxiom "AddDef" typeNatAddTyCon $
              \x y -> Just $ num (x + y)

axMulDef = mkBinAxiom "MulDef" typeNatMulTyCon $
              \x y -> Just $ num (x * y)

axExpDef = mkBinAxiom "ExpDef" typeNatExpTyCon $
              \x y -> Just $ num (x ^ y)

axLeqDef = mkBinAxiom "LeqDef" typeNatLeqTyCon $
              \x y -> Just $ bool (x <= y)

axCmpNatDef   = mkBinAxiom "CmpNatDef" typeNatCmpTyCon
              $ \x y -> Just $ ordering (compare x y)

axCmpSymbolDef =
  CoAxiomRule
    { coaxrName      = fsLit "CmpSymbolDef"
    , coaxrTypeArity = 2
    , coaxrAsmpRoles = []
    , coaxrRole      = Nominal
    , coaxrProves    = \ts cs ->
        case (ts,cs) of
          ([s,t],[]) ->
            do x <- isStrLitTy s
               y <- isStrLitTy t
               return (mkTyConApp typeSymbolCmpTyCon [s,t] ===
                      ordering (compare x y))
          _ -> Nothing
    }

axSubDef = mkBinAxiom "SubDef" typeNatSubTyCon $
              \x y -> fmap num (minus x y)

axAdd0L     = mkAxiom1 "Add0L"    $ \t -> (num 0 .+. t) === t
axAdd0R     = mkAxiom1 "Add0R"    $ \t -> (t .+. num 0) === t
axSub0R     = mkAxiom1 "Sub0R"    $ \t -> (t .-. num 0) === t
axMul0L     = mkAxiom1 "Mul0L"    $ \t -> (num 0 .*. t) === num 0
axMul0R     = mkAxiom1 "Mul0R"    $ \t -> (t .*. num 0) === num 0
axMul1L     = mkAxiom1 "Mul1L"    $ \t -> (num 1 .*. t) === t
axMul1R     = mkAxiom1 "Mul1R"    $ \t -> (t .*. num 1) === t
axExp1L     = mkAxiom1 "Exp1L"    $ \t -> (num 1 .^. t) === num 1
axExp0R     = mkAxiom1 "Exp0R"    $ \t -> (t .^. num 0) === num 1
axExp1R     = mkAxiom1 "Exp1R"    $ \t -> (t .^. num 1) === t
axLeqRefl   = mkAxiom1 "LeqRefl"  $ \t -> (t <== t) === bool True
axCmpNatRefl    = mkAxiom1 "CmpNatRefl"
                $ \t -> (cmpNat t t) === ordering EQ
axCmpSymbolRefl = mkAxiom1 "CmpSymbolRefl"
                $ \t -> (cmpSymbol t t) === ordering EQ
axLeq0L     = mkAxiom1 "Leq0L"    $ \t -> (num 0 <== t) === bool True

typeNatCoAxiomRules :: Map.Map FastString CoAxiomRule
typeNatCoAxiomRules = Map.fromList $ map (\x -> (coaxrName x, x))
  [ axAddDef
  , axMulDef
  , axExpDef
  , axLeqDef
  , axCmpNatDef
  , axCmpSymbolDef
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
  , axCmpNatRefl
  , axCmpSymbolRefl
  , axLeq0L
  , axSubDef
  ]



{-------------------------------------------------------------------------------
Various utilities for making axioms and types
-------------------------------------------------------------------------------}

(.+.) :: Type -> Type -> Type
s .+. t = mkTyConApp typeNatAddTyCon [s,t]

(.-.) :: Type -> Type -> Type
s .-. t = mkTyConApp typeNatSubTyCon [s,t]

(.*.) :: Type -> Type -> Type
s .*. t = mkTyConApp typeNatMulTyCon [s,t]

(.^.) :: Type -> Type -> Type
s .^. t = mkTyConApp typeNatExpTyCon [s,t]

(<==) :: Type -> Type -> Type
s <== t = mkTyConApp typeNatLeqTyCon [s,t]

cmpNat :: Type -> Type -> Type
cmpNat s t = mkTyConApp typeNatCmpTyCon [s,t]

cmpSymbol :: Type -> Type -> Type
cmpSymbol s t = mkTyConApp typeSymbolCmpTyCon [s,t]

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

orderingKind :: Kind
orderingKind = mkTyConApp promotedOrderingTyCon []

ordering :: Ordering -> Type
ordering o =
  case o of
    LT -> mkTyConApp promotedLTDataCon []
    EQ -> mkTyConApp promotedEQDataCon []
    GT -> mkTyConApp promotedGTDataCon []

isOrderingLitTy :: Type -> Maybe Ordering
isOrderingLitTy tc =
  do (tc1,[]) <- splitTyConApp_maybe tc
     case () of
       _ | tc1 == promotedLTDataCon -> return LT
         | tc1 == promotedEQDataCon -> return EQ
         | tc1 == promotedGTDataCon -> return GT
         | otherwise                -> Nothing

known :: (Integer -> Bool) -> TcType -> Bool
known p x = case isNumLitTy x of
              Just a  -> p a
              Nothing -> False




-- For the definitional axioms
mkBinAxiom :: String -> TyCon ->
              (Integer -> Integer -> Maybe Type) -> CoAxiomRule
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
                           z <- f x y
                           return (mkTyConApp tc [s,t] === z)
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

matchFamAdd :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamAdd [s,t]
  | Just 0 <- mbX = Just (axAdd0L, [t], t)
  | Just 0 <- mbY = Just (axAdd0R, [s], s)
  | Just x <- mbX, Just y <- mbY =
    Just (axAddDef, [s,t], num (x + y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamAdd _ = Nothing

matchFamSub :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamSub [s,t]
  | Just 0 <- mbY = Just (axSub0R, [s], s)
  | Just x <- mbX, Just y <- mbY, Just z <- minus x y =
    Just (axSubDef, [s,t], num z)
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamSub _ = Nothing

matchFamMul :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamMul [s,t]
  | Just 0 <- mbX = Just (axMul0L, [t], num 0)
  | Just 0 <- mbY = Just (axMul0R, [s], num 0)
  | Just 1 <- mbX = Just (axMul1L, [t], t)
  | Just 1 <- mbY = Just (axMul1R, [s], s)
  | Just x <- mbX, Just y <- mbY =
    Just (axMulDef, [s,t], num (x * y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamMul _ = Nothing

matchFamExp :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamExp [s,t]
  | Just 0 <- mbY = Just (axExp0R, [s], num 1)
  | Just 1 <- mbX = Just (axExp1L, [t], num 1)
  | Just 1 <- mbY = Just (axExp1R, [s], s)
  | Just x <- mbX, Just y <- mbY =
    Just (axExpDef, [s,t], num (x ^ y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamExp _ = Nothing

matchFamLeq :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamLeq [s,t]
  | Just 0 <- mbX = Just (axLeq0L, [t], bool True)
  | Just x <- mbX, Just y <- mbY =
    Just (axLeqDef, [s,t], bool (x <= y))
  | tcEqType s t  = Just (axLeqRefl, [s], bool True)
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamLeq _ = Nothing

matchFamCmpNat :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamCmpNat [s,t]
  | Just x <- mbX, Just y <- mbY =
    Just (axCmpNatDef, [s,t], ordering (compare x y))
  | tcEqType s t = Just (axCmpNatRefl, [s], ordering EQ)
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamCmpNat _ = Nothing

matchFamCmpSymbol :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
matchFamCmpSymbol [s,t]
  | Just x <- mbX, Just y <- mbY =
    Just (axCmpSymbolDef, [s,t], ordering (compare x y))
  | tcEqType s t = Just (axCmpSymbolRefl, [s], ordering EQ)
  where mbX = isStrLitTy s
        mbY = isStrLitTy t
matchFamCmpSymbol _ = Nothing


{-------------------------------------------------------------------------------
Interact with axioms
-------------------------------------------------------------------------------}

interactTopAdd :: [Xi] -> Xi -> [Pair Type]
interactTopAdd [s,t] r
  | Just 0 <- mbZ = [ s === num 0, t === num 0 ]                          -- (s + t ~ 0) => (s ~ 0, t ~ 0)
  | Just x <- mbX, Just z <- mbZ, Just y <- minus z x = [t === num y]     -- (5 + t ~ 8) => (t ~ 3)
  | Just y <- mbY, Just z <- mbZ, Just x <- minus z y = [s === num x]     -- (s + 5 ~ 8) => (s ~ 3)
  where
  mbX = isNumLitTy s
  mbY = isNumLitTy t
  mbZ = isNumLitTy r
interactTopAdd _ _ = []

{-
Note [Weakened interaction rule for subtraction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A simpler interaction here might be:

  `s - t ~ r` --> `t + r ~ s`

This would enable us to reuse all the code for addition.
Unfortunately, this works a little too well at the moment.
Consider the following example:

    0 - 5 ~ r --> 5 + r ~ 0 --> (5 = 0, r = 0)

This (correctly) spots that the constraint cannot be solved.

However, this may be a problem if the constraint did not
need to be solved in the first place!  Consider the following example:

f :: Proxy (If (5 <=? 0) (0 - 5) (5 - 0)) -> Proxy 5
f = id

Currently, GHC is strict while evaluating functions, so this does not
work, because even though the `If` should evaluate to `5 - 0`, we
also evaluate the "then" branch which generates the constraint `0 - 5 ~ r`,
which fails.

So, for the time being, we only add an improvement when the RHS is a constant,
which happens to work OK for the moment, although clearly we need to do
something more general.
-}
interactTopSub :: [Xi] -> Xi -> [Pair Type]
interactTopSub [s,t] r
  | Just z <- mbZ = [ s === (num z .+. t) ]         -- (s - t ~ 5) => (5 + t ~ s)
  where
  mbZ = isNumLitTy r
interactTopSub _ _ = []





interactTopMul :: [Xi] -> Xi -> [Pair Type]
interactTopMul [s,t] r
  | Just 1 <- mbZ = [ s === num 1, t === num 1 ]                        -- (s * t ~ 1)  => (s ~ 1, t ~ 1)
  | Just x <- mbX, Just z <- mbZ, Just y <- divide z x = [t === num y]  -- (3 * t ~ 15) => (t ~ 5)
  | Just y <- mbY, Just z <- mbZ, Just x <- divide z y = [s === num x]  -- (s * 3 ~ 15) => (s ~ 5)
  where
  mbX = isNumLitTy s
  mbY = isNumLitTy t
  mbZ = isNumLitTy r
interactTopMul _ _ = []

interactTopExp :: [Xi] -> Xi -> [Pair Type]
interactTopExp [s,t] r
  | Just 0 <- mbZ = [ s === num 0 ]                                       -- (s ^ t ~ 0) => (s ~ 0)
  | Just x <- mbX, Just z <- mbZ, Just y <- logExact  z x = [t === num y] -- (2 ^ t ~ 8) => (t ~ 3)
  | Just y <- mbY, Just z <- mbZ, Just x <- rootExact z y = [s === num x] -- (s ^ 2 ~ 9) => (s ~ 3)
  where
  mbX = isNumLitTy s
  mbY = isNumLitTy t
  mbZ = isNumLitTy r
interactTopExp _ _ = []

interactTopLeq :: [Xi] -> Xi -> [Pair Type]
interactTopLeq [s,t] r
  | Just 0 <- mbY, Just True <- mbZ = [ s === num 0 ]                     -- (s <= 0) => (s ~ 0)
  where
  mbY = isNumLitTy t
  mbZ = isBoolLitTy r
interactTopLeq _ _ = []

interactTopCmpNat :: [Xi] -> Xi -> [Pair Type]
interactTopCmpNat [s,t] r
  | Just EQ <- isOrderingLitTy r = [ s === t ]
interactTopCmpNat _ _ = []

interactTopCmpSymbol :: [Xi] -> Xi -> [Pair Type]
interactTopCmpSymbol [s,t] r
  | Just EQ <- isOrderingLitTy r = [ s === t ]
interactTopCmpSymbol _ _ = []




{-------------------------------------------------------------------------------
Interaction with inerts
-------------------------------------------------------------------------------}

interactInertAdd :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertAdd [x1,y1] z1 [x2,y2] z2
  | sameZ && tcEqType x1 x2         = [ y1 === y2 ]
  | sameZ && tcEqType y1 y2         = [ x1 === x2 ]
  where sameZ = tcEqType z1 z2
interactInertAdd _ _ _ _ = []

interactInertSub :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertSub [x1,y1] z1 [x2,y2] z2
  | sameZ && tcEqType x1 x2         = [ y1 === y2 ]
  | sameZ && tcEqType y1 y2         = [ x1 === x2 ]
  where sameZ = tcEqType z1 z2
interactInertSub _ _ _ _ = []

interactInertMul :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertMul [x1,y1] z1 [x2,y2] z2
  | sameZ && known (/= 0) x1 && tcEqType x1 x2 = [ y1 === y2 ]
  | sameZ && known (/= 0) y1 && tcEqType y1 y2 = [ x1 === x2 ]
  where sameZ   = tcEqType z1 z2

interactInertMul _ _ _ _ = []

interactInertExp :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertExp [x1,y1] z1 [x2,y2] z2
  | sameZ && known (> 1) x1 && tcEqType x1 x2 = [ y1 === y2 ]
  | sameZ && known (> 0) y1 && tcEqType y1 y2 = [ x1 === x2 ]
  where sameZ = tcEqType z1 z2

interactInertExp _ _ _ _ = []


interactInertLeq :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertLeq [x1,y1] z1 [x2,y2] z2
  | bothTrue && tcEqType x1 y2 && tcEqType y1 x2 = [ x1 === y1 ]
  | bothTrue && tcEqType y1 x2                 = [ (x1 <== y2) === bool True ]
  | bothTrue && tcEqType y2 x1                 = [ (x2 <== y1) === bool True ]
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



--------------------------------------------------------------------------------
-- Interface

data ExternalSolver = ExternalSolver
  { extSolPush  :: IO ()
  , extSolPop   :: IO ()
  , extSolSend  :: Ct -> IO Bool
  , extSolCheck :: IO ExtSolRes
  , extSolStop  :: IO ()
  }

data ExtSolRes = ExtSolSat [(TyVar, Type)]
               | ExtSolUnsat
               | ExtSolUnknown

--------------------------------------------------------------------------------
-- Concrete implementation

newExternalSolver :: FilePath -> [String] -> IO ExternalSolver
newExternalSolver exe opts =
  do proc <- startSolverProcess exe opts
     let ackCmd c = do res <- solverDo proc c
                       case res of
                         SAtom "success" -> return ()
                         _  -> unexpected res
         simpleCmd = ackCmd . SList . map SAtom

     -- Prep the solver
     simpleCmd [ "set-option", ":print-success", "true" ]
     simpleCmd [ "set-option", ":produce-models", "true" ]
     simpleCmd [ "set-logic", "QF_LIA" ]


     viRef <- newIORef emptyVarInfo

     return ExternalSolver
       { extSolPush =
          do simpleCmd [ "push" ]
             modifyIORef' viRef startScope

       , extSolPop =
          do simpleCmd [ "pop" ]
             modifyIORef' viRef endScope

       , extSolSend = \ct ->
           case knownCt ct of
             Nothing -> return False
             Just (vs,e) ->
               do stmtss <- atomicModifyIORef' viRef $ \vi ->
                              mapAccumL smtDeclarePerhaps vi (eltsUFM vs)
                  mapM_ (mapM_ ackCmd) stmtss
                  ackCmd $ smtAssert e
                  return True

       , extSolCheck =
          do res <- solverDo proc (SList [ SAtom "check-sat" ])
             case res of
               SAtom "unsat"   -> return ExtSolUnsat
               SAtom "unknown" -> return ExtSolUnknown
               SAtom "sat" -> undefined

               _ -> unexpected res

       , extSolStop = solverStop proc
       }
  where
  unexpected e = fail $ "Unexpected response by the solver: " ++
                        (renderSExpr e "")

smtDeclarePerhaps :: VarInfo -> (TyVar, String, Ty) -> (VarInfo, [SExpr])
smtDeclarePerhaps vi (tv,x,ty) = (vi1, decls)
  where
  (vi1, status) = declareVar tv vi
  decls = case status of
            Constrained -> []
            Declared    -> map smtAssert (smtExtraConstraints x ty)
            Undeclared  -> smtDeclare x ty
                         : map smtAssert (smtExtraConstraints x ty)




--------------------------------------------------------------------------------
-- Recognizing constraints that we can work with.


data Ty     = TNat | TBool

data Op     = Add | Sub | Mul | Leq | Eq
              deriving Show

data Expr   = Op Op [Expr]
            | Not Expr
            | Num Integer
            | Bool Bool
            | Var String

type VarTypes = UniqFM (TyVar,String,Ty)

-- For debugging
instance Show Expr where
  showsPrec p v =
    case v of
      Num n -> shows n
      Var s -> showString s
      Not v -> showParen (p > 8) $ showString "!" . showsPrec 8 v
      Bool b -> shows b
      Op o [a,b] -> showParen (p > pOp) $ showsPrec pL a . showString opStr
                                        . showsPrec pR b
        where
          (pOp, pL, pR, opStr) =
            case o of
              Add -> (6, 6, 7, " + ")
              Sub -> (6, 6, 7, " - ")
              Mul -> (7, 7, 8, " * ")
              Leq -> (4, 5, 5, " <= ")
              Eq  -> (4, 5, 5, " == ")

      Op o as -> showsPrec p (o,as)


knownCt :: Ct -> Maybe (VarTypes, Expr)
knownCt ct =
  case ct of
    CTyEqCan _ x xi ->
      do (vs1,e1) <- knownVar x
         (vs2,e2) <- knownXi xi
         return (plusUFM vs1 vs2, Op Eq [e1,e2])
    CFunEqCan _ f args rhs ->
      do (vs1,e1) <- knownTerm f args
         (vs2,e2) <- knownXi rhs
         return (plusUFM vs1 vs2, Op Eq [e1,e2])
    _ -> Nothing

knownTerm :: TyCon -> [Xi] -> Maybe (VarTypes, Expr)
knownTerm tc xis =
  do op <- knownTC tc
     as <- mapM knownXi xis
     -- XXX: Check for linearity here?
     let (varMaps,es) = unzip as
     return (foldl' plusUFM emptyUFM varMaps, Op op es)

knownTC :: TyCon -> Maybe Op
knownTC tc
  | tc == typeNatAddTyCon = Just Add
  | tc == typeNatSubTyCon = Just Sub
  | tc == typeNatMulTyCon = Just Mul
  | tc == typeNatLeqTyCon = Just Leq
  | otherwise             = Nothing

knownXi :: Xi -> Maybe (VarTypes, Expr)
knownXi xi
  | Just x <- getTyVar_maybe xi   = knownVar x
  | Just x <- isNumLitTy xi       = Just (emptyUFM, Num x)
  | Just x <- isBoolLitTy xi      = Just (emptyUFM, Bool x)
  | otherwise                     = Nothing

knownVar :: TyVar -> Maybe (VarTypes, Expr)
knownVar x =
  do t <- knownKind (tyVarKind x)
     let v = thyVarName x
     return (unitUFM x (x, v, t), Var v)

knownKind :: Kind -> Maybe Ty
knownKind k =
  case splitTyConApp_maybe k of
    Just (tc,[])
      | tc == promotedBoolTyCon -> Just TBool
      | tc == typeNatKindCon    -> Just TNat
    _ -> Nothing


thyVarName :: TyVar -> String
thyVarName x = occNameString (nameOccName n) ++ "_" ++ show u
  where n = tyVarName x
        u = nameUnique n

--------------------------------------------------------------------------------
-- SMTLIB standard

smtExpr :: Expr -> SExpr
smtExpr expr =
  case expr of
    Op op es -> SList (smtOp op : map smtExpr es)
    Not e    -> SList [ SAtom "not", smtExpr e ]
    Num n    -> SAtom (show n)
    Bool b   -> SAtom (if b then "true" else "false")
    Var x    -> SAtom x

smtOp :: Op -> SExpr
smtOp op =
  SAtom $
    case op of
      Add -> "+"
      Sub -> "-"
      Mul -> "*"
      Leq -> "<="
      Eq  -> "="

smtTy :: Ty -> SExpr
smtTy ty =
  SAtom $
    case ty of
      TNat  -> "Int"
      TBool -> "Bool"

smtExtraConstraints :: String -> Ty -> [Expr]
smtExtraConstraints x ty =
  case ty of
    TNat  -> [ Op Leq [Num 0, Var x] ]
    TBool -> [ ]

smtDeclare :: String -> Ty -> SExpr
smtDeclare x t = SList [SAtom "declare-fun", SAtom x, SList [], smtTy t]

smtAssert :: Expr -> SExpr
smtAssert e = SList [SAtom "assert", smtExpr e]

--------------------------------------------------------------------------------

{-
Note [Variable Scopes]

The SMTLIB format has an unfortunate "feature": push/pop commands affect
assertions but not variable declarations.  So, if we declare a variable
after a push, then when we pop, all assertions about the variable will go away,
but not the variable declaration.

This is particularly troublesome when variables have additional well-formedness
constraints that go beyond their type.   In particular, we work with natural
numbers, while the basic type supported by a solver is integers.  So, whenever
we declare a variable, we also add an additional assertion, constraining
the variable to be >= 0.

Unfortunately, when we `pop` the well-formedness constraints are going 
to go away, but not the variable declaration.  So, if we want to mention
the variable again, in a new scope, then we have to add the additional
constraint but *not* add a declaration.

Thus, a variable can be in one of three states:
  1. Declared and constrained
  2. Declared, but not constrained
  3. Not declared, and not constrained.
-}

data VarInfo = VarInfo
  { smtDeclaredVars :: Map String TyVar -- ^ all declared vars
  , smtCurScope     :: Set String       -- ^ constrained
  , smtOtherScopes  :: [Set String]     -- ^ constrained
  }

emptyVarInfo :: VarInfo
emptyVarInfo = VarInfo
  { smtDeclaredVars = Map.empty
  , smtCurScope     = Set.empty
  , smtOtherScopes  = []
  }

startScope :: VarInfo -> VarInfo
startScope vi = vi { smtCurScope    = Set.empty
                   , smtOtherScopes = smtCurScope vi : smtOtherScopes vi }

endScope :: VarInfo -> VarInfo
endScope vi =
  case smtOtherScopes vi of
    [] -> panic "endScope with no start scope"
    s : ss -> vi
      { smtCurScope     = s
      , smtOtherScopes  = ss
      }

data VarStatus = Undeclared | Declared | Constrained

-- | Update var info, and indicate what declaring we need to do.
declareVar :: TyVar -> VarInfo -> (VarInfo, VarStatus)
declareVar tv vi
  | x `Set.member` smtCurScope vi = (vi, Constrained)

  | any (x `Set.member`) (smtOtherScopes vi) = (vi, Constrained)

  | x `Map.member` smtDeclaredVars vi =
    ( vi { smtCurScope = Set.insert x (smtCurScope vi) }, Declared )

  | otherwise =
    ( vi { smtDeclaredVars = Map.insert x tv (smtDeclaredVars vi)
         , smtCurScope     = Set.insert x (smtCurScope vi)
         }
    , Undeclared
    )
  where x = thyVarName tv






--------------------------------------------------------------------------------
-- Low-level interaction with the solver process.

data SExpr = SAtom String | SList [SExpr]

data SolverProcess = SolverProcess
  { solverDo   :: SExpr -> IO SExpr
  , solverStop :: IO ()
  }

startSolverProcess :: String -> [String] -> IO SolverProcess
startSolverProcess exe opts =
  do (hIn, hOut, hErr, h) <- runInteractiveProcess exe opts Nothing Nothing

     -- XXX: Ignore errors for now.
     _ <- forkIO $ do forever (hGetLine hErr)
                        `X.catch` \X.SomeException {} -> return ()

     -- XXX: No real error-handling here.
     getResponse <-
       do txt <- hGetContents hOut
          ref <- newIORef (unfoldr parseSExpr txt)
          return $
            atomicModifyIORef' ref $ \xs ->
               case xs of
                 []     -> (xs, Nothing)
                 y : ys -> (ys, Just y)

     let cmd' c = do hPutStrLn hIn (renderSExpr c "")
                     hFlush hIn

     return SolverProcess
        { solverDo = \c -> do cmd' c
                              mb <- getResponse
                              case mb of
                                Just res -> return res
                                Nothing  -> fail "Missing response from solver"
        , solverStop =
            do cmd' (SList [SAtom "exit"])
               _exitCode <- waitForProcess h
               return ()
        }

renderSExpr :: SExpr -> ShowS
renderSExpr ex =
  case ex of
    SAtom x  -> showString x
    SList es -> showChar '(' .
                foldr (\e m -> renderSExpr e . m) (showChar ')') es

parseSExpr :: String -> Maybe (SExpr, String)
parseSExpr (c : more) | isSpace c = parseSExpr more
parseSExpr ('(' : more) = do (xs,more1) <- list more
                             return (SList xs, more1)
  where
  list (c : txt) | isSpace c = list txt
  list (')' : txt) = return ([], txt)
  list txt         = do (v,txt1) <- parseSExpr txt
                        (vs,txt2) <- list txt1
                        return (v:vs, txt2)
parseSExpr txt     = case break end txt of
                       (as,bs) | not (null as) -> Just (SAtom as, bs)
                       _ -> Nothing
  where end x = x == ')' || isSpace x



