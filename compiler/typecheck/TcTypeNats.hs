module TcTypeNats
  ( typeNatTyCons
  , typeNatCoAxiomRules
  , BuiltInSynFamily(..)
  , ExternalSolver(..), ExtSolRes(..), newExternalSolver
  ) where

import Type
import Pair
import TcType     ( TcType, tcEqType )
import TcEvidence ( mkTcAxiomRuleCo, EvTerm(..) )
import TyCon      ( TyCon, SynTyConRhs(..), mkSynTyCon, TyConParent(..)  )
import Coercion   ( Role(..) )
import TcRnTypes  ( Xi, Ct(..), ctPred, CtEvidence(..), mkNonCanonical
                  , CtLoc, ctLoc, isGivenCt )
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
import           Data.Maybe ( isJust, mapMaybe )
import           Data.Char  ( isSpace )
import           Data.List ( unfoldr, foldl', partition, sortBy, groupBy )
import           Data.IORef ( IORef, newIORef, atomicModifyIORef',
                              atomicModifyIORef, modifyIORef'
                            , readIORef
                            )
import           Control.Monad ( forever, msum )
import           Control.Concurrent ( forkIO )
import qualified Control.Exception as X
import           System.Process ( runInteractiveProcess, waitForProcess )
import           System.IO ( hPutStrLn, hFlush, hGetContents, hGetLine,
                             stderr )

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


decisionProcedure :: String -> CoAxiomRule
decisionProcedure name =
  CoAxiomRule
    { coaxrName      = fsLit name
    , coaxrTypeArity = 2
    , coaxrAsmpRoles = []
    , coaxrRole      = Nominal
    , coaxrProves    = \ts cs ->
        case (ts,cs) of
          ([s,t],[]) -> return (s === t)
          _          -> Nothing
    }


evBySMT :: String -> (Type, Type) -> EvTerm
evBySMT name (t1,t2) =
  EvCoercion $ mkTcAxiomRuleCo (decisionProcedure name) [t1,t2] []



-- Various utilities for making axioms and types
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
  { extSolAssert  :: [Ct] -> IO ()
    -- ^ Add some assertions.
    -- Changes assertions.

  , extSolImprove :: Bool {- are we in the given stage -}
                                                -> [Ct] -> IO ExtSolRes
    -- ^ Check for consistency and new work.
    -- Does not change assertions.

  , extSolSolve   :: [Ct] -> IO ([(EvTerm,Ct)], [Ct])
    -- ^ Solve some constraints.
    -- Does not change assertions.

  , extSolStop    :: IO ()
    -- ^ Exit the solver.
    -- The solver should not be used after this is called.
  }

data ExtSolRes
  = ExtSolContradiction {- inconsistent -} [Ct]
                        {- all others   -} [Ct]
    -- ^ There is no model for assertions.

  | ExtSolOk [Ct]
    -- ^ New work (facts that will hold in all models)

--------------------------------------------------------------------------------
-- Concrete implementation

newExternalSolver :: FilePath -> [String] -> IO ExternalSolver
newExternalSolver exe opts =
  do proc <- startSolverProcess exe opts

     -- Prep the solver
     solverSimpleCmd proc [ "set-option", ":print-success", "true" ]
     solverSimpleCmd proc [ "set-option", ":produce-models", "true" ]
     solverSimpleCmd proc [ "set-logic", "QF_LIA" ]

     viRef <- newIORef emptyVarInfo

     let dbg = solverDebug proc

     return ExternalSolver
       { extSolAssert = \cts ->
          do dbg "=== ASSERT ==="
             (_,ours) <- solverPrepare proc viRef cts
             let expr (_,e,_) = e
             mapM_ (solverAssume proc . expr) ours
             mapM_ (solverDebug proc . show . expr) ours

       , extSolImprove = \b cts -> do dbg "=== IMPROVE ==="
                                      solverImprove proc viRef b cts

       , extSolSolve = \cts -> do dbg "=== SOLVE ==="
                                  solverSimplify proc viRef cts

       , extSolStop = solverStop proc
       }

mkNewFact :: CtLoc -> Bool -> (Type,Type) -> CtEvidence
mkNewFact newLoc withEv (t1,t2)
  | withEv = CtGiven { ctev_pred = newPred
                     , ctev_evtm = evBySMT "SMT" (t1,t2)
                     , ctev_loc  = newLoc
                     }
  | otherwise = CtDerived { ctev_pred = newPred
                          , ctev_loc  = newLoc
                          }
  where
  newPred = mkEqPred t1 t2

-- A command with no interesting result.
solverAckCmd :: SolverProcess -> SExpr -> IO ()
solverAckCmd proc c =
  do res <- solverDo proc c
     case res of
       SAtom "success" -> return ()
       _  -> fail $ unlines
                      [ "Unexpected result from the SMT solver:"
                      , "  Expected: success"
                      , "  Result: " ++ renderSExpr res ""
                      ]

-- A command entirely made out of atoms, with no interesting result.
solverSimpleCmd :: SolverProcess -> [String] -> IO ()
solverSimpleCmd proc = solverAckCmd proc . SList . map SAtom



-- Checkpoint state
solverPush :: SolverProcess -> IORef VarInfo -> IO ()
solverPush proc viRef =
  do solverSimpleCmd proc [ "push", "1" ]
     modifyIORef' viRef startScope

-- Restore to last check-point
solverPop :: SolverProcess -> IORef VarInfo -> IO ()
solverPop proc viRef =
  do solverSimpleCmd proc [ "pop", "1" ]
     modifyIORef' viRef endScope


-- Assume a fact
solverAssume :: SolverProcess -> SExpr -> IO ()
solverAssume proc e = solverAckCmd proc $ SList [ SAtom "assert", e ]

-- Declare a new variable
solverDeclare :: SolverProcess -> IORef VarInfo -> (TyVar, String, Ty) -> IO ()
solverDeclare proc viRef (tv,x,ty) =
  do status <- atomicModifyIORef' viRef (declareVar tv)
     case status of
       Declared    -> return ()
       Undeclared  ->
         do solverAckCmd proc $
                SList [SAtom "declare-fun", SAtom x, SList [], smtTy ty]
            mapM_ (solverAssume proc) (smtExtraConstraints x ty)

data SmtRes = Unsat | Unknown | Sat

-- Check if assumptions are consistent. Does not return a model.
solverCheck :: SolverProcess -> IO SmtRes
solverCheck proc =
  do res <- solverDo proc (SList [ SAtom "check-sat" ])
     case res of
       SAtom "unsat"   -> return Unsat
       SAtom "unknown" -> return Unknown
       SAtom "sat"     -> return Sat
       _ -> fail $ unlines
              [ "Unexpected result from the SMT solver:"
              , "  Expected: unsat, unknown, or sat"
              , "  Result: " ++ renderSExpr res ""
              ]

-- Prove something by concluding that a counter-example is impossible.
solverProve :: SolverProcess -> IORef VarInfo -> SExpr -> IO Bool
solverProve proc viRef e =
  debug $
  do solverPush proc viRef
     solverAssume proc $ SList [ SAtom "not", e ]
     res <- solverCheck proc
     solverPop proc viRef
     case res of
       Unsat -> return True
       _     -> return False

  where
  debug m = do solverDebug proc ("Prove: " ++ show e)
               solverDebugNext proc
               res <- m
               solverDebug proc (show res)
               solverDebugPrev proc
               return res

-- Get values for the variables that are in scope.
solverGetModel :: SolverProcess -> VarInfo -> IO [(String,SExpr)]
solverGetModel proc vi =
  do res <- solverDo proc
          $ SList [ SAtom "get-value", SList [ SAtom v | v <- inScope vi ] ]
     case res of
       SList xs -> return [ (x,v) | SList [ SAtom x, v ] <- xs ]
       _ -> fail $ unlines
                 [ "Unexpected response from the SMT solver:"
                 , "  Exptected: a list"
                 , "  Result: " ++ renderSExpr res ""
                 ]

{- Try to generalize some facts for a model.

In particular, we look for facts of the form:
  * x = K, where `K` is a constant, and
  * x = y, where `y` is a variable.

Returns only the new facts.
-}
solverImproveModel :: SolverProcess -> IORef VarInfo ->
                     [(String,SExpr)] -> IO [ (String,SExpr) ]
solverImproveModel proc viRef imps =
  do xs <- constEq [] imps
     solverDebug proc $ "Improvements: " ++ unwords [ x ++ " = " ++ renderSExpr e "," | (x,e) <- xs ]
     return xs
  where

  constEq imps [] = return imps
  constEq imps ((x,v) : more) =
    -- First we check if this is the only possible concrete value for `x`:
    do proved <- solverProve proc viRef (SList [ SAtom "=", SAtom x, v ])
       if proved
         then constEq ((x,v) : imps) more
         -- If two variables `x` and `y` have the same value in this model,
         -- then we check if they must be equal in all models.
         else let (candidates,others) = partition ((== v) . snd) more
              in varEq x imps others candidates


  varEq _ imps more []  = constEq imps more
  varEq x imps more (def@(y,_) : ys) =
    do let e = SAtom y
       -- Check if `x` and `y` must be the same in all models.
       proved <- solverProve proc viRef (SList [ SAtom "=", SAtom x, e ])
       if proved
          then varEq x ((x, e) : imps)        more  ys
          else varEq x           imps  (def : more) ys


--------------------------------------------------------------------------------
-- Higher level operations on collections of constraints.


-- Identify our constraints, and declare their variables in the current scope.
solverPrepare :: SolverProcess -> IORef VarInfo ->
                 [Ct] -> IO ([Ct], [(Ct,SExpr,Maybe TyVar)])
solverPrepare proc viRef = go [] []
  where
  go others ours [] = return (others, ours)
  go others ours (ct : cts) =
    case knownCt ct of
      Just (vars,e,mbV) ->
        do let g = if isGivenCt ct then "[G] " else "[W/D] "
           solverDebug proc (g ++ show e)
           mapM_ (solverDeclare proc viRef) (eltsUFM vars)
           go       others ((ct,e,mbV) : ours) cts
      Nothing ->
           go (ct : others)              ours  cts


{-
Check a list of constraints for consistency, and computer derived work.
Does not affect set off assertions in the solver.
-}
solverImprove :: SolverProcess -> IORef VarInfo
              -> Bool -- ^ Should we generate given constraints?
                      -- If not, we generate derived ones.
              -> [Ct]
              -> IO ExtSolRes
solverImprove proc viRef withEv cts =
  do push   -- declare variables
     (others,ours') <- solverPrepare proc viRef cts
     let ours = [ (ct,e) | (ct,e,_) <- ours' ]
     case ours of
       [] -> do pop -- declarations
                return (ExtSolOk [])

       (oneOfOurs,_) : _ ->
         do push -- assumptions
            mapM_ (assume . snd) ours
            status <- check

            res <-
              case status of

                -- Inconsistent: find a smaller example, then stop.
                Unsat  ->
                  do pop -- assumptions
                     mbRes <- solverFindConstraidction proc viRef others ours
                     case mbRes of
                       Nothing ->
                         fail "Bug: Failed to reporoduce contradiciton."
                       Just (core,rest) ->
                         return $ ExtSolContradiction core rest

                -- We don't know: treat as consistent.
                Unknown -> do pop -- assumptions
                              return (ExtSolOk [])

                -- Consistent: try to compute derived work.
                Sat ->
                  do m    <- solverGetModel proc =<< readIORef viRef

                     imps <- solverImproveModel proc viRef m
                     pop -- assumptions

                     vi   <- readIORef viRef

                     let loc    = ctLoc oneOfOurs -- XXX: Better location?
                         toCt (x,e) =
                           do tv <- varToTyVar x vi
                              ty <- sExprToType vi e
                              return $ mkNonCanonical
                                     $ mkNewFact loc withEv (mkTyVarTy tv, ty)
                     return $ ExtSolOk $ mapMaybe toCt imps

            pop -- declarations
            return res
  where
  push    = solverPush   proc viRef
  pop     = solverPop    proc viRef
  assume  = solverAssume proc
  check   = solverCheck  proc


{- Identify a sub-set of constraints that leads to a contradiction.

We call this when we find out that a collection of constraints is
inconsistent:  instead of marking them _all_ as insoluable,
we identify a sub-set that is the real cause of the problem.

* Assumes that the variables in our constarints have been declared.
* Does not change the assertions in the solver.
-}
solverFindConstraidction ::
  SolverProcess ->      -- Solver
  IORef VarInfo ->      -- Scoping of variables
  [Ct] ->               -- Constraints not relevant to us
  [(Ct,SExpr)] ->       -- Our constraints
  IO (Maybe ( [Ct]      -- Constraints that cause a contradiciotn
            , [Ct]      -- All other constraints (both others and ours)
            )
     )
solverFindConstraidction proc viRef others ours =
  do push  -- scope for `needed`
     minimize others [] ours

  where
  check   = solverCheck   proc
  push    = solverPush    proc viRef
  pop     = solverPop     proc viRef
  assume  = solverAssume  proc

  minimize notNeeded needed maybeNeeded =
    do res <- check
       case res of
         Unsat -> do pop  -- remove `needed` scope.
                     return $ Just (needed, map fst maybeNeeded ++ notNeeded)
         _     -> do push -- scope for `maybeNeeded`
                     search notNeeded needed [] maybeNeeded

  search _ needed _ [] =
    do pop  -- Remove `maybeNeeded`
       pop  -- Remove `needed`
       case needed of
         [] -> return Nothing    -- No definite contradictions
         _  -> fail "Bug: we found a contradiction, and then lost it!"

  search notNeeded needed maybeNeeded ((ct,e) : more) =
    do assume e    -- Add to `maybeNeeded`
       res <- check
       case res of

         Unsat -> -- We found a contradiction using `needed` and `maybeNeeded`.
           do pop       -- remove `maybeNedded`
              assume e  -- add to `needed`
              minimize (map fst more ++ notNeeded) (ct : needed) maybeNeeded

         -- No contradiction, keep searching.
         _ -> search notNeeded needed ((ct,e) : maybeNeeded) more



{- Try to solve as much as possible from the given list of constraints.
Returns the solved constraints (with evidence), and all other constraints.
-}
{-
Consider this example:

ex4 :: p a -> p b -> p ((a + a) + b) -> p (2 * a + b)
ex4 _ = id

A: a + a = u
B: x + b = v
C: 2 * a = w
D: w + b = v

If we solve `B` or `D` first, then we are essnetially done,
as all the others can be substituted within each other.

However, what if we happen to consider `C` first?

(A,B,D) => C

This goal is essentially:

((a + a) + b ~ (2 * a) + b) => (a + a) ~ 2 * a

which can be proved by cancelling `b` on both sides.

Now, we are left with just `A,B,D`, which amounts to having
to prove:

(a + a) + b   ~   w + b

We can't do this because we've lost the information about `w`.
To avoid this, we first try to solve equations that have the same varibal
on the RHS (e.g., F xs ~ a, G ys ~ a).

However, this is not quite enough.  Here is another tricky example:

data T :: Nat -> * where
  Even  :: T (n + 1) -> T (2 * (n + 1))

addT :: T m -> T n -> T (m + n)
addT (Even x) (Even y) = Even (addT x y)


x :: T (a + 1)
y :: T (b + 1)

[G] m = 2 * (a + 1)
[G] n = 2 * (b + 1)

addT x y :: T ((a + 1) + (b + 1))

[W] (a + 1) + (b + 1) = x + 1  -- Applying Even is OK
[W] 2 * (x + 1) = m + n        -- The result is correct

After canonicalization:

[G] a + 1  = t1
[G] b + 1  = t2
[G] 2 * t1 = m
[G] 2 * t2 = n

[W] t1 + t2 = t3
[W] x  + 1  = t3
[W] 2 * t3  = t4
[W] m + n   = t4

The issue here seems to be that `x = a + b + 1`, but this intermediate
value is not named anywhere.  What to do?


XXX: BUG The following incorrect program causes infinte improvement

addT :: T m -> T n -> T (m + n)
addT (Even x) (Even y) = Even (addT x x)

-}




solverSimplify :: SolverProcess -> IORef VarInfo ->
                  [Ct] -> IO ([(EvTerm,Ct)], [Ct])
solverSimplify proc viRef cts =
  do push     -- `unsolved` scope
     (others,ours) <- solverPrepare proc viRef cts
     let r = reorder ours
     solverDebug proc "Reordered:"
     mapM_ (solverDebug proc . show . snd) r
     go others [] r
  where
  push    = solverPush    proc viRef
  pop     = solverPop     proc viRef
  assume  = solverAssume  proc

  go unsolved solved [] =
    do pop -- remove `unsolved` scope
       return (solved, unsolved)

  go unsolved solved ((ct,e) : more) =
    do push   -- Temporarily assume everything else.
       solverDebug proc "Temporary assuming:"
       solverDebugNext proc
       mapM_ (assume . snd) more
       mapM_ (solverDebug proc . show . snd) more
       solverDebugPrev proc
       proved <- solverProve proc viRef e
       pop
       if proved
          then let ev = evBySMT "SMT" $ getEqPredTys $ ctPred ct
               in go unsolved ((ev,ct) : solved) more
          else do assume e    -- add to `unsolved`
                  go (ct : unsolved) solved more

  reorder = map dropVar
          . concat
          . sortBy  longer    -- most vars first
          . groupBy sameVars  -- join together eqns for same variable
          . sortBy  cmpVars   -- first sort by variable

    where
    cmpVars  (_,_,v1) (_,_,v2) = compare v1 v2
    sameVars (_,_,v1) (_,_,v2) = v1 == v2
    longer xs ys               = compare (length ys) (length xs)
    dropVar (ct,e,_) = (ct,e)





--------------------------------------------------------------------------------

smtTy :: Ty -> SExpr
smtTy ty =
  SAtom $
    case ty of
      TNat  -> "Int"
      TBool -> "Bool"

smtExtraConstraints :: String -> Ty -> [SExpr]
smtExtraConstraints x ty =
  case ty of
    TNat  -> [ smtLeq (smtNum 0) (smtVar x) ]
    TBool -> [ ]

smtVar :: String -> SExpr
smtVar = SAtom

smtEq :: SExpr -> SExpr -> SExpr
smtEq e1 e2 = SList [ SAtom "=", e1, e2 ]

smtLeq :: SExpr -> SExpr -> SExpr
smtLeq e1 e2 = SList [ SAtom "<=", e1, e2 ]

smtBool :: Bool -> SExpr
smtBool b = SAtom (if b then "true" else "false")

smtNum :: Integer -> SExpr
smtNum x = SAtom (show x)




--------------------------------------------------------------------------------
-- Recognizing constraints that we can work with.


data Ty       = TNat | TBool
type VarTypes = UniqFM (TyVar,String,Ty)


{- | See if this constraint is one ofthe ones that we understand.
If the constraint is of the form `F ts ~ a`, where `a` is a type-variable,
we also return `a`.  This is used to decide in what order to solve
constraints, see `solverSimplify`. -}
knownCt :: Ct -> Maybe (VarTypes, SExpr, Maybe TyVar)
knownCt ct =
  case ct of
    CFunEqCan _ f args rhs ->
      do (vs1,e1) <- knownTerm f args
         (vs2,e2) <- knownXi rhs
         return (plusUFM vs1 vs2, smtEq e1 e2, getTyVar_maybe rhs)
    _ -> Nothing

knownTerm :: TyCon -> [Xi] -> Maybe (VarTypes, SExpr)
knownTerm tc xis =
  do op <- knownTC tc
     as <- mapM knownXi xis
     -- XXX: Check for linearity here?
     let (varMaps,es) = unzip as
     return (foldl' plusUFM emptyUFM varMaps, SList (op : es))

knownTC :: TyCon -> Maybe SExpr
knownTC tc
  | tc == typeNatAddTyCon = Just $ SAtom "+"
  | tc == typeNatSubTyCon = Just $ SAtom "-"
  | tc == typeNatMulTyCon = Just $ SAtom "*"
  | tc == typeNatLeqTyCon = Just $ SAtom "<="
  | otherwise             = Nothing

knownXi :: Xi -> Maybe (VarTypes, SExpr)
knownXi xi
  | Just x <- getTyVar_maybe xi   = knownVar x
  | Just x <- isNumLitTy xi       = Just (emptyUFM, smtNum x)
  | Just x <- isBoolLitTy xi      = Just (emptyUFM, smtBool x)
  | otherwise                     = Nothing

knownVar :: TyVar -> Maybe (VarTypes, SExpr)
knownVar x =
  do t <- knownKind (tyVarKind x)
     let v = thyVarName x
     return (unitUFM x (x, v, t), SAtom v)

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


-- From a value back into a type
sExprToType :: VarInfo -> SExpr -> Maybe Type
sExprToType vi expr =
  case expr of
    SAtom "false" -> Just (bool False)
    SAtom "true"  -> Just (bool True)
    SAtom s
      | [(n,"")] <- reads s -> Just (num n)
    SAtom s
      | Just v <- varToTyVar s vi -> Just (mkTyVarTy v)
    _ -> Nothing




--------------------------------------------------------------------------------

-- Information about declared variables, so that we know how to extarct
-- models, and map them back into types.
data VarInfo = VarInfo
  { smtCurScope     :: Map String TyVar
  , smtOtherScopes  :: [Map String TyVar]
  }

emptyVarInfo :: VarInfo
emptyVarInfo = VarInfo
  { smtCurScope     = Map.empty
  , smtOtherScopes  = []
  }

inScope :: VarInfo -> [String]
inScope vi = Map.keys $ Map.unions $ smtCurScope vi : smtOtherScopes vi

startScope :: VarInfo -> VarInfo
startScope vi = vi { smtCurScope    = Map.empty
                   , smtOtherScopes = smtCurScope vi : smtOtherScopes vi }

endScope :: VarInfo -> VarInfo
endScope vi =
  case smtOtherScopes vi of
    [] -> panic "endScope with no start scope"
    s : ss -> vi
      { smtCurScope     = s
      , smtOtherScopes  = ss
      }

varToTyVar :: String -> VarInfo -> Maybe TyVar
varToTyVar x vi = msum $ map (Map.lookup x) $ smtCurScope vi : smtOtherScopes vi


data VarStatus = Undeclared | Declared

-- | Update var info, and indicate if we need to declare the variable.
declareVar :: TyVar -> VarInfo -> (VarInfo, VarStatus)
declareVar tv vi
  | x `Map.member` smtCurScope vi            = (vi, Declared)
  | any (x `Map.member`) (smtOtherScopes vi) = (vi, Declared)
  | otherwise =
    ( vi { smtCurScope = Map.insert x tv (smtCurScope vi) }, Undeclared )
  where x = thyVarName tv






--------------------------------------------------------------------------------
-- Low-level interaction with the solver process.

data SExpr = SAtom String | SList [SExpr]
             deriving Eq

instance Show SExpr where
  showsPrec _ = renderSExpr

data SolverProcess = SolverProcess
  { solverDo   :: SExpr -> IO SExpr
  , solverStop :: IO ()

  -- For debguggning
  , solverDebug     :: String -> IO ()
  , solverDebugNext :: IO ()
  , solverDebugPrev :: IO ()
  }

startSolverProcess :: String -> [String] -> IO SolverProcess
startSolverProcess exe opts =
  do (hIn, hOut, hErr, h) <- runInteractiveProcess exe opts Nothing Nothing

     dbgNest <- newIORef (0 :: Int)
     let dbgMsg x = do n <- readIORef dbgNest
                       hPutStrLn stderr (replicate n ' ' ++ x)
         dbgNext  = modifyIORef' dbgNest (+2)
         dbgPrev  = modifyIORef' dbgNest (subtract 2)

     -- XXX: Ignore errors for now.
     _ <- forkIO $ do forever (putStrLn =<< hGetLine hErr)
                        `X.catch` \X.SomeException {} -> return ()

     -- XXX: No real error-handling here.
     getResponse <-
       do txt <- hGetContents hOut
          ref <- newIORef (unfoldr parseSExpr txt)
          return $
            atomicModifyIORef ref $ \xs ->
               case xs of
                 []     -> (xs, Nothing)
                 y : ys -> (ys, Just y)

     let cmd' c = do let e = renderSExpr c ""
                     -- dbgMsg ("[->] " ++ e)
                     hPutStrLn hIn e
                     hFlush hIn

     return SolverProcess
        { solverDo = \c -> do cmd' c
                              mb <- getResponse
                              case mb of
                                Just res ->
                                   do -- dbgMsg ("[<-] " ++ renderSExpr res "")
                                      return res
                                Nothing  -> fail "Missing response from solver"
        , solverStop =
            do cmd' (SList [SAtom "exit"])
               _exitCode <- waitForProcess h
               return ()

        , solverDebug     = dbgMsg
        , solverDebugNext = dbgNext
        , solverDebugPrev = dbgPrev
        }

renderSExpr :: SExpr -> ShowS
renderSExpr ex =
  case ex of
    SAtom x  -> showString x
    SList es -> showChar '(' .
                foldr (\e m -> renderSExpr e . showChar ' ' . m)
                (showChar ')') es

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



