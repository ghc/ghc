{-# LANGUAGE OverlappingInstances, UndecidableInstances,
             ExistentialQuantification, FlexibleInstances #-}

-- module Formula where
module Main where

import Prelude hiding (logBase)

import Data.Maybe

-------------------------------------------------------------------------------

-- Formula
-- The data type for formulas (algegraic expressions).
--
-- It should be an extensible type, so that users of
-- the library can add new kinds of formulas.
-- For example, in this prototype I explore:
--   integer constants (FInt)
--   unknown variables (FVar)
--   sums (FSum)
--   products (FPro)
--   powers (FPow)
--   logarithms (FLog)
-- The user of the library may want to extend it with
-- trigonometric formulas or derivative formulas, for
-- example.
--
-- The idea is to let each kind of formula be a new data
-- type. Similar operations with them are implemented
-- using overloading. So there is a class (FORMULA) to collect
-- them and each kind of formula should be an instance of it.

class (Eq f, Show f) => FORMULA f where
    ty      :: f -> FType
    intVal  :: f -> Integer
    varName :: f -> String
    argList :: f -> [Formula]
    same    :: (FORMULA f1) => f -> f1 -> Bool
    intVal   = error ""
    varName  = error ""
    argList  = error ""
    same _ _ = False

-- By now extensibility is accomplished by existentialy
-- quantified type variables.

data Formula = forall f . ( FORMULA f
                          , AddT f
                          ) =>
               Formula f

instance Show Formula where
    show (Formula f) = show f

instance Eq Formula where
    (Formula x) == (Formula y) = same x y

instance FORMULA Formula where
    ty (Formula f) = ty f
    intVal (Formula f) = intVal f
    varName (Formula f) = varName f
    argList (Formula f) = argList f
    same (Formula f) = same f

-------------------------------------------------------------------------------

-- How to uniquely identify the type of formula?
-- Each type of formula is associated to a key (FType)
-- that identifies it.
--
-- Here I use an enumated data type. When extending
-- the library, the user will have to modify this
-- data type adding a new constant constructor.

data FType = INT
           | VAR
           | SUM
           | PRO
           | POW
           | LOG
             deriving (Eq,Ord,Enum,Show)

-------------------------------------------------------------------------------

-- Integer formula

data FInt = FInt Integer
            deriving (Eq,Show)

mkInt = Formula . FInt

instance FORMULA FInt where
    ty _ = INT
    intVal (FInt x) = x
    same (FInt x) y = isInt y && x == intVal y

-- Variable formula

data FVar = FVar String
            deriving (Eq,Show)

mkVar = Formula . FVar

instance FORMULA FVar where
    ty _ = VAR
    varName (FVar x) = x
    same (FVar x) y = isVar y && x == varName y

-- Sum formula

data FSum = FSum [Formula]
            deriving (Eq,Show)

mkSum = Formula . FSum

instance FORMULA FSum where
    ty _ = SUM
    argList (FSum xs) = xs
    same (FSum xs) y = isSum y && xs == argList y

-- Product formula

data FPro = FPro [Formula]
            deriving (Eq,Show)

mkPro = Formula . FPro

instance FORMULA FPro where
    ty _ = PRO
    argList (FPro xs) = xs
    same (FPro xs) y = isPro y && xs == argList y

-- Exponentiation formula

data FPow = FPow Formula Formula
            deriving (Eq,Show)

mkPow x y = Formula (FPow x y)

instance FORMULA FPow where
    ty _ = POW
    argList (FPow b e) = [b,e]
    same (FPow b e) y = isPow y && [b,e] == argList y

-- Logarithm formula

data FLog = FLog Formula Formula
            deriving (Eq,Show)

mkLog x b = Formula (FLog x b)

instance FORMULA FLog where
    ty _ = LOG
    argList (FLog x b) = [x,b]
    same (FLog x b) y = isLog y && [x,b] == argList y

-------------------------------------------------------------------------------

-- Some predicates

isInt x = ty x == INT
isVar x = ty x == VAR
isSum x = ty x == SUM
isPro x = ty x == PRO
isPow x = ty x == POW

isZero x = isInt x && intVal x == 0

-------------------------------------------------------------------------------

-- Adding two formulas
-- This is a really very simple algorithm for adding
-- two formulas.

add :: Formula -> Formula -> Formula
add x y
    | isJust u  = fromJust u
    | isJust v  = fromJust v
    | otherwise = mkSum [x,y]
    where
    u = addT x y
    v = addT y x

class AddT a where
    addT :: a -> Formula -> Maybe Formula
    addT _ _ = Nothing

instance (FORMULA a) => AddT a where {}

instance AddT Formula where
    addT (Formula f) = addT f

instance AddT FInt where
    addT (FInt 0) y  = Just y
    addT (FInt x) y
	 | isInt y   = Just (mkInt (x + intVal y))
	 | otherwise = Nothing

instance AddT FSum where
    addT (FSum xs) y
	 | isSum y   = Just (mkSum (merge xs (argList y)))
	 | otherwise = Just (mkSum (merge xs [y]))
         where
         merge = (++)

instance AddT FLog where
    addT (FLog x b) y
	 | isLog y && b == logBase y = Just (mkLog (mkPro [x,logExp y]) b)
	 | otherwise                 = Nothing
         where
         merge = (++)

isLog x = ty x == LOG

logBase x
    | isLog x = head (tail (argList x))

logExp x
    | isLog x = head (argList x)

-------------------------------------------------------------------------------

-- Test addition of formulas

main = print [ add (mkInt 78)  (mkInt 110)
             , add (mkInt 0)   (mkVar "x")
             , add (mkVar "x") (mkInt 0)
             , add (mkVar "x") (mkVar "y")
             , add (mkSum [mkInt 13,mkVar "x"]) (mkVar "y")
             , add (mkLog (mkVar "x") (mkInt 10))
                   (mkLog (mkVar "y") (mkInt 10))
             , add (mkLog (mkVar "x") (mkInt 10))
                   (mkLog (mkVar "y") (mkVar "e"))
             ]
