{-# LANGUAGE FlexibleInstances #-}

module Settings (
    IntegerLibrary (..), integerLibrary, integerLibraryName,
    buildHaddock
    ) where

import Base
import Ways

data IntegerLibrary = IntegerGmp | IntegerGmp2 | IntegerSimple

integerLibrary :: IntegerLibrary
integerLibrary = IntegerGmp2

integerLibraryName :: String
integerLibraryName = case integerLibrary of
    IntegerGmp    -> "integer-gmp"
    IntegerGmp2   -> "integer-gmp2"
    IntegerSimple -> "integer-simple"

buildHaddock :: Bool
buildHaddock = True

-- A Parameterised Graph datatype for storing argument lists with conditions
data PG a b = Epsilon
            | Vertex a
            | Overlay (PG a b) (PG a b)
            | Sequence (PG a b) (PG a b)
            | Condition b (PG a b)

instance Monoid (PG a b) where
    mempty  = Epsilon
    mappend = Overlay

type ArgsExpression = PG String Predicate
type WaysExpression = PG Way    Predicate

data Match = MatchPackage  FilePath      -- Match a Package name
           | MatchFile     FilePath      -- Match a file
           | MatchStage    Stage         -- Match a Stage
           | MatchWay      Way           -- Match a Way
           | MatchKeyValue String String -- Match a key with a value (config)

-- A Matcher takes a Match description and attempts to evaluate it.
-- Returns Nothing if the attempt fails.
type Matcher = Match -> Maybe Bool

-- A Monoid instance for matchers (returns first successful match)
instance Monoid Matcher where
    mempty        = const Nothing
    p `mappend` q = \m -> getFirst $ First (p m) <> First (q m)

data Predicate = Evaluated Bool          -- Evaluated predicate
               | If  Match               -- Perform a match to evaluate
               | Not Predicate           -- Negate predicate
               | And Predicate Predicate -- Conjunction of two predicates
               | Or  Predicate Predicate -- Disjunction of two predicates

match :: Predicate -> Matcher -> Predicate
match p @ (Evaluated _) _ = p
match p @ (If match   ) m = case m match of
    Just bool -> Evaluated bool
    Nothing   -> p
match (Not p  ) m = match p m
match (And p q) m = And (match p m) (match q m)
match (Or  p q) m = Or  (match p m) (match q m)

-- returns Nothing if the given predicate cannot be uniquely evaluated
evalPredicate :: Predicate -> Maybe Bool
evalPredicate (Evaluated bool) = Just bool
evalPredicate (Not p)          = not <$> evalPredicate p
evalPredicate (And p q)
    | p' == Just False || q' == Just False = Just False
    | p' == Just True  && q' == Just True  = Just True
    | otherwise                            = Nothing
  where
    p' = evalPredicate p
    q' = evalPredicate q
evalPredicate (Or p q)
    | p' == Just True  || q' == Just True  = Just True
    | p' == Just False && q' == Just False = Just False
    | otherwise                            = Nothing
  where
    p' = evalPredicate p
    q' = evalPredicate q
evalPredicate (If _) = Nothing

-- returns Nothing if the given expression cannot be uniquely evaluated
evalPG :: PG a Predicate -> Maybe [a]
evalPG Epsilon = Just []
evalPG (Vertex v) = Just [v]
evalPG (Overlay  p q)  = (++) <$> evalPG p <*> evalPG q
evalPG (Sequence p q)  = (++) <$> evalPG p <*> evalPG q
evalPG (Condition x p) = case evalPredicate x of
    Just True  -> evalPG p
    Just False -> Just []
    Nothing    -> Nothing
