{-# LANGUAGE FlexibleInstances #-}

module Settings (
    IntegerLibrary (..), integerLibrary, integerLibraryName,
    buildHaddock
    ) where

import Base
import Ways
import Package.Base (Package)
import Oracles.Builder

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

-- A generic Parameterised Graph datatype for parameterised argument lists
data PG p v = Epsilon
            | Vertex v
            | Overlay (PG p v) (PG p v)
            | Sequence (PG p v) (PG p v)
            | Condition p (PG p v)

instance Monoid (PG p v) where
    mempty  = Epsilon
    mappend = Overlay

fromList :: [v] -> PG p v
fromList = foldr Sequence Epsilon . map Vertex

type RewritePG p v = PG p v -> PG p v

data Predicate a = Evaluated Bool                  -- Evaluated predicate
                 | Parameter a                     -- To be evaluated later
                 | Not (Predicate a)               -- Negate predicate
                 | And (Predicate a) (Predicate a) -- Conjunction
                 | Or  (Predicate a) (Predicate a) -- Disjunction

multiOr :: [Predicate a] -> RewritePG (Predicate a) v
multiOr = Condition . foldr Or (Evaluated False)

multiAnd :: [Predicate a] -> RewritePG (Predicate a) v
multiAnd = Condition . foldr And (Evaluated True)

type RewrtePredicate a = Predicate a -> Predicate a

-- Evaluator takes an argument and attempts to determine its truth.
-- Returns Nothing if the attempt fails.
type Evaluator a = a -> Maybe Bool

-- Monoid instance for evaluators (returns first successful evaluation)
instance Monoid (Evaluator a) where
    mempty        = const Nothing
    p `mappend` q = \a -> getFirst $ First (p a) <> First (q a)

-- Apply an evalulator to a predicate (partial evaluation, or 'projection').
apply :: Evaluator a -> RewrtePredicate a
apply _ p @ (Evaluated _) = p
apply e p @ (Parameter q) = case e q of
    Just bool -> Evaluated bool
    Nothing   -> p
apply e (Not p  ) = Not (apply e p)
apply e (And p q) = And (apply e p) (apply e q)
apply e (Or  p q) = Or  (apply e p) (apply e q)

-- Map over all PG predicates, e.g., apply an evaluator to a given PG.
mapP :: RewrtePredicate a -> RewritePG (Predicate a) v
mapP _ Epsilon         = Epsilon
mapP _ v @ (Vertex _)  = v
mapP r (Overlay p q)   = Overlay (mapP r p) (mapP r q)
mapP r (Sequence p q)  = Sequence (mapP r p) (mapP r q)
mapP r (Condition x p) = Condition (r x) (mapP r p)

project :: Evaluator a -> RewritePG (Predicate a) v
project = mapP . apply

-- Attempt to evaluate a predicate. Returns Nothing if the predicate
-- cannot be uniquely evaluated due to remaining parameters.
-- An alternative type: evalPredicate :: Evaluator (Predicate a)
evalPredicate :: Predicate a -> Maybe Bool
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
evalPredicate (Parameter _) = Nothing -- cannot evaluate Parameter

-- Linearise a PG into a list. Returns Nothing if the given expression
-- cannot be uniquely evaluated due to remaining parameters.
linearise :: PG (Predicate a) v -> Maybe [v]
linearise Epsilon = Just []
linearise (Vertex v) = Just [v]
linearise (Overlay  p q)  = (++) <$> linearise p <*> linearise q -- TODO: union
linearise (Sequence p q)  = (++) <$> linearise p <*> linearise q
linearise (Condition x p) = case evalPredicate x of
    Just True  -> linearise p
    Just False -> Just []
    Nothing    -> Nothing

-- GHC build specific

type Expression a = PG (Predicate BuildParameter) a
type Rewrite a = Expression a -> Expression a

--type ArgsExpression = Expression String
--type Args = Expression String

--args :: [String] -> Args
--args = fromList

data BuildParameter = WhenPackage  Package
                    | WhenBuilder  Builder
                    | WhenStage    Stage
                    | WhenWay      Way
                    | WhenFile     FilePattern
                    | WhenKeyValue String String -- from config files

-- Predicates

alternatives :: (b -> BuildParameter) -> [b] -> Rewrite a
alternatives p = multiOr . map (Parameter . p)

whenPackages :: [Package] -> Rewrite a
whenPackages = alternatives WhenPackage

whenBuilders :: [Builder] -> Rewrite a
whenBuilders = alternatives WhenBuilder

whenStages :: [Stage] -> Rewrite a
whenStages = alternatives WhenStage

unlessStage :: Stage -> Rewrite a
unlessStage stage = Condition (Not $ Parameter $ WhenStage stage)

whenWays :: [Way] -> Rewrite a
whenWays = alternatives WhenWay

whenFiles :: [FilePattern] -> Rewrite a
whenFiles = alternatives WhenFile

whenKeyValues :: String -> [String] -> Rewrite a
whenKeyValues key = alternatives (WhenKeyValue key)

whenKeyValue :: String -> String -> Rewrite a
whenKeyValue key value = whenKeyValues key [value]

-- Evaluators

packageEvaluator :: Package -> Evaluator BuildParameter
packageEvaluator p (WhenPackage p') = Just $ p == p'
packageEvaluator _ _                = Nothing

builderEvaluator :: Builder -> Evaluator BuildParameter
builderEvaluator b (WhenBuilder b') = Just $ b == b'
builderEvaluator _ _                = Nothing

stageEvaluator :: Stage -> Evaluator BuildParameter
stageEvaluator s (WhenStage s') = Just $ s == s'
stageEvaluator _ _              = Nothing

wayEvaluator :: Way -> Evaluator BuildParameter
wayEvaluator w (WhenWay w') = Just $ w == w'
wayEvaluator _ _            = Nothing

fileEvaluator :: FilePath -> Evaluator BuildParameter
fileEvaluator file (WhenFile pattern) = Just $ pattern ?== file
fileEvaluator _ _                     = Nothing

keyValueEvaluator :: String -> String -> Evaluator BuildParameter
keyValueEvaluator key value (WhenKeyValue key' value')
    | key == key' = Just $ value == value'
    | otherwise   = Nothing
keyValueEvaluator _ _ _ = Nothing

setPackage :: Package -> Rewrite a
setPackage = project . packageEvaluator

setBuilder :: Builder -> Rewrite a
setBuilder = project . builderEvaluator

setStage :: Stage -> Rewrite a
setStage = project . stageEvaluator

setWay :: Way -> Rewrite a
setWay = project . wayEvaluator

setFile :: FilePath -> Rewrite a
setFile = project . fileEvaluator

setKeyValue :: String -> String -> Rewrite a
setKeyValue key = project . keyValueEvaluator key

whenPackageKey :: Rewrite a
whenPackageKey = whenKeyValue "supports-package-key" "YES" . unlessStage Stage0

--packageArgs =
--    Vertex "-hide-all-packages"
--    ~>
--    Vertex "-no-user-package-db"
--    ~>
--    Vertex "-include-pkg-deps"
--    ~> If (MatchStage Stage0)
--          (Vertex "-package-db libraries/bootstrapping.conf")
--    ~> If usePackageKey
--          (

--          )

--packageArgs :: Stage -> FilePath -> Args
--packageArgs stage pathDist = do
--    usePackageKey <- SupportsPackageKey || stage /= Stage0
--    args [ arg "-hide-all-packages"
--         , arg "-no-user-package-db"
--         , arg "-include-pkg-deps"
--         , when (stage == Stage0) $
--           arg "-package-db libraries/bootstrapping.conf"
--         , if usePackageKey
--           then productArgs ["-this-package-key"] [arg  $ PackageKey pathDist]
--             <> productArgs ["-package-key"     ] [args $ DepKeys    pathDist]
--           else productArgs ["-package-name"    ] [arg  $ PackageKey pathDist]
--             <> productArgs ["-package"         ] [args $ Deps       pathDist]
--         ]
