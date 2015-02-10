{-# LANGUAGE FlexibleInstances #-}

module Settings (
    IntegerLibrary (..), integerLibrary, integerLibraryName,
    buildHaddock
    ) where

import Base
import Ways
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

data Predicate a = Evaluated Bool                  -- Evaluated predicate
                 | Parameter a                     -- To be evaluated later
                 | Not (Predicate a)               -- Negate predicate
                 | And (Predicate a) (Predicate a) -- Conjunction
                 | Or  (Predicate a) (Predicate a) -- Disjunction

-- Evaluator takes a Parameter and attempts to evaluate it.
-- Returns Nothing if the attempt fails.
type Evaluator a = a -> Maybe Bool

-- Monoid instance for evaluators (returns first successful evaluation)
instance Monoid (Evaluator a) where
    mempty        = const Nothing
    e `mappend` f = \p -> getFirst $ First (e p) <> First (f p)

-- Apply an evalulator to a predicate (partial evaluation, or projection)
apply :: Evaluator a -> Predicate a -> Predicate a
apply _ p @ (Evaluated _) = p
apply e p @ (Parameter q) = case e q of
    Just bool -> Evaluated bool
    Nothing   -> p
apply e (Not p  ) = Not (apply e p)
apply e (And p q) = And (apply e p) (apply e q)
apply e (Or  p q) = Or  (apply e p) (apply e q)

-- Attempt to evaluate a predicate. Returns Nothing if the predicate
-- cannot be uniquely evaluated due to remaining parameters.
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

-- Flatten a PG into a list. Returns Nothing if the given expression
-- cannot be uniquely evaluated due to remaining parameters.
linearise :: PG (Predicate a) v -> Maybe [v]
linearise Epsilon = Just []
linearise (Vertex v) = Just [v]
linearise (Overlay  p q)  = (++) <$> linearise p <*> linearise q
linearise (Sequence p q)  = (++) <$> linearise p <*> linearise q
linearise (Condition x p) = case evalPredicate x of
    Just True  -> linearise p
    Just False -> Just []
    Nothing    -> Nothing

(~>) :: PG p v -> PG p v -> PG p v
a ~> b = Sequence a b

type PGP p v = PG (Predicate p) v

disjuction :: [a] -> (a -> Predicate p) -> PGP p v -> PGP p v
disjuction []     _       = id
disjuction (a:as) convert = Condition (foldr Or (convert a) $ map convert as)

-- GHC build specific

data BuildParameter = WhenPackage  FilePath
                    | WhenBuilder  Builder
                    | WhenStage    Stage
                    | WhenWay      Way
                    | WhenFile     FilePath
                    | WhenKeyValue String String -- from config files

type Expression a = PGP BuildParameter a

type Rewrite a = Expression a -> Expression a

type ArgsExpression = Expression String

alternatives :: (b -> BuildParameter) -> [b] -> Rewrite a
alternatives p bs = disjuction bs (Parameter . p)

whenPackages :: [FilePath] -> Rewrite a
whenPackages = alternatives WhenPackage

whenBuilders :: [Builder] -> Rewrite a
whenBuilders = alternatives WhenBuilder

whenStages :: [Stage] -> Rewrite a
whenStages = alternatives WhenStage

unlessStage :: Stage -> Rewrite a
unlessStage stage = Condition (Not $ Parameter $ WhenStage stage)

whenWays :: [Way] -> Rewrite a
whenWays = alternatives WhenWay

whenFiles :: [FilePath] -> Rewrite a
whenFiles = alternatives WhenFile

whenKeyValues :: String -> [String] -> Rewrite a
whenKeyValues key = alternatives (WhenKeyValue key)

whenKeyValue :: String -> String -> Rewrite a
whenKeyValue key value = whenKeyValues key [value]

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
