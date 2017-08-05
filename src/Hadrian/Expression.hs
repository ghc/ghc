{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Hadrian.Expression (
    -- * Expressions
    Expr, Predicate, Args,

    -- ** Construction and modification
    expr, exprIO, arg, remove, (?),

    -- ** Evaluation
    interpret, interpretInContext,

    -- * Convenient accessors
    getContext, getBuilder, getOutputs, getInputs, getInput, getOutput, getSingleton
    ) where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Semigroup
import Development.Shake

import Hadrian.Target

-- | 'Expr' @c b a@ is a computation that produces a value of type 'Action' @a@
-- and can read parameters of the current build 'Target' @c b@.
newtype Expr c b a = Expr (ReaderT (Target c b) Action a)
    deriving (Applicative, Functor, Monad)

instance Semigroup a => Semigroup (Expr c b a) where
    Expr x <> Expr y = Expr $ (<>) <$> x <*> y

-- TODO: The 'Semigroup a' constraint will at some point become redundant.
instance (Semigroup a, Monoid a) => Monoid (Expr c b a) where
    mempty  = pure mempty
    mappend = (<>)

-- | Expressions that compute a Boolean value.
type Predicate c b = Expr c b Bool

-- | Expressions that compute lists of arguments to be passed to builders.
type Args c b = Expr c b [String]

-- | Lift actions independent from the current build 'Target' into the 'Expr'
-- monad.
expr :: Action a -> Expr c b a
expr = Expr . lift

-- | Lift IO computations independent from the current build 'Target' into the
-- 'Expr' monad.
exprIO :: IO a -> Expr c b a
exprIO = Expr . liftIO

-- | Remove given elements from a list expression.
remove :: Eq a => [a] -> Expr c b [a] -> Expr c b [a]
remove xs e = filter (`notElem` xs) <$> e

-- | Add a single argument to 'Args'.
arg :: String -> Args c b
arg = pure . pure

-- | Values that can be converted to a 'Predicate'.
class ToPredicate p c b where
    toPredicate :: p -> Predicate c b

infixr 3 ?

-- | Apply a predicate to an expression.
(?) :: (Monoid a, Semigroup a, ToPredicate p c b) => p -> Expr c b a -> Expr c b a
p ? e = do
    bool <- toPredicate p
    if bool then e else mempty

instance ToPredicate (Predicate c b) c b where
    toPredicate = id

instance ToPredicate Bool c b where
    toPredicate = pure

instance ToPredicate (Action Bool) c b where
    toPredicate = expr

-- | Interpret a given expression according to the given 'Target'.
interpret :: Target c b -> Expr c b a -> Action a
interpret target (Expr e) = runReaderT e target

-- | Interpret a given expression by looking only at the given 'Context'.
interpretInContext :: c -> Expr c b a -> Action a
interpretInContext c = interpret $ target c
    (error "contextOnlyTarget: builder not set")
    (error "contextOnlyTarget: inputs not set" )
    (error "contextOnlyTarget: outputs not set")

-- | Get the current build 'Context'.
getContext :: Expr c b c
getContext = Expr $ asks context

-- | Get the 'Builder' for the current 'Target'.
getBuilder :: Expr c b b
getBuilder = Expr $ asks builder

-- | Get the input files of the current 'Target'.
getInputs :: Expr c b [FilePath]
getInputs = Expr $ asks inputs

-- | Run 'getInputs' and check that the result contains one input file only.
getInput :: (Show b, Show c) => Expr c b FilePath
getInput = Expr $ do
    target <- ask
    getSingleton ("Exactly one input file expected in " ++ show target) <$> asks inputs

-- | Get the files produced by the current 'Target'.
getOutputs :: Expr c b [FilePath]
getOutputs = Expr $ asks outputs

-- | Run 'getOutputs' and check that the result contains one output file only.
getOutput :: (Show b, Show c) => Expr c b FilePath
getOutput = Expr $ do
    target <- ask
    getSingleton ("Exactly one output file expected in " ++ show target) <$> asks outputs

-- | Extract a value from a singleton list, or raise an error if the list does
-- not contain exactly one value.
getSingleton :: String -> [a] -> a
getSingleton _ [res] = res
getSingleton msg _   = error msg
