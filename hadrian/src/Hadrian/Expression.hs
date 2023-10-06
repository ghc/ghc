{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Hadrian.Expression (
    -- * Expressions
    Expr, Predicate, Args,

    -- ** Construction and modification
    expr, exprIO, arg, remove,

    -- ** Predicates
    (?), input, inputs, output, outputs,
    ToPredicate(..),

    -- ** Evaluation
    interpret, interpretInContext,

    -- * Convenient accessors
    getBuildRoot, getContext, getBuilder, getOutputs, getInputs, getInput, getOutput
    ) where

import Control.Monad.Extra
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Development.Shake

import qualified Hadrian.Target as Target
import Hadrian.Target (Target, target)
import Hadrian.Utilities

-- | 'Expr' @c b a@ is a computation that produces a value of type 'Action' @a@
-- and can read parameters of the current build 'Target' @c b@.
newtype Expr c b a = Expr (ReaderT (Target c b) Action a)
    deriving (Applicative, Functor, Monad)

instance Semigroup a => Semigroup (Expr c b a) where
    Expr x <> Expr y = Expr $ (<>) <$> x <*> y

instance Monoid a => Monoid (Expr c b a) where
    mempty  = pure mempty

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
(?) :: (Monoid a, ToPredicate p c b) => p -> Expr c b a -> Expr c b a
p ? e = do
    bool <- toPredicate p
    if bool then e else mempty

instance ToPredicate Bool c b where
    toPredicate = pure

instance ToPredicate p c b => ToPredicate (Action p) c b where
    toPredicate = toPredicate . expr

instance (c ~ c', b ~ b', ToPredicate p c' b') => ToPredicate (Expr c b p) c' b' where
    toPredicate p = toPredicate =<< p

-- | Interpret a given expression according to the given 'Target'.
interpret :: Target c b -> Expr c b a -> Action a
interpret target (Expr e) = runReaderT e target

-- | Interpret a given expression by looking only at the given 'Context'.
interpretInContext :: c -> Expr c b a -> Action a
interpretInContext c = interpret $ target c
    (error "contextOnlyTarget: builder not set")
    (error "contextOnlyTarget: inputs not set" )
    (error "contextOnlyTarget: outputs not set")

-- | Get the directory of build results.
getBuildRoot :: Expr c b FilePath
getBuildRoot = expr buildRoot

-- | Get the current build 'Context'.
getContext :: Expr c b c
getContext = Expr $ asks Target.context

-- | Get the 'Builder' for the current 'Target'.
getBuilder :: Expr c b b
getBuilder = Expr $ asks Target.builder

-- | Get the input files of the current 'Target'.
getInputs :: Expr c b [FilePath]
getInputs = Expr $ asks Target.inputs

-- | Run 'getInputs' and check that the result contains one input file only.
getInput :: (Show b, Show c) => Expr c b FilePath
getInput = Expr $ do
    target <- ask
    fromSingleton ("Exactly one input file expected in " ++ show target) <$>
        asks Target.inputs

-- | Get the files produced by the current 'Target'.
getOutputs :: Expr c b [FilePath]
getOutputs = Expr $ asks Target.outputs

-- | Run 'getOutputs' and check that the result contains one output file only.
getOutput :: (Show b, Show c) => Expr c b FilePath
getOutput = Expr $ do
    target <- ask
    fromSingleton ("Exactly one output file expected in " ++ show target) <$>
        asks Target.outputs

-- | Does any of the input files match a given pattern?
input :: FilePattern -> Predicate c b
input f = any (f ?==) <$> getInputs

-- | Does any of the input files match any of the given patterns?
inputs :: [FilePattern] -> Predicate c b
inputs = anyM input

-- | Does any of the output files match a given pattern?
output :: FilePattern -> Predicate c b
output f = any (f ?==) <$> getOutputs

-- | Does any of the output files match any of the given patterns?
outputs :: [FilePattern] -> Predicate c b
outputs = anyM output
