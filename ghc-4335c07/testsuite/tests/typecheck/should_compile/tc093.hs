module ShouldSucceed where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

data State c a = State (c -> (a,c))

unState :: State c a -> (c -> (a,c))
unState (State x) = x

unitState :: a -> State c a
unitState a = State (\s0 -> (a,s0))

bindState :: State c a -> (a -> State c b) -> State c b
bindState m k = State (\s0 -> let (a,s1) = (unState m) s0
                                  (b,s2) = (unState (k a)) s1 
                              in (b,s2))

instance Eq c => Functor (State c) where
    fmap = liftM

instance Eq c => Applicative (State c) where
    pure = return
    (<*>) = ap

instance Eq c => Monad (State c) where
    return = unitState 
    (>>=)  = bindState 

data TS = TS { vs::Int } deriving (Show,Eq)

type St a = State TS a

foo :: Int -> St Int  -- it works if this line is not given
foo x = return x
