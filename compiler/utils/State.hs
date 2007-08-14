
module State where

newtype State s a
	= State
	{ runState :: s -> (# a, s #) }

instance Monad (State s) where
    return x	= State $ \s -> (# x, s #)
    m >>= n	= State $ \s ->
    			case runState m s of
			  (# r, s' #)	-> runState (n r) s'

get ::	State s s
get	= State $ \s -> (# s, s #)

put :: 	s -> State s ()
put s'	= State $ \s -> (# (), s' #)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (# (), f s #)

evalState :: State s a -> s -> a
evalState s i
	= case runState s i of
		(# a, s' #)	-> a

execState :: State s a -> s -> s
execState s i
	= case runState s i of
		(# a, s' #) 	-> s'
