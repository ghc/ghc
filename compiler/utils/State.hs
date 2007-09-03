{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/CodingStyle#Warnings
-- for details

module State where

newtype State s a
	= State
	{ runState' :: s -> (# a, s #) }

instance Monad (State s) where
    return x	= State $ \s -> (# x, s #)
    m >>= n	= State $ \s ->
    			case runState' m s of
			  (# r, s' #)	-> runState' (n r) s'

get ::	State s s
get	= State $ \s -> (# s, s #)

gets :: (s -> a) -> State s a
gets f 	= State $ \s -> (# f s, s #)

put :: 	s -> State s ()
put s'	= State $ \s -> (# (), s' #)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (# (), f s #)


evalState :: State s a -> s -> a
evalState s i
	= case runState' s i of
		(# a, s' #)	-> a


execState :: State s a -> s -> s
execState s i
	= case runState' s i of
		(# a, s' #) 	-> s'


runState :: State s a -> s -> (a, s)
runState s i
	= case runState' s i of
		(# a, s' #)	-> (a, s')


mapAccumLM
	:: Monad m
	=> (acc -> x -> m (acc, y))	-- ^ combining funcction
	-> acc				-- ^ initial state
	-> [x]				-- ^ inputs
	-> m (acc, [y])			-- ^ final state, outputs

mapAccumLM _ s []	= return (s, [])
mapAccumLM f s (x:xs)
 = do
	(s1, x')	<- f s x
	(s2, xs')	<- mapAccumLM f s1 xs
	return		(s2, x' : xs')
