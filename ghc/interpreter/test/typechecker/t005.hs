--!!! Test for error in type error message (fixed in Hugs 1.4)
module TyErr where

newtype StateMonad m s a = MkStateMonad (s -> (m (s, a)))

instance Monad m => Monad (StateMonad m s) where
    (>>=) (MkStateMonad fn1) f
	= MkStateMonad (\st -> (do res <- fn1 st
				   case res of
				       (st', res') -> extrStateMonad (f res') st'))
    return val = MkStateMonad (\st -> (return (st, val)))
			   
extrStateMonad (MkStateMonad f) = f

getState :: Monad m => StateMonad m s s
getState = MkStateMonad (\st -> return (st, st))

-- popIndentList :: StateMonad IO Int ()
popIndentList = 
    (do getState
	return ())
