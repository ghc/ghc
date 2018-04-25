-----------------------------------------------------------------------------
The parser monad.

(c) 2001 Simon Marlow
-----------------------------------------------------------------------------

> module ParseMonad where
> import Control.Monad(ap)

> data ParseResult a = OkP a | FailP String
> newtype P a = P (String -> Int -> ParseResult a)
> runP (P f) = f

> lineP :: P Int
> lineP = P $ \_ l -> OkP l

> instance Functor ParseResult where
>       fmap f (OkP a) = OkP (f a)
>       fmap f (FailP e) = FailP e

> instance Functor P where
>       fmap f m = P $ \s l -> fmap f (runP m s l)

> instance Applicative P where
>       pure = return
>       (<*>) = ap

> instance Monad P where
>	return m = P $ \ _ _ -> OkP m
>	m >>= k =  P $ \s l -> case runP m s l of
>		OkP a -> runP (k a) s l
>		FailP s -> FailP s
>	fail s = P $ \ _ _ -> FailP s
