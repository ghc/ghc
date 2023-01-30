module T22707 where

newtype Cont o i a = Cont {runCont ::(a -> i) -> o }

t1:: Cont (i2 -> o) i1 a -> Cont o i2 (a -> i1)
t1 c = Cont $ \ati1tti2 -> (runCont c) (ati1tti2 $ \a -> evalCont (t1 c) >>== \ati1 -> return ati1 a )

evalCont:: Cont o a a -> o
evalCont c = (runCont c)id

instance Monad (Cont p p) where
  return a = Cont ($ a)
  (>>=) = (>>==)

class PMonad m where
  (>>==):: m p q a -> (a -> m q r b) -> m p r b

instance PMonad Cont where
 (Cont cont) >>== afmb = Cont $ \bti -> cont $ \a -> (runCont . afmb) a bti

main:: IO ()
main = putStrLn "bug"
