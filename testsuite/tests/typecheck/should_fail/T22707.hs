module T22707 where

newtype Cont o i a = Cont {runCont ::(a -> i) -> o }

t1:: Cont (i2 -> o) i1 a -> Cont o i2 (a -> i1)
t1 c = Cont $ \ati1tti2 -> (runCont c) (ati1tti2 $ \xa -> evalCont (t1 c) >>== \ati1 -> return ati1 xa )


{-  This is a complicated and confused program.
    We end up unifying
        m0 p0 q0 b0 ~ (->) LiftedRep LiftedRep t1 t2
    which unifies q0~LiftedRep, and m0 with the (polymorphically-kinded)
      (->) LiftedRep
    Getting a decent error message out of this mess is a challenge!

  runCont :: Cont oo ii aa -> ((aa -> ii) -> oo)
  (>>==)  :: forall k (m:k->k->*->*) (p:k) (q:k) a.
             PMonad m => m p q a -> (a -> m q r b) -> m p r b

  c :: Cont (i2 -> o) i1 a
  Result type: Cont o i2 (a -> i1)
  Arg of cont: ((a->i1) -> i2) -> o
  ati1tti2 :: (a->i1) -> i2
  runCont c :: (a -> i1) -> i2 -> o
  xa :: a -> i1
  t1 c :: Cont o i2 (a -> i1)
  evalCont (t1 c) :: o
  (>>==) @k0,m0,p0,q0,a0,r0) (evalCont (t1 c))
     [W] o ~ m0 p0 q0 a0
  ati1 :: a10
  return @m10 @a10 ati1 xa :: a11
    [W] m10 a10 ~ (a -> i1) -> a11
    => [W] m10 ~ (->) @LiftedRep @LiftedRep (a -> i1) 
       [W] a10 ~ a11
  Result of (\ati1 -> ..)
    (>>==) @m0,p0,q0,a0) (evalCont (t1 c)) (\ati1 -> ..) :: m0 p0 r0 b0
    [W] a11 ~ m0 q0 r0 b0
  Result of (>>==) call
    [W] i1 ~ m0 p0 r0 b0
-}

evalCont:: Cont o a a -> o
evalCont c = (runCont c)id

instance Functor (Cont p p) where
instance Applicative (Cont p p) where

instance Monad (Cont p p) where
  return a = Cont ($ a)
  (>>=) = (>>==)

class PMonad m where
  (>>==):: m p q a -> (a -> m q r b) -> m p r b

instance PMonad Cont where
 (Cont cont) >>== afmb = Cont $ \bti -> cont $ \a -> (runCont . afmb) a bti

main:: IO ()
main = putStrLn "bug"
