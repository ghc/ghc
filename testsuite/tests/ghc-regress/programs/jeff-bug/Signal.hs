module Signal where
import Monad
import LazyST
import List
import Random
import IOExts
import Int

-- infixl 9 *!

-- Begin Signature -------------------------------------------------------
{-
  In essence Hawk is Haskell with built-in Lustre-like signals.  The
  rest are libraries built upon this structure.  In the event of
  circuit synthesis, the Signal type and its operators represent
  the residule of elaboration (partial-evaluation).
-}

{-data Signal a-}

infix  4 *==, */=, *<, *<=, *>=, *>
infixr 3 *&&
infixr 2 *||
infixr 5  *:, *++
infixl 9 `at`
infixr 0 `delay`
infixr 0 `before`

at     :: Signal a -> Int -> a

-- [1,3,2] `before` <10 .. > = <1,3,2,10 .. >
before :: [a] -> Signal a -> Signal a

-- loop s f, apply f to s at each cycle, saving the state....
loop   :: Signal a -> (a -> ST st c)-> ST st (Signal c)

view   :: Signal a -> [a]

-- delay x <x1,x2 .. >  = <x,x1,x2 .. >
delay  :: a -> Signal a -> Signal a

-- if,then,else lifted on signals...
cond   :: Signal Bool -> Signal a -> Signal a -> Signal a

-- apply a function pointwise to a signal
lift0  :: a -> Signal a 
lift1  :: (a -> b) -> Signal a -> Signal b
lift2  :: (a->b->c)          -> Signal a -> Signal b -> Signal c
lift3  :: (a->b->c->d)       -> Signal a -> Signal b -> Signal c -> Signal d
lift4  :: (a->b->c->d->e)    -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
lift5  :: (a->b->c->d->e->f) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f

-- make a single signal of tuples out of tuple of signals
bundle2 :: (Signal a,Signal b) -> Signal (a,b)
bundle3 :: (Signal a,Signal b,Signal c) -> Signal (a,b,c)
bundle4 :: (Signal a,Signal b,Signal c,Signal d) -> Signal (a,b,c,d)
bundle5 :: (Signal a,Signal b,Signal c,Signal d,Signal e) -> Signal (a,b,c,d,e)
bundle6 :: (Signal a,Signal b,Signal c,Signal d,Signal e,Signal f) -> 
           Signal (a,b,c,d,e,f)
bundleList :: [Signal a] -> Signal [a]

-- make a tuple of signals from a signal of tuples
unbundle2 :: Signal (a,b)       -> (Signal a,Signal b)
unbundle3 :: Signal (a,b,c)     -> (Signal a,Signal b,Signal c)
unbundle4 :: Signal (a,b,c,e)   -> (Signal a,Signal b,Signal c,Signal e)
unbundle5 :: Signal (a,b,c,e,d) -> 
             (Signal a,Signal b,Signal c,Signal e,Signal d)

-- careful using this function.  the size of the list of the input
-- must be the same at each cycle.
unbundleList :: Signal [a] -> [Signal a]


-- corresponding functions lifted on signals.
(*==)           :: Eq a => Signal a -> Signal a -> Signal Bool
(*/=)           :: Eq a => Signal a -> Signal a -> Signal Bool
(*<)            :: Ord a => Signal a -> Signal a -> Signal Bool
(*<=)           :: Ord a => Signal a -> Signal a -> Signal Bool
(*>)            :: Ord a => Signal a -> Signal a -> Signal Bool
(*>=)           :: Ord a => Signal a -> Signal a -> Signal Bool
(*&&)           :: Signal Bool -> Signal Bool -> Signal Bool
(*||)           :: Signal Bool -> Signal Bool -> Signal Bool
(*++)           :: MonadPlus m => Signal (m a) -> Signal (m a) -> Signal (m a)
(*:)            :: Signal a -> Signal [a] -> Signal [a]


{-instance Eq a => Eq (Signal a)-}
{-instance Ord a => Ord (Signal a)-}
{-instance Enum a => Enum (Signal a)-}
{-instance Bounded a => Bounded (Signal a)-}
{-instance Num a => Num (Signal a)-}
{-instance Real a => Real (Signal a)-}
{-instance Integral a => Integral (Signal a)-}
{-instance Functor Signal where-}

-- make the trivial superscalar circuit from a scalar circuit by
-- applying it sequentially (left to right) 
superscalar :: (Signal a -> Signal b) -> Signal [a] -> Signal [b]

{-
   The following functions will give different streams for every use ----
   giving a form of non-determinism.

   NOTE that these functions should be used carefully.  They
   break referential transparency

-}

ints :: (Int,Int) -> Signal Int
integers :: (Integer,Integer) -> Signal Integer
-- End Signature ------------------------------------------------------



-- End Signature -------------------------------------------------------


delay i s = [i] `before` s
cond x y z = lift3 (\x y z -> if x then y else z) x y z




bundle2 (a,b)       = lift2 (,) a b
bundle3 (a,b,c)     = lift3 (,,) a b c
bundle4 (a,b,c,d)   = lift4 (,,,) a b c d
bundle5 (a,b,c,d,e) = lift5 (,,,,) a b c d e
bundle6 (a,b,c,d,e,f) = lift6 (,,,,,) a b c d e f

bundleList []     = lift0 []
bundleList (s:ss) = lift2 (:) s (bundleList ss)
 
unbundle2 s = (a,b)
        where a = lift1 (\(x,_) -> x) s
              b = lift1 (\(_,x) -> x) s
unbundle3 s = (a,b,c)
        where a = lift1 (\(x,_,_) -> x) s
              b = lift1 (\(_,x,_) -> x) s
              c = lift1 (\(_,_,x) -> x) s
unbundle4 s = (a,b,c,d)
        where a = lift1 (\(x,_,_,_) -> x) s
              b = lift1 (\(_,x,_,_) -> x) s
              c = lift1 (\(_,_,x,_) -> x) s
              d = lift1 (\(_,_,_,x) -> x) s
unbundle5 s = (a,b,c,d,e)
        where a = lift1 (\(x,_,_,_,_) -> x) s
              b = lift1 (\(_,x,_,_,_) -> x) s
              c = lift1 (\(_,_,x,_,_) -> x) s
              d = lift1 (\(_,_,_,x,_) -> x) s
              e = lift1 (\(_,_,_,_,x) -> x) s

    -- not particularily safe....
unbundleList s = map (nth s) szs
        where sz =  length $ head $ view s
              szs = [0 .. sz-1]
              nth s n = lift1 (!!n) s



instance Eq a => Eq (Signal a) where
    (==) = error "Cannot compare two signals for equality in general"

instance Ord a => Ord (Signal a) where
    compare = error "Cannot compare two signals in general"
    min     = lift2 min
    max     = lift2 max

instance Enum a => Enum (Signal a) where
    toEnum		  = lift0 . toEnum
    fromEnum		  = error "Trying to convert a Signal to an Enum"
    enumFrom		  = unbundleList . lift1 enumFrom
    enumFromThen n m 	  = unbundleList $ lift2 enumFromThen n m
    enumFromTo n m	  = unbundleList $ lift2 enumFromTo n m
    enumFromThenTo n n' m = unbundleList $ lift3 enumFromThenTo n n' m

instance Bounded a => Bounded (Signal a) where
    minBound = lift0 minBound
    maxBound = lift0 maxBound

instance Num a => Num (Signal a) where
    (+)		= lift2 (+)
    (-)		= lift2 (-)
    (*)		= lift2 (*)
    negate	= lift1 negate
    fromInteger	= lift0 . fromInteger
    --fromInt	= lift0 . fromInt
    abs		= lift1 abs
    signum	= lift1 signum

instance Real a => Real (Signal a) where
    toRational	= error "Trying to convert a signal to a Rational"

instance Integral a => Integral (Signal a) where
    quot	= lift2 quot
    rem		= lift2 rem
    div		= lift2 div
    mod		= lift2 mod
    x `quotRem` y	= unbundle2 (lift2 quotRem x y)
    x `divMod` y	= unbundle2 (lift2 divMod x y)
    toInteger	= error "Trying to convert a Signal to an Integer"
    --toInt		= error "Trying to convert a Signal to an Int"


------------------------------------------------------------------
-- definitons



(*==) = lift2 (==)
(*/=) = lift2 (/=)
(*<)  = lift2 (<)
(*<=) = lift2 (<=)
(*>)  = lift2 (>)
(*>=) = lift2 (>=)
(*&&) = lift2 (&&)
(*||) = lift2 (||)
(*++) = lift2 mplus
(*:)  = lift2 (:)

data Then = Then
data Else = Else

if' x Then y Else z = cond x y z

{-
if' ~(Sig x) Then ~(Sig y) Else ~(Sig z) = Sig (cond x y z)
  where
   cond  ~(x:xs) ~(y:ys) ~(z:zs) =
          let v = if x then y else z
              vs = cond  xs ys zs
          in (v:vs)
-}


then' = Then
else' = Else

------------------------------------------------------------------------
-- Specific to List implementation:


newtype Signal a = List [a]
        deriving Show

instance Functor Signal where
  fmap f ~(List as) = List (map f as)


at ~(List l) n = l!!n
before l ~(List l') = List (l ++ l')
loop ~(List l) f = do {l' <- mapM f l; return $ List l'}

lift0 x = List (repeat x)


----------------------------
-- UGH!!  the lazy pattern matching found in lazyMap  is pretty important when 
-- using signals to communicate with closely timed mutually dependant
-- signal transducers.  Probably, lazy versions of zipWith should be
-- used too.   
---  Byron , Sun Dec  6 16:46:09 PST 1998

lift1 f (List xs) = List $ lazyMap f xs
     where
     lazyMap f ~(x:xs) = f x :  lazyMap f xs

lift2 f ~(List as) ~(List bs)
     = List (zipWith f as bs)

lift3 f ~(List as) ~(List bs) ~(List cs)
     = List (zipWith3 f as bs cs)

lift4 f ~(List as) ~(List bs) ~(List cs) ~(List ds)
     = List (zipWith4 f as bs cs ds)

lift5 f ~(List as) ~(List bs) ~(List cs) ~(List ds) ~(List es)
     = List (zipWith5 f as bs cs ds es)

lift6 f ~(List as) ~(List bs) ~(List cs) ~(List ds) ~(List es) ~(List fs)
     = List (zipWith6 f as bs cs ds es fs)

view ~(List s) = s

superscalar f (List input) = List (chop lens output)
  where
  lens = map length input
  List output = f (List $ concat input)
  chop (n:ns) l = let (l',l'') = splitAt n l
                  in l' : chop ns l''


------------------------------------------------------------------------
-- Non-determinism

-- integers :: (Integer,Integer) -> Signal Integer
integers rng = List (unsafePerformIO (do { g <- newStdGen ;
					   return (randomRs rng g) }))

ints = fmap toInt . integers . toIntegers
    where
    toIntegers (x,y) = (toInteger x,toInteger y)



