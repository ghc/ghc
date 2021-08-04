{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -Wall #-}

-- http://www.mail-archive.com/haskell@haskell.org/msg17244.html
module History where

import Control.Category
import Control.Comonad
import Data.Foldable hiding (sum)
import Data.Traversable
import Prelude hiding (id,(.),sum)

infixl 4 :>

data History a = First a | History a :> a
  deriving (Functor, Foldable, Traversable, Show)

runHistory :: (History a -> b) -> [a] -> [b]
runHistory _ [] = []
runHistory f (a0:as0) = run (First a0) as0
  where
    run az [] = [f az]
    run az (a:as) = f az : run (az :> a) as

instance Comonad History where
  extend f w@First{} = First (f w)
  extend f w@(as :> _) = extend f as :> f w
  extract (First a) = a
  extract (_  :> a) = a

instance ComonadApply History where
  First f   <@> First a   = First (f a)
  (_  :> f) <@> First a   = First (f a)
  First f   <@> (_  :> a) = First (f a)
  (fs :> f) <@> (as :> a) = (fs <@> as) :> f a

fby :: a -> History a -> a
a `fby` First _ = a
_ `fby` (First b :> _) = b
_ `fby` ((_ :> b) :> _) = b

pos :: History a -> Int
pos dx = wfix $ dx $> fby 0 . fmap (+1)

sum :: Num a => History a -> a
sum dx = extract dx + (0 `fby` extend sum dx)

diff :: Num a => History a -> a
diff dx = extract dx - fby 0 dx

ini :: History a -> a
ini dx = extract dx `fby` extend ini dx

fibo :: Num b => History a -> b
fibo d = wfix $ d $> fby 0 . extend (\dfibo -> extract dfibo + fby 1 dfibo) 

fibo' :: Num b => History a -> b
fibo' d = fst $ wfix $ d $> fby (0, 1) . fmap (\(x, x') -> (x',x+x'))

plus :: Num a => History a -> History a -> History a
plus = liftW2 (+)
