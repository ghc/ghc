module T8603 where

import Control.Monad
import Data.Functor
import Control.Monad.Trans.Class( lift )
import Control.Monad.Trans.State( StateT )

newtype RV a = RV { getPDF :: [(Rational,a)] } deriving (Show, Eq)

instance Functor RV where
  fmap f = RV . map (\(x,y) -> (x, f y)) . getPDF

instance Applicative RV where
  pure = return
  (<*>) = ap

instance Monad RV where
  return x = RV [(1,x)]
  rv >>= f = RV $
    do (p,a) <- getPDF rv
       guard (p > 0)
       (q,b) <- getPDF $ f a
       guard (q > 0)
       return (p*q, b)

type RVState s a = StateT s RV a

uniform :: [a] -> RV a
uniform x = RV [(1/fromIntegral (length x), y) | y <- x]

testRVState1 :: RVState s Bool
testRVState1
  = do prize <- lift uniform [1,2,3]
       return False

-- lift :: (MonadTrans t, Monad m) => m a -> t m a
