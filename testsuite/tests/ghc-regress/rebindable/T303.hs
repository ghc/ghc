{-# LANGUAGE RebindableSyntax #-}

-- Trac #303

module T where
import qualified Prelude as P

class IxMonad m where
    return :: a -> m i i a
    (>>=) :: m i j a -> (a -> m j k b) -> m i k b
    (>>)  :: m i j a -> m j k b -> m i k b
    m >> n = m >>= \_ -> n

    fail :: P.String -> m i j a
    fail s = P.error s

data T a b c = T
instance IxMonad T where
    return _ = T
    m >>= f  = T
    fail _   = T

testM :: T (a,b) b a
testM = T

test1 = testM >>= \x -> return x

test2  = do
   x <- testM
   return x
