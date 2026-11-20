{-# LANGUAGE CPP, DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
#ifdef DUPLICATE
{-# LANGUAGE ExperimentalDependentTypes #-}
#else
{-# LANGUAGE NoExperimentalDependentTypes #-}
#endif
module Main where

import Data.Type.Equality
import ErasedStream (Stream, fromList)
import Data.Proxy (Proxy(..))
import StreamPreview (prefix, preview)

type family IsBoolean p where
    IsBoolean (Stream Bool) = 'True
    IsBoolean p = 'False

class FirstElement a stream where firstElement :: Proxy a -> stream -> Int
instance {-# OVERLAPPABLE #-} FirstElement a (Stream a) where firstElement _ _ = 1
instance {-# OVERLAPPING #-} FirstElement Bool (Stream Bool) where firstElement _ _ = 2

class StreamOnly stream where streamOnly :: stream -> Int
instance StreamOnly (Stream a) where streamOnly _ = 0

forward :: forall a. Stream a -> [a]
forward xs = prefix 1 xs

#if defined(UNDETERMINED)
undetermined :: Int
undetermined = streamOnly @(Stream Bool) (fromList [True])
#elif defined(CONFLICT)
conflict :: Int
conflict = firstElement @Bool @(Stream Char) Proxy (fromList "x")
#elif defined(STUCK)
type family Element a
wrong :: IsBoolean (Stream (Element Int)) :~: 'False
wrong = Refl
#elif defined(OPEN)
wrong :: proxy a -> IsBoolean (Stream a) :~: 'False
wrong _ = Refl
#elif defined(INVERSE)
wrong :: Stream a :~: Stream b -> a :~: b
wrong Refl = Refl
#elif defined(REPEATED)
class Sample a
instance Sample (a,a) => Sample (Maybe (Stream a))
#elif defined(DUPLICATE)
class Sample a
type First = exists i. (i, i -> Bool)
type Second = exists j. (j, j -> Bool)
instance Sample First
instance Sample Second
#endif

choices :: (IsBoolean (Stream Bool) :~: 'True, IsBoolean (Stream Char) :~: 'False)
choices = (Refl, Refl)

main :: IO ()
main = print (preview (fromList ('G' : 'H' : error "unused character tail")),
              preview (fromList (True : error "unused Boolean tail")),
              preview @(Either Char (Stream Char)) (Left 'x'), choices,
              forward @Int (fromList [7]), prefix 1 (fromList "a") :: String,
              firstElement @Bool @(Stream Bool) Proxy (fromList [True]),
              firstElement @Char @(Stream Char) Proxy (fromList "x"))
