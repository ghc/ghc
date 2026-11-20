{-# LANGUAGE CPP, DataKinds, FlexibleContexts, FlexibleInstances, GADTs, TypeApplications, TypeFamilies, TypeOperators #-}
#ifdef DUPLICATE
{-# LANGUAGE ExperimentalDependentTypes #-}
#else
{-# LANGUAGE NoExperimentalDependentTypes #-}
#endif
module Main where

import Data.Type.Equality
import ErasedStream (Stream, fromList)
import StreamPreview (preview)

type family IsBoolean p where
    IsBoolean (Stream Bool) = 'True
    IsBoolean p = 'False

#if defined(STUCK)
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
              preview @(Either Char (Stream Char)) (Left 'x'), choices)
