{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module T13242 where

import Data.STRef
import Control.Monad.ST

data A = forall a. A a

st :: ST s ()
st = do
      A _ <- pure $ A True
      ref <- newSTRef 1
      readSTRef ref
      pure ()
