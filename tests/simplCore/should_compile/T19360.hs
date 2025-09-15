{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module T19360 where

import GHC.Exts (oneShot)
import Data.Typeable (Typeable, cast)

newtype SDoc = SDoc' (Bool -> Bool)

data EpAnn = EpAnn

showAstData :: D a => Bool -> a -> SDoc
showAstData z = showAstData'
  where
    showAstData' :: D a => a -> SDoc
    showAstData' x =
      case cast x of
         Just (a :: EpAnn) ->
           case z of
             True -> showAstData' a
             False -> SDoc' (oneShot (\_ -> False))
         Nothing -> gmapQr showAstData' x

class Typeable a => D a where
  gmapQr :: forall r. (forall d. D d => d -> r) -> a -> r

instance D EpAnn where
  gmapQr g EpAnn = g EpAnn
