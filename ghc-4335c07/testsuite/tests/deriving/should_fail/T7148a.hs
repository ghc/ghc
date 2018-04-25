{-# LANGUAGE TypeFamilies, ScopedTypeVariables,
             GeneralizedNewtypeDeriving #-}

module T7148a where

import Control.Monad.ST
data Proxy a = Proxy
type family Result a b
 
class Convert a where
  coerce :: Proxy b -> a -> Result a b
 
newtype SAFE a = SAFE a
type instance Result (SAFE a) b = a
 
instance Convert (SAFE a) where
  coerce _ (SAFE a) = a

newtype IS_NO_LONGER a = IS_NO_LONGER a deriving Convert
type instance Result (IS_NO_LONGER a) b = b

--infered type is 
unsafeCoerce :: forall a b. a -> b
unsafeCoerce = coerce (Proxy :: Proxy b) . IS_NO_LONGER . SAFE

--use it safely
id' :: a -> a
id' = unsafeCoerce
 
--segfault (with high probability)
crash :: segfault 
crash = unsafeCoerce . tail . tail . tail . unsafeCoerce $ True


--time for side effects
unsafePerformIO :: IO a -> a
unsafePerformIO x = runST $ unsafeCoerce x