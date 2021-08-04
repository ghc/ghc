{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Ratio.Compat (
  module Base
#if MIN_VERSION_base(4,4,0) && !(MIN_VERSION_base(4,9,0))
, denominator
, numerator
#endif
) where

#if !(MIN_VERSION_base(4,4,0)) || MIN_VERSION_base(4,9,0)
import Data.Ratio as Base
#else
import Data.Ratio as Base hiding (
    denominator
  , numerator
  )
import GHC.Real (Ratio(..))
#endif

#if MIN_VERSION_base(4,4,0) && !(MIN_VERSION_base(4,9,0))
-- | Extract the numerator of the ratio in reduced form:
-- the numerator and denominator have no common factor and the denominator
-- is positive.
numerator :: Ratio a -> a
numerator (x :% _) = x

-- | Extract the denominator of the ratio in reduced form:
-- the numerator and denominator have no common factor and the denominator
-- is positive.
denominator :: Ratio a -> a
denominator (_ :% y) =  y
#endif
