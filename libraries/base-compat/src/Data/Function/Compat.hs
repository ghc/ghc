{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Function.Compat (
  module Base
, (&)
) where
import Data.Function as Base

#if !(MIN_VERSION_base(4,8,0))
infixl 1 &

-- | '&' is a reverse application operator.  This provides notational
-- convenience.  Its precedence is one higher than that of the forward
-- application operator '$', which allows '&' to be nested in '$'.
--
-- /Since: 4.8.0.0/
(&) :: a -> (a -> b) -> b
x & f = f x

#endif
