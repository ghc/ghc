{-# LANGUAGE DefaultSignatures #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-} -- Not required with GHC 8.0.1.

module T12151 where

class Put a where
    put :: a

    default put :: t  -- This should be `a` instead of `t`
    put = undefined
