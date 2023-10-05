{-# LANGUAGE FlexibleContexts #-}

module T8555 where
import Data.Coerce

foo :: Coercible [a] [b] => a -> b
foo = coerce
