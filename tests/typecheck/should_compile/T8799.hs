{-# LANGUAGE FlexibleContexts #-}

module T8555 where
import Data.Coerce

foo :: Coercible a b => b -> a
foo = coerce

bar :: (Coercible a b, Coercible b c) => b -> c -> a
bar b c = coerce c
