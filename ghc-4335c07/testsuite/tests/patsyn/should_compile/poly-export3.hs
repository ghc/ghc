{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}

-- Testing polykindedness

module Foo ( A(P) ) where

data A a = A

pattern P = A
