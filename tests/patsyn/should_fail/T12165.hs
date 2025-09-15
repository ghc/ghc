{-# LANGUAGE PatternSynonyms #-}
module ShouldFail where

pattern P :: a -> b -> Maybe (a,b)
pattern P :: foo => bar => blah -> urgh
pattern P x y = Just (x, y)
