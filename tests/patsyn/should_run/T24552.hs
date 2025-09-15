{-# language PatternSynonyms #-}

module Main where

import Prelude
import qualified Prelude as P

pattern MyNothing :: Maybe a
pattern MyNothing <- Nothing where MyNothing = Nothing

pattern MyJust :: a -> Maybe a
pattern MyJust x <- Just x where MyJust = Just

main = print $ do MyJust x <- [MyNothing, MyJust ()] ; return x
