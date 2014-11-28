{-# LANGUAGE NamedWildcards, ScopedTypeVariables #-}
module TidyClash2 where

barry :: forall w_. _ -> _ -> w_
barry (x :: _) (y :: _) = undefined :: _
