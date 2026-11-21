{-# LANGUAGE DataKinds, ExperimentalDependentTypes, RankNTypes #-}
module LexicalRetainedReader (RetainedReader, Display, worker, getText) where

class Show a => Display a
instance Display Bool
instance Display Char

data Tagged (tag :: Bool) = Tagged String

type RetainedReader = foreach (tag :: Bool) -> forall a. Display a => a -> Tagged tag

{-# NOINLINE worker #-}
worker :: foreach (tag :: Bool) -> forall a. Show a => a -> Tagged tag
worker _ value = Tagged (show value)

getText :: Tagged tag -> String
getText (Tagged text) = text
