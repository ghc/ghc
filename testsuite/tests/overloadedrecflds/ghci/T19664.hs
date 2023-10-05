{-# LANGUAGE QuasiQuotes, TemplateHaskell, DuplicateRecordFields #-}

module T19664 where

import Language.Haskell.TH

left = undefined

([] <$) $ runIO . print =<< [d|
  data Tree
    = Node { left :: Tree, right :: Tree }
    | Leaf { value :: Int }
    deriving Show
  |]
