{-# LANGUAGE TypeFamilies #-}

module T11623 where

type family T where { Maybe T = Int }
