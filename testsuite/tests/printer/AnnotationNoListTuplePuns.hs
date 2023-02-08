{-# language NoListTuplePuns #-}
module AnnotationNoListTuplePuns where

type A =
  -- comment pre
  [
    -- comment inside
  ]
  -- comment post

type B =
  -- comment pre
  [
    -- comment inside
    Bool
  ]
  -- comment post
