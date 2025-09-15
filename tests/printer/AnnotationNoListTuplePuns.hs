{-# language NoListTuplePuns #-}
{-# OPTIONS -ddump-parsed-ast #-}
module AnnotationNoListTuplePuns where

type A =
  -- comment pre A
  [
    -- comment inside A
  ]
  -- comment post A

type B =
  -- comment pre B
  [
    -- comment inside B
    Bool
  ]
  -- comment post B
