module Deprecation
{-# Deprecated ["This is a module \"deprecation\"",
             "multi-line",
             "with unicode: Frère" ] #-}
   ( foo )
 where

{-# DEPRECATEd   foo
         ["This is a multi-line",
          "deprecation message",
          "for foo"] #-}
foo :: Int
foo = 4

{-# DEPRECATED withBool        "The C2HS module will soon stop providing unnecessary\nutility functions. Please use standard FFI library functions instead." #-}
