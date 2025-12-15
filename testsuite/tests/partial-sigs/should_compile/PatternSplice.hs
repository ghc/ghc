{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
module PatternSplice where

foo $( [p| (x :: _) |] ) = x
