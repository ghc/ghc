{-# LANGUAGE ExperimentalDependentTypes #-}
module NativeTypeCaseFallback where
type Bad a = case a of
  True -> Int
  x -> Int
