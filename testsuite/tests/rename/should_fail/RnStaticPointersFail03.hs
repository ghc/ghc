{-# LANGUAGE StaticPointers #-}

module RnStaticPointersFail03 where

f x = static (x . id)

f0 x = static (k . id)
  where
    k = const (const () x)

f1 x = static (k . id)
  where
    k = id
