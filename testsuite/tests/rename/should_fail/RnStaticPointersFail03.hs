{-# LANGUAGE StaticPointers #-}

module RnStaticPointersFail03 where

f x = static (x . id)
