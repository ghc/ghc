
{-# LANGUAGE PatternGuards #-}

module M where

f | Just x <- undefined,
    let y = x,
    undefined x y
  = ()

