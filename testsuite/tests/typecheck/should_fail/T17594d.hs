{-# LANGUAGE TypeAbstractions #-}

module T17594d where

-- We want this code to check, but currently it doesn't
-- because inference mode for type abstractions is not
-- implemented yet
id' @t x = x :: t
