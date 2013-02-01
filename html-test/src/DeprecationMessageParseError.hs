-- |
-- What is tested here:
--
-- * If parsing of a deprecation message fails, the message is included
--   verbatim.
--
module DeprecationMessageParseError where

-- | some documentation for foo
foo :: Int
foo = 23
{-# DEPRECATED foo "use @bar instead" #-}
