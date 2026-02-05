{-# LANGUAGE OverloadedStrings #-}

module T26860ppr_overloaded where

-- Test that the error message containing the string literal is well-formatted.
-- See also: parser/should_fail/MultilineStringsError
x :: Int
x = "first line \
    \asdf\n\
    \second line"

