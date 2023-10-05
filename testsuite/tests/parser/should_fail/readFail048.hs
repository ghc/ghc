-- Test for trac #314

{-|
/*
  This
  uses
  up
  some
  lines
  The
  following
  pragmas
  should
  not
  be
  parsed
 */
# 23
#pragma

-}

module ShouldFail where

type_error = "Type error on line 25":"Type error on line 25"
