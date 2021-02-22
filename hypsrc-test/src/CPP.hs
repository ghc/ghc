{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE CPP #-}
module CPP where

#define SOMETHING1

foo :: String
foo = {-  " single quotes are fine in block comments
          {- nested block comments are fine -}
       -} "foo"

#define SOMETHING2

bar :: String
bar = "block comment in a string is not a comment {- "

#define SOMETHING3

-- " single quotes are fine in line comments
-- {- unclosed block comments are fine in line comments

-- Multiline CPP is also fine
#define FOO\
  1

baz :: String
baz = "line comment in a string is not a comment --"
