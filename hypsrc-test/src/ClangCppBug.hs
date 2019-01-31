{-# LANGUAGE CPP #-}
module ClangCppBug where

foo :: Int
foo = 1

-- Clang doesn't mind these:
#define BAX 2
{-# INLINE bar #-}

bar :: Int
bar = 3

-- But it doesn't like this:
{-# RULES
"bar/qux" bar = qux
"qux/foo" qux = foo
  #-}

qux :: Int
qux = 88
