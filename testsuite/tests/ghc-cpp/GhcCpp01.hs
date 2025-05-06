{-# LANGUAGE GHC_CPP #-}
-- {-# OPTIONS -ddump-ghc-cpp -dkeep-comments #-}
module GhcCpp01 where

-- Check leading whitespace on a directive
   # define FOO(A,B) A + B
#define FOO(A,B,C) A + B + C
#if FOO(1,FOO(3,4)) == 8

-- a comment
x = 1
#else
x = 5
#endif

#if defined(BAR) || defined FOO
y = 1
#endif

#undef FOO
#ifdef FOO
complete junk!
#endif

-- nested undef
#define AA
#if 0
#undef AA
#endif

#ifdef AA
aa = 1
#endif

-- undef and rewrite base name only
#define MIN_VERSION_Cabal(a,b,c) 1

#ifdef MIN_VERSION_Cabal
#undef CH_MIN_VERSION_Cabal
#define CH_MIN_VERSION_Cabal MIN_VERSION_Cabal
#endif

#if CH_MIN_VERSION_Cabal(1,22,0)
z = 1
#endif
