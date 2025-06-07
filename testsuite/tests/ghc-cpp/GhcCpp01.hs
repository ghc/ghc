{-# LANGUAGE GHC_CPP #-}
{-# OPTIONS -ddump-ghc-cpp -dkeep-comments #-}
module GhcCpp01 where

-- Check leading whitespace on a directive
#     define FOO(A,B) A + B
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
#ifdef FOO /* Check for FOO */
complete junk!
#endif

-- nested undef
#define AA
#if /* hard code for now */ 0
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

{-
/* multi-line
   cpp-style comment */
{- Haskell comment
   /* ignores cpp comments, so unclosed is fine -}
-}

-- CPP directives in pragmas -----------------

-- No directive
{-# RULES
"foldg/Empty"   forall e v o c. foldg e v o c Empty = e
#-}
foldg = undefined
data Empty = Empty

-- With directive
{-# RULES
"Lazy Bitstream streamChunks/unstreamChunks fusion"
    forall s. streamChunks s = s
#if 0
"Lazy Bitstream unstreamChunks/streamChunks fusion"
    forall v. unId (unstreamChunks (streamChunks v)) = v
#endif
#-}
unstreamChunks = undefined
streamChunks = undefined
