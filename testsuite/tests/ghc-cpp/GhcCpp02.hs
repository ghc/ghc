{-# LANGUAGE GHC_CPP #-}
module GhcCpp02 where

foo =
#else
    13
#endif

#define EXISTENT_MACRO(X) 2 + NONEXISTENT_MACRO(X)

-- Note the evaluation error is reported on the *expanded* macro
#if EXISTENT_MACRO(4)
bar = 3
#endif
