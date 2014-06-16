{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -DTEST #-}
{-# OPTIONS_GHC -fffi #-} -- deprecation warning
#ifdef TEST
{-# LANGUAGE EmptyDataDecls #-}
#endif
#if __GLASGOW_HASKELL__ < 610
{-# INCLUDE "foo.h" #-} -- would generate a deprecation warning if enabled
#endif

module Test2464 where
data T
