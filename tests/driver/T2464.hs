{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -DTEST #-}
{-# OPTIONS_GHC -fffi #-} -- deprecation warning
#if defined(TEST)
{-# LANGUAGE EmptyDataDecls #-}
#endif

module Test2464 where
data T
