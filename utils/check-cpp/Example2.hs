{-# LANGUAGE GhcCPP #-}
module Example2 where

/* package ghc-exactprint-1.7.0.1 */
#ifndef VERSION_ghc_exactprint
#define VERSION_ghc_exactprint "1.7.0.1"
#endif /* VERSION_ghc_exactprint */
-- #ifndef MIN_VERSION_ghc_exactprint
-- #define MIN_VERSION_ghc_exactprint(major1,major2,minor) (\
--   (major1) <  1 || \
--   (major1) == 1 && (major2) <  7 || \
--   (major1) == 1 && (major2) == 7 && (minor) <= 0)
-- #endif /* MIN_VERSION_ghc_exactprint */

#ifdef VERSION_ghc_exactprint
x = "got version"
#else
x = "no version"
#endif
