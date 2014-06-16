{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 707
{-# OPTIONS_GHC -fno-warn-amp #-}
#endif
module T7145b ( A.Applicative(pure) ) where

import qualified Control.Applicative as A

pure :: ()
pure = ()
