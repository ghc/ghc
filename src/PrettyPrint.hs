module PrettyPrint ( module Pretty ) where

#if __GLASGOW_HASKELL__ < 503
import Pretty
#else
import Text.PrettyPrint as Pretty
#endif
