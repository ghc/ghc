{-# OPTIONS_GHC -fwrite-if-simplified-core -fbyte-code-and-object-code -fprefer-byte-code #-}

module T22840B where

import T22840A

theT :: T
theT = MkT (Just True)
