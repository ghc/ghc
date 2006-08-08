{-# OPTIONS -fth #-}

-- We should get a civilised report of conflicting variable
-- bindings in the definition spliced in by foo

module TH_spliceD1 where

import TH_spliceD1_Lib

$(foo)
