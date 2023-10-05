{-# LANGUAGE OverloadedLabels, DataKinds, FlexibleContexts #-}

import GHC.OverloadedLabels

-- No instance for (OverloadedLabel "x" t0)
a = #x

-- No instance for (OverloadedLabel "x" Int)
b :: Int
b = #x

-- Could not deduce (OverloadedLabel "y" t) from (OverloadedLabel "x" t)
c :: IsLabel "x" t => t
c = #y

main = return ()
