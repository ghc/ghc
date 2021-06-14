{-# LANGUAGE OverloadedLabels, DataKinds, FlexibleContexts #-}

import GHC.OverloadedLabels

-- No instance for (OverloadedLabel "x" t0)
a = #x

-- No instance for (OverloadedLabel "x" (t0 -> t1), OverloadedLabel "y" t0)
b = #x #y

-- Could not deduce (OverloadedLabel "y" t) from (OverloadedLabel "x" t)
c :: IsLabel "x" t => t
c = #y

main = return ()
