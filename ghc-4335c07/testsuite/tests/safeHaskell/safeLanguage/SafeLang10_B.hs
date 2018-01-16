{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

-- Untrusted plugin! Don't wan't it changing behaviour of our
-- trusted code
module SafeLang10_B where

import SafeLang10_A

instance {-# OVERLAPPABLE #-} Pos a where
    res _ = False

instance {-# OVERLAPPING #-} Pos [Int] where
    res _ = error "This curry is poisoned!"

function :: Int
function = 3


