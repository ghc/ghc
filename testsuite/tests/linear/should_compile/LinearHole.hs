{-# LANGUAGE LinearTypes #-}
{-# OPTIONS_GHC -fdefer-typed-holes -Wno-typed-holes #-}

module LinearHole where  -- #18491

f :: Int %1 -> Bool %1 -> Char
f x y = _1
