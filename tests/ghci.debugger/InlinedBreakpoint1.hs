module InlinedBreakpoint1 where

-- proof that optimization is enabled
rul :: String
rul = "unoptimized"
{-# noinline rul #-}
{-# rules "rul" rul = "optimized" #-}

inl :: IO ()
inl = putStrLn "interpretation"
{-# inline inl #-}
