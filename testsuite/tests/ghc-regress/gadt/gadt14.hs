{-# OPTIONS -fglasgow-exts #-}

-- Check that trailing parens are ok in data con signatures

module ShouldCompile where
 
data T where
   MkT :: Int -> (Int -> T)
