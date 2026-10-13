module T25665Lib (t25665) where

foreign import javascript "t25665_wrapper" t25665 :: Int -> IO Int
