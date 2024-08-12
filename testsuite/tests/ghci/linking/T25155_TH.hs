module T25155_TH (foobar) where

foreign import ccall "foobar" foobar :: Int -> IO Int
