-- {-# OPTIONS_GHC -fno-full-laziness #-}
module A (toTypedData, toTypedDataNoDef) where

toTypedData :: String -> IO Int
toTypedData s = wrapPrint "yoyo" $ toTypedDataNoDef s

wrapPrint :: String -> IO Int -> IO Int
wrapPrint s act = do
    putStrLn s
    act

toTypedDataNoDef  :: String -> IO Int
toTypedDataNoDef s = return $ length s
