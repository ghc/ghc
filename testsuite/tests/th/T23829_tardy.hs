{-# LANGUAGE TemplateHaskellQuotes, TypeApplications #-}
module Main (main) where

import Language.Haskell.TH
import Data.Char
import Data.Proxy

tardy :: forall a. Proxy a -> IO Type
tardy _ = [t| a |]

tardy2 :: forall a. Proxy a -> IO Exp
tardy2 _ = [| id @a |]

tardy3 :: forall a. Proxy a -> Code IO (a -> a)
tardy3 _ = [|| id @a ||]

main :: IO ()
main = do
    tardy (Proxy @Int) >>= putStrLn . filt . show
    tardy2 (Proxy @Int) >>= putStrLn . filt . show
    examineCode (tardy3 (Proxy @Int)) >>= putStrLn . filt . show . unType

-- ad-hoc filter uniques, a_12313 -> a
filt :: String -> String
filt = go where
    go []           = []
    go ('_' : rest) = go (dropWhile isDigit rest)
    go (c:cs)       = c : go cs
