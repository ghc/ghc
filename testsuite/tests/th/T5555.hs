{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
import qualified T5555_Lib as L

test :: String
test = [L.s|hello world|]

main :: IO ()
main = putStrLn test
