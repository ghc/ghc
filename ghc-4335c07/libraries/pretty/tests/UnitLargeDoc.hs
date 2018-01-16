module UnitLargeDoc where

import Text.PrettyPrint.HughesPJ

import Control.DeepSeq
import Control.Exception

testLargeDoc :: IO ()
testLargeDoc = do
  putStrLn "Testing large doc..."
  evaluate largeDocRender
  return ()

largeDocRender :: String
largeDocRender = force $ render $ vcat $ replicate 10000000 $ text "Hello"

