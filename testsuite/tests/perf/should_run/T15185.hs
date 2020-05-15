{-# LANGUAGE TypeApplications #-}

-- Ensure that we do list fusion on `foldr f z [from..to]` for sized `Int` and
-- `Word` types. Related tickets: #15185, #8763.

import Control.Exception (evaluate)
import Data.Int
import Data.Word

fact :: Integral t => t -> t
fact n = product [1..n]

main :: IO ()
main = do
  _ <- evaluate (fact @Int 50)
  _ <- evaluate (fact @Int64 50)
  _ <- evaluate (fact @Int32 50)
  _ <- evaluate (fact @Int16 50)
  _ <- evaluate (fact @Int8 50)
  _ <- evaluate (fact @Word 50)
  _ <- evaluate (fact @Word64 50)
  _ <- evaluate (fact @Word32 50)
  _ <- evaluate (fact @Word16 50)
  _ <- evaluate (fact @Word8 50)
  pure ()
