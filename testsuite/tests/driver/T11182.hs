{-# LANGUAGE Strict #-}
-- | Strict should imply StrictData
module Main where

data Lazy a = Lazy ~a

main :: IO ()
main =
  case Lazy undefined of
    Lazy _ -> putStrLn "Lazy"
