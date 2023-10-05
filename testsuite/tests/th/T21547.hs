{-# LANGUAGE TemplateHaskell #-}
module T21547 where
import T21547A

data T a = T a deriving (Show)

main :: IO ()
main = do
  let x = $$(foo [|| T () ||])
  let y = $$(foo [|| F () ||])
  return ()
