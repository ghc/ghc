{-# LANGUAGE GHC2021 #-}
module CaretDiagnostics1 where

main :: IO ()
main = do
  10000000000000000000000000000000000000 +
    2 +
      (3 :: Int)
  pure ("this is not an IO" + (            ))

  where

    _ = case id of
      "γηξ" -> (
        ) '0'

fóo :: Int
fóo = ()

tabby :: Int
tabby =  	()

tabby2 :: Int
tabby2 =		()
