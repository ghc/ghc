{-# OPTIONS_GHC -no-base #-}

-- Test that -no-base flag works as expected
main :: IO ()
main = do
   let x = Just "test"
   case x of
      Nothing -> putStrLn "Nothing"
