{-# LANGUAGE MultilineStrings #-}

main :: IO ()
main = do
  checkEqual "\65\  \0" "A0"
  checkEqual
    """
      a
    \
    \  b
    """
    "  a\n  b"

checkEqual :: String -> String -> IO ()
checkEqual actual expected = do
  putStrLn $ "Expected: " ++ show expected
  putStrLn $ "Actual:   " ++ show actual
  putStrLn "========================================"
