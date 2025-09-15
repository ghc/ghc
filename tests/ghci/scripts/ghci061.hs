two_args :: [String] -> IO String
two_args _ = return "two_args> "

three_args :: [String] -> Int -> IO String
three_args _ _ = return $ "three_args> "
