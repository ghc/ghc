data D = D deriving Eq

instance Num D where
  fromInteger _ = D

main = do
    case 3 :: D of
        1 -> putStrLn "A"
