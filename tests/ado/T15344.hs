{-# LANGUAGE ApplicativeDo #-}

f :: Maybe (Maybe Int) -> Maybe Int -> Maybe Int
f mgs mid = do
    _ <- mid
    (Just moi) <- mgs
    pure (moi + 42)

main :: IO ()
main = print (f (Just Nothing) (Just 2))
