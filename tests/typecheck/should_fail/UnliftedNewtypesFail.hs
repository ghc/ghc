{-# LANGUAGE UnliftedNewtypes #-}

main :: IO ()
main = return ()

newtype Baz = Baz (Show Int)
