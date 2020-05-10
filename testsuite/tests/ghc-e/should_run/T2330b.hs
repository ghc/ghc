{-# LANGUAGE MonadComprehensions #-}

main :: IO ()
main = [ () | (Just x) <- pure Nothing ]
