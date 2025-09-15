main :: IO ()
main = main' ()
  where
    main' _ = main >> main' ()
