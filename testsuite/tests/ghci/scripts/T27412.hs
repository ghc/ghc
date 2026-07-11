module T27412 where

foo :: IO ()
foo =
  mapM_ (\(x, y) -> do
      print x
      print y)
    [(True, False)]
