main :: IO ()
main = do
  (Just x) <- pure Nothing
  pure ()
