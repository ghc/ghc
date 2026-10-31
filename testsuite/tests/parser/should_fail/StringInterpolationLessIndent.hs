{-# LANGUAGE StringInterpolation #-}

test :: String
test = example
  where
    example = s"a ${fromMaybe
  False (Just True)} b"
