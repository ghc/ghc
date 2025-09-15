{-# LANGUAGE MultilineStrings #-}

-- Test that multiline strings disallow tabs in the middle
-- of the string, like normal strings
x :: String
x =
  """
  ab	sadf
  """
