{-# LANGUAGE MultilineStrings #-}

-- Test that multiline strings disallow smart quotes and show
-- a helpful error message, like normal strings
x :: String
x =
  """
  a
  ”””
