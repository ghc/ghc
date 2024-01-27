{-# LANGUAGE MultilineStrings #-}

-- Test that the error message containing multiline strings is well-formatted.
x :: Int
x =
  """
  this is
  a test
  """
