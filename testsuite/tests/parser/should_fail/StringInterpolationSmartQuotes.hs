{-# LANGUAGE StringInterpolation #-}

-- Test that interpolated strings show a helpful error message if one
-- tries to use smart quotes as a delimiter, like in normal strings
x :: String
x = s"a”
