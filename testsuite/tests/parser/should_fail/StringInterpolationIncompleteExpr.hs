{-# LANGUAGE StringInterpolation #-}

test :: String
test = s"a ${let x = 1 in} b"
