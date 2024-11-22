{-# LANGUAGE StringInterpolation #-}

test :: String -> ()
test s"hello ${x}" = ()

x :: String
x = "world"
