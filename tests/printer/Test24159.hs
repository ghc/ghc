module Test24159 where

f (show {- comment -} -> "True" :: String) = ()

g (a {-a-} -> b {-b-} -> c {-c-} :: {-x1-} ( {-x2-} x) {-x3-}) = ()
