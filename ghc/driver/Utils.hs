module Utils where

prefixMatch :: Eq a => [a] -> [a] -> Bool
prefixMatch [] _str = True
prefixMatch _pat [] = False
prefixMatch (p:ps) (s:ss) | p == s    = prefixMatch ps ss
                          | otherwise = False

suffixMatch :: String -> String -> Bool
suffixMatch pat str = prefixMatch (reverse pat) (reverse str)
