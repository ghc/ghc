module Utils
    ( stripLocalAnchors
    , stripLocalLinks
    , stripLocalReferences
    ) where


import Data.List


replaceBetween :: Eq a => [a] -> a -> [a] -> [a] -> [a]
replaceBetween _ _ _ [] = []
replaceBetween pref end val html@(x:xs') = case stripPrefix pref html of
    Just strip -> pref ++ val ++ (replaceBetween' . dropWhile (/= end)) strip
    Nothing -> x:(replaceBetween' xs')
  where
    replaceBetween' = replaceBetween pref end val

stripLocalAnchors :: String -> String
stripLocalAnchors = replaceBetween "<a name=\"local-" '\"' "0"

stripLocalLinks :: String -> String
stripLocalLinks = replaceBetween "<a href=\"#local-" '\"' "0"

stripLocalReferences :: String -> String
stripLocalReferences = stripLocalLinks . stripLocalAnchors
