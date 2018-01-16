module Main (main) where
import Data.List

needles = ["","1","2","12","123","1234"]
haystacks = ["","a","ab","abc","1","2","3","a1","1a",
             "23","123","a123","ab123","abc123"]

main :: IO()
main = mapM_ print $ [needle `isSuffixOf` haystack
                       | needle <- needles, haystack <- haystacks]
