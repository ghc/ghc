import Data.List
import LibTest

-- Properties that hold

prop_intercalate_split :: Int -> [Int] -> Bool
prop_intercalate_split x xs = intercalate [x] (split x xs) == xs

prop_unwords_intercalate xs = collect xs $ unwords xs == intercalate " " xs

-- Properties that don't hold

prop_unlines_intercalate xs = unlines xs == intercalate "\n" xs
-- unlines ["",""] = "\n\n"
-- intercalate "\n" ["",""] = "\n"

prop_words_split xs = words xs == split ' ' xs
-- words "" = []
-- split ' ' "" = [""]

prop_lines_split xs = lines xs == split '\n' xs
-- lines "" = []
-- split '\n' "" = [""]

-- Helper class

instance Arbitrary Char where
  arbitrary = frequency [(1,return '\n')
                        ,(5,return ' ')
                        ,(25,choose (toEnum 0,toEnum 255))]
  coarbitrary = variant . fromEnum
  
main = runTests "test_List" ["prop_intercalate_split"
                            ,"prop_unwords_intercalate"]
                            
                            [run prop_intercalate_split
                            ,run prop_unwords_intercalate]
