import Data.List
import LibTest

-- Properties that hold

prop_unwords_intercalate xs = collect xs $ unwords xs == intercalate " " xs

prop_length_intercalate :: [Int] -> [[Int]] -> Bool
prop_length_intercalate x xs 
  = length (intercalate x xs) == 
    sum (map length xs) + length x * (length (drop 1 xs))

-- Properties that don't hold

prop_unlines_intercalate xs = unlines xs == intercalate "\n" xs
-- unlines ["",""] = "\n\n"
-- intercalate "\n" ["",""] = "\n"

-- Helper class

instance Arbitrary Char where
  arbitrary = frequency [(1,return '\n')
                        ,(5,return ' ')
                        ,(25,choose (toEnum 0,toEnum 255))]
  coarbitrary = variant . fromEnum
  
main = runTests "test_List" ["prop_unwords_intercalate"
                            ,"prop_length_intercalate"]
                            
                            [run prop_unwords_intercalate
                            ,run prop_length_intercalate]
