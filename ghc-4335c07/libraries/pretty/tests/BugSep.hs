-- | Demonstration of ambiguity in HughesPJ library at this time. GHC's
-- internal copy has a different answer than we currently do, preventing them
-- using our library.
module Main (main) where

import Text.PrettyPrint.HughesPJ

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Note that the correct definition of sep is currently unclear"
    putStrLn "It is neither foldr ($+$) empty nor foldr ($$) empty"    
    putStrLn "------------------------------------------------------------"
    let test1 = [ text "" $+$  text "c", nest 3 ( text "a") ]
    let test2 = [ text "c", nest 3 ( text "b") ]
    putStrLn "--------------------------Test 1----------------------------"
    putStrLn "[ text \"\" $+$  text \"c\", nest 3 ( text \"a\") ]"
    putStrLn "-----------------------------sep----------------------------"
    print $ renderStyle style{lineLength=1} $ sep test1
    putStrLn "-----------------------------<+>----------------------------"
    print $ renderStyle style{lineLength=1} $ foldr (<+>) empty test1
    putStrLn "-----------------------------$+$----------------------------"
    print $ renderStyle style{lineLength=1} $ foldr ($+$) empty test1
    putStrLn "------------------------------$$----------------------------"
    print $ renderStyle style{lineLength=1} $ foldr ($$)  empty test1
    putStrLn "--------------------------Test 2----------------------------"
    putStrLn "[ text \"c\", nest 3 ( text \"b\") ]"
    putStrLn "-----------------------------sep----------------------------"
    print $ renderStyle style{lineLength=1} $ sep test2
    putStrLn "-----------------------------<+>----------------------------"
    print $ renderStyle style{lineLength=1} $ foldr (<+>) empty test2
    putStrLn "-----------------------------$+$----------------------------"
    print $ renderStyle style{lineLength=1} $ foldr ($+$) empty test2
    putStrLn "------------------------------$$----------------------------"
    print $ renderStyle style{lineLength=1} $ foldr ($$)  empty test2

