import Data.Char

main :: IO ()
main = do
    print $ isMark '\768'
    print $ isAlphaNum '\768'
    print $ (isAlpha '\768', isNumber '\768')
