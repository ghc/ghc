import GHC.AtFile

main :: IO ()
main = print =<< expandAtFile [j: i | i <- ["foo", "bar", "baz"], j <- "+@-"]
