{-# LANGUAGE MagicHash #-}
import GHC.Exts

main :: IO ()
main = print $ C# (indexCharOffAddr# "foo"# -9223372036854775808#)
