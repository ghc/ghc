{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax (lift)
import A

main :: IO ()
main = putStrLn $(lift foo)
