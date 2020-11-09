{-# LANGUAGE TemplateHaskell #-}

import T18102b_aux

x :: Int
x = $$(intQuote)

main :: IO ()
main = print x
