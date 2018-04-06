{-# LANGUAGE TemplateHaskell #-}

import T13587A

main :: IO ()
main = do
    let sin' = $$(importDoubleToDouble "sin")
        cos' = $$(importDoubleToDouble "cos")
    --
    print (sin' 0)
    print (cos' pi)

