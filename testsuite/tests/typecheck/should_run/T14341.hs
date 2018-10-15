{-# LANGUAGE TypeApplications #-}

import Type.Reflection

main :: IO ()
main = do
    print $ typeRep @((,,))
    print $ typeRep @((,,) Int)
    print $ typeRep @((,,) Int Int Int)
