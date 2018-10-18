{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Werror=typed-holes #-}

main :: IO ()
main = undefined

data Proxy k = Proxy

test1 = _ :: Proxy '[ 'True ]
test2 = _ :: Proxy '[ '[ 1 ] ]
test3 = _ :: Proxy '[ '( "Symbol", 1 ) ]
