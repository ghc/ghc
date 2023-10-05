{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Werror=typed-holes #-}

main :: IO ()
main = undefined

data Proxy k = Proxy

test1 = _ :: Proxy '( 'True, 'False )
test2 = _ :: Proxy '( '( 'True, 'False ), 'False )
test3 = _ :: Proxy '( '[ 1 ], 'False )
