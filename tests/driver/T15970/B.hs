module B ( TypeClass(..) ) where

import A

class Show a => TypeClass a where
    getSize :: a -> IO Int
    getSize a = toTypedData (show a)

    printA :: a -> IO ()
