{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}

import GHC.Exts
import GHC.Types

class BoxIt (a :: TYPE 'WordRep) where
    type Boxed a :: Type
    boxed :: a -> Boxed a

instance BoxIt Char# where
    type Boxed Char# = Char
    boxed x = C# x

main :: IO ()
main = print $ boxed 'c'#
