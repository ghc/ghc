{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}
import GHC.Exts
import Type.Reflection

main = do
    case typeRep @(Int -> Char) of
      App a b -> print (a, b)

    case typeRep @(Int# -> Char) of
      App a b -> print (a, b)

    case typeRep @(Int# -> Char) of
      App a b -> print $ App a (typeRep @String)
