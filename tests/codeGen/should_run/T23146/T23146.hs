{-# LANGUAGE GADTs #-}

import T23146A

fieldsSam :: NP xs -> NP xs -> Bool
fieldsSam (x' ::* xs) (y' ::* ys) = fieldsSam xs ys
fieldsSam UNil UNil = True

main :: IO ()
main = print (fieldsSam UNil UNil)

