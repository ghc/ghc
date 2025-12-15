import T23146_liftedeqA

fieldsSam :: NP xs -> NP xs -> Bool
fieldsSam UNil UNil = True
{-# OPAQUE fieldsSam #-}

main = print (fieldsSam UNil UNil)

