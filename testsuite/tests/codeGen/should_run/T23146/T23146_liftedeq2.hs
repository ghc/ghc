import T23146_liftedeqA

fieldsSam :: NP xs -> NP xs -> Bool
fieldsSam UNil UNil = True

x :: Bool
x = fieldsSam UNil UNil

