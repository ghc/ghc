import T23146_liftedeqA

fieldsSam :: NP xs -> NP xs -> Bool
fieldsSam UNil UNil = True

main = print (fieldsSam UNil UNil)

