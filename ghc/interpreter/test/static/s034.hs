--!!! Trying to export restricted type synonyms
module M(T(..)) where
type T = Char in x :: T
x = 'a'