-- !!! Exporting "constructor" of a type synonym
module M(T(K1)) where
type T = T'
data T' = K1