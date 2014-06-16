-- !!! THIS TEST IS FOR TYPE SYNONIMS AND FACTORISATION IN THEIR PRESENCE.

module Test where
data M a = A | B a (M a)
data L a = N | C a (Syn a)
type Syn b = L b

idL :: L (Syn c) -> L (Syn c)
idL N       = N
idL (C x l) = C x (idL l) 

idM:: M (L (Syn x)) -> M (L (Syn x))
idM A       = A
idM (B x l) = B (idL x) (idM l) 

