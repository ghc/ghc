module FiniteMap
      (FM, emptyFM, unitFM, extendFM, makeFM, unmakeFM, thenFM, plusFM,
       lookupFM, lookupElseFM, mapFM, domFM, ranFM, disjointFM) where

data  FM a b  =  MkFM [(a,b)]
emptyFM                               ::  FM a b
emptyFM                               =   MkFM []
unitFM                                ::  a -> b -> FM a b
unitFM a b                            =   MkFM [(a,b)]
extendFM                              ::  FM a b -> a -> b -> FM a b
extendFM (MkFM abs) a b               =   MkFM ((a,b) : abs)
makeFM                                ::  [(a,b)] -> FM a b
makeFM abs                            =   MkFM abs
unmakeFM                              ::  FM a b -> [(a,b)]
unmakeFM (MkFM abs)                   =   abs
thenFM                                ::  FM a b -> FM a b -> FM a b
(MkFM abs1) `thenFM` (MkFM abs2)      =   MkFM (abs2 ++ abs1)
plusFM                                ::  (Eq a) => FM a b -> FM a b -> FM a b
f `plusFM` g  |  f `disjointFM` g     =   f `thenFM` g
lookupFM                              ::  (Eq a) => FM a b -> a -> b
lookupFM f a                          =   lookupElseFM (error "lookup") f a
lookupElseFM                          ::  (Eq a) => b -> FM a b -> a -> b
lookupElseFM b (MkFM abs) a           =   head (  [ b' | (a',b') <- abs, a==a' ]
                                               ++ [ b ] )
mapFM                                 ::  (b -> c) -> FM a b -> FM a c
mapFM h (MkFM abs)                    =   MkFM [ (a, h b) | (a,b) <- abs ]
domFM                                 ::  FM a b -> [a]
domFM (MkFM abs)                      =   [ a | (a,b) <- abs ]
ranFM                                 ::  FM a b -> [b]
ranFM (MkFM abs)                      =   [ b | (a,b) <- abs ]
disjointFM                            ::  (Eq a) => FM a b -> FM a b -> Bool
f `disjointFM` g                      =   domFM f `disjoint` domFM g
disjoint                              ::  (Eq a) => [a] -> [a] -> Bool
xs `disjoint` ys                      =   and [ not (x `elem` ys) | x <- xs ]
