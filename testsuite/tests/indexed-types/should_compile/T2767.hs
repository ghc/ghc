{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction #-}

module T2767a where

main = return ()

-- eval' :: Solver solver => Tree solver a -> [(Label solver,Tree solver a)] -> solver [a]
eval' (NewVar f) wl = do v <- newvarSM
                         eval' (f v) wl
eval' Fail       wl = continue wl

-- continue :: Solver solver => [(Label solver,Tree solver a)] -> solver [a] 
continue ((past,t):wl) = do gotoSM past
                            eval' t wl
data Tree s a
                = Fail
                | NewVar (Term s -> Tree s a)

class Monad solver => Solver solver where
        type Term solver        :: *
        type Label solver       :: *
        newvarSM        :: solver (Term solver)
        gotoSM          :: Label solver -> solver ()
