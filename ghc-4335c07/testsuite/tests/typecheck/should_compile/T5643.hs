{-# LANGUAGE MultiParamTypeClasses
            ,FunctionalDependencies
            ,FlexibleContexts #-}

module T5643 where

import Prelude ()

class FoldableLL a b | a -> b

class FoldableLL full item => ListLike full item | full -> item where
    
    cons :: item -> full -> full
    head :: full -> item
    tail :: full -> full 

    init :: full -> full
    init l = cons (head l) (init xs)
        where xs = tail l
              
-- Deriveds from the functional dependencies were escaping in the Wanted
-- constraints and emitWantedCts was giving a panic error.
