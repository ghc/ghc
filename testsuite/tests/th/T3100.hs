{-# LANGUAGE RankNTypes, FlexibleContexts, ImplicitParams, TemplateHaskell #-}

-- This test makes sure TH understands types where
-- there is a predicate but no 'forall'
--
-- There are two tests in here; both should be fine
-- Trac ticket: #3100

module T3100 where

import Language.Haskell.TH

flop :: Ord Int => Int -> Int
flop x = x

$(do { t <- reify 'flop; return [] })

type T a = Eq a => a

$(do { reify ''T; return []})
