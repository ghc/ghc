{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE RankNTypes, FlexibleContexts, ImplicitParams, TemplateHaskell #-}

-- This test makes sure TH understands types where
-- there is a predicate but no 'forall'
--
-- There are two tests in here; both should be fine
-- Ticket: #3100

module T3100 where

import Language.Haskell.TH

flop :: Ord Int => Int -> Int
-- Weird test case: (Ord Int) is simplifiable and redundant
flop x = x

$(do { t <- reify 'flop; return [] })

type T a = Eq a => a

$(do { reify ''T; return []})
