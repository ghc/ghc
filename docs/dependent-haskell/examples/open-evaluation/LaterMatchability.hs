{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module LaterMatchability where

import AnchorReceiver

type MatchResult = Receiver (Select 'True) 'Match
type UnmatchResult = Receiver (Select 'True) 'Unmatch

type DefaultPrefix = Select 'True

signed :: Receiver (Select 'True) 'Match
signed = Just True
