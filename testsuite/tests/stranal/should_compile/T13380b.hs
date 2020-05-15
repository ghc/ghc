module Lib (m) where

import Control.Exception

throws :: IO ()
throws = throwIO (userError "What")
{-# NOINLINE throws #-}

bigDeadAction :: IO Int
bigDeadAction = return $ sum $ [0..999]
{-# NOINLINE bigDeadAction #-}

m :: IO Int
m = throws >> bigDeadAction
