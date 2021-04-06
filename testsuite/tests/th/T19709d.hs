{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module T19709d where

$( case tail "hello" of "hello" -> return [] )
