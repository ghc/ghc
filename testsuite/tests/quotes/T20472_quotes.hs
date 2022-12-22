{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fdefer-out-of-scope-variables #-}
module T20472_quotes where

foo = [| Prelude.a |]
