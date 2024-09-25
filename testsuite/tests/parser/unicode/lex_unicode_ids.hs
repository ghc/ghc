-- Regression tests for unicode identifiers

{-# LANGUAGE TemplateHaskellQuotes #-}

module ShouldCompile where

ƞ = 1
eta = 'ƞ

data Ʊ
upsilon = ''Ʊ
