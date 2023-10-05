module T18347 (function) where

import Data.Coerce

newtype All = All Bool

data Encoding = Encoding (Char -> Bool)

function :: Encoding -> Char -> All
function enc v = coerce (case enc of Encoding x -> x) v
