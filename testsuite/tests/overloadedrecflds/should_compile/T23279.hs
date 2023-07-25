{-# LANGUAGE DuplicateRecordFields, DataKinds
           , OverloadedLabels, OverloadedRecordDot #-}

module T23279 where

import T23279_aux

import GHC.Records

bar :: Bar
bar = Bar { x = 3, y = 'x', z = False, w = 17.28 }

baz :: Baz
baz = Baz { z = 1.1 }

v = w

barDot :: Bar -> Int
barDot b = b.x

barGetField :: Bar -> Bool
barGetField = getField @"z"

