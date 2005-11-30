{-# OPTIONS -fth #-}

-- Test name quoting and splicing, for built-in syntax

module TH_mkName where

import Language.Haskell.TH

x1 = $( return (ConE '()))
x2 = $( return (ConE '(,)))
x3 = $( return (ConE '[]))
x4 = $( return (ConE '(:)))
x5 = $( return (ConE 'Just))

y1 = $( return (ConE (mkName "()")))
y2 = $( return (ConE (mkName "(,)")))
y3 = $( return (ConE (mkName "[]")))
y4 = $( return (ConE (mkName ":")))
y5 = $( return (ConE (mkName "Just")))

