
-- Testing performance of type-checking rewriting plugins.
-- Test based on T9872b.

{-# OPTIONS_GHC -dcore-lint #-}
{-# OPTIONS_GHC -freduction-depth=400 #-}
{-# OPTIONS_GHC -fplugin RewritePerfPlugin #-}

{-# LANGUAGE DataKinds, KindSignatures, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import RewritePerfDefs

data Proxy (a :: k) = Proxy

type Cube1 = 'Cube B G W G B R
type Cube2 = 'Cube W G B W R R
type Cube3 = 'Cube G W R B R R
type Cube4 = 'Cube B R G G W W

type Cubes = [Cube1, Cube2, Cube3, Cube4]

main = print (Proxy :: Proxy (Solutions Cubes))
