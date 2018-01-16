-- Copyright (c) 2000 Galois Connections, Inc.
-- All rights reserved.  This software is distributed as
-- free software under the license in the file "LICENSE",
-- which is included in the distribution.

module Primitives where

rad2deg :: Double -> Double
rad2deg r = r * 180 / pi

deg2rad :: Double -> Double
deg2rad d = d * pi / 180

addi :: Int -> Int -> Int
addi = (+)

addf :: Double -> Double -> Double
addf = (+)

acosD :: Double -> Double
acosD x = acos x * 180 / pi

asinD :: Double -> Double
asinD x = asin x * 180 / pi
