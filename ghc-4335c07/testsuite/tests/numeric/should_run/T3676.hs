-- test conversion of funny numbers through Rational (#3676)

main = mapM_ putStrLn $ concat $
  [map (show.d2d) doubles1,
   map (show.d2f) doubles1,
   map (show.d2r) doubles1,
   map (show.f2d) floats1,
   map (show.f2f) floats1,
   map (show.f2r) floats1,
   map (show.d2d) doubles2,
   map (show.d2f) doubles2,
   map (show.d2r) doubles2,
   map (show.f2d) floats2,
   map (show.f2f) floats2,
   map (show.f2r) floats2
  ]

d2d = realToFrac :: Double -> Double
d2f = realToFrac :: Double -> Float
d2r = realToFrac :: Double -> Rational
f2d = realToFrac :: Float -> Double
f2f = realToFrac :: Float -> Float
f2r = realToFrac :: Float -> Rational

doubles1 = [0/0, 1/0, -1/0, 0/(-1)] :: [Double]
floats1  = [0/0, 1/0, -1/0, 0/(-1)] :: [Float]

doubles2 = names :: [Double]
floats2  = names :: [Float]

names :: Read a => [a]
names = map read ["NaN", "Infinity", "-Infinity", "-0"]
