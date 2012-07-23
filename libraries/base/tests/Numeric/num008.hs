-- showing/reading floats
--
module Main(main) where

import Numeric

main = do
  let dbls   =  map (shEFloat (Just 7)) doubles
             ++ map (shEFloat (Just 0)) doubles
             ++ map (shEFloat Nothing)  doubles
             ++ map (shFFloat (Just 7)) doubles
             ++ map (shFFloat (Just 0)) doubles
             ++ map (shFFloat Nothing)  doubles
             ++ map (shGFloat (Just 7)) doubles
             ++ map (shGFloat (Just 0)) doubles
             ++ map (shGFloat Nothing)  doubles

      flts   =  map (shEFloat (Just 7)) floats
             ++ map (shEFloat (Just 0)) floats
             ++ map (shEFloat Nothing)  floats
             ++ map (shFFloat (Just 7)) floats
             ++ map (shFFloat (Just 0)) floats
             ++ map (shFFloat Nothing)  floats
             ++ map (shGFloat (Just 7)) floats
             ++ map (shGFloat (Just 0)) floats
             ++ map (shGFloat Nothing)  floats

  putStrLn (unlines dbls)
  putStrLn (unlines flts)
  print (map read dbls :: [Double])
  print (map read flts :: [Double])

shEFloat p f = showEFloat p f ""
shFFloat p f = showFFloat p f ""
shGFloat p f = showGFloat p f ""

doubles :: [ Double ]
doubles = [ 0.0
          , 420
          ,  42
          ,   4.2
          ,   0.42
          ,   0.042
          , 1.82173691287639817263897126389712638972163
          , 1.82173691287639817263897126389712638972163e-300
          ]

floats :: [ Float ]
floats = [ 0.0
          , 420
          ,  42
          ,   4.2
          ,   0.42
          ,   0.042
          , 1.82173691287639817263897126389712638972163
          , 1.82173691287639817263897126389712638972163e-300
          ]
