{-# Language HexFloatLiterals #-}

import Numeric(showHFloat)

main :: IO ()
main =
  do print [ 0xF.0
           , 0xF.1, 0xF.01
           , 0xF1p-4, 0xF01p-8
           , 0x0.F1p4, 0x0.00F01p12
           ]

     mapM_ putStrLn [ showHFloat (212.21 :: Double) ""
                    , showHFloat (-12.76 :: Float) ""
                    , showHFloat (-0 :: Double) ""
                    ]
