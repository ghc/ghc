module Main (main) where

main :: IO ()
main
  = try getChar >>=
    {-then-}either  (\ _ -> return ())
    {-or-}          (\ c -> putChar c >>
                            main)

-- 51,156 bytes/sec (600KB input)
