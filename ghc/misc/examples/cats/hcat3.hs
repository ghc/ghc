module Main (main) where

import LibPosix

main :: IO ()
main = copy standardInput standardOutput
 where
  copy inn out
   = try (readFileDescriptor inn 4096) >>=
     either
        (\ _ -> return ())
        (\ s -> writeFileDescriptor out s >>
                copy inn out)

-- 124,879 bytes/sec ( 600KB input)
-- 130,694 bytes/sec ( 9.3MB input)
-- 127,263 bytes/sec (25.5MB input)
