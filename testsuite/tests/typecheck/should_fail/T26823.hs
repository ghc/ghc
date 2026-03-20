{-# LANGUAGE DeepSubsumption #-}

module T26823 where

allocArray :: Int -> IO ()
allocArray n = do
  let
    !off    = 18
    !size   = 8
    !vecAli = size

    !rem   = off `rem` vecAli
    !start = if rem == 0 then off else off + ( vecAli - rem )

  return ()
