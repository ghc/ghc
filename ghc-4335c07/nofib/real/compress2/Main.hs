-- The current version of compress acts as a filter: the data to be
-- compressed is taken from standard input, and the result is placed on
-- the standard output. 

module Main (main)
where

import Encode
import WriteRoutines
import System.IO

main = do
  hSetBinaryMode stdin  True
  hSetBinaryMode stdout True
  i <- getContents
  putStr (compress i)
           
-- The output is given by a magic header consisting of two fixed numbers,
-- and a third representing the maximum number of bits used per code and
-- whether or not block compression is being used. There is currently no
-- option for turning off block compression in this program. The maximum
-- number of bits is imported from the encode module.

compress = magic_header . processInput

magic_header cs
     = "\o037\o235" ++ [toEnum third_byte] ++ cs
       where
       third_byte = block_compress + maxBits
       block_compress = 128  -- 0   if block compression is not required

-- The two phases of the compression technique are joined together in a
-- pipeline with a feedback loop. This is because the encoding function
-- needs to know how many characters have been output at any time so it
-- can determine whether or not the compression ratio has fallen. This is
-- known, however, only when the codes are combined. Therefore the result
-- of the output function is a string containing the output, and a list
-- of integers representing the size of the output.

processInput cs 
     = fst output
       where
       output = outputCodes codes
       codes = encode (snd output) cs
