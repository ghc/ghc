module Main(main) where
import Numbers
import Vectors
import Hide
import MyIO
import EdgePlate	( Input(..) )  -- partain
import Postscript	( Output(..) ) -- partain
import System.IO

main = 
  do
   ls <- hGetContents stdin
   (getFilename $
      process (\viewdir -> hiddenline viewdir. map read. lines)) (lines ls)
