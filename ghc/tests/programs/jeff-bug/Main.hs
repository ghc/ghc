module Main where

import System
import Hawk--(hawkMain)
import DLX
import DLX_Cell
import DLX_Reg
import DLX_Op
import Processor
import Signal
import Trans
import LazyST

import Probe

main =
 do { clearProbes_UNIX
    ; args <- getArgs
    ; case args of
        [file]           -> run file
        ["-count", file] -> count file    
        ["-sample",n,file] -> sample (read n) file    
        otherwise        -> error "Usage: pgm [-count] file\n"
    ; return ()
    }

p :: ((ArrayDesc Int (Instr DLXReg Int), a), Int) -> Signal [Trans DLX_Op (DLX_Cell DLXReg Int)]
--p :: ((ArrayDesc Int (Instr DLXReg Int), a), Int) -> Signal [Trans DLX_Op (DLX_Cell (Virtual DLXReg Int) Int)]
p = processor

run file =
     hawkMain file (mapM_ (putStrLn . show) . (view . p))
count file = 
     hawkMain file (mapM_ (putStrLn . show) . (cnt. view . p))

sample n file =
     hawkMain file (mapM_ (putStrLn . pretty) . (take n . view . p))

pretty tss =
    foldr (\x y -> show x ++ "\n" ++ y) "\n" tss

cnt l = runST (
  do n <- newSTRef 0
     mapM (\l -> n+=(length l)) l
  )

n += v = 
  do n' <- readSTRef n
     let v' = v+n'
     writeSTRef n v'
     return v'

