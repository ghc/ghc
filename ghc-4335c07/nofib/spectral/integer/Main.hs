module Main (main) where

import System.Environment

main = do
  --runalltests 1 20 10000 1 20 10000
  --runalltests (-1000000) 4001 1000000 (-1000000) 4001 1000000
  --runalltests (-2100000000) 4000001 2100000000 (-2100000000) 4000001 2100000000
  [_1,_2,_3] <- getArgs
  let astart = read _1; astep = read _2; alim = read _3
  runalltests astart astep alim astart astep alim

runalltests
	:: Integer -> Integer -> Integer
	-> Integer -> Integer -> Integer
	-> IO ()
runalltests astart astep alim bstart bstep blim = do
  runbench (+) (+) "(+)" astart astep alim astart astep alim
  runbench (-) (-) "(-)" astart astep alim astart astep alim
  runbench (*) (*) "(*)" astart astep alim astart astep alim

  runbench div div "div" astart astep alim astart astep alim
  runbench mod mod "mod" astart astep alim astart astep alim
  runbench quot quot "quot" astart astep alim astart astep alim
  runbench rem rem "rem" astart astep alim astart astep alim

  runbench gcd gcd "gcd" astart astep alim astart astep alim
  runbench lcm lcm "lcm" astart astep alim astart astep alim

  runbench (==) (==) "(==)" astart astep alim astart astep alim
  runbench (<) (<) "(<)" astart astep alim astart astep alim
  runbench (<=) (<=) "(<=)" astart astep alim astart astep alim
  runbench (>) (>) "(>)" astart astep alim astart astep alim
  runbench (>=) (>=) "(>=)" astart astep alim astart astep alim

runbench 
	:: (Integer -> Integer -> a) 
	-> (Int -> Int -> b)
	-> String
	-> Integer -> Integer -> Integer
	-> Integer -> Integer -> Integer
	-> IO ()
runbench jop iop opstr astart astep alim bstart bstep blim = do
 intbench iop astart astep alim astart astep alim
 integerbench jop astart astep alim astart astep alim

integerbench :: (Integer -> Integer -> a) 
	-> Integer -> Integer -> Integer
	-> Integer -> Integer -> Integer
	-> IO ()
integerbench op astart astep alim bstart bstep blim = do
  seqlist ([ a `op` b 
	   | a <- [ astart,astart+astep..alim ]
	   , b <- [ bstart,astart+bstep..blim ]])
  return ()

intbench :: (Int -> Int -> a) 
	-> Integer -> Integer -> Integer
	-> Integer -> Integer -> Integer
	-> IO ()
intbench op astart astep alim bstart bstep blim = do
  seqlist ([ a `op` b 
	   | a <- [ fromInteger astart,fromInteger astart + fromInteger astep .. fromInteger alim ]
	   , b <- [ fromInteger bstart,fromInteger astart + fromInteger bstep .. fromInteger blim ]])
  return ()

seqlist [] = return ()
seqlist (x:xs) = x `seq` seqlist xs
