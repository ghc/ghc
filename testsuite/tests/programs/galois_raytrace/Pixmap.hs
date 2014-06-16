-- Copyright (c) 2000 Galois Connections, Inc.
-- All rights reserved.  This software is distributed as
-- free software under the license in the file "LICENSE",
-- which is included in the distribution.

module Pixmap where

import Data.Char
import System.IO hiding (try)
import Text.ParserCombinators.Parsec

readPPM f
  = do  h <- openFile f ReadMode
	s <- hGetContents h
	case (parse parsePPM f s) of
	  Left err -> error (show err)
	  Right x  -> return x

writePPM f ppm
  = do  h <- openFile f WriteMode
	let s = showPPM (length (head ppm)) (length ppm) ppm
	hPutStr h s

-- parsing

parsePPM
  = do  string "P6"
	whiteSpace
	width <- number
	whiteSpace
	height <- number
	whiteSpace
	colormax <- number
	whiteSpace
	cs <- getInput
	return (chop width (chopColors cs))

chopColors [] = []
chopColors (a:b:c:ds) = (ord a, ord b, ord c) : chopColors ds

chop n [] = []
chop n xs = h : chop n t
    where (h, t) = splitAt n xs

number
  = do  ds <- many1 digit
	return (read ds :: Int)

whiteSpace
  = skipMany (simpleSpace <|> oneLineComment <?> "")
    where simpleSpace = skipMany1 (oneOf " \t\n\r\v")
	  oneLineComment =
	      do  char '#'
		  skipMany (noneOf "\n\r\v")
		  return ()

-- printing

showPPM :: Int -> Int -> [[(Int,Int,Int)]] -> String
showPPM wid ht pss
  = header ++ concat [[chr r, chr g, chr b] | ps <- pss, (r, g, b) <-ps]
  where
    header = "P6\n#Galois\n" ++ show wid ++ " " ++ show ht ++ "\n255\n"
showPPM _ _ _ = error "incorrect length of bitmap string"
