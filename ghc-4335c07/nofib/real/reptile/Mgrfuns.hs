-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991

module Mgrfuns(
       aligntext, bitcopy, circle, clear, clearevent,
       clearmode, destroywin, dragcircle, dragline,
       dragrect, font, func, go, highlight,
       line, mapevent, newwin, rcircle, 
       selectwin, setevent, setmode, shapewindow, standend, 
       stringto, standout, textregion, textreset) where

import Diff

--CR should try to generalise this to cope with more (all?) commands
escom :: [Char] -> [Int] -> [Char]
escom str ns = '\ESC' : foldr f "" ns
               where
               f n "" = show n ++ str
               f n s = show n ++ "," ++ s

aligntext = '\ESC' : "l"

bitcopy = escom "b"  -- xd yd w h xs ys

circle = escom "o"   -- x y r

clear = "\FF"

clearevent event = escom "e" [mapevent event]

clearmode mode = escom "s" [mode]

destroywin n = escom ",OZ" [n]

dragcircle [x1,y1,x2,y2] =
	circle [x1,y1,r]
        where
	r = bcroot (square (diff x1 x2) + square (diff y1 y2))

dragline = "%l\n"

dragrect = "%r\n"

font x = escom "F" [x]

func mode = escom "b" [mode]

go = escom "g" -- x y

highlight = escom "H" -- x y w h

line = escom "l"      -- x0 y0 x1 y1

--CR explanation or abstraction needed for numeric literals here
mapevent event = if event == 3 || event == 4 then 2-event else event

newwin = escom "Z"    -- x y w h

rcircle r = escom "o" [r]

selectwin n = escom "Z" [n]

setevent event str = escom ("e"++str) [mapevent event, length str]

setmode mode = escom "S" [mode]

shapewindow = escom "W" -- x y w h

standend = '\ESC':",n"

standout = '\ESC':"i" -- OK

stringto win x y str = escom ("."++str) [win,x,y,length str]

textregion = escom "t" -- x y wide high

textreset = '\ESC':"t"  -- OK



