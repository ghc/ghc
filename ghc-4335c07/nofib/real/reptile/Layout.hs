-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991

module Layout( setup,dpxyorig, dpxygap, dpxynum,invisibletext,
               vistextreg, designarea,tilearea,inpicarea,picarea,
	       textarea, helptextarea, dpgrid,tpgrid,picgrid,
	       cleara, picxorig, picyorig, picbox,menumark,unmenumark,
	       tpxorig, tpyorig, tpxygap,inrect, incirc,
	       todesign, tofiddle, totile, tohelp, indgrid, 
	       inbigtile, indesign, indsave, indclear,
	       indget, intsave, intclear, intget, intile4, 
	       inhelp, inquit, intodraw, intotile, intoalter) where

import Mgrfuns
import Drawfuns
import Diff

-- dp definitions relate to the design phase of the program
--CR so they are now obsolete - in name at least

dpxyorig, dpxygap, dpxynum :: Int
dpxyorig = 20
dpxygap  = 20
dpxynum  = 19

dpfun :: Int -> Int -> [Char]
dpfun    = drawdot

dpgrid :: [Char]
dpgrid = grid dpxyorig dpxyorig dpxygap dpxygap dpxynum dpxynum dpfun

designarea :: [Int]
designarea = [0,0,390,390]

-- tp definitions relate to the tiling phase of the program
--CR so they too are obsolete - in name at least

tpxorig, tpyorig, tpxygap, tpxynum :: Int
tpxorig = 524
tpyorig = 20
tpxygap = 72
tpxynum = 9

tpfun :: Int -> Int -> [Char]
tpfun   = drawdot

tpgrid :: [Char]
tpgrid = grid tpxorig tpyorig tpxygap tpxygap tpxynum tpxynum tpfun

tilearea :: [Int]
tilearea = [521,18,580,580]

-- tm definitions relate to the menu for the tiling phase

tmxorig, tmyorig, tmxygap, tmxnum, tmynum :: Int
tmxorig = 485
tmyorig = 282
tmxygap = 57
tmxnum  = 1
tmynum  = 6

tmfun :: Int -> Int -> [Char]
tmfun   = circ 28

tmgrid :: [Char]
tmgrid = grid tmxorig tmyorig tmxygap tmxygap tmxnum tmynum tmfun

-- dm definitions relate to the menu for the design phase

dmxorig, dmyorig, dmxygap, dmxnum, dmynum :: Int
dmxorig = 425
dmyorig = 54
dmxygap = 57
dmxnum  = 1
dmynum  = 4

dmfun :: Int -> Int -> [Char]
dmfun   = circ 28

dmgrid :: [Char]
dmgrid = grid dmxorig dmyorig dmxygap dmxygap dmxnum dmynum dmfun

-- pic definitions relate to the display of the eight orientations
-- of the print

picxorig, picyorig, picxygap, picxnum, picynum :: Int
picxorig = 624
picyorig = 676
picxygap = 100
picxnum  = 4
picynum  = 2

picfun :: Int -> Int -> [Char]
picfun   = squ 74

picgrid :: [Char]
picgrid =
  grid (picxorig -1) (picyorig-1) picxygap picxygap picxnum picynum picfun

inpicarea :: Int -> Int -> Bool
inpicarea = inrect 624 676 400 200

picarea :: [Int]
picarea = [623,675,400,200]

picbox :: Int -> [Int]
picbox 1 = [picxorig,picyorig,72,72]
picbox 2 = [picxorig + picxygap,picyorig,72,72]
picbox 3 = [picxorig + (2 * picxygap),picyorig,72,72]
picbox 4 = [picxorig + (3 * picxygap),picyorig,72,72]
picbox 5 = [picxorig,picyorig + picxygap,72,72]
picbox 6 = [picxorig + picxygap,picyorig + picxygap,72,72]
picbox 7 = [picxorig + (2 * picxygap),picyorig + picxygap,72,72]
picbox 8 = [picxorig + (3 * picxygap),picyorig + picxygap,72,72]

textarea :: [Int]
textarea = [50,550,300,300]

-- vistextreg is the region into which to type filenames
vistextreg :: [Char]
vistextreg = textregion [50,615,200,100]

helptextarea :: [Int]
helptextarea = [50,500,380,400]

-- cleartextreg incorporates vistextreg and the prompt region
cleara :: [Int] -> [Char]
cleara area = textregion area ++ clear ++ invisibletext

-- these are strings to go in the menu boxes
menustrings :: [Char]
menustrings = func 4 ++
	      font 8 ++
	      stringto 0 405 64 "DRAW" ++
	      stringto 0 405 121 "SAVE" ++
	      stringto 0 410 178 "GET" ++
	      stringto 0 402 235 "CLEAR" ++
	      stringto 0 468 292 "TILE" ++
	      stringto 0 462 349 "ALTER" ++
	      stringto 0 468 404 "SAVE" ++
	      stringto 0 471 463 "GET" ++
	      stringto 0 464 520 "CLEAR" ++
	      stringto 0 474 577 "T4" ++
	      font 12 ++
	      stringto 0 457 729 "HELP" ++
	      stringto 0 457 829 "QUIT" ++
	      font 13 ++
	      stringto 0 112 450 "STAMP DESIGN"++
	      stringto 0 730 666 "TILE DESIGN"++
	      func 15

-- invisibletext sets up a scrolling text region, then moves the
-- text cursor out of it. 

invisibletext :: [Char]
invisibletext = vistextreg ++
  		go [500,500] ++
  		aligntext ++ "\n"


inrect :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
inrect x y w h xp yp = xp > x && xp <= x + w &&
		       yp > y && yp <= y + h

dmcirc, tmcirc, amcirc, hmcirc :: [Char]
dmcirc = circle [425,54,30]
tmcirc = circle [485,282,30]
amcirc = circle [485,339,30]
hmcirc = circle [485,712,38] 

modemark :: [Char] -> [Char]
modemark str = 
	case str of
	  "draw" -> dmcirc ++ undo (tmcirc ++ amcirc ++ hmcirc)
	  "tile" -> tmcirc ++ undo (dmcirc ++ amcirc ++ hmcirc)
	  "alter"-> amcirc ++ undo (dmcirc ++ tmcirc ++ hmcirc)
	  "help" -> hmcirc ++ undo (dmcirc ++ tmcirc ++ amcirc)

menumark :: [Char] -> [Char]
menumark "tsave" = circ 30 tmxorig (tmyorig + (2 * tmxygap))
menumark "tget"  = circ 30 tmxorig (tmyorig + (3 * tmxygap))
menumark "tclear"= circ 30 tmxorig (tmyorig + (4 * tmxygap))
menumark "t4"    = circ 30 tmxorig (tmyorig + (5 * tmxygap))
menumark "dsave" = circ 30 dmxorig (dmyorig +  dmxygap)
menumark "dget"  = circ 30 dmxorig (dmyorig + (2 * dmxygap))
menumark "dclear"= circ 30 dmxorig (dmyorig + (3 * dmxygap))

unmenumark :: [Char] -> [Char]
unmenumark = undo . menumark

incirc :: Int -> Int -> Int -> Int -> Int -> Bool
incirc xc yc r xp yp = square (xp - xc) + square (yp - yc) <= square r

indgrid :: [Int] -> Bool
indgrid [x0,y0,x1,y1] = indesign x0 y0 && indesign x1 y1

inbigtile, indesign :: Int -> Int -> Bool
inbigtile = inrect tpxorig tpyorig tpwh tpwh
	    where 
	    tpwh = tpxygap * (tpxynum - 1)
indesign = inrect 0 0 390 390

intodraw, indsave, indget, indclear :: Int -> Int -> Bool
-- some abstraction needed here!!!
intodraw = incirc dmxorig dmyorig 28
indsave = incirc dmxorig (dmyorig + dmxygap) 28
indget = incirc dmxorig (dmyorig + (2 * dmxygap)) 28
indclear = incirc dmxorig (dmyorig + (3 * dmxygap)) 28

intotile, intoalter, intsave, intget, intclear, intile4 :: Int -> Int -> Bool
intotile = incirc tmxorig tmyorig 28
intoalter = incirc tmxorig (tmyorig + tmxygap) 28
intsave = incirc tmxorig (tmyorig + (2 * tmxygap)) 28
intget = incirc tmxorig (tmyorig + (3 * tmxygap)) 28
intclear = incirc tmxorig (tmyorig + (4 * tmxygap)) 28
intile4 = incirc tmxorig (tmyorig + (5 * tmxygap)) 28

inhelp, inquit :: Int -> Int -> Bool
inhelp = incirc 485 712 36
inquit = incirc 485 812 36

-- the mode buttons:
buttons :: [Char]
buttons = circle [485,712,36] ++ circle [485,812,36]

setup :: [Char]
setup = textregion [0,0,0,0] ++
	clear ++ 
 	tpgrid ++
 	dpgrid ++
 	tmgrid ++
 	dmgrid ++
 	picgrid ++
  	buttons ++
 	invisibletext ++
 	menustrings ++
	todesign

-- cs shouldn't have an effect if all the coordinates are
-- outside the design area
todesign :: [Char]
todesign = setevent 1 "msc %p\n" ++ setevent 2 "cs %l\n" ++ modemark "draw"

-- in this mode 1 should be able to select orientations if clicked
-- over one of them, perhaps incorporate this in msa
totile :: [Char]
totile = setevent 1 "msa %p\n" ++
         setevent 2 "put %p\n" ++ 
         tpgrid ++
         modemark "tile" 

tofiddle :: [Char]
tofiddle = setevent 1 "msb %p\n" ++ setevent 2 "rot %p\n" ++ modemark "alter"

tohelp :: [Char]
tohelp = setevent 1 "msd %p\n" ++ setevent 2 "msd %p\n" ++ modemark "help"



