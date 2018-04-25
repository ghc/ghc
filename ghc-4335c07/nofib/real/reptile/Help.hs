-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991
module Help(
       helpend, helpdraw, helptile, helpalter, helptsave, helptclear,
       helptget, helpt4, helpquit, helpbt, helppic, helpdesign,
       helpdsave, helpdclear, helpdget, inithelp, errmes) where 

import Layout
import Mgrfuns

helpsetup, helpend :: [Char]
helpdraw, helpdsave, helpdclear, helpdget :: [Char]
helptile, helpalter, helptsave, helptclear, helptget, helpt4 :: [Char]
helpquit, helpbt, helppic, helpdesign :: [Char] 

helpsetup = textregion helptextarea ++ font 8

helpend = cleara helptextarea ++ font 13

helpdraw = 
	"This button puts you in drawing mode.\n" ++
	"Lines can be drawn in the STAMP DESIGN \n" ++
	"area by holding down the middle button,\n" ++
	"and deleted by clicking\n" ++
	"with the right one.\n" ++
	"\nLittle circles will appear if a line\n" ++
	"touches the edge of the square.\n" ++
	"These indicate positions on all the sides\n" ++
	"that would contact that line in each of\n" ++
	"the possible orientations of the print.\n" ++
	"Unless special effects are being sought,\n"++
	"the recommendation is that all little\n" ++
	"circles be attached to a line.\n" ++
	endmes

helptile = 
	"In this mode, orientations of the print\n" ++
	"can be placed in the TILE DESIGN area\n" ++
	"to create a 64 X 64 big tile\n" ++
	"\nUse the right button to select a print\n" ++
	"and the middle button to place it.\n" ++
	"\nWithin the tiling area the right button\n" ++
	"may also be used to delete a square\n" ++
	"\nTo rotate or invert squares within\n" ++
	"the big tile it may be more convenient\n" ++
	"to use the ALTER mode." ++
	endmes

helpalter = 
	"In ALTER mode, squares within the\n" ++
	"big tile can be adjusted.\n" ++
	"The middle button causes them to \n" ++
	"rotate clockwise.\n" ++
	"The right button causes them to\n" ++
	"invert." ++
	endmes

helptsave = 
--	"This SAVE button prompts for a filename.\n" ++
--	"It creates a file from which\n" ++
--	"the actual big tile may be printed out\n" ++
--	"(from outside this program)\n" ++
--	"with the command: " ++ printcommand ++
--	"\nThe filename can also be used\n" ++
--	"to retrieve the pattern of orientations used\n" ++
--	"in the big tile, so that this may be used\n" ++
--	"in conjunction with another print.\n" ++
--	"\n\nThe pattern is retrieved by use of the\n" ++
--	"GET button\n" ++
	"Sorry, SAVE is temporarily inoperative." ++  --CR
	endmes

helptclear = 
	"This CLEAR button clears the TILE DESIGN\n" ++
	"region and draws an empty grid." ++
	endmes

helptget =
--	"GET enables previously stored patterns\n" ++
--	"of orientations to be retrieved.\n" ++
--	"Type in the name of the pattern\n" ++
--	"to be retrieved\n" ++
--	"\n\nIn addition to this there are some\n" ++
--	"predefined patterns that can be imposed\n" ++
--	"on the current print: " ++ predefinedpats ++
--	"\nFor these, type the pattern name\n" ++
--	"preceded by *\n" ++
	"Sorry, GET is temporarily inoperative." ++  --CR
	endmes

helpt4 = 
	"The T4 button tiles the whole big tile\n" ++
	"with the pattern of the four squares\n" ++
	"in the top left hand corner\n" ++
	endmes

helpquit = 
	"\n\n\nClicking on QUIT allows you\n" ++
	"to leave the program.\n" ++
	endmes

helpbt = 
	"\n\n\nWithin the TILE DESIGN area,\n" ++
	"a big tile, based on orientations of\n" ++
	"a print design, can be built.\n" ++
	"\nUsing TILE mode the right button\n" ++
	"will select from a palette at the bottom\n" ++
	"of the screen, and the middle button will\n" ++
	"place the selection within the big tile.\n" ++
	"Within the area the right button will\n" ++
	"delete squares.\n" ++
	"\nUsing ALTER mode the right button will\n" ++
	"invert squares, and the middle button \n" ++
	"will rotate them." ++
	endmes

helppic = 
	"\nThese boxes show the eight possible\n" ++
	"orientations of the print that is\n" ++
	"to be used in tiling\n" ++
	"\nWhen in tiling mode, clicking with the\n" ++
	"right button over one of these\n" ++
	"will make it the \"current selection\".\n" ++
	"Clicking with the middle button in\n" ++
	"the TILE DESIGN grid, will put that\n" ++
	"orientation of the print at that place" ++
	endmes

helpdesign = 
	"\n\n\nThis is the area in which to design \nyour print.\n" ++
	"\nDraw lines by holding down the\n middle button.\n" ++
	"Delete lines by clicking with the\n right button.\n" ++
	"\n\nA print that has previously been saved\n" ++
	"can be restored by clicking on GET\n" ++
	"then typing in the filename at the prompt.\n" ++
	endmes

helpdsave = 
--	"\nClicking on this SAVE button allows\n" ++
--	"you to save a design for future\n" ++
--	"re-use.\n\n" ++
--	"You will be prompted for a file name\n" ++
--	"in which it will be saved\n\n" ++
--	"To restore the design, click on GET\n" ++
--	"and type in the name of the file in which\n" ++
--	"it has been kept." ++
	"Sorry, SAVE is temporarily inoperative." ++  --CR
	endmes

helpdclear = 
	"\n\n\nThis clears the PRINT DESIGN grid.\n" ++
	"The print currently being worked on\n" ++
	"will be lost, unless it has been\n" ++
	"explicitly SAVEd first\n" ++
	endmes

helpdget = 
--	"\n\n\nTo restore a print that has previously\n" ++
--	"been saved, click on GET and then\n" ++
--	"type in the filename." ++
	"Sorry, GET is temporarily inoperative." ++  --CR
	endmes

inithelp, errmes, endmes{-, printcommand, predefinedpats-} :: [Char]

inithelp = helpsetup ++ clear ++  
	"\n\n\n\nTo find out the use of a particular\n" ++
	"menu button or region of the screen, \n" ++
	"click over the item you wish to\n" ++
	"investigate.\n"

errmes = "\n\n\nYou have clicked over an area \n" ++
	    "of no particular interest.\n" ++
	    endmes

endmes = "\n\n PRESS RETURN TO RETURN TO THE PROGRAM\n" ++
	     "OR CLICK SOMEWHERE ELSE TO FIND OUT MORE\n"

--UNUSED: printcommand = "cat filename|lpr -apple1"

{- UNUSED:
predefinedpats = "\n	quartet\tquartets\n" ++
		     "	lwheel\trwheel\n" ++
		     "	wheels1\twheels2\n" ++
		     "	pic1\tpic2\n" ++
		     "	escher1\tescher2\n" ++
		     "	symcols\tplain"
-}
