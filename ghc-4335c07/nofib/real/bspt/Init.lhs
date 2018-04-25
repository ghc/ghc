> module Init 
	
	Module to deal with the screen and mouse initialisation.
	Also contains function to control active screen regions.

>	(	initialiseMouse,initialiseScreen,
> 		labelButtons,clearRender,clearTree,clearText,
> 		toNoTextRegion,toTextRegion,
> 		toMouseRegion,
>		indicate,labelClassify,labelDefinePoly,
>		unlabelButtons,mark,reset)

> where
> import Params (Command(..),Button(..),delimiter,renderHeight,windowWidth,
>		 windowHeight,renderLeft,buttons,mouseCaptionAcross,
>		 mouseCaptionDown,button1TextOrigin,button,button1Box,
>		 button2Box,button3Box,mouseBox,
>		 button2TextOrigin,button3TextOrigin,gap,renderRegion,
>		 treeRegion,windowRegion,noTextRegion,textRegion,mouseRegion,
>		 textIn,textDown,buttonIndent,buttonWidth,buttonHeight)
> import Stdlib (map2,mapcat,seQuence)
> import MGRlib (setEvent, setMode, setCursor, textReset, clear,
>		 line, movePrintTo, printOver, setTextRegion, writeVert)


	initialiseMouse: Sets up the middle button to return the
			 string defined by delimiter.
			 Sets up the right button to return the
			 string "x y\n" where x and y are co-ordinates
			 of the mouse pointer at the moment the button
			 is pressed.

> initialiseMouse :: String
> initialiseMouse = seQuence [	setEvent 1 "%p\n",
>				setEvent 2 (delimiter++"\n")]


	initialiseScreen: Sets the screen into overwrite mode,
			  switches the off,
			  sets the textscreen to be the whole window,
			  clear the textscreen.
			  It then gets drawscreen to set up the 
			  Geometric modeller interface screen.
			  Finally it switches to the 'notext' region.
			  
> initialiseScreen :: String
> initialiseScreen =  	seQuence [	setMode 7,
>					setCursor 5, 
>					textReset, 
>					clear,
> 					drawScreen,
> 					toNoTextRegion]


	drawScreen: Draws two lines splitting the screen horizontally
			and vertically. Creates four regions:
				The Button Pad
				The Rendering Window
				The Tree Form Window
				The Mouse State/Text window.
			It thens intiates the drawing of the buttons on the 
			Button Pad and the Mouse State diagram.

> drawScreen :: String
> drawScreen = 	seQuence [	line [0,renderHeight+1,windowWidth,renderHeight+1],
>		 	      	line [renderLeft-1,0,renderLeft-1,windowHeight],
>				drawButtons buttons,
>				drawMouse]


	drawMouse: Draws the Mouse State diagram. This is a Box labelled
			"Mouse" with three interior boxes representing
			the mouses button. The left is permanently
			marked "SYSTEM RESERVED" since the MGR window 
			manager reserves this button for MGR operations.
			The remaining button are labelled for the initial
			state.	

> drawMouse :: String
> drawMouse = seQuence [	movePrintTo mouseCaptionAcross mouseCaptionDown "Mouse", 
> 		    		drawBox mouseBox,
> 				drawBox button1Box,
> 				drawBox button2Box,
> 				drawBox button3Box,
> 				writeVert button1TextOrigin (map2 spacer "RESERVED" "SYSTEM !"),
> 		    		labelButtons ("BUTTON   ","DISABLED ") ("SELECT   ","OPERATION")]


	labelButtons: Takes two pairs of strings and labels the 
			middle and right button boxes on the mouse state
			diagram respectively.

> labelButtons :: (String,String) -> (String,String) -> String
> labelButtons (label2_1,label2_2) (label3_1,label3_2)
> 			= seQuence [	toMouseRegion,
> 			  		writeVert button2TextOrigin (map2 spacer label2_1 label2_2),
> 					writeVert button3TextOrigin (map2 spacer label3_1 label3_2),
> 			  		toNoTextRegion]

> spacer :: Char -> Char -> String
> spacer x y = x:gap:[y]



	clearRender: Clear the render section of the screen of its contents

> clearRender :: String
> clearRender = clearRegion renderRegion


	cleartree: Clear the tree-form section of the screen of its contents

> clearTree :: String
> clearTree = clearRegion treeRegion


	clearButton: Clear the button at height d of the screen of its contents

> clearButton :: Button -> String
> clearButton (_,d,_) = clearRegion (button d)

> clearRegion :: [Int] -> String
> clearRegion region = seQuence [setTextRegion (inside region), clear, toNoTextRegion]

> clearText :: String
> clearText = clear

> inside :: [Int] -> [Int]
> inside [top,left,width,height] = [top+1,left+1,width-2,height-2]

	toScreenRegion: Set up the whole screen as the current text window	

> toScreenRegion :: String
> toScreenRegion = setTextRegion windowRegion


	toNoTextRegion: Set up the notext region as the current text window	

> toNoTextRegion :: String
> toNoTextRegion = setTextRegion noTextRegion


	toTextRegion: Set up the text region as the current text window	

> toTextRegion :: String
> toTextRegion = setTextRegion textRegion


	toMouseRegion: Set up the mouse region as the current text window	

> toMouseRegion :: String
> toMouseRegion = setTextRegion mouseRegion



	drawButtons: Draw the button on the button pad

> drawButtons :: [Button] -> String
> drawButtons = mapcat drawButton


	drawButton: Draws one button. A button is defined by
			its representing Command, its position
			on the pad and its label.
			Drawing the button involves drawing the button
			box on the pad at a depth d and labelling it.

> drawButton :: Button -> String
> drawButton (_,d,name) = seQuence [ 	printOver textIn (d+textDown) name,
> 					drawBox [buttonIndent,d,buttonWidth,buttonHeight]  ]


	mark: Takes a button indicated by its position on the pad
		and wipes of the remaining button labels. Thus 
		indicates the button selected.
	
> mark :: Command -> String
> mark cmd = seQuence [	toScreenRegion,
>			mapcatButton clearButton cmd buttons,
>			toNoTextRegion]

	noMark: Restores the buttons after an operation has taken place

> noMark :: Command -> String
> noMark cmd = seQuence [	toScreenRegion, 
>			mapcatButton title cmd buttons, 
>			toNoTextRegion]
> 		where 	
> 		title (_,d,str) = movePrintTo textIn (d+textDown) str 


	mapcatButton : applies a function f to each button except the one
		at height h. Note can use simple map once button h is
		encountered.

> mapcatButton :: (Button->String) -> Command -> [Button] -> String
> mapcatButton f cmd [] = []
> mapcatButton f cmd (butt@(cmd',_,_):butts) 
>				| cmd==cmd' = mapcat f butts
>		 		| otherwise = f butt ++ mapcatButton f cmd butts



	drawBox: Draws a box on the screen with origin (i.e. topLeft corner)
		at point (x,y) and of height h and width w.

> drawBox :: [Int] -> String
> drawBox [x,y,width,height] = seQuence [	line [x,y,x,y+height],
> 				 		line [x,y+height,x+width,y+height],
> 						line [x+width,y+height,x+width,y],
> 						line [x+width,y,x,y]]


	reset: Disclaims control of the window when GMS is Quit.
		Cursor returned to normal and text region 
		returned to full screen.

> reset :: String
> reset = seQuence [mark Quit, textReset, setCursor 0]

        indicate : Unlabels all button but the one selected,
                        evaluates the string given
                 and labels the buttons again.
 
> indicate :: Command -> [String] -> String
> indicate Null _ = ""
> indicate cmd str = mark cmd ++ seQuence str ++ noMark cmd
 

        unlabelButtons, labelDefinePoly, labelClassify:
                Shorthand definitions for labelling actions.

> unlabelButtons = labelButtons ("BUTTON   ","DISABLED ")
>                                ("SELECT   ","OPERATION")

> labelDefinePoly = labelButtons ("COMPLETE ","POLYGON  ")
>                                  ("DEFINE   ","VERTEX   ")

> labelClassify = labelButtons ("FINISH   ","CLASSIFY ")
>                               ("CLASSIFY ","POINT    ")
 

