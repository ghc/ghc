> module Interface

	Module defines the functionality of the GMS.

>	(modeller)

> where

> import Init (indicate,labelClassify,labelDefinePoly,unlabelButtons,clearRender,
>		clearText,toNoTextRegion,toTextRegion,reset,clearTree)
> import Params (Command(..))
> import Stdlib (mapcat)
> import Euclid (Point(..),mkPolygon,Face,Faces)
> import BSPT (BSPT,Status(..),classifyPoint,buildBSPT,area)
> import Render (render,drawBSPT,partitionedDraw,drawFaces,prettyPrintBSPT)
> import GeomNum 
> import Merge (union,intersection,subtract_YORK,complement)
> import Interpret (Operation,Operations)


	modeller: This is the core function of the program
		It interprets the users requests and evaluates
		the appropriate functions. There is a different 
		clause for each command. Each will be described 
		individually. However there are some common 
		features: all use indicate to mark the button
		pressed for the current action and to switch it
		off after. All use the command sequence to cause
		a list of actions to be performed in sequence.
		Each request performed is followed by a call
		back to modeller with the users next command
		to be interpreted.
		

	Partition: Clears the rendering section of the screen,
		Causes the partitionings of the current of the
		current object to displayed on the screen.
	
> modeller :: BSPT -> Operations -> String	
> modeller current ((Quit,_):_) = reset
> modeller current (operation@(op,_):more) =
>	indicate op actions ++ modeller newstate more
>	where 
>	(actions,newstate) = perform current operation

> perform :: BSPT -> Operation -> ([String],BSPT)
> perform current (Partition,_) 
>	= ([clearRender, partitionedDraw current],
>		current)


	Render: Fills in the current object with hashed pattern

> perform current (Render,_)
>	= ([render current], current)


	Classify: Labels the buttons for classify mode,
		switches to and clears the text region,
		prints a string derived from the users 
		points selections and the classifications
		of those points w.r.t. the current object.
		'Unlabels' the buttons, switched back to the
		noText region.

> perform current (Classify,points) 
>	= ([ labelClassify, toTextRegion, clearText,
>	   (str ++ "\n"), unlabelButtons, toNoTextRegion],
>	   current)
>	where 	
>	str = mapcat printstatus points
>	printstatus pt = "Status: "++
>			 (show (classifyPoint pt current))++"\n"


	Area: Switches to the text region, displays the area
		of the current object in pixels, switches
		back to noText region. Derives the area
		by calculating the summation of the area of each
		region of the object.

> perform current (Area,_) =
>	([toTextRegion, "Area (pixels)\n",
>	  show (rnd objArea), toNoTextRegion],
>	  current)
>	where	
>	objArea = area current


	Complement: Clears the tree-from window, draw the tree-form
		of the complemented current object,
		clears the render screen and draws the complemented
		object. The complemented object becomes current.
		
> perform current (Complement,_) 
>	= ([clearTree,prettyPrintBSPT btree,clearRender,
>	    drawBSPT btree],
>	    btree) 
>	where	 
>	btree = complement current
	

	Polygon: clears the tree-from and render windows,
		labels the buttons for definition of the polygon.
		Draws the polygon formed by the points defined
		by the user. Note this is done lazily, so that 
		the polygon is seen as it is created. The render
		screen is then cleared and the BSPT representation
		of the same object is drawn. The tree-form of the 
		object is also displayed. The buttons are unlabelled.
		
> perform current (Polygon,operand) 
>	= ([clearRender, labelDefinePoly, drawFaces polygon, 
>	    "\n", clearTree, grip_stats (prettyPrintBSPT btree),
>		clearRender, drawBSPT btree, unlabelButtons],
>	   btree) 
>	where	
>	btree = buildBSPT (validate polygon)
>	polygon = mkPolygon (transform operand)
>	grip_stats :: String -> String	-- ******** partain **********
>	grip_stats s = s



	Null: The Null command occurs when the mouse has been pressed
		outside of all buttons. It simply calls perform
		with the next user request supplied by interpret.

> perform current (Null,_) = ([],current)

	Intersect, Union, Subtract : Defined with the same
		function. For each the buttons are labelled
		for polygon definition. The polygon (the
		second operand to the operation) is drawn.
		The render screen is cleared and the object 
		defined by the operation applied to the 
		current object and the new polygon.
		The tree-form of this object is then 
		printed on a cleared tree window.
		The buttons are unlabelled.
		Note that the funtion boolOp 
		actually evaluates the boolOp of the two
		BSP trees.
		
> perform current (cmd,operand) 
>	= ([ labelDefinePoly, drawFaces polygon,
> 	     "\n", clearTree, prettyPrintBSPT btree, 
>	     clearRender, drawBSPT btree, unlabelButtons],
>	     btree) 
>	where	
>	btree = boolOp cmd current (buildBSPT (validate polygon))
>	polygon = mkPolygon(transform operand)


	boolOp: Takes two BSP trees and an operation and
		merges the trees according to the operation returning
		a new BSP tree.

> boolOp :: Command -> BSPT -> BSPT -> BSPT
> boolOp Union current operand = union current operand 
> boolOp Intersect current operand = intersection current operand
> boolOp Subtract current operand = subtract_YORK current operand 

         validate - ensures that all polygons defined are closed, is have more then two
                vertices

> validate :: [a] -> [a]
> validate pts = if (length pts<3) then [] else pts      

	transform - execute a transformation to input points making them to the nearest 10 pixels

> transform :: [Point] -> [Point]
> transform = map trans
>		  where trans (Pt x y) = Pt (grid x) (grid y)
