> module Render 

	Render controls the displaying of BSP images and
	BSP trees.

>	(	render, 
> 		drawBSPT, drawFaces,
> 		partitionedDraw,prettyPrintBSPT)
  
> where
 
> import BSPT (BSPT(..),Status(..),countLeaves,scanLine,foldBSPT)
> import EuclidGMS (Point(..),Line,Face(..),Region,getRegion,
>		    Faces,mkFace,section,drawSegment,Segment,Partition)
> import Stdlib (mapcat,middle,const3)
> import Params (renderTop,renderHeight,renderLeft,windowWidth)
> import GeomNum
> import MGRlib (line,func,movePrintTo,writeVert)

 	render - fills in the image with a hashed pattern
 		Algorithm creates scan_line segments from
 		the list of rules accross the screen
 
> render :: BSPT -> String
> render btree = drawFaces (mapcat (scanLine btree) scanLines)
> 	where scanLines = rules (fromIntegral renderLeft) (fromIntegral windowWidth) (fromIntegral (renderTop+1))
  
 	rules - create the list of lines across the screen
 		between current and max lines skipping every
 		skip lines
  
> rules :: Numb -> Numb -> Numb -> Faces
> rules left right = rules'
>			where rules' current | current > (fromIntegral renderHeight) = []
>			      rules' current = mkFace ((Pt left current),(Pt right current)):rules' (current+15) 
  
  
 	drawBSPT - draws the edges of the image by drawing the lines
 		stored in the sub-hyperplanes
  
> drawBSPT :: BSPT -> String 
> drawBSPT = foldBSPT (const3 []) drawEmbedded
>		where 
>		drawEmbedded _ (faces,_) x y = drawFaces faces ++ x ++ y

	drawFaces - draws a list of faces
 
> drawFaces :: Faces -> String
> drawFaces = mapcat drawSegment.map faceToSection
>		where 
>		faceToSection (Fc section _) = section
  

	partitionedDraw - draws a BSPT image along with its partitions

> partitionedDraw :: BSPT -> String                
> partitionedDraw = foldBSPT temp3 temp 
>               where 
>		temp3 _ r _ = drawFaces (getRegion r)
>		temp _ _ = (++)

		const3 []
               drawPartitions (Fc part _) (faces,region) x y 
					= drawFaces faces ++
					    x ++ y

	prettyPrintBSPT - pretty print the tree form of a BSP tree

> prettyPrintBSPT :: BSPT -> String
> prettyPrintBSPT tree = func 4 ++ 
> 			 printBSPT  renderLeft windowWidth (renderHeight+40) tree ++  
> 			 func 15


	printBSPT - does the work for prettyPrintBSPT

> printBSPT :: Int -> Int -> Int -> BSPT -> String
> printBSPT le re d (Cell x _ _) | re-le<=20  =  writeVert ((middle le re),d) (map (\x->x:[]) (show x))
> printBSPT le re d (Cell x _ _) =  movePrintTo (middle le re) d (show x)
> printBSPT le re d tree | re-le<=20 = writeVert (mid,d) (map (\x->x:[]) (show (countLeaves tree)))
>						  where mid = middle le re
> printBSPT le re d (BSP part nodeinfo left right) = movePrintTo mid d "@" ++ 
>                                                (printBSPT le mid (d+14) left) ++
> 				 	 	 (printBSPT mid re (d+14) right)
> 						  where mid = middle le re
