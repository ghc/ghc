> module EuclidGMS

	Module that provides addition Euclidean operations.
	Operations here are more application based.

>		(	Region,mkRegion,getRegion,newRegion,
>			Partition,mkPart,getPart,
> 			Location(..),location, flip_YORK,
> 			bisect,toBack,section,findVertices,
>			inScreen,renderBorder,
>			-- And the following to reduce imports higher up
>			Point(..),Halfspace(..),Line,Face(..),Faces,space,convert,
>			mkFace,mkPoint,drawSegment,triangleArea, Segment)

> where
> import GeomNum
> import Euclid (Point(..),Line,Halfspace(..),Face(..),Faces,Segment,
>		 mkFace,getMyLine,getSegment,drawSegment,mkPoint,
>		 space,solve,invert,
>		 triangleArea,mkPolygon,convert)
> import Params (renderTop,renderHeight,renderLeft,windowWidth)
> import Stdlib (all_YORK,mkset)



> type Partition = Face 

> mkPart :: Region -> Line -> Partition
> mkPart region line = Fc (section region line) line

> getPart :: Partition -> Line
> getPart p = getMyLine p


	The type Region describes a convex sub_space as the space formed
	by the intersection of the Rear halfspaces of the lines present
	in the list representation.

> data Region = Rg [Face]

> mkRegion :: [Face] -> Region
> mkRegion faces = Rg faces

> getRegion :: Region -> [Face]
> getRegion (Rg faces) = faces

> newRegion :: Region -> Face -> Region
> newRegion (Rg faces) face = Rg (face:faces)


	Data type Location is an enumeration of the possible relationships
	between a line and a face.

> data Location = Coincident | Intersects | ToTheRear | ToTheFore deriving (Eq)


	location: This function returns an indicator to the relationship
		between the given Line and Face. Relationship
		is determined by the halfspace indicated by space. 

> location :: Line -> Segment -> Location
> location line (p1,p2) = case (locale p1,locale p2) of
> 				(Coin,Coin) 	-> Coincident
> 				(Fore,Rear) 	-> Intersects
> 				(Rear,Fore) 	-> Intersects 
> 				(Rear,_) 	-> ToTheRear 
> 				(_,Rear) 	-> ToTheRear
> 				(_,_) 		-> ToTheFore
> 			where 
> 			locale = space line


	bisect : Returns a pair of faces formed by splitting the given face 
		 at the point where the line given intersects the face.
		 The faces are returned as a pair such that the first
		 element is the section of the original face that lies
		 in the Rear halfspace of the line given.
		 Note that it is assumed that the line does indeed intersect 
		 the face.

> bisect :: Face -> Line -> (Face,Face)
> bisect (Fc (pt1,pt2) line1) line2 = 
>		if toBack pt1 line2 then (face1,face2) else (face2,face1) 
> 		where
> 		face1 = Fc (pt1,pti) line1
>		face2 = Fc (pti,pt2) line1
> 		pti = solve line1 line2 


	flip_YORK : reverse the orientation of a face

> flip_YORK :: Face -> Face
> flip_YORK (Fc (a,b) l) = Fc (b,a) (invert l)

	toBack: Predicate to test that a point does not lie in the
		 Fore half space of the line given.

> toBack :: Point -> Line -> Bool
> toBack pt line = space line pt /= Fore


	inScreen: Predicate to test that a point lies somewhere on the rendering
			screen. Note that the rendering screen in implicitly 
			defined (by parameters in Params.hs).

> inScreen :: Point -> Bool
> inScreen (Pt x y) = xCoordInRange x && yCoordInRange y


	renderBorder: Describes the Rendering screen by the equations of
			its borderlines.

> renderBorder :: Region
> renderBorder = mkRegion (mkPolygon [  Pt left top,
>					Pt right top,
>					Pt right bottom,
>					Pt left bottom])
>		 where
>		 top = fromIntegral renderTop
>                bottom = fromIntegral renderHeight
>                left = fromIntegral renderLeft
>                right = fromIntegral windowWidth



  
	section: Generate the segment of a line that lies in the 
			convex region given.  

> section :: Region -> Line -> Segment
> section region line = f x
>	where
>	x = [x| x <- map (solve line.getPart) (getRegion region), inRegion region x]
>	f [pta,ptb] = (pta,ptb)
>	f a = f (mkset a)



 
	findVertices - obtains the list of vertices bounding a region
		The list is genereated by observation that the vertices will
		be a subset of those points stored in segments of regions Faces
		The list is unordered

> findVertices :: Region -> [Point]
> findVertices region = [pts | pts <- xs ++ ys, inRegion region pts]
>       where
>       xs = [x | (x,_) <- segments]
>       ys = [y | (_,y) <- segments] 
>       segments = map getSegment (getRegion region)



	inRegion - predicate - true if the point given is in the region

> inRegion :: Region -> Point -> Bool
> inRegion region pt = all_YORK (map (toBack pt.getPart) (getRegion region))
