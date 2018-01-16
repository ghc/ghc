> module BSPT

	Module that defines Binary Space Partitioning Trees and basic 
	operations thereon.

>	(	BSPT(..),Status(..),Point,Region,Line,
>		Face,Faces,buildBSPT,bsp',bsp'',mkCell, partFaces,
>		scanLine,countLeaves,classifyPoint,area, 
>		foldBSPT)

> where
> import EuclidGMS (	Location(..),Partition,Region,
>			 mkPart,getPart,newRegion,location,renderBorder,
>			 bisect,toBack,findVertices,flip_YORK,
>			 Point(..),Line,Face(..),Faces,Halfspace(..),
>			 space,convert,triangleArea,Segment )
> import GeomNum 
> import Stdlib (mapcat,const3)
> import Libfuns


	A binary space partition tree is either:
		A leaf node tagged Cell with three arguments--
			1. Status (as defined below)
				describes the state of the convex cell
			2. Region indicates the area described by the
				cell
			3. Gives the Area of the cell (used as a memoing
				argument).
		A branch node tagged BSP with the following arguments
			1. a Line equation description of the partitioning 
				of space at this node. 
			2. a triple that stores the faces that lie in the plane
				described by 1. an indication of the area 
				covered the tree whose root is this node and 
				the section of the partition that runs through the
				region.
			3/4. BSP trees representing the Rear and Fore 
				halfspaces formed by the partition at this node.

> data BSPT = Cell Status Region Numb | 
>		BSP Partition (Faces,Region) BSPT BSPT -- deriving (Text)


	Status is an enumeration of classifications of points with
	respect to objects.

> data Status = In | Out | On deriving (Eq,Show{-was:Text-})

	A partitioning is a triple of Faces representing the faces
	(w.r.t. a partition) in the rear halfspace, coincident to, and 
	in the fore halfspace respectively.
	
> type Partitioning = (Faces,Faces,Faces)



	buildBSPT : Creates a BSPT for a polygon defined by a B-rep
			(i.e. a list of faces). Building is done by
			auxiliary buildBSPTAux.

> buildBSPT :: Faces -> BSPT
> buildBSPT = buildBSPTAux Out renderBorder 

	buildBSPTAux: Builds a BSPT from the first argument list of 
			faces. The second argument carries the current
			region in consideration and is used for 
			augmenting created nodes and cells. The
			third argument indicates the default value
			of the branch and is In when building the
			left subtree and Out when building the right.
			The function heuristic selects a single face according
			to some determined rule to form the partition at the node.
			convert takes the Face and creates the equation for the
			line that embeds that face.
			partFaces creates a partitioning of the remaining 
			polygon face w.r.t. the partition chosen.

> buildBSPTAux :: Status -> Region -> Faces -> BSPT
> buildBSPTAux status region [] = mkCell status region 
> buildBSPTAux _ region faces   = par right (seq left (BSP partition (coin,region) left right))
>                       where 
>			left = buildBSPTAux In (newRegion region partition) rear
> 			right = buildBSPTAux Out (newRegion region (flip_YORK partition)) fore
>			(rear,coin,fore) = partFaces part faces
>			partition = mkPart region part
>			part = heuristic faces


	bsp' : Used to construct a BSPT when there is a possibility that
			both children are cells with the same status.
			When this occurs the cells are condensed.

> bsp' :: Partition -> (Faces,Region) -> BSPT -> BSPT -> BSPT
> bsp' part (faces,region) (Cell x _ a) (Cell y _ b) | x==y = Cell x region (a+b)
> bsp' part nodeInfo left right = BSP part nodeInfo left right


	bsp'' : Used to construct a BSPT when there is a possibility of 
			condensing as above and, additionally, there may
			be a need to update the faces that lie in the 
			sub-hyperplane formed by the partition.
			bsp'' also checks that a partition is needed.
			A partition is needed if there are faces stored
			in its hyperplane. bsp'' simplifies the tree when
			such a partition occurs, removing the redundent partition.

> bsp'' :: Partition -> (Faces, Region) -> BSPT -> BSPT -> BSPT
> bsp'' part (faces,region) left right 
>		= if newfaces==[] 
>	 	  then simplify part region left right 
>		  else BSP part (newfaces,region) left right
>			where 
>			newfaces = updateFaces left right faces

	simplify : implements the removal of a redundant partition. 
			Note need for region to be passed in. So that
			replacement node represents the correct area.

> simplify :: Partition -> Region -> BSPT -> BSPT -> BSPT
> simplify _ region (Cell _ _ _) (BSP part (faces,_) left right) 
>		= BSP (mkPart region (getPart part)) (faces,region) left right
> simplify _ region (BSP part (faces,_) left right) (Cell _ _ _) 
>		= BSP (mkPart region (getPart part)) (faces,region) left right
> simplify part region tree1 tree2 = bsp' part ([],region) tree1 tree2

	mkCell: construction function defined for simplification.
		Area calculation done by default method.

> mkCell :: Status -> Region -> BSPT
> mkCell status region = Cell status region (areaRegion region)


	partFaces : Splits a list of faces into three groups.
			Those lying in the rear halfspace defined by part.
			Those lying in the sub-hyperplane defined by part.
			Those lying in the fore halfspace defined by part.
			The function location determines the which case 
			applies for each face in the list.
			Note that when the face intersects the partition,
			that face is bisect(ed) and the two resulting faces
			added to the appropriate group.

> partFaces :: Line -> Faces -> (Faces,Faces,Faces) 
> partFaces part [] = ([],[],[])
> partFaces part (face@(Fc section _):faces) 
>			      = par rest 
>				(case (location part section) of
> 					Coincident -> (rear,face:coin,fore)
> 					Intersects -> (rearHalf:rear,coin,foreHalf:fore)
> 					ToTheRear  -> (face:rear,coin,fore)
> 					ToTheFore  -> (rear,coin,face:fore))
> 				where
> 				(rear,coin,fore) = rest
>				rest = partFaces part faces
> 				(rearHalf,foreHalf) = bisect face part


	heuristic : decides according to some rule which faces to pick
			to form the current partition. Returns this face and the rest 
			as a pair.
			Currently is selects the first.
			When going parallel it will need to split the faces
			into two equal(ish) sized groups.

> heuristic :: Faces -> Line
> heuristic (Fc _ l:_) = l


	updateFaces : Takes a list of faces and produces a new list
			where the new list is the old list with segments
			of lines removed that no longer live on the edge
			of the object. It does this by first classfiying 
			each edge with respect to the left subtree.
			These classification are passed down into the right
			subtree which as it classifies the face can decide
			whether it lies on the edge or not. This is done
			on the basis that to lie on the edge a faces 
			classification most differ for each subtree.

> updateFaces :: BSPT -> BSPT -> Faces -> Faces
> updateFaces left right = mapcat (rubout right).classifyFace left


	classifyFace : produces a list of face-classification pairs
			formed by taking the segments of each face 
			with respect a BSP tree and tagging appropriate
			at the Cell nodes.

> classifyFace :: BSPT -> Faces -> [(Face,Status)]
> classifyFace tree = mapcat (segments tagStatus tree) 
> 			where
> 			tagStatus x face = [(face,x)]


	rubout : Effectively classfies to the right and filters away faces
			that have the same classification as they did to the
			right.

> rubout :: BSPT -> (Face,Status) -> Faces
> rubout tree (face,x) = segments (erase x) tree face
> 	where erase x y face | x==y = []
> 			      | otherwise = [face]


	segments : Higher order generalised function. 
			First argument is a cell operation.
			The face defined in the third argument
			is cut into segments by a decent of the BSP tree
			defined in the second argument. A segment of a face
			reaching a Cell node has the cell operation performed
			on it. 
			For example to segment a face into part lying in the different
			concave cells defined by the BSP tree. Use
				segments ignoreStatus
					where ignoreStatus a b = b

> segments :: (Status->Face->[a]) -> BSPT -> Face -> [a]
> segments cellop (Cell status _ _) face = cellop status face
> segments cellop (BSP part@(Fc _ p) _ left right) face@(Fc fs _) 
>		= case (location p fs) of
> 			Coincident -> cellop In face 
> 			Intersects -> segments cellop left leftside ++ 
> 					segments cellop right rightside
> 			ToTheRear  -> segments cellop left face
> 			ToTheFore  -> segments cellop right face
> 		where
> 		(leftside,rightside) = bisect face p


	scanLine : Uses segments to filter away parts of the face given
			that are not in the object.
			filterInside ignores faces in Out cells.

> scanLine :: BSPT -> Face -> Faces
> scanLine = segments filterInside
> 		where
> 		filterInside In face = [face]
> 		filterInside Out _ = []


	foldBSPT : folds up a BSPT applying nodeop to the partition, node
			data and children at nodes and applying cellop to the 
			leafs.
			
> foldBSPT :: (Status->Region->Numb->a)->(Partition->(Faces,Region)->a->a->a)->BSPT->a
> foldBSPT cellop nodeop (Cell x r a) = cellop x r a
> foldBSPT cellop nodeop (BSP part nodeinfo left right)
>               = nodeop part nodeinfo left' right'
>                 where 
>		        left' = f left 
>			right' = f right
>			f = foldBSPT cellop nodeop 

	countLeaves : Uses foldBSPT to fold up the tree into a count 
			of the number of Leaves. cellop counts one for 
			each leaf. Nodeop is simply addition, but we first
			arrange to drop the partition arguments.

> countLeaves :: BSPT -> Int
> countLeaves = foldBSPT (const3 1) plus
>		where
>		plus _ _ = (+)

	area: determines the area of a BSPT represented object
		does this by folding plus over the tree with In cells
		counting their value Out cells counting zero.

> area :: BSPT -> Numb
> area = foldBSPT sumInRegions plus 
>		where 
>		sumInRegions In _ a = a
>		sumInRegions _ _ _ = 0
>		plus _ _ = (+)

	areaRegion - calculates the area of an convex region.

> areaRegion :: Region -> Numb
> areaRegion = sum.map triangleArea.triangles.findVertices

	triangles - splits a convex region into triangles

> triangles :: [Point] -> [[Point]]
> triangles [p1,p2] = []
> triangles [p1,p2,p3] = [[p1,p2,p3]]
> triangles (p1:p2:ps) = if left/=[] && right /=[] then
>                               triangles (p1:p2:left) ++ triangles (p1:p2:right)
>                        else triangles (p1:ps++[p2])
>                              where
>                              (left,right) = partPoints (convert p1 p2) ps



	partPoints - partition a list of points w.r.t. a line

> partPoints :: Line -> [Point] -> ([Point],[Point])
> partPoints eqn [] = ([],[])
> partPoints eqn (p:pts) = if toBack p eqn 
>				then (p:left,right) 
>				else (left,p:right)
> 				where 
> 				(left,right) = partPoints eqn pts


	classifyPoint - point classification w.r.t. object

> classifyPoint :: Point -> BSPT -> Status
> classifyPoint pt = foldBSPT status (deter pt) 
>                       where
>                       status s _ _ = s
>			deter pt (Fc _ part) _ = deter' (space part pt)
>			      where 
>			      deter' Fore _ x = x
>			      deter' Rear x _ = x
>			      deter' Coin x y | x==y = x	-- was: At (no such thing)
>					      | otherwise = On
