> module Merge

	Module deals with the merging of BSP trees

>	(union,intersection,subtract_YORK,complement)

> where

> import BSPT (BSPT(..),Status(..),bsp',bsp'',mkCell,partFaces,foldBSPT)
> import EuclidGMS (Point,Line,Face(..),Region,Location(..),Partition,Faces,
>		    newRegion,getPart,bisect,location,section,flip_YORK, Segment)
> import Stdlib (mappair)
> import GeomNum
> import Libfuns

> -- -------- Type decls ------------------------

	Definitions of boolean operators. Note use of higher order 
		function merge.

> union :: BSPT -> BSPT -> BSPT
> union = merge rules
>		where
> 		rules :: BSPT -> BSPT -> BSPT						
> 		rules cell@(Cell In _ _) tree = cell 
> 		rules cell@(Cell Out _ _) tree = tree
> 		rules tree cell@(Cell In _ _) = cell
> 		rules tree cell@(Cell Out _ _) = tree


> intersection :: BSPT -> BSPT -> BSPT
> intersection = merge rules
>		where
> 		rules :: BSPT -> BSPT -> BSPT						
> 		rules cell@(Cell In _ _) tree = tree
> 		rules cell@(Cell Out _ _) tree = cell
> 		rules tree cell@(Cell In _ _) = tree 
> 		rules tree cell@(Cell Out _ _) = cell
		
> subtract_YORK :: BSPT -> BSPT -> BSPT
> subtract_YORK x y = intersection x (complement y)


	merge - Merge is a higher order function that produces either
		the intersection or the union of the objects according
		to the function passed in.
		Merger effectively merges two BSPT trees into one. This
		Note the basic algorithm is to split one BSP into two by
		partitioning by the root partition of the other 
		(function partTree) and then to merge the 
		respective halves of each. When one tree is found to be a cell
		the merge is dependent on the semantics of op.
		Note bsp'' is used to used to reconstruct the tree 
		- this maintains credibility of sub-hyperplane data, it also
		removes redundant partitions.

> merge :: (BSPT -> BSPT -> BSPT) -> BSPT -> BSPT -> BSPT
> merge op (Cell x r a) tree = op (Cell x r a) tree
> merge op tree (Cell x r a) = op tree (Cell x r a)
> merge op (BSP p nodeinfo left right) tree
> 				= bsp'' p nodeinfo left' right'
> 					where	
> 					left'= merge op left rear
> 					right'= merge op right fore
> 					(rear,fore) = partTree p tree 

	partTree - partitions a single BSPT into two BSP trees. The
		half in positive halfspace of p and the half in the negative
		halfspace of p where p is the partition. 
		The partitioning is dependent on the relationship of the
		root partitions of the the trees involved on the region
		in question. The function classify returns the appropriate 
		partitioning function - this is then applied to the tree. 
	
> partTree :: Partition -> BSPT -> (BSPT,BSPT)
> partTree part (Cell x region a) = (mkCell x (newRegion region part), 
>				     mkCell x (newRegion region (flip_YORK part)))
> partTree part@(Fc sp p) tree@(BSP (Fc st t) (_,region) _ _) 
>	=  case (location p st, location t sp) of
>                         (Coincident,_)        -> if p==t
>                                                  then onParallel part tree
>                                                  else onAntiparallel part tree
>                         (ToTheFore,ToTheFore) -> pinPostinPos part tree
>                         (ToTheFore,ToTheRear) -> pinNegtinPos part tree
>                         (ToTheRear,ToTheFore) -> pinPostinNeg part tree
>                         (ToTheRear,ToTheRear) -> pinNegtinNeg part tree
>                         (_,_)                 -> inBoth part tree

	partitioning functions - depending on the classification, the
	appropriate partitioning function produces two tree from the argument
	tree. These are the rear and fore of the tree with respect to the 
	partition

> onParallel :: Partition -> BSPT -> (BSPT,BSPT)
> onParallel p (BSP t _ rear fore) = (rear,fore)

> onAntiparallel :: Partition -> BSPT -> (BSPT,BSPT)
> onAntiparallel p (BSP t _ rear fore) = (fore,rear)

> pinPostinNeg :: Partition -> BSPT -> (BSPT,BSPT)
> pinPostinNeg p (BSP t (faces,region) tRear tFore) 
> 			= (bsp' t (faces,newRegion region p) tRear tForepRear, 
>			   tForepFore)
> 		 	  where 
> 			  (tForepRear,tForepFore) = partTree p tFore 

> pinPostinPos :: Partition -> BSPT -> (BSPT,BSPT)
> pinPostinPos p (BSP t (faces,region) tRear tFore) 
> 			= (tForepRear, 
>			   bsp' t (faces,newRegion region (flip_YORK p)) tRear tForepFore)
> 			  where 
> 			  (tForepRear,tForepFore) = partTree p tFore 

> pinNegtinPos :: Partition -> BSPT -> (BSPT,BSPT)
> pinNegtinPos p (BSP t (faces,region) tRear tFore) 
> 			= (tRearpRear, 
>			   bsp' t (faces,newRegion region (flip_YORK p)) tRearpFore tFore)
> 			  where 
> 			  (tRearpRear,tRearpFore) = partTree p tRear 

> pinNegtinNeg :: Partition -> BSPT -> (BSPT,BSPT)
> pinNegtinNeg p (BSP t (faces,region) tRear tFore) 
> 			= (bsp' t (faces,newRegion region p) tRearpRear tFore, 
>			   tRearpFore)
> 				where 
> 				(tRearpRear,tRearpFore) = partTree p tRear

> inBoth :: Partition -> BSPT -> (BSPT,BSPT)
> inBoth p (BSP t (faces,region) tRear tFore) 
> 		= (bsp' tLeft (rearFaces,leftRegion) tRearpRear tForepRear, 
> 		   bsp' tRight (foreFaces,rightRegion) tRearpFore tForepFore) 
> 		where 
> 		(tRearpRear,tRearpFore) = partTree pLeft tRear
> 		(tForepRear,tForepFore) = partTree pRight tFore 
> 		(rearFaces,_,foreFaces) = partFaces p' faces 
>		(leftRegion,rightRegion) = mappair (newRegion region) (pLeft,pRight)
>		(tLeft,tRight) = bisect t p'
>		(pLeft,pRight) = bisect p (getPart t)
>		p' = getPart p



	complement - invert a BSPT - NB: faces not reversed 

> complement :: BSPT -> BSPT
> complement = foldBSPT compCell BSP
>		where 
>		compCell In = Cell Out 
>		compCell Out = Cell In 
