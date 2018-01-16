module Hide(hiddenline) where
import Numbers
import Vectors
import EdgePlate
import Geometric
import Comparing
import Memo
import Postscript
import Data.Array

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- section 5: Hiding

-- The following function does the entire job...
hiddenline :: Vector -> Input -> Output
hiddenline v corners
	= (draw . scale factor base . hideObj . makeObject) rotated
	  where rotated = v `rot` corners
		(base,factor) = getExtremes (concat rotated)

-- `hide' calculates the leftovers of plate bottom as compared to plates tops
hide :: Array (Triangle Int) Bool -> Object -> Plate -> [Edge]
hide memory obj bottom@(Plt _ ls) =
	foldr cmp ls tops
	where tops = [top| top <- obj, lazyAbove memory top bottom]

-- `hideObj' hides edgeparts in an object, with view direction vec[0,0,1]
hideObj :: Object -> [Edge]
hideObj obj = concat [hide (mkmemo above obj) obj plt| plt <- obj]


getExtremes :: [Vector] -> (Vector,Number)
getExtremes vs =
        (vec [minimum xs, minimum ys, 0], factor)
        where
        factor = ((400 / ((maximum xs-minimum xs) `max` 1)) `min`
                  (640 / ((maximum ys-minimum ys) `max` 1)))
        xs = map x vs
        ys = map y vs
