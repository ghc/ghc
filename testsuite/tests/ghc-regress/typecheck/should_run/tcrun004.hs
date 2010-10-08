{-# LANGUAGE ExistentialQuantification #-}
-- !!! Tests existential data types
--     Originally from Kevin Glynn
module Main(main) where

data Coordinate3D = Coord3D {cx, cy, cz::Double} 
                    deriving (Eq, Show)

-- We Represent a line by two coordinates which it passes through.
data Line = MkLine Coordinate3D Coordinate3D 


class PictureObject pot where

      -- Returns ordered (rel to 0 0 0) of points where the object
      -- intersects the given line. 
      intersectLineObject :: pot -> Line -> [Coordinate3D]

      getPictureName :: pot -> String

data Sphere = 
   Sphere Coordinate3D			-- Centre
          Double			-- Radius
	  Double			-- ambient coeff
	  Double			-- diffuse coeff
	  Double			-- specular coeff
	  Double			-- phong specular exponent

intersectLineSphere :: Sphere -> Line -> [Coordinate3D]
intersectLineSphere sp line = []

instance PictureObject Sphere where
 	 intersectLineObject = intersectLineSphere
	 getPictureName _ = "Sphere"

data Cube = 
   Cube Coordinate3D		-- Origin corner 
        Coordinate3D		-- Opposite corner
	Double			-- ambient coeff
	Double			-- diffuse coeff
	Double			-- specular coeff
	Double			-- phong specular exponent
   deriving (Eq, Show)

intersectLineCube :: Cube -> Line -> [Coordinate3D]
intersectLineCube cube line = []

instance PictureObject Cube where
	 intersectLineObject = intersectLineCube
	 getPictureName _ = "Cube"


data GenPic = forall pot. (PictureObject pot) => MkGenPic pot

sphere :: Sphere
sphere = Sphere (Coord3D 1 1 1) 1 1 1 1 1

cube :: Cube
cube = Cube (Coord3D 1 1 1) (Coord3D 2 2 2) 1 1 1 1

obj_list:: [GenPic] 
obj_list = [MkGenPic sphere, MkGenPic cube]

putName :: PictureObject pot => pot -> IO ()
putName x = putStr $ getPictureName x


main :: IO ()
main = do { sequence_ $ map put_it obj_list }
     where
       put_it (MkGenPic s) = putStrLn (getPictureName s)

