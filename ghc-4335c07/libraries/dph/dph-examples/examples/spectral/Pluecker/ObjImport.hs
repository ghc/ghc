module ObjImport
    ( parseObj
    , model)
where

import Common
import qualified Common as C
import Data.List
import qualified Data.Vector.Unboxed as VU

import Debug.Trace

parseObj :: String -> ([Vec3], [(Int,Int,Int,Colour)])
parseObj s = go (lines s) ([],[])
 where
  go []     (vs,ts) = (reverse vs,ts)
  go (c:cs) acc = go cs (read1 c acc)

  read1 ('#':_) acc = acc

  read1 ('v':'n':cs) acc
   = acc

  read1 ('v':cs) (vs,ts)
   | [x,y,z]      <- readNums [Nothing,Nothing,Nothing] cs
   = ((x,y,z):vs, ts)
   | otherwise
   = trace ('v':cs) (vs,ts)

  read1 ('f':cs) (vs,ts)
   | [a,_,b,_,c,_]      <- readNums [Nothing, Just "//", Nothing,
                                   Nothing, Just "//", Nothing,
                                   Nothing, Just "//", Nothing] cs
   = (vs, (a-1,b-1,c-1, (1,1,1)):ts)
   | otherwise
   = trace ('f':cs) (vs,ts)

  read1 ('g':_) (vs,ts)
   = (vs, ts)

  read1 _        acc = acc

readNums :: (Read a, Num a) => [Maybe String] -> String -> [a]
readNums (pattern : rest) str
 | Just prefix <- pattern
 , Just s'     <- stripPrefix prefix str
 = readNums rest s'

 | Nothing     <- pattern
 , [(i,s')]    <- reads str
 = i : readNums rest s'

 | otherwise
 = []

readNums [] _ = []


-- read the model, fix it up a bit, add a floor plane
model :: IO (VU.Vector Vec3, VU.Vector (Int,Int,Int,Colour))
model
 = do
    s <- readFile "model.obj"
    let (vs, ts)  = parseObj s
    let (vs',ts') = (VU.map (\v -> rotateY (-C.pi/1.2) `mvmul` (rotateX (-C.pi/2) `mvmul` v) `vsmul` 0.3 `vadd` (0,-23,2) ) $ VU.fromList vs, VU.fromList ts)

    -- 
    let lvs = VU.length vs'
    let extra_vs = VU.fromList [(-50,-25,-5 )
                               ,( 50,-25,-5 )
                               ,( 50,-25, 50)
                               ,(-50,-25, 50)]

    let extra_ts = VU.fromList [(lvs,lvs+1,lvs+2, (0,1,0))
                               ,(lvs,lvs+2,lvs+3, (0,0,1))]


    return (vs' VU.++ extra_vs, ts' VU.++ extra_ts)


