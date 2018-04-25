module Solver
    (Solver,solvers)
where

import Common
import qualified Vector                         as SV
import qualified Vectorised                     as SPA
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as VU
import qualified Data.Array.Parallel	as P
import qualified Data.Array.Parallel.PArray	as P


type Solver = VU.Vector Vec3 -> VU.Vector (Int,Int,Int,Colour) -> VU.Vector Vec3 -> Double -> VU.Vector Colour

solvers :: [(String,Solver)]
solvers =
 [("vectorised", solverPA)
 ,("vector",     SV.solveV)]

solverPA verts tris rays time
 = tu3 (SPA.solvePA (fu3 verts) (fu4_3 tris) (fu3 rays) time)

 
fu as = P.fromUArray as

fu3 as
 = let (xs,ys,zs) = VU.unzip3 as
   in  P.zip3 (fu xs) (fu ys) (fu zs)

fu4_3 as
 = let (xs,ys,zs,bs) = VU.unzip4 as
   in  P.zip4 (fu xs) (fu ys) (fu zs) (fu3 bs)


tu as = P.toUArray as

tu3 as
 = let (xs,ys,zs) = P.unzip3 as
   in  VU.zip3 (tu xs) (tu ys) (tu zs)
