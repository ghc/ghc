-- MANDELBROT SET GENERATOR
-- DAVID HANLEY 11/03/1992
-- BUILDS A MAND-TREE, which contains, at its leaves, a colour.
-- Tree is then traversed and points along with their colour are output

module Main where

a `par` b = b
--1.3 a `seq` b = b

-- MandTree - either contains a NS node with two subtrees
--                   contains a EW node with two subtrees
--                   a leaf holding a colour
data MandTree = NS   MandTree MandTree
               |EW   MandTree MandTree
               |Leaf Colour

-- Colour - colour of a point or rectangle
type Colour = Int

-- Point - 2-Tuple of integers, (Int1,Int2),
--         where Int1 - x-coord of point,
--               Int2 - y-coord of point.
type Point = (Int,Int)

-- MandTuple
type MandTuple = (Int,Int,Int,Int,Colour)

-- size: Size of window - Square 400x400
size :: Int
size =  200

-- build_tree - Constructs mandtree
build_tree :: Point -> Point -> MandTree
build_tree p1@(x1,y1)
           p2@(x2,y2) 
           = 
            if rec_col /= -1 then   -- All points in currnet rectangle are same colour
              Leaf rec_col
            else
              if split == "NS" then
                 btns1 `par` (btns2 `seq` (NS btns1 btns2))
              else
                 btew1 `par` (btew2 `seq` (EW btew1 btew2))
            where
                 rec_col = check_perim p1 p2
                 split   = if (x2-x1) >= (y2-y1) then  -- x-axis longer, NS split
                              "NS" 
                           else                        -- y-axis longer, EW split
                              "EW"
                 nsp1    = p1
                 nsp2    = (split_x, y2)
                 nsp3    = (split_x+1, y1)
                 nsp4    = p2
                 ewp1    = p1
                 ewp2    = (x2, split_y)
                 ewp3    = (x1, split_y+1)
                 ewp4    = p2
                 split_x = (x2+x1) `div` 2
                 split_y = (y2+y1) `div` 2
                 btns1   = build_tree nsp1 nsp2
                 btns2   = build_tree nsp3 nsp4
                 btew1   = build_tree ewp1 ewp2
                 btew2   = build_tree ewp3 ewp4


check_perim :: Point -> Point -> Colour
check_perim p1@(x1,y1)
            p2@(x2,y2)
            = if (equalp p1 p2) then  -- single point, just return its colour
                 point_colour p1
              else if corners_diff then  -- check corners of current rectangle aren't same
                 -1   
              else
                check_sides
              where
              corners_diff = if col1 == col2 then
                               if col1 == col3 then
                                 if col1 == col4 then
                                   False
                                 else
                                   True
                               else
                                 True
                             else
                               True

              col1         = point_colour p1
              col2         = point_colour (x2,y1)
              col3         = point_colour (x2,y2)
              col4         = point_colour (x1,y2)

              check_sides  = if (check_line (x1+1,y1) right) then
                              if (check_line (x2,y1+1) down) then
                               if (check_line (x2-1,y2) left) then
                                if (check_line (x1,y2-1) up) then
                                  col1
                                else
                                  -1
                               else
                                 -1
                              else
                               -1
                             else
                              -1

                             where
                             check_line pc@(xc,yc) pd@(xd,yd)
                               =
                               if finished then
                                 True
                               else if (point_colour pc) /= col1 then
                                 False
                               else 
                                 check_line (xc+xd, yc+yd) pd
                               where
                                    finished  = if (equalp pd right) then
                                                  (xc >= x2)
                                                else if (equalp pd down) then
                                                  (yc <= y2)
                                                else if (equalp pd left) then
                                                  (xc <= x1)
                                                else
                                                  (yc >= y1)


-- Evaluate the color index of a point
-- This is the algoritm described on page 121 of "The Beauty of Fractals"
-- The code is commented with the step numbers from the algorithm.

-- point_colour - Calculates the dwell value of a point.
point_colour :: Point -> Colour
point_colour (x, y)
             = check_radius (np x) (nq y) 0 0.0 0.0 -- step1


-- check_radius - Calculates the escape radius of a point
check_radius :: Float -> Float -> Int -> Float -> Float -> Colour
check_radius p q k x y = if kp == num_cols then
                           0                             -- step 3.ii
                         else
                           if r > (fromIntegral m ) then
                            kp -- step 3.i
                           else
                             check_radius p q (kp) xn yn -- step 3.iii
                                  where xn = new_x x y p -- step 2
                                        yn = new_y x y q -- step 2
                                        r = radius xn yn -- step 3
                                        kp = k + 1       -- step 2


-- M Set Properties.
 
pmn :: Float -- Min p value.
pmn =  -2.25
 
pmx :: Float -- Max p value.
pmx =   0.75
 
qmn :: Float -- Min q value.
qmn = -1.5
 
qmx :: Float -- Max q value
qmx = 1.5

m :: Int      -- The escape radius, M.
m =  20
 
--- Misc functions.
 
equalp :: Point -> Point -> Bool
equalp (x1, y1) (x2, y2) = ((x1 == x2) && (y1 == y2))
 
 
-- Set calculation functions.
 
num_cols :: Int -- The number of colors; num_cols+1 = length (the_colors).
num_cols = 26
 
delta_p :: Float      -- The change in p per pixel.
delta_p =  (pmx - pmn) / fromIntegral (size - 1)

delta_q :: Float      -- The change in q per pixel.
delta_q =  (qmx - qmn) / fromIntegral (size - 1)
 
np :: Int -> Float     -- Calculate a new value of p.
np x = pmn + (fromIntegral x) * delta_p 
 
nq :: Int -> Float     -- Calculate a new value of q.
nq y = qmn + (fromIntegral y) * delta_q

radius :: Float -> Float -> Float       -- Current radius of apoint (x,y).
radius x y = x*x + y*y
 
new_x :: Float -> Float -> Float -> Float       -- iterate new x value.
new_x x y p = x*x - y*y + p
 
new_y :: Float -> Float -> Float -> Float       -- iterate new y value.
new_y x y q = 2.0 * x * y + q


-- Misc. functions for traversing perimeter of rectangle.

up,down,left,right :: Point
up    = ( 0,-1)  
down  = ( 0, 1)
left  = (-1, 0)
right = ( 1, 0)

-- finite: forces evaluation of a tree
finite            :: MandTree -> Bool
finite (Leaf c)   =  (c == c)
finite (NS t1 t2) =  (finite t1 && finite t2)
finite (EW t1 t2) =  (finite t1 && finite t2)

main =  if finite(build_tree (0,0) (size,size `div` 2)) then
            print "Success"
          else
            print "Fail"



