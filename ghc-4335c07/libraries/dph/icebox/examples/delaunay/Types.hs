module Types ( Point, Line, IPoint, ILine ) where

type Point = (Double,Double)
type Line  = (Point,Point)

type IPoint = (Int,Point)
type ILine  = (IPoint,IPoint)

