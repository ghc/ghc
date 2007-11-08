{-# OPTIONS_GHC -funbox-strict-fields #-}
import Data.List

data Vec4 = Vec4 !Float !Float !Float !Float


main :: IO ()
main = print traceList

traceList = concatMap (\(x,y) -> let (r,g,b,a) = getPixel (x,y) in [r,g,b,a])
    [(0,0)]
    where
    getPixel (x,y) = (red,green,blue,alpha)
        where
        Vec4 fr fg fb fa = seq x (Vec4 1 2 3 4)
        red = round fr
        green = round fg
        blue = round fb
        alpha = round fa
