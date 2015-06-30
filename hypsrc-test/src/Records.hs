{-# LANGUAGE NamedFieldPuns #-}

module Records where


data Point = Point
    { x :: !Int
    , y :: !Int
    }


point :: Int -> Int -> Point
point x y = Point { x = x, y = y }


lengthSqr :: Point -> Int
lengthSqr (Point { x = x, y = y }) = x * x + y * y

lengthSqr' :: Point -> Int
lengthSqr' (Point { x, y }) = y * y + x * x


translateX, translateY :: Point -> Int -> Point
translateX p d = p { x = x p + d }
translateY p d = p { y = y p + d }
