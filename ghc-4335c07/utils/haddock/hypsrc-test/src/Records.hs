{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}


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

translate :: Int -> Int -> Point -> Point
translate x y p =
    aux p
  where
    (dx, dy) = (x, y)
    aux Point{..} = p { x = x + dx, y = y + dy }
