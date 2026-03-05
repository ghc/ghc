module T9445 (Transform) where

data Transform = Transform
    { m00 :: !Float, m10 :: !Float, m20 :: !Float
    , m01 :: !Float, m11 :: !Float, m21 :: !Float
    , m02 :: !Float, m12 :: !Float, m22 :: !Float
    }

instance Num Transform where
  abs (Transform a00 a01 a02 a03 a04 a05 a06 a07 a08) =
        (Transform (abs a00) (abs a01) (abs a02) (abs a03)
            (abs a04) (abs a05) (abs a06) (abs a07) (abs a08))

