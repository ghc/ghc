{-# LANGUAGE ParallelListComp #-}
module Test10357 where

legendres = one : x :
    [ multPoly
        (poly LE [recip (n' + 1)])
        (addPoly (poly LE [0, 2 * n' + 1] `multPoly` p_n)
                 (poly LE           [-n'] `multPoly` p_nm1)
        )
    | n     <- [1..], let n' = fromInteger n
    | p_n   <- tail legendres
    | p_nm1 <- legendres
    ]
