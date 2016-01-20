data Vec3 = Vec3 !Double !Double !Double
    deriving (Show)

infixl 6 ^+^, ^-^
infixr 7 *^, <.>

negateV :: Vec3 -> Vec3
negateV (Vec3 x y z) = Vec3 (-x) (-y) (-z)

(^+^), (^-^) :: Vec3 -> Vec3 -> Vec3
Vec3 x1 y1 z1 ^+^ Vec3 x2 y2 z2 = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
v ^-^ v' = v ^+^ negateV v'

(*^) :: Double -> Vec3 -> Vec3
s *^ Vec3 x y z = Vec3 (s * x) (s * y) (s * z)

(<.>) :: Vec3 -> Vec3 -> Double
Vec3 x1 y1 z1 <.> Vec3 x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

magnitudeSq :: Vec3 -> Double
magnitudeSq v = v <.> v

normalized :: Vec3 -> Vec3
normalized v = (1 / sqrt (magnitudeSq v)) *^ v

class Surface s where
    intersectSurfaceWithRay :: s -> Vec3 -> Vec3 -> Maybe Vec3

data Sphere = Sphere Vec3 Double

instance Surface Sphere where
    intersectSurfaceWithRay (Sphere c r) o d =
        let c' = c ^-^ o
            b = c' <.> d
            det = b^2 - magnitudeSq c' + r^2
            det' = sqrt det
            t1 = b - det'
            t2 = b + det'

            returnIntersection t =
                let x = o ^+^ t *^ d
                in Just (normalized (x ^-^ c))
        in if det < 0 then Nothing
           else if t1 > 1e-6 then returnIntersection t1
           else if t2 > 1e-6 then returnIntersection t2
           else Nothing

iappend :: Maybe Vec3 -> Maybe Vec3 -> Maybe Vec3
Nothing `iappend` i2 = i2
i1 `iappend` _ = i1

main :: IO ()
main = print $ foldl combine Nothing [Sphere (Vec3 0 0 0) 1]
  where combine accum surf = accum `iappend`
            intersectSurfaceWithRay surf (Vec3 0 0 5) (Vec3 0 0 (-1))
