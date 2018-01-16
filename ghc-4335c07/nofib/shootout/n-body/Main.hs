--
-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org/
--
-- Contributed by Olof Kraigher and Don Stewart.
--
-- To be compiled with:
--
--  -O2 -fglasgow-exts -funbox-strict-fields -fbang-patterns -optc-O 
--
-- Don't enable -optc-mfpmath=sse -optc-msse2, this triggers a gcc bug on x86
--

import Foreign (Ptr, Storable(..), plusPtr, mallocBytes)
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.IORef
import Control.Monad
import System.Environment
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf

main = do
    n <- getArgs >>= readIO.head
    initialize
    offset_momentum
    energy 0 planets >>= printf "%.9f\n"
    replicateM_ n (advance planets)
    energy 0 planets >>= printf "%.9f\n"

offset_momentum = do
    m <- foldr (.+.) (Vec 0 0 0)
             `fmap` (mapM momentum
                   . take (nbodies - 1)
                   . iterate next $ next planets)

    setVec (vel planets) $ (-1/solar_mass) *. m
  where
    momentum !p = liftM2 (*.) (mass p) (getVec (vel p))

energy :: Double -> Ptr Double -> IO Double
energy !e !p
    | p == end = return e
    | otherwise      = do
        p1 <- getVec (pos p)
        v1 <- getVec (vel p)
        m1 <- mass p
        e  <- energy2 p1 m1 e p2
        energy (e + 0.5 * m1 * magnitude2 v1) p2
    where p2 = next p

energy2 !p1 !m1 !e !p
    | p  == end = return e
    | otherwise = do
        p2 <- getVec (pos p)
        v2 <- getVec (vel p)
        m2 <- mass p
        let distance = sqrt . magnitude2 $ p1 .-. p2
        energy2 p1 m1 (e - m1 * m2 / distance) (next p)

advance :: Ptr Double -> IO ()
advance !p1 = when (p1 /= end) $ do
    pos1 <- getVec $ pos p1
    m1   <- mass p1
    let go !p2
            | p2 /= end = do
                pos2 <- getVec (pos p2)
                m2   <- mass p2
                let vel2       = vel p2
                    difference = pos1 .-. pos2
                    distance2  = magnitude2 difference
                    distance   = sqrt distance2
                    magnitude  = delta_t / (distance2 * distance)
                    mass_magn  = magnitude *. difference
                vel1 -= m2 *. mass_magn
                vel2 += m1 *. mass_magn
                go (next p2)

            | otherwise = do
                v1 <- getVec vel1
                p1 += delta_t *. v1
    go p2
    advance  p2
  where
    vel1 = vel p1
    p2   = next p1

------------------------------------------------------------------------

planets :: Ptr Double
planets = unsafePerformIO $ mallocBytes (7 * nbodies * 8) -- sizeOf double = 8

nbodies :: Int
nbodies = 5

solar_mass, delta_t, days_per_year :: Double
days_per_year = 365.24
solar_mass    = 4 * pi ** 2;
delta_t       = 0.01

initialize = mapM_ newPlanet planets
  where
   dp = days_per_year
   planets =
    [0, 0, 0,
     0, 0, 0,
     1 * solar_mass,
     4.84143144246472090e+00,        (-1.16032004402742839e+00), (-1.03622044471123109e-01),
     1.66007664274403694e-03*dp,     7.69901118419740425e-03*dp, (-6.90460016972063023e-05)*dp,
     9.54791938424326609e-04 * solar_mass,

     8.34336671824457987e+00,        4.12479856412430479e+00,    (-4.03523417114321381e-01),
     (-2.76742510726862411e-03)*dp,  4.99852801234917238e-03*dp, 2.30417297573763929e-05*dp,
     2.85885980666130812e-04 * solar_mass,

     1.28943695621391310e+01,        (-1.51111514016986312e+01), (-2.23307578892655734e-01),
     2.96460137564761618e-03*dp,     2.37847173959480950e-03*dp, (-2.96589568540237556e-05)*dp,
     4.36624404335156298e-05 * solar_mass,

     1.53796971148509165e+01,        (-2.59193146099879641e+01), 1.79258772950371181e-01,
     2.68067772490389322e-03*dp,     1.62824170038242295e-03*dp, (-9.51592254519715870e-05)*dp,
     5.15138902046611451e-05 * solar_mass
    ]

------------------------------------------------------------------------
-- Support for 3 dimensional mutable vectors

data Vector3 = Vec !Double !Double !Double

end :: Ptr Double
end = inc planets $ nbodies * 7

next  :: Ptr Double -> Ptr Double
next p = inc p 7

cursor :: IORef (Ptr Double)
cursor = unsafePerformIO $ newIORef planets

inc :: Ptr Double -> Int -> Ptr Double
inc ptr n = plusPtr ptr (n * 8)

newPlanet :: Double -> IO ()
newPlanet !d = do
    ptr <- readIORef cursor
    pokeElemOff ptr 0 d
    writeIORef cursor (inc ptr 1)

pos :: Ptr Double -> Ptr Double
pos ptr = ptr

vel :: Ptr Double -> Ptr Double
vel ptr = inc ptr 3

mass :: Ptr Double -> IO Double
mass ptr = peekElemOff ptr 6

------------------------------------------------------------------------

(Vec x y z) .+. (Vec u v w) = Vec (x+u) (y+v) (z+w)

(Vec x y z) .-. (Vec u v w) = Vec (x-u) (y-v) (z-w)

k *. (Vec x y z) = Vec (k*x) (k*y) (k*z) -- allocates

magnitude2 (Vec x y z) = x*x + y*y + z*z

------------------------------------------------------------------------

getVec !p = liftM3 Vec (peek p) (f 1) (f 2)
    where f = peekElemOff p

setVec p (Vec x y z)= do
    poke        p   x
    pokeElemOff p 1 y
    pokeElemOff p 2 z

infix 4 +=
infix 4 -=

v1 += (Vec u v w) = do
    x <- peek v1;          poke        v1   (x+u)
    y <- peekElemOff v1 1; pokeElemOff v1 1 (y+v)
    z <- peekElemOff v1 2; pokeElemOff v1 2 (z+w)

v1 -= (Vec u v w)  = do
    x <- peek v1;          poke        v1   (x-u)
    y <- peekElemOff v1 1; pokeElemOff v1 1 (y-v)
    z <- peekElemOff v1 2; pokeElemOff v1 2 (z-w)
