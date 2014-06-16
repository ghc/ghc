module Physical where
import TypesettingTricks
class (Eq a, Show a) => Physical a where
  fromPhysical:: a -> Float
  toPhysical:: Float -> a
instance Physical Float where
  fromPhysical x = x
  toPhysical x = x
data PlaneAngle =
     Rad Float                 |
     Mrad Float                |
     Urad Float                |
     Deg Float                 |
     Rev Float
     deriving (Eq, Show)
instance Physical PlaneAngle where
    fromPhysical (Rad x) = x
    fromPhysical (Mrad x) = realdiv x 1000
    fromPhysical (Urad x) = realdiv x 1000000
    fromPhysical (Deg x) = realdiv (realmul x pi) 180
    fromPhysical (Rev x) = realdiv x (realmul 2.0 pi)
    toPhysical x = Rad x
data SolidAngle =
     Sr Float            |
     Msr Float
     deriving (Eq, Show)
instance Physical SolidAngle where
    fromPhysical (Sr x) = x
    fromPhysical (Msr x) = realdiv x 1000
    toPhysical x = Sr x
data BurstLength =
     Cycle Float          |
     Pulse Float
     deriving (Eq, Show)
instance Physical BurstLength where
    fromPhysical (Cycle x) = x
    fromPhysical (Pulse x) = x
    toPhysical x = Cycle x
data Capacitance =
     Fd Float            |
     Ufd Float           |
     Nfd Float           |
     Pfd Float
     deriving (Eq, Show)
instance Physical Capacitance where
     fromPhysical (Fd x) = x
     fromPhysical (Ufd x) = realdiv x 1000000
     fromPhysical (Nfd x) = realdiv x 1000000000
     fromPhysical (Pfd x) = realdiv x 1000000000000
     toPhysical x = Fd x
data Charge =
     C Float              |
     Kc Float             |
     Uc Float             |
     Nc Float
     deriving (Eq, Show)
instance Physical Charge where
    fromPhysical (C x) = x
    fromPhysical (Kc x) = realmul 1000 x
    fromPhysical (Uc x) = realdiv x 1000000
    fromPhysical (Nc x) = realdiv x 1000000000
    toPhysical x = C x
data Current =
     A Float             |
     Ka Float            |
     Ma Float            |
     Ua Float            |
     Na Float
     deriving (Eq, Show)
instance Physical Current where
    fromPhysical (A x) = x
    fromPhysical (Ka x) = realmul 1000 x
    fromPhysical (Ma x) = realdiv x 1000
    fromPhysical (Ua x) = realdiv x 1000000
    fromPhysical (Na x) = realdiv x 1000000000
    toPhysical x = A x
data Distance =
     M Float              |
     Km Float             |
     Mm Float             |
     Um Float             |
     Nm Float             |
     In Float             |
     Ft Float             |
     SMi Float            |
     NMi Float
     deriving (Eq, Show)
instance Physical Distance where
    fromPhysical (M x) = x
    fromPhysical (Km x) = realmul 1000 x
    fromPhysical (Mm x) = realdiv x 1000
    fromPhysical (Um x) = realdiv x 1000000
    fromPhysical (Nm x) = realdiv x 1000000000
    fromPhysical (In x) = realmul 25.4 x
    fromPhysical (Ft x) = realmul 2.12 x
    fromPhysical (SMi x) = realdiv x 2490.57
    fromPhysical (NMi x) = realdiv x 1825
    toPhysical x = M x
data Energy =
     J Float              |
     Kj Float             |
     Mj Float             |
     Ev Float             |
     Kev Float            |
     Mev Float
     deriving (Eq, Show)
instance Physical Energy where
    fromPhysical (J x) = x
    fromPhysical (Kj x) = realmul 1000 x
    fromPhysical (Mj x) = realdiv x 1000
    fromPhysical (Ev x) = realmul 1.6E-19 x
    fromPhysical (Kev x) = realmul 1.6E-16 x
    fromPhysical (Mev x) = realmul 1.6E-13 x
    toPhysical x = J x
data MagFlux =
     Wb Float             |
     Mwb Float
     deriving (Eq, Show)
instance Physical MagFlux where
    fromPhysical (Wb x) = x
    fromPhysical (Mwb x) = realdiv x 1000
    toPhysical x = Wb x
data FluxDensity =
     T Float              |
     Mt Float             |
     Ut Float             |
     Gam Float
     deriving (Eq, Show)
instance Physical FluxDensity where
    fromPhysical (T x) = x
    fromPhysical (Mt x) = realdiv x 1000
    fromPhysical (Ut x) = realdiv x 1000000
    fromPhysical (Gam x) = realdiv x 1000000000
    toPhysical x = T x
data Force =
     N Float              |
     Kn Float             |
     Mn Float             |
     Un Float
     deriving (Eq, Show)
instance Physical Force where
    fromPhysical (N x) = x
    fromPhysical (Kn x) = realmul 1000 x
    fromPhysical (Mn x) = realdiv x 1000
    fromPhysical (Un x) = realdiv x 1000000
    toPhysical x = N x
data Frequency =
     Hz Float  |
     Khz Float |
     Mhz Float |
     Ghz Float
     deriving (Eq, Show)
instance Physical Frequency where
    fromPhysical (Hz x) = x
    fromPhysical (Khz x) = realmul 1000 x
    fromPhysical (Mhz x) = realmul 1000000 x
    fromPhysical (Ghz x) = realmul 1000000000 x
    toPhysical x = Hz x
data Illuminance =
     Lx Float
     deriving (Eq, Show)
instance Physical Illuminance where
    fromPhysical (Lx x) = x
    toPhysical x = Lx x
data Inductance =
    H  Float              |
    Mh Float              |
    Uh Float              |
    Nh Float              |
    Ph Float
    deriving (Eq, Show)
instance Physical Inductance where
    fromPhysical (H x) = x
    fromPhysical (Mh x) = realdiv x 1000
    fromPhysical (Uh x) = realdiv x 1000000
    fromPhysical (Nh x) = realdiv x 1000000000
    fromPhysical (Ph x) = realdiv x 1000000000000
    toPhysical x = H x
data Luminance =
     Nt Float
     deriving (Eq, Show)
instance Physical Luminance where
    fromPhysical (Nt x) = x
    toPhysical x = Nt x
data LuminFlux =
     Lm Float
     deriving (Eq, Read, Show)
instance Physical LuminFlux where
    fromPhysical (Lm x) = x
    toPhysical x = Lm x
data LuminInten =
     Cd Float
     deriving (Eq, Read, Show)
instance Physical LuminInten where
    fromPhysical (Cd x) = x
    toPhysical x = Cd x
data Mass =
     Kg Float             |
     G Float              |
     Mg Float             |
     Ug Float
     deriving (Eq, Show)
instance Physical Mass where
    fromPhysical (Kg x) = x
    fromPhysical (G x) = realdiv x 1000
    fromPhysical (Mg x) = realdiv x 1000000
    fromPhysical (Ug x) = realdiv x 1000000000
    toPhysical x = Kg x
data Power =
     W Float              |
     Kw Float             |
     Mw Float             |
     Uw Float
     deriving (Eq, Show)
instance Physical Power where
    fromPhysical (W x) = x
    fromPhysical (Kw x) = realmul 1000 x
    fromPhysical (Mw x) = realdiv x 1000
    fromPhysical (Uw x) = realdiv x 1000000
    toPhysical x = W x
data Pressure =
     Pa Float             |
     Kpa Float            |
     Mpa Float            |
     Upa Float            |
     Mb Float
     deriving (Eq, Show)
instance Physical Pressure where
    fromPhysical (Pa x) = x
    fromPhysical (Kpa x) = realmul 1000 x
    fromPhysical (Mpa x) = realdiv x 1000
    fromPhysical (Upa x) = realdiv x 1000000
    fromPhysical (Mb x) = realmul 100 x
    toPhysical x = Pa x
data Pulse =
     Pulses Float
     deriving (Eq, Show)
instance Physical Pulse where
    fromPhysical (Pulses x) = x
    toPhysical x = Pulses x
data RatioInOut =
     Db Float
     deriving (Eq, Show)
instance Physical RatioInOut where
    fromPhysical (Db x) = x
    toPhysical x = Db x
data Resistance =
     Ohm Float            |
     Kohm Float           |
     Mohm Float
     deriving (Eq, Show)
instance Physical Resistance where
    fromPhysical (Ohm x) = x
    fromPhysical (Kohm x) = realmul 1000 x
    fromPhysical (Mohm x) = realmul 1000000 x
    toPhysical x = Ohm x
data Temperature =
     Degk Float           |
     Degc Float           |
     Degf Float
     deriving (Eq, Show)
instance Physical Temperature where
    fromPhysical (Degk x) = x
    fromPhysical (Degc x) = x + 273
    fromPhysical (Degf x) = (realdiv (realmul 5 (x-32)) 9) + 273
    toPhysical x = Degk x
data Time =
     Sec Float            |
     Msec Float           |
     Usec Float           |
     Nsec Float           |
     Min Float            |
     Hr Float
     deriving (Eq, Show)
instance Physical Time where
    fromPhysical (Sec x) = x
    fromPhysical (Msec x) = realdiv x 1000
    fromPhysical (Usec x) = realdiv x 1000000
    fromPhysical (Nsec x) = realdiv x 1000000000
    fromPhysical (Min x) = realmul 60 x
    fromPhysical (Hr x) = realmul 3600 x
    toPhysical x = Sec x
data Voltage =
     V Float |
     Kv Float |
     Mv Float |
     Uv Float
     deriving (Eq, Show)
instance Physical Voltage where
    fromPhysical (V x) = x
    fromPhysical (Kv x) = realmul 1000 x
    fromPhysical (Mv x) = realdiv x 1000
    fromPhysical (Uv x) = realdiv x 1000000
    toPhysical x = V x
data Volume =
     L Float            |
     Ml Float
     deriving (Eq, Show)
instance Physical Volume where
    fromPhysical (L x) = x
    fromPhysical (Ml x) = realdiv x 1000
    toPhysical x = L x
