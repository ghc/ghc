
module Basic where
import TypesettingTricks
--import Int( Num(fromInt) )
import Physical
--import GHC( (->) )
infixr 7 |>
class Signal s where
  mapSignal:: (Physical a, Physical b) => (s a b) -> a -> b
  mapSigList:: (Physical a, Physical b) => (s a b) -> [a] -> [b]
  toSig:: (Physical a, Physical b) => (s a b) -> SignalRep a b
  mapSignal = mapSignal . toSig
  mapSigList = map . mapSignal
  toSig = FunctionRep . mapSignal
instance Signal (->) where
  mapSignal f = f
  toSig = FunctionRep
data {- (Physical a, Physical b) => -} SignalRep a b =
   FunctionRep (a -> b) |
   PieceContRep (PieceCont a b)

instance Eq (SignalRep a b) where
  (==) a b = error "No equality for SignalRep"

instance Show (SignalRep a b) where
  show sr = error "No show for SignalRep"

instance Signal SignalRep where
  mapSignal (FunctionRep f) = mapSignal f
  mapSignal (PieceContRep f) = mapSignal f
  mapSigList (FunctionRep f) = mapSigList f
  mapSigList (PieceContRep f) = mapSigList f
  toSig = id
instance (Physical a, Physical b) => Eq (a -> b) where
  a == b = error "Attempt to apply equality to functions"
binop:: (Physical a, Physical b) => (Float -> Float -> Float) -> 
                                    (a -> b) -> (a -> b) -> a -> b
binop op f g t = toPhysical ((fromPhysical (f t)) `op` (fromPhysical (g t)))
unop:: (Physical a, Physical b ) => (Float -> Float) -> 
                                    (a -> b) -> a -> b
unop op f t = toPhysical (op (fromPhysical (f t)))
instance (Physical a, Physical b) => Num (SignalRep a b) where
  f + g = FunctionRep (binop (+) (mapSignal f) (mapSignal g))
  f * g = FunctionRep (binop (*) (mapSignal f) (mapSignal g))
  negate f = FunctionRep (unop negate (mapSignal f))
  abs f = FunctionRep (unop abs (mapSignal f))
  signum f = FunctionRep (unop abs (mapSignal f))
  fromInteger i = FunctionRep (\t -> toPhysical (fromInteger i))
  --fromInt i = FunctionRep (\t -> toPhysical (fromInt i))
instance (Physical a, Physical b) => 
         Fractional (SignalRep a b) where
  f / g = FunctionRep (binop (/) (mapSignal f) (mapSignal g))
  fromRational r = FunctionRep (\t -> (toPhysical (fromRational r)))
instance (Physical a, Physical b) => 
          Floating (SignalRep a b) where
  pi = FunctionRep (\t -> (toPhysical pi))
  exp   f = FunctionRep (unop exp (mapSignal f))
  log   f = FunctionRep (unop log (mapSignal f))
  sin   f = FunctionRep (unop sin (mapSignal f))
  cos   f = FunctionRep (unop cos (mapSignal f))
  asin  f = FunctionRep (unop asin (mapSignal f))
  acos  f = FunctionRep (unop acos (mapSignal f))
  atan  f = FunctionRep (unop atan (mapSignal f))
  sinh  f = FunctionRep (unop sinh (mapSignal f))
  cosh  f = FunctionRep (unop cosh (mapSignal f))
  asinh f = FunctionRep (unop asinh (mapSignal f))
  acosh f = FunctionRep (unop acosh (mapSignal f))
  atanh f = FunctionRep (unop atanh (mapSignal f))
data Event =
  TimeEvent Float | 
  FunctionEvent (Float -> Bool) |
  BurstEvent Int Event

instance Show Event where
  show (TimeEvent f) = "TimeEvent " ++ show f
  show (FunctionEvent _) = "FunctionEvent"
  show (BurstEvent i e)  = "BurstEvent " ++ show i ++ " " ++ show e

instance Eq Event where
  (TimeEvent x) == (TimeEvent y) = x == y
  (BurstEvent i e) == (BurstEvent i' e') = (i' == i) && (e' == e)
eventOccurs:: Event -> Float -> Float
eventOccurs (TimeEvent t) x = if x < t then x else t
eventOccurs (FunctionEvent f) x = stepEval f x
eventOccurs (BurstEvent i e) x = 
          if i == 1 then
            eventOccurs e x
          else
            eventOccurs (BurstEvent (i-1) e) ((eventOccurs e x) + eventEps x)
stepEval:: (Float -> Bool) -> Float -> Float
stepEval f x = if f x then x else stepEval f (x + eventEps x)
data ZeroIndicator = LocalZero | GlobalZero deriving (Eq, Show)
data {- (Physical a, Physical b) => -} FunctionWindow a b = 
     Window ZeroIndicator Event (SignalRep a b)
     deriving (Eq, Show)
data PieceCont a b = Windows [FunctionWindow a b]
     deriving (Eq, Show)
instance Signal PieceCont where
  mapSignal (Windows []) t = toPhysical 0.0
  mapSignal (Windows wl) t = (mapSignal s) (toPhysical t')
      where (t', (Window z e s), wl') = getWindow 0.0 (fromPhysical t) wl
  toSig = PieceContRep
getWindow:: (Physical a, Physical b) => 
            Float -> Float -> [ FunctionWindow a b ] -> 
            (Float, FunctionWindow a b, [ FunctionWindow a b ])
getWindow st t [] = (t, Window LocalZero e f, [])
                    where e = TimeEvent (realmul 2 t)
                          f = FunctionRep (\t -> toPhysical 0.0)
getWindow st t (w:wl) = if t' <= wt then (t',w,w:wl) 
                        else getWindow (st+wt) t wl
    where wt = eventOccurs e t'
          (Window z e s) = w
          t' = if z == LocalZero then t-st else t
(|>) :: (Physical a, Physical b) => FunctionWindow a b -> 
        PieceCont a b -> PieceCont a b
w |> (Windows wl) = Windows (w:wl)
nullWindow = Windows []
cycleWindows:: (Physical a, Physical b) => 
                PieceCont a b -> PieceCont a b
cycleWindows (Windows wl) = Windows (cycle wl)
constant:: (Physical a, Physical b) => b -> SignalRep a b
constant x = FunctionRep (\t -> x)
linear:: (Physical a, Physical b) => Float -> b -> SignalRep a b
linear m b  = FunctionRep (\x -> toPhysical (realmul m (fromPhysical x) + (fromPhysical b)))
sine:: (Physical a, Physical b) => 
       b -> Frequency -> Float -> SignalRep a b
sine mag omeg phase = FunctionRep (\x -> toPhysical (realmul (fromPhysical mag) (sin (realmul (realmul (realmul 2 pi) (fromPhysical omeg)) (fromPhysical x) + phase))))
waveform:: (Physical a, Physical b) => a -> [b] -> SignalRep a b
waveform samp ampls =
  let stepSlope y y' = realdiv ((fromPhysical y') - (fromPhysical y)) (fromPhysical samp)
      makeWin (v,v') = Window LocalZero (TimeEvent (fromPhysical samp)) 
                       (linear (stepSlope v v') v)
      points = cycle ampls
  in PieceContRep (Windows (map makeWin (zip points (tail points))))
random:: (Physical a, Physical b) => 
         Integer -> a -> SignalRep a b
random i s = waveform s (map toPhysical (rand i))
ramp:: (Physical a, Physical b) => a -> b -> SignalRep a b
ramp per v = 
  let sig = linear (realdiv (fromPhysical v) (fromPhysical per)) (toPhysical 0.0)
  in PieceContRep (Windows (cycle ([Window LocalZero (TimeEvent (fromPhysical per)) sig ])))
triangle:: (Physical a, Physical b) => a -> b -> SignalRep a b
triangle per v =
  let sl = realmul 2.0 (realdiv (fromPhysical v) (fromPhysical per))
      qper = realdiv (fromPhysical v) 4.0
      wins =  (Window LocalZero (TimeEvent qper) (linear sl (toPhysical 0.0))) |>
              (Window LocalZero (TimeEvent (realmul 2.0 qper)) (linear (- sl) v)) |>
              (Window LocalZero (TimeEvent qper) (linear sl (toPhysical (- (fromPhysical v))))) |>
               nullWindow
  in PieceContRep (cycleWindows wins)
step:: (Physical a, Physical b) => a -> b -> SignalRep a b
step tr lvl = FunctionRep (\t -> if (fromPhysical t) < (fromPhysical tr) then (toPhysical 0.0) else lvl)
square:: (Physical a, Physical b) => a -> b -> SignalRep a b
square per lvl =
  let trans = realdiv (fromPhysical per) 2.0
      nlvl = asTypeOf (toPhysical (- (fromPhysical lvl))) lvl
      f t = if (fromPhysical t) < trans then lvl else nlvl
      wins = Windows [Window LocalZero (TimeEvent (fromPhysical per)) (FunctionRep f)]
  in PieceContRep (cycleWindows wins)
pulse:: (Physical a, Physical b) => a -> a -> b -> SignalRep a b
pulse st wid lvl =
  let tr = (fromPhysical st) + (fromPhysical wid)
      f t = if (fromPhysical t) < (fromPhysical st) then (toPhysical 0.0)
            else if (fromPhysical t) < tr then lvl else (toPhysical 0.0)
  in FunctionRep f
trap:: (Physical a, Physical b) => a -> a -> a -> a -> b -> 
                                   SignalRep a b
trap st r wid f lvl =
  let stepSlope y y' t = realdiv (y' -  y) (fromPhysical t)
      bigwin = realmul 10000000 ((fromPhysical st) + (fromPhysical wid))
      wins = Window LocalZero (TimeEvent (fromPhysical st)) (constant (toPhysical 0.0)) |>
             Window LocalZero (TimeEvent (fromPhysical r)) (linear (stepSlope 0.0 (fromPhysical lvl) r) (toPhysical 0.0)) |>
             Window LocalZero (TimeEvent (fromPhysical wid)) (constant lvl) |>
             Window LocalZero (TimeEvent (fromPhysical f)) (linear (stepSlope (fromPhysical lvl) 0.0 f) lvl) |>
             Window LocalZero (TimeEvent bigwin) (constant (toPhysical 0.0)) |>
             nullWindow
  in PieceContRep wins
expc:: (Physical a, Physical b) => Float -> SignalRep a b
expc damp = FunctionRep (\t -> toPhysical (exp (- (realmul (fromPhysical t) damp))))
data {- (Physical indep, Physical dep) => -} BasicSignal indep dep =
    Overshoot {start_delay::indep,
               pulse_width::indep,
               ringing::dep,
               oscillation::Frequency,
               damp_fac::Float}
  | Pulse_dc {start_delay::indep,
              pulse_width::indep,
              rise_time::indep,
              fall_time::indep,
              period::indep,
              dc_offset::dep,
              amplitude::dep,
              over::BasicSignal indep dep,
              under::BasicSignal indep dep}
  | Pulse_ac {start_delay::indep,
              pulse_width::indep,
              period::indep,
              dc_offset::dep,
              amplitude::dep,
              frequency::Frequency,
              phase::Float}
  deriving (Eq, Show)

data {- (Eq a, Eq b) => -} Foo a b = Foo { x :: a, y :: b}

foo :: (Eq a, Eq b) => Foo a b
foo = Foo{}

{-
overshoot:: (Physical a, Physical b) => BasicSignal a b
overshoot = Overshoot{}
pulse_dc:: (Physical a, Physical b) => BasicSignal a b
pulse_dc = Pulse_dc {over = Overshoot{start_delay=toPhysical 0.0,
                                            ringing=(toPhysical 0.0),
                                            oscillation=toPhysical 1.0,
                                            damp_fac=1.0},
                     under = Overshoot{start_delay=toPhysical 0.0,
                                             ringing=(toPhysical 0.0),
                                             oscillation=toPhysical 1.0,
                                             damp_fac=1.0},
                     start_delay = toPhysical 0.0,
                     dc_offset = toPhysical 0.0}

pulse_ac:: (Physical a, Physical b) => BasicSignal a b
pulse_ac = Pulse_ac {dc_offset = toPhysical 0.0,
                     amplitude = toPhysical 0.0}
-}

makeWin:: (Physical a, Physical b) => a -> a -> 
           SignalRep a b -> SignalRep a b
makeWin st wid sig =
  let wins = Window LocalZero (TimeEvent (fromPhysical st)) (constant (toPhysical 0.0)) |>
             Window LocalZero (TimeEvent (fromPhysical wid)) sig |>
             nullWindow
  in PieceContRep wins
instance Signal BasicSignal where
  toSig (Overshoot start_delay pulse_width ringing oscillation damp_fac) =
    let ring = sine ringing oscillation 0.0
        cond = asTypeOf (expc damp_fac) ring
        sig = temp ring cond
        temp:: (Physical a, Physical b) => SignalRep a b -> 
                SignalRep a b -> SignalRep a b
        temp f g = FunctionRep (binop (*) (mapSignal f) (mapSignal g))
--        temp f g = f * g
--        temp f g = asTypeOf (f * g) ring
        wins = Window LocalZero (TimeEvent (fromPhysical start_delay)) (constant (toPhysical 0.0)) |>
               Window LocalZero (TimeEvent (fromPhysical pulse_width)) sig |>
               nullWindow
    in PieceContRep wins
  toSig Pulse_dc{ start_delay = start_delay
                , rise_time   = rise_time
		, pulse_width = pulse_width
		, fall_time   = fall_time
		, dc_offset   = dc_offset
		, period      = period
		, amplitude   = amplitude
		, over        = over
		, under       = under
		} =
    let pul = trap start_delay rise_time pulse_width fall_time amplitude
        so = toPhysical ((fromPhysical start_delay) + (fromPhysical rise_time))
        su = toPhysical ((fromPhysical so) + (fromPhysical pulse_width) + (fromPhysical fall_time))
        oversh = toSig over{start_delay=so}
        undersh = toSig under{start_delay=su}
        off = constant dc_offset
        temp:: (Physical a, Physical b) => SignalRep a b -> 
                SignalRep a b -> SignalRep a b
        temp f g = FunctionRep (binop (+) (mapSignal f) (mapSignal g))
        sig = temp (temp (temp pul oversh) undersh) off
        wins = (Window LocalZero (TimeEvent (fromPhysical period)) sig) |>
                nullWindow
    in PieceContRep (cycleWindows wins)
sumSig:: (Physical a, Physical b, Signal s, Signal s') =>
         (s a b) -> (s' a b) -> SignalRep a b
sumSig f f' = 
   let s1 t = fromPhysical (mapSignal f t)
       s2 t = fromPhysical (mapSignal f' t)
   in FunctionRep (\t -> toPhysical ((s1 t) + (s2 t)))
mulSig:: (Physical a, Physical b, Signal s, Signal s') =>
         (s a b) -> (s' a b) -> SignalRep a b
mulSig f f' = 
   let f1 t = fromPhysical (mapSignal f t)
       f2 t = fromPhysical (mapSignal f' t)
   in FunctionRep (\t -> toPhysical ((f1 t) * (f2 t)))

eventEps:: Float -> Float
eventEps x = let eps = realdiv x 1000 in if 0.01 < eps then 0.01 else eps
