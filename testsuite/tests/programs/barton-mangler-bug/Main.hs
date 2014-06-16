module Main where
import Physical
import Basic
import TypesettingTricks
import PlotExample
import Expected
sinExample:: SignalRep Time Voltage
sinExample = sine (V 2.0) (Hz 10) 0.0
sinPlot = plotExample "sine" sinExpected sinExample 0.0 1.0
pieceExample = toSig Pulse_dc
  { start_delay=(Sec 1.0),
    rise_time=(Sec 0.2),
    pulse_width=(Sec 3.0),
    fall_time=(Sec 0.3),
    dc_offset=(V (- 1.0)),
    period=(Sec 10.0),
    amplitude=(V 5.0),
    over=Overshoot{ringing=(V 0.2),
                   pulse_width=(Sec 3.0),
                   oscillation=(Hz 2.0),
                   damp_fac=1.0},
    under=Overshoot{ringing=(V (- 0.25)),
                    pulse_width=(Sec 3.0),
                    oscillation=(Hz 2.10),
                    damp_fac=1.10} }
piecePlot = plotExample "piece" pieceExpected pieceExample 0.0 20.0
main = putStrLn (sinPlot ++ piecePlot)

