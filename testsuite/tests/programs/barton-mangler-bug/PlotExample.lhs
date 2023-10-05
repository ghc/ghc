This file contains code that is explicitly designed to plot examples
from the signal modeling language.

> module PlotExample where

> import Plot
> import Physical
> import Basic

Our main task is to take a signal and a begin and start point (both
reals) and convert it into something that plotExam can take in the
Plot module.

> plotExample:: (Signal s, Physical a, Physical b) =>
>               String -> [(Float, Float)] -> s a b -> Float -> Float -> String
> plotExample fl expected sig s e = plotExam fl expected s e f
>                    where f  = toFloatFunc f'
>                          f' = mapSignal sig

> toFloatFunc:: (Physical a, Physical b) => (a -> b) -> Float -> Float
> toFloatFunc f x = fromPhysical (f (toPhysical x))
