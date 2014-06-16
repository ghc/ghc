The functions in this file (well, the single function) will allow the
user to plot different functions using the Gnuplot program.  In fact,
all it really does is output a number of points on the list and allow
the user to activate Gnuplot and use the plotting program to get the
appropriate output.

The first line just gives the module name.  For the moment, I don't
anticipate using any modules (although this may change).

> module Plot where

Now we give the type of the function.  This consists of a file name, a
list of values, and a function that goes from the appropriate types.

> plot2d:: (Fractional a, Fractional b, Ord a, Ord b, Show a, Show b)
>       => String -> [(a, b)] -> [a] -> (a -> b) -> String
> plot2d fl expected inp f = plot2d' expected inp f  

> plot2d':: (Fractional a, Fractional b, Ord a, Ord b, Show a, Show b)
>        => [(a, b)] -> [a] -> (a -> b) -> String
> plot2d' [] [] f     = []
> plot2d' ((e1, e2):es) (x:xs) f
>  | (e1 `isSame` x) && (e2 `isSame` x')
>                  = (show e1)       ++
>                    "  "            ++
>                    (show e2)       ++
>                    "\n"            ++
>                    plot2d' es xs f
>  | otherwise
>                  = "Mismatch: "    ++
>                    (show e1)       ++
>                    "  "            ++
>                    (show e2)       ++
>                    "  "            ++
>                    (show x)        ++
>                    "  "            ++
>                    (show x')       ++
>                    "\n"            ++
>                    plot2d' es xs f
>   where x' = f x
> plot2d' es [] _ = "Extra expected: " ++ show es
> plot2d' [] xs _ = "Extra results: " ++ show xs

> isSame :: (Fractional a, Ord a) => a -> a -> Bool
> x `isSame` y = abs (x - y) < 0.00001

And now, let's create a function to make a range out of a triple of a
start point, an end point, and an increment.

> createRange:: (Num a, Ord a) => a -> a -> a -> [a]
> createRange s e i = if s > e then []
>                     else s : createRange (s+i) e i

We now settle down to a couple of more specific functions that do
things that are more unique to gnuplot.  First, we have something that
creates the appropriate gnuplot command file.

> createGnuPlot:: Show a => String -> a -> a -> String
> createGnuPlot fl s e = 
>                         "set terminal latex\n"  ++
>                         "set output \""         ++
>                         (fl ++ ".tex\"\n")      ++
>                         "set nokey\n"           ++
>                         "plot ["                ++
>                         (show s)                ++
>                         ":"                     ++
>                         (show e)                ++
>                         "] \""                  ++
>                         (fl ++ ".plt\"")        ++
>                         " with lines\n"

And now we create a fairly specific plotExam function that takes a
string, a function, and two floats and produces the correct files

> plotExam:: String -> [(Float, Float)] -> Float -> Float -> (Float -> Float)
>         -> String
> plotExam fl expected s e f = plot2d (fl++".plt") expected r f ++
>                              createGnuPlot fl s e
>               where r = createRange s e ((e - s) / 2500)
