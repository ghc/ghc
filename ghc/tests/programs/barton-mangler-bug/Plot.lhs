The functions in this file (well, the single function) will allow the
user to plot different functions using the Gnuplot program.  In fact,
all it really does is output a number of points on the list and allow
the user to activate Gnuplot and use the plotting program to get the
appropriate output.

The first line just gives the module name.  For the moment, I don't
anticipate using any modules (although this may change).

> module Plot where
> import IO

Now we give the type of the function.  This consists of a file name, a
list of values, and a function that goes from the appropriate types.

> plot2d:: (Show a, Show b) => String -> [a] -> (a -> b) -> IO()
> plot2d fl inp f = openFile fl WriteMode      >>= \flh ->
>                   plot2d' flh inp f         >>
>                   hClose flh

> plot2d':: (Show a, Show b) => Handle -> [a] -> (a -> b) -> IO()
> plot2d' fl [] f     = return ()
> plot2d' fl (x:xs) f = hPutStr fl  (show x)        >>
>                       hPutStr fl  "  "            >>
>                       hPutStr fl  (show (f x))    >>
>                       hPutStr fl  "\n"            >>
>                       plot2d' fl xs f

> plot3d:: (Show a, Show b, Show c) => String -> [a] -> [b] -> 
>                                      (a -> b -> c) -> IO()
> plot3d fl inp1 inp2 f = openFile fl WriteMode      >>= \flh ->
>                         plot3d' flh inp1 inp2 f    >>
>                         hClose flh

> plot3d':: (Show a, Show b, Show c) => Handle -> [a] -> [b] -> 
>                                       (a -> b -> c) -> IO()
> plot3d' fl []     inp f = return ()
> plot3d' fl (x:xs) inp f = plot3d'' fl x inp f           >>
>                           hPutStr fl "\n"               >>
>                           plot3d' fl xs inp f

> plot3d'':: (Show a, Show b, Show c) => Handle -> a -> [b] -> 
>                                        (a -> b -> c) -> IO()
> plot3d'' fl inp [] f        = return ()
> plot3d'' fl x (y:ys) f = hPutStr fl  (show x)        >>
>                          hPutStr fl  "  "            >>
>                          hPutStr fl  (show y)        >>
>                          hPutStr fl  "  "            >>
>                          hPutStr fl  (show (f x y))  >>
>                          hPutStr fl  "\n"            >>
>                          plot3d'' fl x ys f


And now, let's create a function to make a range out of a triple of a
start point, an end point, and an increment.

> createRange:: (Num a, Ord a) => a -> a -> a -> [a]
> createRange s e i = if s > e then []
>                     else s : createRange (s+i) e i

We now settle down to a couple of more specific functions that do
things that are more unique to gnuplot.  First, we have something that
creates the appropriate gnuplot command file.

> createGnuPlot:: Show a => String -> a -> a -> IO()
> createGnuPlot fl s e = openFile (fl ++ ".gnp") WriteMode   >>= \flh ->
>                        hPutStr flh "set terminal latex\n"  >>
>                        hPutStr flh "set output \""         >>
>                        hPutStr flh (fl ++ ".tex\"\n")      >>
>                        hPutStr flh "set nokey\n"           >>
>                        hPutStr flh "plot ["                >>
>                        hPutStr flh (show s)                >>
>                        hPutStr flh ":"                     >>
>                        hPutStr flh (show e)                >>
>                        hPutStr flh "] \""                  >>
>                        hPutStr flh (fl ++ ".plt\"")        >>
>                        hPutStr flh " with lines\n"         >>
>                        hClose  flh

And now we create a fairly specific plotExam function that takes a
string, a function, and two floats and produces the correct files

> plotExam:: String -> Float -> Float -> (Float -> Float) -> IO()
> plotExam fl s e f = plot2d (fl++".plt") r f                >>
>                     createGnuPlot fl s e
>               where r = createRange s e ((e - s) / 2500)
