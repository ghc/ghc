--!!! Testing IOExts.fixIO

> module FixIOTest where
> import Monad
> import Maybe
> import IOExts( fixIO )

First a recursively-defined environment in the normal way:

> env = foldl (\env' (s,v) -> enter env' s v) 
>             empty 
>             [ ("f", (1, fst (fromJust (look env "g")))) ,
>               ("g", (2, fst (fromJust (look env "f")))) ]

> env2 = let vF = (1, fst (fromJust (look env2 "g")))
>            vG = (2, fst (fromJust (look env2 "f")))
>        in enter (enter empty "f" vF) "g" vG

Which yields these correct evaluations:
  look env' "f"  ==>  (1,2)
  look env' "g"  ==>  (2,1)

Now let's add some IO to each "store action" and use foldM/fixIO to
tie it all together:

> main =
>   do env <- fixIO (\env -> do
>               foldM (\env' (s,vM) -> do v <- vM
>                                         return (enter env' s v)) 
>                     empty 
>                     [ ("f", do putStrLn "storing f"
>                                return (1, fst (fromJust (look env "g")))) ,
>                       ("g", do putStrLn "storing g"
>                                return (2, fst (fromJust (look env "f")))) ] )
>      print (look env "f")
>      print (look env "g")
>      return ()

> main2 =
>   do env <- fixIO (\env -> do
>               let vF = (1,fst (fromJust (look env "g")))
>                   vG = (2,fst (fromJust (look env "f")))
>               putStrLn "storing f and g"
>               return $ enter (enter empty "f" vF) "g" vG
>               )
>      putStrLn "Constructed environment"
>      print env
>      print (look env "f")
>      print (look env "g")
>      return ()

But this unfortunately dies a horrible death:

FixIOTest> main
storing f
storing g
Just (1,
Program error: {_Gc Black Hole}

If I comment out the "print" statements I get:

FixIOTest> main
storing f
storing g

and it terminates properly.

----------------------------------------------------------------
-- Environments
----------------------------------------------------------------

> empty  :: Table a
> enter :: Table a -> String -> a -> Table a
> look :: Table a -> String -> Maybe a

----------------------------------------------------------------
-- A very simple environment implemented as functions:
----------------------------------------------------------------

> {-
> type Table a = String -> Maybe a
> empty s = Nothing
> enter t s1 x s2 | s1==s2    = Just x
>                 | otherwise = look t s2 
> look t s = t s
> -}

----------------------------------------------------------------
-- A very simple environment implemented using association lists:
----------------------------------------------------------------

> type Table a = [(String,a)]
> empty = []
> enter t s x = (s,x):t
> look t s = lookup s t


