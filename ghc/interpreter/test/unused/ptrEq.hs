foo :: Float -> Float
foo = cache sin

-- A lazy cache.
-- Uses pointer equality (which is not referentially transparent)
-- in a referentially transparent way to allow the test to be:
-- 1) Fully polymorphic (no Eq context)
-- 2) Safe (no assumption that Eq is correct)
-- 3) Lazy -- no need to evaluate the entire argument.
-- Unlike John Hughes' lazy memo functions, there's no assistance
-- from the garbage collector to delete entries which can never be
-- used in the future.

cache :: (a -> b) -> (a -> b)
cache f = \x -> unsafePerformIO (f' x)
 where
  ref  = unsafePerformIO (newRef (error "cache", error "cache"))
  f' x = derefRef ref >>= \ (x',a) ->
         if x `primPtrEq` x' then
           hit >>
           return a
	 else
	   miss                 >>
	   let a = f x in
	   assignRef ref (x, a) >>
	   return a

primitive primPtrEq "primPtrEq" :: a -> a -> Bool


-- Hooks for recording cache hits and misses
{-
hit  = return ()
miss = return ()
-}

hit  = putStrLn "hit"
miss = putStrLn "miss"

{-
hitRef, missRef :: Ref Int
hitRef  = unsafePerformIO (newRef 0)
missRef = unsafePerformIO (newRef 0)
hit  = derefRef hitRef  >>= \ x -> assignRef hitRef (x+1)
miss = derefRef missRef >>= \ x -> assignRef missRef (x+1)

report = 
  derefRef hitRef  >>= \ hits ->
  derefRef missRef >>= \ misses ->
  putStrLn ("Cache hits: " ++ show hits ++ "; cache misses: " ++ show misses)
-}

        
