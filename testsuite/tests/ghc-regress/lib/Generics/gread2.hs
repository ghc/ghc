{-# OPTIONS -fglasgow-exts #-}

{-

For the discussion in the 2nd boilerplate paper,
we favour some simplified generic read, which is checked to compile.
For the full/real story see Data.Generics.Text.

-}

module Main where
import Data.Generics

gread :: Data a => String -> Maybe a
gread input = runDec input readM

-- The decoder monad
newtype DecM a = D (String -> Maybe (String, a))

instance Monad DecM where
    return a = D (\s -> Just (s,a))
    (D m) >>= k = D (\s ->
      case m s of
        Nothing -> Nothing
        Just (s1,a) -> let D n = k a
                        in n s1)
        
runDec :: String -> DecM a -> Maybe a
runDec input (D m) = do (_,x) <- m input
                        return x

parseConstr :: DataType -> DecM Constr
parseConstr ty = D (\s ->
      match s (dataTypeConstrs ty))
 where
  match :: String -> [Constr]
        -> Maybe (String, Constr)
  match _ [] = Nothing
  match input (con:cons)
    | take n input == showConstr con
    = Just (drop n input, con)
    | otherwise
    = match input cons
    where
      n = length (showConstr con)


readM :: forall a. Data a => DecM a
readM = read
      where
	read :: DecM a
	read = do { let val = argOf read
           	  ; let ty  = dataTypeOf val
           	  ; constr <- parseConstr ty
           	  ; let con::a = fromConstr constr
           	  ; gmapM (\_ -> readM) con }

argOf :: c a -> a
argOf = undefined

yareadM :: forall a. Data a => DecM a
yareadM = do { let ty = dataTypeOf (undefined::a)
             ; constr <- parseConstr ty
             ; let con::a = fromConstr constr
             ; gmapM (\_ -> yareadM) con }

main = print $ True
