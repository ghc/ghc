#ifdef HEAD
module Maybe(
    isJust, fromJust, fromMaybe, listToMaybe, maybeToList,
    catMaybes, mapMaybe, unfoldr ) where
import PreludeBuiltin
#endif /* HEAD */
#ifdef BODY

isJust                 :: Maybe a -> Bool
isJust (Just a)        =  True
isJust Nothing         =  False

fromJust               :: Maybe a -> a
fromJust (Just a)      =  a
fromJust Nothing       =  error "Maybe.fromJust: Nothing"

fromMaybe              :: a -> Maybe a -> a
fromMaybe d Nothing    =  d
fromMaybe d (Just a)   =  a

maybeToList            :: Maybe a -> [a]
maybeToList Nothing    =  []
maybeToList (Just a)   =  [a]

listToMaybe            :: [a] -> Maybe a
listToMaybe []         =  Nothing
listToMaybe (a:_)      =  Just a
 
catMaybes              :: [Maybe a] -> [a]
catMaybes ms           =  [ m | Just m <- ms ]

mapMaybe               :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f             =  catMaybes . map f

unfoldr                :: ([a] -> Maybe ([a], a)) -> [a] -> ([a],[a])
unfoldr f x =
  case f x of
  Just (x',y) -> let (ys,x'') = unfoldr f x' in (x'',y:ys)
  Nothing     -> (x,[])

#endif /* BODY */
