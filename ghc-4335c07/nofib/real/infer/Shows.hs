module  Shows
      (Shows, showsEmpty, showsConcat, showsString, showsChar, showsStar,
       showsStarSep, showsSurround, showsListOf, showsParen, showsParenIf)
      where

type  Shows x  =  x -> ShowS
showsEmpty                    :: ShowS
showsEmpty r                  =  r
showsConcat                   :: [ShowS] -> ShowS
showsConcat                   =  foldr (.) showsEmpty
showsString                   :: Shows String
showsString                   =  (++)
showsChar                     :: Shows Char
showsChar                     =  (:)
showsStar                     :: Shows x -> Shows [x]
showsStar showsX xs           =  showsConcat (map showsX xs)
showsStarSep                  :: String -> Shows x -> Shows [x]
showsStarSep s showsX []      =  showsEmpty
showsStarSep s showsX (x:xs)  =  showsX x
                              .  showsConcat [showString s . showsX x' | x' <- xs]
showsSurround                 :: String -> Shows x -> String -> Shows x
showsSurround l showsX r x    =  showString l . showsX x . showString r
showsListOf                   :: Shows x -> Shows [x]
showsListOf showsX            =  showsSurround "[" (showsStarSep ", " showsX) "]"
showsParen                    :: ShowS -> ShowS
showsParen                    =  showsSurround "(" id ")"
showsParenIf                  :: Bool -> ShowS -> ShowS
showsParenIf b xS             =  if  b  then  showsParen xS  else  xS
