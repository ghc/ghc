import LibMatchPS

_tailPS' x
 = if _nullPS x then
     error "_tailPS []"
   else
     _substrPS x 1 (_lengthPS x)

subst :: String
      -> String
      -> [Char]
      -> String
      -> String
subst rexp repl flags str 
 = _unpackPS (substPS (_packString rexp)
		      (_packString repl)
		      flags
		      (_packString str))
		

pickFirst :: String
	  -> String
	  -> String
pickFirst str substr
 = let
    str' = _packString str
   in
    case (findPS (_packString str) (_packString substr)) of
	Nothing -> ""
	Just x  -> _unpackPS (_dropPS x str')

pickLast :: String
	 -> String
	 -> String
pickLast str substr
 = let
    str' = _packString str
   in
    case (rfindPS (_packString str) (_packString substr)) of
	Nothing -> ""
	Just x  -> _unpackPS (_dropPS x str')

main 
 = getArgs  >>= \ (pattern:replacement:stuff:xs) ->
   let
    flags
     = case xs of
        [] -> []
	(x:xs) -> x
   in
    (case stuff of
      (':':xs) ->
	     openFile xs ReadMode  >>= \ hndl ->
	     hGetContents hndl
      _ -> return stuff) >>= \ stuff' ->
    putStr (subst pattern replacement flags stuff') >>
    putStr "\n"

