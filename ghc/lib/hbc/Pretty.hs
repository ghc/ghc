module Pretty(text, separate, cseparate, nest, pretty, (~.), (^.), IText(..), Context(..)) where

infixr 8 ~.
infixr 8 ^.

type IText   = Context -> [String]
type Context = (Bool,Int,Int,Int)
-- Bool		laying out in vertical context
-- Int		character left on the line before margin is reached
-- Int		maximum preferred number of significant characters on a line
-- Int		number of characters on last line, excluding leading blanks

text :: String -> IText
text s (v,w,m,m') = [s]

getContext t (v,w,m,m') =
	let tn = last t
	    indent = length tn
	    sig = if length t == 1
		  then m' + indent
		  else length (dropWhile (==' ') tn)
	in  (False,w-indent,m,sig)

(~.) :: IText -> IText -> IText
d1 ~. d2 = \ c@(v,w,m,m') ->
	let t = d1 (False,w,m,m')
	    cx@(_,w',_,_) = getContext t c
	    indent = w-w'
	    tn = last t
	    (l:ls) = d2 cx
	in  init t ++
	    [tn ++ l] ++
	    map (space indent++) ls

space :: Int -> String
space n = [' ' | i<-[1..n]]

(^.) :: IText -> IText -> IText
d1 ^. d2 = \ (v,w,m,m') -> d1 (True,w,m,m') ++ d2 (True,w,m,0)

separate :: [IText] -> IText
separate [] _ = [""]
separate ds c@(v,w,m,m') = 
	let hor = joinText (text " ") ds
	    ver = foldr1 (^.) ds
	    t = hor c
	in  if lengthLe t 1 && lengthLe (head t) ((w `min` (m-m')) `max` 0)
	    then t
	    else ver c

-- Try to put as many things as possible on each line.
-- Inefficient!
cseparate :: [IText] -> IText
cseparate [] _ = [""]
cseparate ds c@(v,w,m,m') = 
	let csep r a (d:ds) =
		let t = joinText (text " ") (a ++ [d]) c
		in  if lengthLe t 1 then
			if lengthLe (head t) ((w `min` (m-m')) `max` 0) then
			    csep r (a ++ [d]) ds
			else
			    csep (r++adda a) [d] ds
		    else
			csep (r ++ adda a ++ [d]) [] ds
	    csep r a [] = r ++ adda a
	    adda [] = []
	    adda a = [joinText (text " ") a]
	in  foldr1 (^.) (csep [] [] ds) c

joinText t ds = foldr1 (\d1 d2 -> d1 ~. t ~. d2) ds

-- Check if the length of a list is less than n, without evaluating it completely.
lengthLe :: [a] -> Int -> Bool
lengthLe []     n = n >= 0
lengthLe (_:_)  0 = False
lengthLe (_:xs) n = lengthLe xs (n-1)

nest :: Int -> IText -> IText
nest n d (v,w,m,m') = 
	if v then
	    map (space n++) (d (v,w-n,m,if m'==0 then 0 else m'+n)) 
	else 
	    d (v,w,m,m')

pretty :: Int->Int->IText->String
pretty w m d = unlines (d (False,w,m,0))
