module Solution (solve) where

import Board
import Move
import Data.List (sortBy)

solve :: Board -> Colour -> Int -> String
solve bd c n = showResult (solution bd c (2*n-1))

data Solution = Solution MoveInFull [(MoveInFull,Solution)]

solution :: Board -> Colour -> Int -> Maybe Solution
solution bd c n | n > 0 = 
	let mds = moveDetailsFor c bd in
	foldr solnOr Nothing mds
	where
	solnOr (mif,b) other =
		let rsm = replies b (opponent c) (n-1) in
		case rsm of
		Nothing -> other
		Just [] -> if kingincheck (opponent c) b then
			   	Just (Solution mif [])
			   else other
		Just rs -> Just (Solution mif rs)

replies :: Board -> Colour -> Int -> Maybe [(MoveInFull, Solution)]
replies bd c n | n==0 = if null mds then Just [] else Nothing
	       | n>0  =
	foldr solnAnd (Just []) mds
	where
	mds = moveDetailsFor c bd
	solnAnd (mif,b) rest =
		let sm = solution b (opponent c) (n-1) in
		case sm of
		Nothing -> Nothing
		Just s ->  case rest of
				Nothing -> Nothing
				Just ms -> Just ((mif,s):ms)

showResult Nothing = "No solution!\n"
showResult (Just s) = showSoln (compact s) 1

data Soln = Soln MoveInFull [([MoveInFull],Soln)] deriving (Eq,Ord)

compact :: Solution -> Soln
compact (Solution mif rs) = Soln mif (foldr insertCompact [] rs)

insertCompact (mif,s) = ic
	where
	ic [] = [([mif],cs)]
	ic crs@((mifs,cs'):etc) =
		case compare (showSoln cs 1) (showSoln cs' 1) of
		LT -> ([mif], cs) : crs
		EQ -> (insert mif mifs,cs) : etc
		GT -> (mifs,cs') : ic etc
	cs = compact s
	insert x [] = [x]
	insert x (y:ys) = case compare x y of
			  GT -> y : insert x ys
			  _  -> x : y : ys

showSoln (Soln mif rs) n =
	show n ++ ". " ++ showMoveInFull mif ++
	( case rs of
	  []	     -> "++\n"
	  [(mifs,s)] -> ", " ++
			( if length mifs > 1 then "..." else showMoves mifs) ++
			"; " ++ showSoln s (n+1)
	  _	     -> ",\n" ++ showReplies (sortBy cmpLen rs) n )
	where
	cmpLen (xs,_) (ys,_) = compare (length xs) (length ys)

showReplies [] n = ""
showReplies ((mifs,s):rs) n =
	tab n ++ "if " ++
	( if null rs && length mifs > 1 then "others"
	  else showMoves mifs ) ++ "; " ++
        showSoln s (n+1) ++ showReplies rs n

tab :: Int -> String
tab n = take n (repeat '\t')
