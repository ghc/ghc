module Encode (CodeEvent(..), encode, maxBits)
where

--import GlaExts

data PrefixTrie a b = PTNil |
                      PT a b (PrefixTrie a b) (PrefixTrie a b) (PrefixTrie a b)

type CodeTable = PrefixTrie Char Int

data CodeEvent = 
                 Code Int |
                 NewWordSize |
                 Clear deriving Show{-was:Text-}

data CodeState = CS
                 Int {-# STRICT #-}
                 Int {-# STRICT #-}
                 Int {-# STRICT #-}
                 Int {-# STRICT #-}
                 Int {-# STRICT #-}
                 Int {-# STRICT #-}

firstEnt    = 257       :: Int
maxBits     = 16        :: Int
checkGap    = 10000     :: Int
firstCheck  = 10000     :: Int
firstChange = (2^9) + 1 :: Int

maxmaxCode = 2^maxBits + 1 :: Int

encode :: [Int] -> String -> [CodeEvent]
encode = encode' (CS 3 1 firstCheck 0 firstEnt firstChange) initial_table 

encode' :: CodeState -> CodeTable -> [Int] -> String -> [CodeEvent]
encode' _ _ _ [] = []
encode' c@(CS bo ci cp ra nx cg) t sizes input 
  = if nx == cg then
    NewWordSize : encode' (CS (bo+s) ci cp ra nx cg') t ss input 
  else
    if nx == maxmaxCode then
      if ci >= cp then
        let ra' = (ci * 256) `div` bo in
          if ra' > ra then
            encode' (CS bo ci (ci+checkGap) ra' nx cg) t sizes input 
          else
            Clear :
            encode' (CS (bo+s) ci (ci+checkGap) 0 firstEnt firstChange)
                    initial_table ss input 
       else 
         let (input', n, i) = code_string_r (input, 0, 0) nx t
         in  Code n :
             encode' (CS (bo+s) (ci+i) cp ra nx cg) t ss input' 
     else
       (\ ((input', n, i), t') ->
       Code n :
       encode' (CS (bo+s) (ci+i) cp ra (nx+1) cg) t' ss input')
       (code_string_rw (input, 0, 0) nx t)
  where
  (s:ss) = sizes
  cg' = let val = ((cg - 1) * 2) + 1 in
             if val == maxmaxCode then 0 else val

csForced (CS a b c d e f) = (a==a) && (b==b) && (c==c) && (d==d) 
                                   && (e==e) && (f==f)

code_string_r :: (String, Int, Int) -> Int -> CodeTable -> (String, Int, Int)
code_string_r s@([], _, _) _ _
     = s
code_string_r s _ PTNil
     = s
code_string_r s@(c:cs, old_code, n) next_code (PT k v k_pt l r)
     = if c == k then
            code_string_r (cs, v, (n+1)) next_code k_pt
       else
            code_string_r s next_code (if c < k then l else r)

code_string_rw :: (String, Int, Int) -> Int -> CodeTable
                       -> ((String, Int, Int), CodeTable)
code_string_rw s@([], _, _) _ _
     = (s, PTNil)
code_string_rw s@(c:_,_,_) next_code PTNil
     = (s, PT c next_code PTNil PTNil PTNil)
code_string_rw s@(c:cs, old_code, n) next_code (PT k k_code k_pt l r)
     | c < k     = (\ (s', l') -> (s', PT k k_code k_pt l' r))
                         (code_string_rw s next_code l)
     | c > k     = (\ (s', r') -> (s', PT k k_code k_pt l r'))
                         (code_string_rw s next_code r)
     | otherwise = (\ (s', t') -> (s', PT k k_code t' l r))
                         (code_string_rw (cs, k_code, n+1) next_code k_pt)

initial_table :: CodeTable
initial_table = build_table 0 255

build_table :: Int -> Int -> CodeTable
build_table lo hi
     = if lo > hi then
           PTNil
       else let mid = (lo + hi) `div` 2 in
	      --trace (show (lo,hi,mid))
              PT (toEnum mid) mid PTNil
                   (build_table lo (mid - 1))
                   (build_table (mid + 1) hi)
