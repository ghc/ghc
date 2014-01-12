-- 
--                  Lrc Prelude for Haskell/Gofer 
-- 
--
-- by João Saraiva
-- Mon Mar  5
--

module LrcPrelude  where


type INT  = Integer
type BOOL = Bool
type STR  = String
type CHAR = Char
type REAL = Float


lrc_REALtoSTR r = show r
lrc_INTtoSTR i = show i
sf_INTtoSTR i = show i

data BTree a  = Fork !a !(BTree a) !(BTree a)
              | Nil 
              deriving (Show, Eq , Ord)

lrc_tree_insert a b Nil = (Fork (a,b) Nil Nil)
lrc_tree_insert a b (Fork (c,d) l r) 
        | a < c  = (Fork (c,d) (lrc_tree_insert a b l) r)
        | a == c = (Fork (a,b) l r)
        | a > c  = (Fork (c,d) l (lrc_tree_insert a b r))

type MAP a b = (BTree (a,b),b)

lrc_empty_map :: a -> (MAP b a)
lrc_empty_map a = (Nil , a)

lrc_map_single_update :: (Eq a , Ord a) => a -> b -> (MAP a b) -> (MAP a b)
lrc_map_single_update a b (t,d)     = ((lrc_tree_insert a b t),d) 

lrc_map_application :: (Eq a , Ord a) => a -> (MAP a b) -> b
lrc_map_application a (Nil, b)                    = b
lrc_map_application a ((Fork (b,c) l r),d)
      | a == b = c
      | a < b  = lrc_map_application a (l,d)
      | a > b  = lrc_map_application a (r,d)


lrc_map_in :: (Eq a , Ord a) => a -> (MAP a b) -> Bool
lrc_map_in _ (Nil, _)                             = False
lrc_map_in a ((Fork (b,c) l r),d)
      | a == b = True
      | a < b  = lrc_map_in a (l,d)
      | a > b  = lrc_map_in a (r,d)

lrc_map_update :: (Eq a , Ord a) => (MAP a b) -> (MAP a b) -> (MAP a b)
lrc_map_update e1 e2 = e1   -- XXXXXXXXXXXXXXXX


lrc_string_index :: String -> Int -> Char
lrc_string_index s i = s!!i


lrc_string_sub :: String -> Int -> Int -> String
lrc_string_sub s i1 i2 = s

lrc_string_opensub s i = s

lrc_strindex s c = 0

repeatCHAR :: a -> Integer -> [a]
repeatCHAR c i = replicate  (fromIntegral i) c
