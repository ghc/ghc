module T21360b where

foo :: Maybe Char -> Char
foo x
  | y@(Just _) <- x
  , let z = case y of { Just w -> w }
  , let _ = case x of { Just _ -> 'r' }
  = z
  | otherwise
  = 'o'
