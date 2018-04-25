{-# LANGUAGE MagicHash, UnboxedTuples #-}
import GHC.Exts
newtype Eval a = Eval {runEval :: State# RealWorld -> (# State# RealWorld, a #)}

-- inline sequence ::  [Eval a] -> Eval [a]
well_sequenced ::  [Eval a] -> Eval [a]
well_sequenced = foldr cons nil where
  cons e es = Eval $ \s -> case runEval e s of
                       (# s', a #) -> case runEval es s' of
                         (# s'', as #) -> (# s'', a : as #)
  nil = Eval $ \s -> (# s, [] #)

-- seemingly demonic use of spark#
ill_sequenced ::  [Eval a] -> Eval [a]
ill_sequenced  as = Eval $ spark# (case well_sequenced as of
             Eval f -> case f realWorld# of  (# _, a' #) -> a')

-- 'parallelized' version of (show >=> show >=> show >=> show >=> show)
main :: IO ()
main = putStrLn ((layer . layer . layer . layer . layer) (:[]) 'y')
  where
  layer :: (Char -> String) -> (Char -> String)
  layer f = (\(Eval x) -> case x realWorld# of (# _, as #) -> concat as)
        . well_sequenced    -- [Eval String] -> Eval [String]
        . map ill_sequenced -- [[Eval Char]] -> [Eval String];
                            -- 'map well_sequenced' is fine
        . map (map (\x -> Eval $ \s -> (# s, x #))) -- wrap each Char in Eval
        . chunk'            -- String -> [String]
        . concatMap f
        . show              -- add single quotes

  chunk' ::  String -> [String]
  chunk' [] = []
  chunk' xs =  as : chunk' bs where (as,bs) = splitAt 3 xs

  -- this doesn't work:
  -- chunk (a:b:c:xs) = [a,b,c]:chunk xs
  -- chunk xs = [xs]
