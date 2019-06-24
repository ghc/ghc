{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
module Main where

import Language.Haskell.TH

import T15843a

ppr_and_show a = print a >> (putStrLn $ pprint a)

main :: IO ()
main = do
  mapM_ (\q -> runQ q >>= ppr_and_show)
        [first_of_2, second_of_2, empty_2, full_2, third_of_3]

  mapM_ (\q -> runQ (fmap unType q) >>= ppr_and_show)
        [first_of_2_T, second_of_2_T]

  runQ (fmap unType empty_2_T) >>= ppr_and_show
  runQ (fmap unType full_2_T) >>= ppr_and_show
  runQ (fmap unType third_of_3_T) >>= ppr_and_show

  print $ "(909,) applied to 'c' should be (909, 'c') ===> "
            ++ (show $ (909, 'c') == ($first_of_2 'c'))

  print $ "(,909) applied to False should be (False, 909) ===> "
            ++ (show $ (False, 909) == ($second_of_2 False))

  print $ "(,,909) applied to 606 and True should be (606, True, 909) ===> "
            ++ (show $ (606, True, 909) == ($third_of_3 606 True))

  mapM_ (\q -> runQ q >>= ppr_and_show)
        [unb0, unb1, unb2, unb3, unb4]
