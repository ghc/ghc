{-# LANGUAGE TupleSections #-}
module Main where

import Language.Haskell.TH

import T15843a

main :: IO ()
main = do
  mapM_ (\q -> runQ q >>= print)
        [_1_2, _2_2, _empty_2, _full_2, _3_3]

  mapM_ (\q -> runQ (fmap unType q) >>= print)
        [_1_2_T, _2_2_T]

  runQ (fmap unType _empty_2_T) >>= print
  runQ (fmap unType _full_2_T) >>= print
  runQ (fmap unType _3_3_T) >>= print

  print $ "$_1_2 applied to 'c' should be (909, 'c') ===> "
            ++ (show $ (909, 'c') == ($_1_2 'c'))

  print $ "$_2_2 applied to False should be (False, 909) ===> "
            ++ (show $ (False, 909) == ($_2_2 False))

  print $ "$_3_3 applied to 606 and True should be (606, True, 909) ===> "
            ++ (show $ (606, True, 909) == ($_3_3 606 True))
