module Main where

import T14285a
import Prelude hiding (null)

main :: IO ()
main = do
  let args = "hw"
  print $ null $ pre_images 'a' (Rel (fromList [('a',sfromList args)]) (fromList [('b',sfromList args)]))
