{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module T14931_Bug where

import Prelude (Int, IO, Bool(..), Num(..), Monad(..), not, print)
import qualified Language.Haskell.TH.Syntax as TH
import T14931_State

wat :: IO ()
wat = print $(let playGame []     = do
                      (_, score) <- get
                      return score
                  playGame (x:xs) = do
                      (on, score) <- get
                      case x of
                           'a' | on -> put (on, score + 1)
                           'b' | on -> put (on, score - 1)
                           'c'      -> put (not on, score)
                           _        -> put (on, score)
                      playGame xs

                  startState :: (Bool, Int)
                  startState = (False, 0)
              in TH.lift (evalState (playGame "abcaaacbbcabbab") startState) )
