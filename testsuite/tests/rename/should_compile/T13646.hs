{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module T13646 where

import Control.Exception

foo :: IO ()
foo = do let !() = assert False ()
             -- Should not give a warning

         let () = assert False ()
             -- Should give a warning

         pure ()
