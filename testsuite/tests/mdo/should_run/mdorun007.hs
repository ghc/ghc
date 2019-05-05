{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE Strict #-}

import Control.Monad.Identity

foo :: Identity Int
foo = do
  rec ~_ <- pure ()
  pure 1

main :: IO ()
main = print foo
