module T7856 where

tmp :: String -> IO ()
tmp = sequence_ lst
  where lst = [putStrLn "hi"]

-- sequence_ :: Monad m => [m a] -> m ()

{-    m () ~ (->) String (IO ())    -- From result of sequence_
      m a  ~ IO ()                  -- From argument of sequence_

Depends which one gets treated first.
  m := IO 
is better than
  m := (->) String
It's a bit random which is chosen.

I'll put it in regression suite so we see if it wobbles around.
-}