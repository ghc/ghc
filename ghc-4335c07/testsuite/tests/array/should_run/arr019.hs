
-- Test for trac #2158

import Data.Array

data Pos = Pos Integer Integer
    deriving (Show, Eq, Ord, Ix)

myBounds :: (Pos, Pos)
myBounds = (Pos 0 0, Pos 2 3)

main :: IO ()
main = do print $ range myBounds
          print $ index myBounds (Pos 0 0)
          print $ index myBounds (Pos 0 1)
          print $ index myBounds (Pos 0 2)
          print $ index myBounds (Pos 0 3)
          print $ index myBounds (Pos 1 0)
          print $ index myBounds (Pos 1 1)
          print $ index myBounds (Pos 1 2)
          print $ index myBounds (Pos 1 3)
          print $ index myBounds (Pos 2 0)
          print $ index myBounds (Pos 2 1)
          print $ index myBounds (Pos 2 2)
          print $ index myBounds (Pos 2 3)
          print $ listArray myBounds [(123 :: Integer) ..]

