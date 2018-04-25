module ShouldFail where

f x = g+1
      where g y = h+2
                  where h z = z z
