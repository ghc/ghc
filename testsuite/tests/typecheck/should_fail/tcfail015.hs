module ShouldFail where

data AList a = ANull | ANode a (AList a)

type IntList = AList Int

g (ANull) = 2
g (ANode b (ANode c d)) | b = c+1
                        | otherwise = 4
