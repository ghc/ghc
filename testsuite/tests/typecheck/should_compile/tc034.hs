module ShouldSucceed where

data AList a = ANull | ANode a (AList a)

type IntList = AList Int

g (ANull) = 2
g (ANode b (ANode c d)) | b = 3
                        | True = 4


