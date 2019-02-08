module T16288C where

data Doc = Empty | Beside Doc

hcat :: Doc -> Doc
hcat Empty = Empty
hcat xs = hcat xs

pretV = hcat Empty

foo :: Doc -> Doc
foo Empty = hcat Empty
foo val = Beside val
