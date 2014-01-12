module T8570 where

import T8570a (Image(filepath), logo)
import T8570b (Field(Image))

foo = let Image {filepath = x} = logo in x

