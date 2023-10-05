module T3823A where

import T3823B

data A = X { x :: Bool } | Y

y :: A -> A
y = \_ -> Y
