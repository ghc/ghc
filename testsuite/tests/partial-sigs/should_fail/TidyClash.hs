module TidyClash where

-- Type variables originating from wildcards are normally given the name w_,
-- but in this case there is already a type variable called w_. Tidying the
-- types should result in w_1 and w_2 for the two new type variables
-- originating from the wildcards.

bar :: w_ -> (w_, _ -> _)
bar x = (x, \y -> undefined)
