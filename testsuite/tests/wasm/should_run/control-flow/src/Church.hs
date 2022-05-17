module Church
where


type Churchlist t u = (t->u->u)->u->u

nil :: Churchlist t u 
nil = \c n -> n
cons :: t -> Churchlist t u -> Churchlist t u
cons x xs = \c n -> c x (xs c n)
