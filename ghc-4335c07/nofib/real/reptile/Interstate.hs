-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991

module Interstate(inter) where

inter :: (a->[b]) -> (a->c->Bool) -> (a->c->([b],a,c)) -> (a->c->[b])
inter prompt endp transact =
	interprog
	where
        interprog state inpt =
	  prompt state ++
          if endp state inpt then []
          else response ++ interprog newstate restofinput
	       where
               (response,newstate,restofinput) = transact state inpt



