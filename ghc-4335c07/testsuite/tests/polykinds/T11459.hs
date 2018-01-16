{-# LANGUAGE  PolyKinds, RankNTypes #-}

module T11459 where


type Failure f r   = String -> f r
type Success a f r = a -> f r

newtype Parser a = Parser {
      unParser :: forall f r.
                  Failure f r
               -> Success a f r
               -> f r
    }
