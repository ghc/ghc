-- !!! Testing handling of troublesome constructor name (:::)
module MyList (MyList(Empty, (:::))) where

data MyList a =   Empty
                | (MyList a) ::: (MyList a)
