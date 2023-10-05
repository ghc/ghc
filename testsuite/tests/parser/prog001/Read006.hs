-- !!! Testing handling of troublesome constructor name (:::)
module Read006 (MyList(Empty, (:::))) where

data MyList a =   Empty
                | (MyList a) ::: (MyList a)
