-- Test for Trac #1899

module T1899 where

 data Constraint a = Formula [[Proposition a]]
 data Proposition a = Prop a
                    | Auxiliary [Proposition a]

 transRHS :: [a] -> Int -> Constraint a
 transRHS varSet b =
     if b < 0
       then Formula [[Prop (Auxiliary undefined)]]
       else Formula $
                 [[Prop (Auxiliary varSet),
                   Prop (Auxiliary varSet)]
                 ]
