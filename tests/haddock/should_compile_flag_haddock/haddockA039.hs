module CommentsBeforeArguments where

data A = A
data B = B

f1 :: {-| Comment before -}
     () ->

     ()
     {-^ Comment after -} ->

     ()
     {-^ Result after -}
f1 _ _ = ()


f2 :: {-| Comment before -}
     () ->

     ()
     {-^ Comment after -} ->

     {-| Result after -}
     ()
f2 _ _ = ()




