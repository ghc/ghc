-- matches
-----
f x = case x of
       True -> True
       False -> x

-----

f ((x:a),y) = x
f (a,b) = 2

-----
