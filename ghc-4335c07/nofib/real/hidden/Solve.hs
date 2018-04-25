module Solve(solve,alt) where
import Numbers
import Vectors
import EdgePlate
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Mark R: solving sets of equations
-- ignore the z coordinate
-- lambda*proj v1 + mu*proj v2 = proj w =>
-- solve v1 v2 w yields the (empty or singleton) list of solutions (lambda,mu)

solve :: Vector -> Vector -> Vector -> [(Number,Number)]
solve v1 v2 w =
	let determinant = z (v1*v2) in
	if determinant==0
	then []
	else [( z(w*v2) / determinant, z(v1*w) / determinant)]

-- The point (x(p), y(p), alt ls p) lies in the plate in which ls lies.
-- Hence proj p == proj (s(l1)+lambda*h(l1)+mu*h(l2) ) so use solve
-- lambda*proj h(l1) + mu*proj h(l2) = proj (p-s(l1))

alt :: Plate -> Vector -> Number
alt (Plt _ (l1:l2:_)) p =
	z( sl1 + lambda `mulv` hl1 + mu `mulv` hl2)
	where
	sl1 = s(l1) ; hl1 = h(l1) ; hl2 = h(l2)
	[(lambda,mu)] = solve hl1 hl2 (p-sl1)
	-- this is always solvable

