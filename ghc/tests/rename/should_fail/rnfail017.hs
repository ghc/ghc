module ShouldFail where

-- !!! Precedence of unary negation

f1 x y = x + -y 	-- Fails
f2 x y = x * -y		-- Fails


f3 x y = -x + y		-- OK: means  (-x) + y
			-- since - is left associative

f4 x y = - x*y		-- OK: means -(x*y)
			-- since - binds less tightly than *

f5 x y = x >= -y	-- OK means x >= (-y)


