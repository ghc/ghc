-- Record construction should fail statically 
-- if there are any strict fields,
-- including in the non-record case.

module ShouldFail where

data S = S { x::Int, y:: ! Int }
data T = T Int !Int
data U = U Int  Int

s1 = S {}	-- Bad
s2 = S { x=3 }	-- Bad
s3 = S { y=3 }	-- Ok
t  = T {}	-- Bad
u  = U {}	-- Ok
