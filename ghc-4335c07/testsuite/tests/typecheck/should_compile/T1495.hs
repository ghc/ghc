-- Test Trac #1495

module CompilerBug where

newtype Fix a = Fix (a (Fix a))
data ID a = ID a
newtype I a = I a

testOk :: Fix ID
testOk = undefined

-- this definition causes the compiler to fail to terminate
testInfiniteLoop :: Fix I
testInfiniteLoop = undefined


newtype T = MkT T
test :: T
test = undefined
