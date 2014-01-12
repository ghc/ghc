module ShouldFail where
data T = T{a::Int}
x = T{,a=42}
