-- !!! recursive funs tangled in an AbsBind

module ShouldCompile where


flatten :: Int		-- Indentation
        -> Bool		-- True => just had a newline
        -> Float	-- Current seq to flatten
        -> [(Int,Float)]-- Work list with indentation
        -> String

flatten n nlp 0.0 seqs = flattenS nlp seqs
flatten n nlp 1.0 seqs = flatten n nlp 1.1 ((n,1.2) : seqs)

flattenS :: Bool -> [(Int, Float)] -> String
flattenS nlp [] = ""
flattenS nlp ((col,seq):seqs) = flatten col nlp seq seqs
