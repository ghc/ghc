--!!! Testing error-line numbers II (fixed from Hugs 1.01)
f :: (Show a, Read a) => a -> String
(f,g) = (show,read)
