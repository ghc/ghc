module TypeClassConstraintsInformedHoles where

-- Make sure that constraints imposed by the call site
-- are handled. See trac #14273
pleaseShow :: Show a => Bool -> a -> Maybe String
pleaseShow False _ = Nothing
pleaseShow True a = Just (show _a)

k :: String
k = "I fit into the hole in foo!"

foo :: [a] -> String
foo xs = show (_h ++ [])
