module T11167_ambiguous_fixity_B where
data B = MkB { foo :: Int -> Int }
infixl 5 `foo`
