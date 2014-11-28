module ExtraConstraintsWildcardNotEnabled where


show' :: _ => a -> String
show' x = show x

-- with the PartialTypeSignatures extension enabled this would lead to the
-- type Show a => a -> String
