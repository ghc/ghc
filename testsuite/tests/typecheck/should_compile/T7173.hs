module T7173 where


-- se and sb are mutually recursive
--   se takes a numeric parameter (which it ignores)
--   sb passes it a constant 10
--
-- so sb :: Num a => ...type not involving a...
--
-- So the ambiguity check must default ambiguous p, 
-- rather than complaining that sb is ambiguous.

showEnv' env t = se 10 env t 
 where
   se p env (Bind n b sc) = sb env n b
   sb env n t             = se 10 env t

data TT n = Bind n (TT n) (TT n)
