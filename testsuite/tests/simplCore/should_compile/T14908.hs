{-# LANGUAGE OverloadedStrings, LambdaCase #-}
import Data.String
import T14908_Deps
import Data.List (nub, sort)
import System.Environment

data Expr = Var String | Not Expr | Expr :|: Expr | Expr :&: Expr deriving Eq

nfd e = if e == e' then e else nfd e'
  where e' = rec e
        rec x@(Var _) = x
        rec x@(Not (Var _)) = x
        rec (Not (Not x)) = x
        rec (Not (a :|: b)) = rec $ Not a :&: Not b
        rec (Not (a :&: b)) = rec $ Not a :|: Not b
        rec ((a :|: b) :&: c) = rec $ (a :&: c) :|: (b :&: c)
        rec (a :&: (b :|: c)) = rec $ (a :&: b) :|: (a :&: c)
        rec (a :&: b) = rec a :&: rec b
        rec (a :|: b) = rec a :|: rec b

eval :: Expr -> Reader [(String, Bool)] Bool
eval (Var x) = asks (lookup x) >>= maybe (error $ "Var `" ++ x ++ "` not defined!") return
eval (Not x) = not <$> eval x
eval (a :&: b) = (&&) <$> eval a <*> eval b
eval (a :|: b) = (||) <$> eval a <*> eval b

instance Show Expr where
  show (Var x) = x
  show (Not (Var x)) = "¬" ++ x
  show (Not x) = "¬(" ++ show x ++ ")"
  show (a :|: b) = show' a ++ " ∨ " ++ show' b where show' c@(_ :&: _) = "(" ++ show c ++ ")"; show' c = show c
  show (a :&: b) = show' a ++ " ∧ " ++ show' b where show' c@(_ :|: _) = "(" ++ show c ++ ")"; show' c = show c

instance IsString Expr where
  fromString = Var

fullImage e = (runReader (eval e) . zipWith (,) vs) <$> bits (length vs)
  where vs = nub $ sort $ vars e
        vars (Var x) = [x]
        vars (Not x) = vars x
        vars (a :&: b) = vars a ++ vars b
        vars (a :|: b) = vars a ++ vars b
        bits 0 = [[]]
        bits n = ((False:) <$> xs) ++ ((True:) <$> xs) where xs = bits (n - 1)

testNFD e = fullImage e == fullImage (nfd e)

instance Arbitrary Expr where
  arbitrary = sized gen
              where gen n = choose (1, max 1 (min 4 n)) >>= \case
                              1 -> (Var . (:"") . (['a'..]!!)) <$> choose (1, 5) -- no más de 5 vars
                              2 -> (:&:) <$> gen (n-1) <*> gen (n-1)
                              3 -> (:|:) <$> gen (n-1) <*> gen (n-1)
                              4 -> Not <$> gen (n-1)

main = do
  [a,b] <- map (read :: String -> Int) <$> getArgs
  quickCheckWith (stdArgs {maxSize = fromIntegral 10, maxSuccess = fromIntegral 1000}) testNFD
