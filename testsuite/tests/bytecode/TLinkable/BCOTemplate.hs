
-- Lambda calculus AST
data Expr
  = Var String
  | Lam String Expr
  | App Expr Expr
  | Lit Int
  | Add Expr Expr
  | IfZero Expr Expr Expr
  deriving (Eq, Show)

-- 1. Count total nodes (simple structural recursion)
countNodes :: Expr -> Int
countNodes (Var _) = 1
countNodes (Lam _ body) = 1 + countNodes body
countNodes (App f arg) = 1 + countNodes f + countNodes arg
countNodes (Lit _) = 1
countNodes (Add e1 e2) = 1 + countNodes e1 + countNodes e2
countNodes (IfZero cond then_ else_) =
  1 + countNodes cond + countNodes then_ + countNodes else_

-- 2. Collect all free variables (accumulator-based recursion)
freeVars :: Expr -> [String]
freeVars expr = go expr []
  where
    go (Var x) bound = if x `elem` bound then [] else [x]
    go (Lam x body) bound = go body (x : bound)
    go (App f arg) bound = go f bound ++ go arg bound
    go (Lit _) _ = []
    go (Add e1 e2) bound = go e1 bound ++ go e2 bound
    go (IfZero c t e) bound = go c bound ++ go t bound ++ go e bound

-- 3. Substitute variable with expression (multiple case handling)
subst :: String -> Expr -> Expr -> Expr
subst var replacement expr = case expr of
  Var x | x == var -> replacement
        | otherwise -> Var x
  Lam x body | x == var -> Lam x body
             | otherwise -> Lam x (subst var replacement body)
  App f arg -> App (subst var replacement f) (subst var replacement arg)
  Lit n -> Lit n
  Add e1 e2 -> Add (subst var replacement e1) (subst var replacement e2)
  IfZero c t e -> IfZero
    (subst var replacement c)
    (subst var replacement t)
    (subst var replacement e)

-- 4. Evaluate to normal form (call-by-value, nested pattern matching)
eval :: Expr -> Expr
eval (Lit n) = Lit n
eval (Lam x body) = Lam x body
eval (Var x) = Var x
eval (Add e1 e2) = case (eval e1, eval e2) of
  (Lit n1, Lit n2) -> Lit (n1 + n2)
  (v1, v2) -> Add v1 v2
eval (IfZero cond then_ else_) = case eval cond of
  Lit 0 -> eval then_
  Lit _ -> eval else_
  v -> IfZero v then_ else_
eval (App f arg) = case eval f of
  Lam x body -> eval (subst x (eval arg) body)
  v -> App v (eval arg)

-- 5. Check if expression is closed (uses freeVars)
isClosed :: Expr -> Bool
isClosed expr = null (freeVars expr)

-- 6. Compute expression depth (max recursion depth)
depth :: Expr -> Int
depth (Var _) = 1
depth (Lit _) = 1
depth (Lam _ body) = 1 + depth body
depth (App f arg) = 1 + max (depth f) (depth arg)
depth (Add e1 e2) = 1 + max (depth e1) (depth e2)
depth (IfZero c t e) = 1 + maximum [depth c, depth t, depth e]

-- 7. Pretty print (string building recursion)
pretty :: Expr -> String
pretty (Var x) = x
pretty (Lit n) = show n
pretty (Lam x body) = "(λ" ++ x ++ "." ++ pretty body ++ ")"
pretty (App f arg) = "(" ++ pretty f ++ " " ++ pretty arg ++ ")"
pretty (Add e1 e2) = "(" ++ pretty e1 ++ " + " ++ pretty e2 ++ ")"
pretty (IfZero c t e) =
  "(if0 " ++ pretty c ++ " then " ++ pretty t ++ " else " ++ pretty e ++ ")"

-- 8. Alpha equivalence (renaming-aware comparison)
alphaEq :: Expr -> Expr -> Bool
alphaEq e1 e2 = go e1 e2 []
  where
    go (Var x) (Var y) env = case lookup x env of
      Just y' -> y == y'
      Nothing -> x == y
    go (Lam x1 b1) (Lam x2 b2) env =
      go b1 b2 ((x1, x2) : env)
    go (App f1 a1) (App f2 a2) env =
      go f1 f2 env && go a1 a2 env
    go (Lit n1) (Lit n2) _ = n1 == n2
    go (Add e1a e1b) (Add e2a e2b) env =
      go e1a e2a env && go e1b e2b env
    go (IfZero c1 t1 e1) (IfZero c2 t2 e2) env =
      go c1 c2 env && go t1 t2 env && go e1 e2 env
    go _ _ _ = False

-- 9. Collect all variable names (duplicates allowed)
allVars :: Expr -> [String]
allVars (Var x) = [x]
allVars (Lit _) = []
allVars (Lam x body) = x : allVars body
allVars (App f arg) = allVars f ++ allVars arg
allVars (Add e1 e2) = allVars e1 ++ allVars e2
allVars (IfZero c t e) = allVars c ++ allVars t ++ allVars e

-- 10. Beta-reduce once at root (if possible), with capture-avoiding substitution
reduceOnce :: Expr -> Maybe Expr
reduceOnce (App (Lam x body) arg) = Just (subst x arg body)
reduceOnce (Add (Lit n1) (Lit n2)) = Just (Lit (n1 + n2))
reduceOnce (IfZero (Lit 0) t _) = Just t
reduceOnce (IfZero (Lit _) _ e) = Just e
reduceOnce _ = Nothing

-- Exported functions for TH splice testing
func001 :: Int -> Int
func001 x = countNodes (App (Lam "y" (Add (Var "y") (Lit x))) (Lit 10))

func002 :: Int -> Int
func002 x = length $ freeVars (Lam "z" (Add (Var "z") (Var (show x))))

func003 :: Int -> Int
func003 x = depth (IfZero (Lit x) (Lit 1) (App (Var "f") (Var "g")))

func004 :: Int -> Int
func004 x = case eval (Add (Lit x) (Lit 5)) of
  Lit n -> n
  _ -> 0

func005 :: Int -> Int
func005 x = length (allVars (Lam "a" (Lam "b" (App (Var "a") (Lit x)))))

func006 :: Int -> Int
func006 x = if isClosed (Lam "n" (Add (Var "n") (Lit x))) then 1 else 0

func007 :: Int -> Int
func007 x = length (pretty (App (Var "f") (Lit x)))

func008 :: Int -> Int
func008 x = if alphaEq (Var "x") (Var "x") then x else 0

func009 :: Int -> Int
func009 x = case reduceOnce (App (Lam "n" (Var "n")) (Lit x)) of
  Just (Lit n) -> n
  _ -> -1

func010 :: Int -> Int
func010 x = maximum [0, depth (Lit x), countNodes (Lit x)]
