[converted to Haskell by partain]

\begin{verbatim}
Date:    Tue, 23 Apr 91 16:49:16 +0000
From:    Mike.Spivey@prg.oxford.ac.uk
To:      simonpj
Subject: Orwell benchmark
\end{verbatim}


Here is a benchmark written in Orwell.  Category notes are also on
their way by GPO.

Quentin Miller's Orwell interpreter claims to take about 128 secs to
reduce "result" to true, doing 250 000 reductions.  Quentin may
implement an Orwell --> LML translator soon, so he may have a better
figure before long.

- -- Mike

- ----8<--------8<--------8<--------8<--------8<--------8<--------8<----

EQUATIONAL REWRITING BENCHMARK

Mike Spivey,
Programming Research Group,
University of Oxford.

mike@@prg.oxford.ac.uk

St. George's Day, 1991.

- -*-orwell-*-

This Orwell program defines "result" so that it evaluates to True, by
completing the axioms

        (a * b) * c = a * (b * c)
        E * a = a
        I(a) * a = E

into a decision procedure for free groups, then proving the equation

        I(a * b) = I(b) * I(a).

The program makes heavy use of the "maybe" exception type, and various
sorts of higher-order functions.  The inner loop is in the
implementation of rewriting.

- ----8<--------8<--------8<--------8<--------8<--------8<--------8<----

> import System.Environment (getArgs)

> infixr 1             ??
> infixr 1             |||
> infixr 1             `cross`
> infixr 2             -=>
> infixr 2             ##

EXCEPTIONS

> --1.3:data Maybe a = Just a | Nothing

> succeed x             = Just x
> croak                 = Nothing

> exists Nothing        = False
> exists x              = True

> the (Just x)          = x

> Nothing ||| y          = y
> x ||| y                = x

> (Just x) ?? y         = x
> Nothing ?? y          = y

> (##) p q s            = p s ||| q s

> p -=> x               = if p then Just x else Nothing

> first_ok              = foldr (|||) Nothing
> sift                  = concat . map listify

> listify x             = lift box x ?? []
> box x                 = [x]

> lift f Nothing        = Nothing
> lift f (Just x)       = Just (f x)

> prop :: (a -> Maybe b) -> (Maybe a -> Maybe b)
> prop f Nothing = Nothing
> prop f (Just x) = f x

> prop2 :: (a -> b -> Maybe c) -> (Maybe a -> b -> Maybe c)
> prop2 f Nothing y = Nothing
> prop2 f (Just x) y = f x y

> prop_fold :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
> prop_fold f x [] = Just x
> prop_fold f x (y:ys) = prop2 (prop_fold f) (f x y) ys

> squash                :: Maybe (Maybe a) -> Maybe a
> squash (Just x)       = x
> squash Nothing        = Nothing

> pair x y              = (x, y)

> cross              :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
> cross f g (x,y)     = (f x, g y)

EXPRESSIONS

> data EXPR = Func FUNC [EXPR] | Var VAR
>	-- deriving Eq
>
> instance Eq EXPR where 
>   (Func f1 es1) == (Func f2 es2) = f1 == f2 && es1 == es2
>   (Var v1) == (Var v2) = v1 == v2
>   other1 == other2 = False
>
> type FUNC	= String
> type VAR	= String
> type NUM	= Int

> type EXPR_PAIR = (EXPR, EXPR)
> data EQUATION = Eqn NUM EXPR_PAIR

> lhs (Eqn n (a, b)) = a
> rhs (Eqn n (a, b)) = b
> eqno (Eqn n (a, b)) = n
> eqpr (Eqn n pr) = pr

> cost :: (EXPR, EXPR) -> NUM
> cost (a, b) = size a + size b
>  where
>   size = expr_fold f (const 0)
>        where f x a = sum a + 1

> expr_fold :: (FUNC -> [a] -> a) -> (VAR -> a) -> EXPR -> a
> expr_fold ff vv (Var v) = vv v
> expr_fold ff vv (Func f a) = ff f (map (expr_fold ff vv) a)


> occurs :: VAR -> EXPR -> Bool
> occurs v = expr_fold (const or) (== v)


PARSING

> type PARSER	= String -> Maybe (SYNVAL, String)
> data SYNVAL	= Expr EXPR | List [SYNVAL] | MkString String

> unExpr (Expr e)       = e
> unString (MkString s)   = s

> parse                 = unExpr . fst . the . p_expr
> parse_eqn s = (lhs, rhs)
>               where List [Expr lhs, Expr rhs] = fst (the (p_eqn s))

Some parsing tools:

> build                 :: (SYNVAL -> SYNVAL) -> (PARSER -> PARSER)
> build f p             = lift (f `cross` id) . p

> seq2                  :: (SYNVAL -> SYNVAL -> SYNVAL)
>                               -> (PARSER -> PARSER -> PARSER)
> seq2 f p q            = squash . lift g . p
>                         where g (x, s) = lift (f x `cross` id) (q s)

> seQ                   :: ([SYNVAL] -> SYNVAL) -> ([PARSER] -> PARSER)
> seQ f ps              = build (mk_list f) (foldr (seq2 mk_cons) empty ps)
> mk_cons x (List xs)   = List (x:xs)
> mk_list f (List xs)   = f xs

> empty                 :: PARSER
> empty s               = succeed (List [], s)

> list_of               :: PARSER -> Char -> PARSER
> list_of p sep         = p'
>                         where p' = seq2 mk_cons p (seq2 sel2
>                                        (look_for sep) p' ## empty)
>                               sel2 x y = y

> sp p                  = p . dropWhile (== ' ')

> look_for c            = sp (pchar (== c))

> pchar p []            = croak
> pchar p (c:s)         = if p c then succeed (MkString [c], s) else croak

> string_of kind s = (chars /= "") -=> (MkString chars, s')
>                    where chars = takeWhile kind s
>                          s'    = dropWhile kind s

The parser itself:

> p_eqn = seQ q_eqn [p_expr, look_for '=', p_expr]
> q_eqn [lhs, eq, rhs] = List [lhs, rhs]

> p_expr = seQ q_op [p_term, p_op, p_term] ## p_term
> q_op [Expr a, MkString op, Expr b] = Expr (Func op [a, b])

> p_term = seQ q_func [p_ident, look_for '(', 
>                       list_of p_expr ',', look_for ')'] ## p_prim
> q_func [MkString fun, lb, List args, rb] = Expr (Func fun (map unExpr args))

> p_prim = p_name ## seQ (!! 1) [look_for '(', p_expr, look_for ')']

> p_name = build q_name p_ident
> q_name (MkString s) = if s!!0 >= 'a' && s!!0 <= 'z' then Expr (Var s)
>			else Expr (Func s [])

> p_ident = sp (string_of alphanum)

> p_op = sp (string_of opsym)

> opsym = (`elem` "*+%@-/?:")

> alphanum c = (c >= 'A' && c <= 'Z') 
>              || (c >= 'a' && c <= 'z') 
>              || (c >= '0' && c <= '9')

SUBSTITUTIONS

> type SUBST = [(VAR, EXPR)]

> assoc :: (Eq a) => [(a, b)] -> a -> Maybe b
> assoc alist x = first_ok (map match_x alist)
>                 where match_x (u, v) = (x == u) -=> v

> update_alist :: Eq a => a -> (b -> b) -> b -> [(a, b)] -> [(a, b)]
> update_alist x f defalt
>       = upd
>         where upd [] = [(x, f defalt)]
>               upd ((y, z):alist) = if x == y then (x, f z) : alist
>                                    else (y, z):(upd alist)

> apply :: SUBST -> VAR -> EXPR
> apply s v = assoc s v ?? Var v

> sub :: EXPR -> SUBST -> EXPR
> sub t s = expr_fold Func (apply s) t

> comp_sub s1 s2 = [(v, sub t s1) | (v, t) <- s2] ++ s1

> stand :: String -> EXPR -> EXPR
> stand suffix = expr_fold Func f
>                where f v = Var (v ++ suffix)

> stand_eqn :: String -> EQUATION -> EQUATION
> stand_eqn suffix (Eqn n (lhs, rhs)) = Eqn n (stand suffix lhs,
>                                              stand suffix rhs)


SUBTERMS and REPLACEMENT

> type PATH = [NUM]

> subterms :: EXPR -> [(PATH, EXPR)]
> subterms (Var v) = []
> subterms (Func f a) 
>       = [([], Func f a)] 
>         ++ [(i:k, u) | (i, t) <- ([0..] `zip` a), (k, u) <- subterms t]

> replace :: EXPR -> PATH -> EXPR -> EXPR
> replace t [] u = u
> replace (Func f a) (i:k) u = Func f (modify a i replace')
>                              where replace' t = replace t k u

> modify (x:a) 0 f = (f x) : a
> modify (x:a) i f = x : (modify a (i-1) f)


MATCHING

> match :: EXPR -> EXPR -> Maybe SUBST
> match p t = match' [] (p, t)

> match' s (Var v, t) 
>       = if not (exists u) then succeed ((v, t):s)
>         else if the u == t then succeed s
>         else croak
>         where u = assoc s v
> match' s (Func f a, Func g b)
>       = if f == g then prop_fold match' s (a `zip` b) else croak
> match' s (Func f a, Var v) = croak


REWRITING

> type TACTIC = EXPR -> Maybe EXPR

> rewrite :: EQUATION -> TACTIC
> rewrite (Eqn n (l, r)) = lift (sub r) . match l

> try_all :: [TACTIC] -> TACTIC
> try_all = foldr (##) (const croak)

> inside :: TACTIC -> TACTIC
> inside rw t = first_ok [ lift (replace t k) (rw u) 
>                          | (k, u) <- subterms t ]

> reduce1 eqn = inside (rewrite eqn)
> reduce eqns = inside (try_all (map rewrite eqns))


UNIFICATION

> unify t u = unify' [] (t, u)

> unify' s (Var v, u) = univar s v u
> unify' s (Func f a, Var v) = univar s v (Func f a)
> unify' s (Func f a, Func g b)
>       = if f == g then prop_fold unify' s (a `zip` b) else croak

> univar s v u = if exists t then unify' s (the t, u)
>                else if u == Var v then succeed s
>                else if not ( occurs v u') then succeed (comp_sub [(v, u')] s)
>                else croak
>                where t = assoc s v
>                      u' = sub u s


SIMPLIFICATION

> simplify :: TACTIC -> EXPR -> EXPR
> simplify rw = f where f t = lift f (rw t) ?? t

DISCRIMINATION NETS

Discrimination nets use matching on linearized prefix strings:

> data TOKEN = Afunc FUNC | Avar

> g_init exp = [exp]

> g_first (Func f a : z) = Afunc f
> g_first (Var v : z) = Avar

> g_rest (Func f a : z) = a ++ z
> g_rest (Var v : z) = z

> g_skip = tail

> preorder z = if z == [] then []
>              else [g_first z] ++ preorder (g_rest z)

A discrimination net is either a switch on function symbol (with a
default to use for variables) or a return of an equation list:

> data DISC_NET a = Switch [(FUNC, DISC_NET a)] (DISC_NET a) | Return a
> type DNET = DISC_NET [EQUATION]

> empty_net = Return []

> is_switch (Switch alist defalt) = True
> is_switch (Return eqns) = False

> find :: EXPR -> DNET -> [EQUATION]
> find exp d = find' (g_init exp) d

> find' z (Return eqns) = eqns
> find' z (Switch alist defalt)
>       = find_assoc (g_first z) (g_rest z) alist ++ find' (g_skip z) defalt

> find_assoc (Afunc f) z alist
>       = lift (find' z) (assoc alist f) ?? []
> find_assoc Avar z alist = []

> mk_dnet :: [EQUATION] -> DNET
> mk_dnet = foldl add_eqn empty_net

> add_eqn :: DNET -> EQUATION -> DNET
> add_eqn d eqn = thread (preorder (g_init (lhs eqn))) eqn d

> thread :: [TOKEN] -> EQUATION -> DNET -> DNET
> thread (Afunc f : z) e (Switch alist defalt)
>       = Switch (update_alist f (thread z e) empty_net alist) defalt
> thread (Avar : z) e (Switch alist defalt)
>       = Switch alist (thread z e defalt)
> thread [] e (Return eqns)
>       = Return (e : eqns)
> thread (Afunc f : z) e (Return eqns)
>       = thread (Afunc f : z) e (Switch [] (Return eqns))
> thread (Avar : z) e (Return eqns)
>       = if is_switch d' then Switch [] d'
>         else  d'
>         where d' = thread z e (Return eqns)

> map_dnet :: (a -> b) -> DISC_NET a -> DISC_NET b
> map_dnet f (Switch alist defalt) 
>       = Switch (map (id `cross` map_dnet f) alist) (map_dnet f defalt)
> map_dnet f (Return x) = Return (f x)

> delete_eqns :: [NUM] -> DNET -> DNET
> delete_eqns nums 
>       = map_dnet (filter ok)
>         where ok e = not ((eqno e) `elem` nums)

> skeleton :: DNET -> DISC_NET [NUM]
> skeleton = map_dnet (map eqno)

> super_reduce d = inside (dnet_reduce d)
> dnet_reduce d t = try_all (map rewrite (find t d)) t

RECURSIVE PATH ORDERING

> data ANSWER = Equal | Greater | Less | Unrelated
>	-- deriving Eq
>
> instance Eq ANSWER where 
>   Equal	== Equal	= True
>   Greater	== Greater	= True
>   Less	== Less		= True
>   Unrelated	== Unrelated	= True
>   other1	== other2	= False

> type ORDERING a   = (a, a) -> ANSWER
> type PRED a	    = a -> Bool
> type RELATION a b = a -> b -> Bool

> eq ord x y = (ord (x, y) == Equal)
> gt ord x y = (ord (x, y) == Greater)
> lt ord x y = (ord (x, y) == Less)
> ge ord x y = (ord (x, y) `elem` [Greater, Equal])

> num_order :: ORDERING NUM
> num_order (i, j) = if i > j then Greater
>                    else if i < j then Less
>                    else Equal

> rank_order :: (a -> NUM) -> ORDERING a
> rank_order rank (x, y) = num_order (rank x, rank y)

> quant :: (PRED a -> PRED [a]) -> RELATION a b -> RELATION b [a]
> quant q rel y xs = q p xs
>                    where p x = rel x y

> delete :: RELATION a b -> a -> [b] -> Maybe [b]
> delete eq x [] = croak
> delete eq x (y:ys) = if eq x y then succeed ys
>                      else lift (y:) (delete eq x ys)

> rem_eq :: RELATION a b -> ([a], [b]) -> ([a], [b])
> rem_eq eq ([], ys) = ([], ys)
> rem_eq eq (x:xs, ys) 
>       = lift (rem_eq eq . pair xs) (delete eq x ys)
>         ?? add_x (rem_eq eq (xs, ys))
>         where add_x (xs', ys') = (x:xs', ys')

> dominates :: ORDERING a -> RELATION [a] [a]
> dominates ord = quant all (quant some (gt ord))

> multi :: ORDERING a -> ORDERING [a]
> multi ord (a, b) = if dominates ord a' b' then Greater
>                    else if dominates ord b' a' then Less
>                    else Unrelated
>                    where (a', b') = rem_eq (eq ord) (a, b)

> lexico :: ORDERING a -> ORDERING [a]
> lexico ord ([], []) = Equal
> lexico ord (x:a, []) = Greater
> lexico ord ([], y:b) = Less
> lexico ord (x:a, y:b) = lex_combine (ord (x, y)) (lexico ord (a, b))

> lex_combine Equal o = o
> lex_combine Greater o = Greater
> lex_combine Less o = Less
> lex_combine Unrelated o = Unrelated

> type EXTENSION = ORDERING EXPR -> ORDERING EXPR

> multi_ext :: EXTENSION
> multi_ext ord (Func f a, Func g b) = multi ord (a, b)

> lex_ext :: EXTENSION
> lex_ext ord (Func f a, Func g b)
>   = if confirm estimate then estimate
>     else if quant some (ge ord) (Func g b) a then Greater
>     else if quant some (ge ord) (Func f a) b then Less
>     else Unrelated
>     where estimate = lexico ord (a, b)
>           confirm Equal     = True
>           confirm Greater   = quant all (lt ord) (Func f a) b
>           confirm Less      = quant all (lt ord) (Func g b) a
>           confirm Unrelated = False

> rpo :: ORDERING FUNC -> EXTENSION -> ORDERING EXPR
> rpo ord ext = rpo'
>  where rpo' (Var x, Var y)    = if x == y then Equal else Unrelated
>        rpo' (Var x, Func f a) = if occurs x (Func f a) then Less else Unrelated
>        rpo' (Func f a, Var x) = if occurs x (Func f a) then Greater else Unrelated
>        rpo' (Func f a, Func g b) = rcf (ord (f, g))
>         where rcf Equal     = ext (rpo') (Func f a, Func g b)
>               rcf Greater   = if quant all (lt rpo') (Func f a) b then Greater
>                             else if quant some (ge rpo') (Func f a) b then Less
>                             else Unrelated
>               rcf Less      = if quant all (lt rpo') (Func g b) a then Less
>                             else if quant some (ge rpo') (Func g b) a then Greater
>                             else Unrelated
>               rcf Unrelated = Unrelated

AGENDA

> type CRIT_PAIR    = EXPR_PAIR
> data ITEM	    = Item NUM CRIT_PAIR
> type AGENDA	    = [ITEM]

> item_cost (Item c e) = c

> mk_item :: (EXPR_PAIR -> NUM) -> CRIT_PAIR -> ITEM
> mk_item cfun eqn = Item (cfun eqn) eqn

> addby :: (a -> NUM) -> [a] -> [a] -> [a]
> addby f xs ys 
>       = foldr insert ys xs
>         where insert x []     = [x]
>               insert x (y:ys) = if f x <= f y then x:y:ys else y:insert x ys

> add_agenda :: (EXPR_PAIR -> NUM) -> [CRIT_PAIR] -> AGENDA -> AGENDA
> add_agenda cfun eqns = addby item_cost (map (mk_item cfun) eqns)


SUPERPOSITION

> superpose :: EXPR -> EXPR -> [(PATH, SUBST)]
> superpose t u = sift [ lift (pair k) (unify t w) | (k, w) <- subterms u ]

> strict_super :: EXPR -> EXPR -> [(PATH, SUBST)]
> strict_super t (Func f a)
>       = [ (i:k, s) | (i, u) <- ([0..] `zip` a), (k, s) <- superpose t u ]

> mk_crit :: EQUATION -> EQUATION -> (PATH, SUBST) -> CRIT_PAIR
> mk_crit (Eqn n1 (l1, r1)) (Eqn n2 (l2, r2)) (k, s)
>       = (sub (replace l2 k r1) s, sub r2 s)

> crit_pairs :: EQUATION -> EQUATION -> [CRIT_PAIR]
> crit_pairs e1 e2 = map (mk_crit e1 e2) (superpose (lhs e1) (lhs e2))
>                    ++ map (mk_crit e2 e1) (strict_super (lhs e2) (lhs e1))

> all_crit_pairs :: EQUATION -> [EQUATION] -> [CRIT_PAIR]
> all_crit_pairs eqn theory 
>       = map (mk_crit eqn' eqn'') (strict_super (lhs eqn') (lhs eqn''))
>         ++ concat (map (crit_pairs eqn') theory'')
>         where eqn' = stand_eqn "1" eqn
>               eqn'' = stand_eqn "2" eqn
>               theory'' = map (stand_eqn "2") theory


KNUTH-BENDIX COMPLETION

> type KB_DATA = (ORDERING EXPR, EXPR_PAIR -> NUM)

> knuth_bendix :: ORDERING EXPR -> [EXPR_PAIR] -> DNET
> knuth_bendix ord axs = knuth_bendix1 (ord, cost) axs

> knuth_bendix1 datums axs
>       = process datums 1 [] empty_net todo
>         where todo = add_agenda (snd datums) axs []

> process :: KB_DATA -> NUM -> [EQUATION] -> DNET -> AGENDA -> DNET
> process datums n thy net [] = net
> process datums n thy net (Item w eqn : todo)
>       = process1 datums n thy net todo (preprocess net eqn)

> preprocess :: DNET -> CRIT_PAIR -> CRIT_PAIR
> preprocess net (lhs, rhs)
>       = (simplify (super_reduce net) lhs,
>          simplify (super_reduce net) rhs)

> process1 :: KB_DATA -> NUM -> [EQUATION] -> DNET -> AGENDA 
>                                               -> CRIT_PAIR -> DNET
> process1 datums n thy net todo (lhs, rhs)
>       = case o of
>	    Unrelated -> process datums n thy net (add_agenda (const 1000) [(lhs, rhs)] todo)
>	    Equal ->     process datums n thy net todo
>           _ ->	 process2 datums (n+1) thy net todo eqn
>         where o = (fst datums) (lhs, rhs)
>               eqn = case o of Greater -> Eqn n (lhs, rhs)
>                               Less    -> Eqn n (rhs, lhs)
>		                _ ->  error "eqn:not Greater or Less\n"

> process2 :: KB_DATA -> NUM -> [EQUATION] -> DNET -> AGENDA
>                                               -> EQUATION -> DNET
> process2 datums n thy net todo new_rule
>       = process datums n (thy' ++ [new_rule]) net' todo'
>         where (deleted, thy') = split (reducible (reduce1 new_rule)) thy
>               net' = add_eqn (delete_eqns (map eqno deleted) net) new_rule
>               deletions = map eqpr deleted
>               critical = all_crit_pairs new_rule thy'
>               new_agenda = sift (map (resolve (super_reduce net')) 
>                                               (deletions ++ critical))
>               todo' = add_agenda (snd datums) new_agenda todo
        
> resolve :: TACTIC -> CRIT_PAIR -> Maybe CRIT_PAIR
> resolve tac pr
>       = if lhs' == rhs' then croak
>         else succeed (lhs', rhs')
>         where lhs' = simplify tac (fst pr)
>               rhs' = simplify tac (snd pr)

> reducible :: TACTIC -> EQUATION -> Bool
> reducible tac (Eqn n (lhs, rhs)) = exists (tac lhs) || exists (tac rhs)

> some = any

> split :: (a -> Bool) -> [a] -> ([a], [a])
> split p xs = ([ x | (b, x) <- tmp, b ], [ x | (b, x) <- tmp, not(b) ])
>              where tmp = (map p xs `zip` xs)

BENCHMARK

> group_rules = map parse_eqn [ 
>               "(a * b) * c = a * (b * c)", 
>               "E * x = x", 
>               "I(x) * x = E" ]

> rank "E" = 1
> rank "*" = 2
> rank "I" = 3

> group_order = rpo (rank_order rank) lex_ext

> group_completion = knuth_bendix group_order group_rules

> result (s1, s2) = (simplify (super_reduce group_completion) (parse s1)
>            == parse s2)

> test n = all result xs
>  where xs = take n (repeat ("I(a * b)", "I(b) * I(a)"))
>	 {-# NOINLINE xs #-}

> main = do
>   (n:_) <- getArgs
>   print (test (read n :: Int))
