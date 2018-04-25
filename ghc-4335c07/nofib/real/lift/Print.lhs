> module Print where


> import LambdaLift
> import Utilities

This section gives a small pretty-printer for the language.

> pprint :: (binder -> Iseq) -> Expr binder -> [Char]
> pprint pb e = i_mkstr (ppr pb e)

pprintExpr prints expressions whose binder is just a name

> pprintExpr = pprint i_str

pprintLevel prints expressions whose binder is an (Expr, Level) pair.

> pprintLevel :: (Expr ([Char], Integer)) -> [Char]
>
> pprintLevel = pprint (\(name,level) -> i_concat [ i_str name, i_str "{",
>						    i_num level, i_str "}" ])

Now we have to print supercombinators too:

> pprintSCs scs = i_mkstr (i_concat (map ppsc scs))
> ppsc (name, args, rhs) = 
>	i_concat [ i_newline, i_str name, i_str " ",
>		   i_interleave (i_str " ") (map i_str args),
>		   i_str " = ",
>		   i_indent 6 (ppr i_str rhs) ]

The function @ppr@ does most of the work.

> ppr pb (EAp e1 e2) = i_concat [ppr pb e1, i_space, ppr_atomic pb e2]
> ppr pb (ELet isrec defs e) =
>   i_concat [	i_str keyword, i_newline,
>		i_indent 4 (i_interleave (i_str ";\n") (map (ppr_def pb) defs)),
>		i_str "\nin ", 
>		ppr pb e
>   ]
>   where
>   keyword | isrec     = "letrec"
>	    | not isrec = "let"
> ppr pb (ELam args body) =
>   i_concat [ i_str "\\[", 
>	       i_interleave (i_str ",") (map pb args),
>	       i_str "] ", ppr pb body ]
>
> ppr pb e = ppr_atomic pb e
>
> ppr_atomic pb (EConst (CNum n)) 	= i_num n
> ppr_atomic pb (EConst (CFun name))	= i_str name
> ppr_atomic pb (EConst (CBool b))	= i_str (show b)
> ppr_atomic pb (EVar v) 		= i_str v
> ppr_atomic pb e = i_concat [i_str "(", ppr pb e, i_str ")"]

@ppr_def@ constructs the @iseq@ for a definition.  The only
complication is that the arguments may be annotated.

> ppr_def pb (binder, (ELam args body)) =
>   i_concat [	pb binder, i_space,
>		i_interleave i_space (map pb args),
>		i_str " = ",
>		ppr pb body
>   ]
> ppr_def pb (binder, rhs) = i_concat [pb binder, i_str " = ", ppr pb rhs]
>

%> example =
%>   ELet Recursive 
%>	[("f", ["x","y"], EAnnot (AnnotArgs [Strict, NonStrict]) (EVar "y")),
%>	 ("g", ["x","y"], EAnnot (AnnotArgs [Strict, Strict]) (EVar "y"))]
%>	(EVar "f")

\section{A pretty-printing data type}
\label{pretty-type}

\section{Specification}

The type @iseq@ can be thought of as much like a list of characters;
the empty @iseq@ is @i_nil@, and @iseq@s can be concatenated with
@i_append@.  A list of @iseq@s can be concatenated into a single
@iseq@ using @i_concat@.
You can convert a string to an @iseq@ with @i_str@, a number into an @iseq@
with @i_num@, and an @iseq@ to
a string with @i_mkstr@.

What makes @iseq@s different from strings is one operation, @i_indent@,
and a performance property:
\begin{itemize}
\item
The performance property is that, unlike @++@, @i_append@ works in
constant time.
\item
The @i_indent@ operation takes a number $n$ and an @iseq@ and returns an
@iseq@ which you can think of as a string in which every newline
has been replaced with a newline followed by $n$ spaces.
Like @i_append@ though, @i_indent@ takes constant time.
\end{itemize}

> type Iseq = Oseq -> Oseq

The following functions can be implemented in terms of the fundamental ones
above.
@i_concat@ concatenates together the members of a list of @Iseq@s.

> i_concat :: [Iseq] -> Iseq
> i_concat = foldr i_append i_nil

@i_interleave@ is like @i_concat@, except that it interleaves a given
@iseq@ between successive elements:

> i_interleave :: Iseq -> [Iseq] -> Iseq
> i_interleave is []  = i_nil
> i_interleave is iss = foldr1 glue iss
>		    	where glue is1 is2 = is1 `i_append` (is `i_append` is2)
>                             foldr1 f [x] = x
>			      foldr1 f (x:xs) = f x (foldr1 f xs)

Finally @i_num@ converts an integer  to an @Iseq@.

> i_num :: (Show a, Num a) => a -> Iseq
> i_num = i_str . show

> i_newline 	= i_str "\n"
> i_space 	= i_str " "


An @Oseq@ is a function whose arguments are a number giving the
desired indentation and a boolean saying whether this indentation is
required right at the beginning of the result, and delivers as
result a character string indented by the appropriate amount.

> type Oseq = Int -> Bool -> [Char]

Here are some useful defintions using @oseq@:

> o_empty :: Oseq              -- An empty oseq
> o_empty indent npend = []
>
> o_mkstr :: Oseq -> [Char]
> o_mkstr oseq = oseq 0 False

An @Iseq@ is just like an @Oseq@, except that it takes an @oseq@ as an
argument, which it appends to its result.


This definition of @Oseq@ leads directly to the following definitions:

> i_nil x = x
> i_append = (.)
> i_str = foldr (i_append . i_char) i_nil
> i_mkstr iseq = o_mkstr (iseq o_empty)

The definition of @i_char@, which turns a character into an @Iseq@ 
contains most of the subtlety.  If the character is a newline, return
a newline, but set the pending-indent flag to @True@;
if it isn't a newline, and the pending-indent flag is set, then
spit out the spaces for the indent and reset the flag; otherwise just
return the character:

> i_char :: Char -> Iseq
> i_char '\n' rest indent npend = '\n' : rest indent True
> i_char c    rest indent False	= c    : rest indent False
> i_char c    rest indent True	= pspaces indent (c : rest indent False)

The @i_indent@ function manipulates the @indent@ arguments of its
component @Oseq@s:

> i_indent n iseq oseq indent npend =
>   iseq oseq' (indent+n) npend
>   where 
>   oseq' indent' npend' = oseq indent npend'
>   -- Ignore the indent passed along to oseq; 
>   -- use the original indent instead.

@spaces@ is just an auxilliary function:

> pspaces 0 cs = cs
> pspaces n cs = ' ' : pspaces (n-1) cs

