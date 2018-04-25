% Config.lhs - configuration constants for the HPG

% @(#)Config.lhs	1.20 dated 92/07/20 at 17:30:48

% Crown Copyright 1991

\section{Configuration constants}

This module implements constants, such as the maximum size of a tuple,
and strings used in printing programs, which one might wish to change
occasionally.
They therefore live in a separate module so that they can be changed with
the minimum of recompilation.
\begin{haskell}

> module Config (
>     maxchar, max_list_len, max_tuple_len, max_array_len, max_vnts,
>     max_flds,
>     line_len, one, apply_prec,
>     constructor_name, type_name, val_name,
>     and_name, div_name, drop_name, eq_name, int_to_char_name, less_name,
>     minus_name, mult_name, negate_name, not_name,
>     or_name, plus_name, print_name, take_name, assoc_name,
>     array_type_name, bool_type_name, char_type_name, double_type_name,
>     int_type_name, integer_type_name, float_type_name,
>     list_separator, field_separator, empty_string, val_def, type_def,
>     union,
>     lbrack, rbrack, lbrace, rbrace, lsq, rsq, space, newline,
>     data_name, mod_name, main_name, where_name, decl_sep, derive_eq,
>     lambda_name, map_name, array_name,
>     file_ext,
>     version
>     ) where

\end{haskell}

\prog{maxchar} is the largest integer code in the implementation.
\begin{haskell}

> maxchar ::  Int
> maxchar  =  127

\end{haskell}

\prog{max\_list\_len} is the length of the longest list value that
will be generated.
This might be better as a parameter to the generator.
\begin{haskell}

> max_list_len ::  Int
> max_list_len  =  5

\end{haskell}

\prog{max\_tuple\_len} is the maximum number of elements in any
tuple that will be generated.
\begin{haskell}

> max_tuple_len ::  Int
> max_tuple_len  =  4

\end{haskell}

\prog{max\_array\_len} is the maximum number of elements in any
array that will be generated.
\begin{haskell}

> max_array_len ::  Int
> max_array_len  =  5

\end{haskell}

\prog{max\_vnts} is the maximum number of variants in each type declaration.
\begin{haskell}

> max_vnts ::  Int
> max_vnts  =  6

\end{haskell}

\prog{max\_flds} is the maximum number of fields in each variant.
\begin{haskell}

> max_flds ::  Int
> max_flds  =  4

\end{haskell}

\prog{line\_len} is an approximate line length for the \prog{split\_str}
function ---
lines are broken at the first space following the \prog{line\_len}th
character.
\begin{haskell}

> line_len ::  Int
> line_len  =  70

\end{haskell}

\prog{one} is the \prog{Int} 1.
The prototype Glasgow compiler has difficulty resolving overloading and
often generates incorrect code for constant arguments to overloaded
functions (such as addition, which is defined for \prog{Int} and \prog{Integer}
arguments).
Using \prog{one} gives the compiler sufficient information to overcome its
problems.
\begin{haskell}

> one :: Int
> one  =  1

\end{haskell}

\prog{apply\_prec} is the precedence of the apply operation; this is used
in printing programs.
\begin{haskell}

> apply_prec :: Int
> apply_prec  =  10

\end{haskell}

\subsection{Strings}
Nearly all literal strings used in the \HPG\ are defined in this section.
This allows them to be changed with minimal effort.

\prog{constructor\_name}, \prog{type\_name} and \prog{val\_name} are the
strings used to construct \prog{Constructor}s, \prog{Type\_name}s and
\prog{Val\_name}s respectively.
\begin{haskell}

> constructor_name, type_name, val_name :: String
> constructor_name  =  "Cons"
> type_name         =  "Type"
> val_name          =  "val"

\end{haskell}

The following definitions are various built-in operator and function names
used in generating expressions.
\begin{haskell}

> and_name, div_name, drop_name, eq_name, int_to_char_name, less_name,
>     minus_name, mult_name, negate_name, not_name, or_name, plus_name,
>     print_name, take_name, assoc_name :: String
> and_name          =  "&&"
> div_name          =  "div"
> drop_name         =  "drop"
> eq_name           =  "=="
> int_to_char_name  =  "chr"
> less_name         =  "<"
> minus_name        =  "-"
> mult_name         =  "*"
> negate_name       =  "negate"
> not_name          =  "not"
> or_name           =  "||"
> plus_name         =  "+"
> print_name        =  "print"
> take_name         =  "take"
> assoc_name        =  ":="

\end{haskell}

The next collection of definitions are used in printing types, values and
expressions.
\begin{haskell}

> array_type_name, bool_type_name, char_type_name, double_type_name,
>     int_type_name, integer_type_name, float_type_name :: ShowS
> array_type_name    =  showString "Array"
> bool_type_name     =  showString "Bool"
> char_type_name     =  showString "Char"
> double_type_name   =  showString "Double"
> int_type_name      =  showString "Int"
> integer_type_name  =  showString "Integer"
> float_type_name    =  showString "Float"

\end{haskell}
\begin{haskell}

> list_separator, field_separator, empty_string, val_def, type_def,
>     union :: ShowS
> list_separator   =  showString ", "
> field_separator  =  showString " "
> empty_string     =  showString ""
> val_def          =  showString " = "
> type_def         =  showString " = "
> union            =  showString " | "

\end{haskell}
\begin{haskell}

> lbrack, rbrack, lbrace, rbrace, lsq, rsq, space, newline :: ShowS
> lbrack   =  showChar '('
> rbrack   =  showChar ')'
> lbrace   =  showChar '{'
> rbrace   =  showChar '}'
> lsq      =  showChar '['
> rsq      =  showChar ']'
> space    =  showChar ' '
> newline  =  showChar '\n'

\end{haskell}

The following definitions are used in printing programs.
\begin{haskell}

> data_name, mod_name, main_name, decl_sep, derive_eq, lambda_name,
>     map_name, array_name :: ShowS
> data_name    =  showString "data"
> mod_name     =  showString "module"
> main_name    =  showString "main"
> where_name   =  showString "where"
> decl_sep     =  showString ";"
> derive_eq    =  showString "deriving (Eq)"
> lambda_name  =  showString "\\"
> map_name     =  showString "->"
> array_name   =  showString "array"

\end{haskell}
\begin{haskell}

> file_ext :: String
> file_ext  =  ".hs"

\end{haskell}

The version number of the \HPG\ is given by the string \prog{version},
which is set by the SCCS revision control system.
\begin{haskell}

> version :: ShowS
> version  =  showString "1.20 dated 92/07/20"

\end{haskell}
