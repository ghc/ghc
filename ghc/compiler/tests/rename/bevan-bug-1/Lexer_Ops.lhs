  $Id: Lexer_Ops.lhs,v 1.1 1996/01/08 20:17:58 partain Exp $

>module Lexer_Ops where

>import Lexer_Buffer(Lexer_Buffer, add, empty, flush, len, to_string)
>  renaming
>  (add to add_char, empty to empty_buffer, to_string to buffer_to_string)

>import Lexer_Combinators(and_with, return)

>import Lexer_State
>  (Lexer_State,Lexer_Action(..),i_buffer,i_source_pos,p_buffer,p_source_pos)

>import Oberon_Id(Oberon_Id,from_string) renaming (from_string to string_to_id)

>import Oberon_Integer
>  (Oberon_Integer, from_decimal_string, from_hex_string, from_int)
>  renaming
>  ( from_decimal_string to decimal_string_to_int
>  , from_hex_string to hex_string_to_int
>  )

>import Oberon_Real(Oberon_Real, from_string) renaming 
>  (from_string to string_to_real)

>import Oberon_String(Oberon_String, from_string) renaming
>  (from_string to string_to_string)

>import Lexer_Combinators(and_then)

>import Source_Position(Source_Position, next_line, shift_column, start)
>  renaming (start to start_position)

>t_source_pos transformer =
>  p_source_pos `and_with` \pos ->
>  i_source_pos (transformer pos)

>t_buffer transformer =
>  p_buffer `and_with` \buff ->
>  i_buffer (transformer buff)


>buffer_len :: Lexer_Action Int
>buffer_len =
>  p_buffer `and_with` \buff ->
>  return (len buff)


>decimal_to_int :: Lexer_Action Oberon_Integer
>decimal_to_int =
>  p_buffer `and_with` \buff ->
>  return (decimal_string_to_int (buffer_to_string buff))

>flush_buffer :: Lexer_Action ()
>flush_buffer = t_buffer flush

>hex_to_int :: Lexer_Action Oberon_Integer
>hex_to_int =
>  p_buffer `and_with` \buff ->
>  return (hex_string_to_int (buffer_to_string buff))

>move_input_column :: Int -> Lexer_Action ()
>move_input_column dist = t_source_pos (flip shift_column dist)

>next_input_line :: Lexer_Action ()
>next_input_line = t_source_pos next_line

>store_char :: Char -> Lexer_Action ()
>store_char c = t_buffer (add_char c)

>to_char :: Lexer_Action Oberon_Integer
>to_char =
>  p_buffer `and_with` \buff ->
>  return ((from_int . toInteger . ord . head . buffer_to_string) buff)

Converts the string in the buffer into a character (actually an
integer since characters are represented as integers).  The
pre-condition is that there is exactly one character in the buffer
when this is called.


>to_id :: Lexer_Action Oberon_Id
>to_id = 
>  p_buffer `and_with` \buff ->
>  return (string_to_id (buffer_to_string buff))

>to_real :: Lexer_Action Oberon_Real
>to_real =
>  p_buffer `and_with` \buff ->
>  return (string_to_real (buffer_to_string buff))

>to_string :: Lexer_Action Oberon_String
>to_string =
>  p_buffer `and_with` \buff ->
>  return (string_to_string (buffer_to_string buff))

% eof
