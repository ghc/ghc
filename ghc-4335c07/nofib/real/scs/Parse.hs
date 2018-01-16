module Parse(parse_circuit, parse_exact, parse_approx, parse_integer) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Ratio

import Types
import ParseLib

parse_circuit = parse (remove_right circuit whitespace)
parse_exact   = parse (remove_right exact   whitespace)
parse_approx  = parse (remove_right approx  whitespace)
parse_integer = parse (remove_right integer whitespace)

circuit	:: Parser (Circuit, [Name])
circuit = transform make_indices (repetition1 element)
  where
	make_indices list	= (circuit, names)
	  where
		list_with_types	= [(e, ("i:" ++ n1, "v:" ++ n2, "v:" ++ n3)) | (e, (n1, n2, n3)) <- list]
		names		= (nub . ("v:ground" :) . concat) [[n1, n2, n3] | (_, (n1, n2, n3)) <- list_with_types]
		circuit		= [(index n1, index n2, index n3, e) | (e, (n1, n2, n3)) <- list_with_types]
		index name	= fromJust (lookup name (zip names [0..]))

element :: Parser (Element, (Name, Name, Name))
element	= choice [conductor, resistor, capacitor, inductor, vsource, isource, junction]
  where
	conductor		= code 'g' (transform Conductor exact)
	resistor		= code 'r' (transform Resistor  exact)
	capacitor		= code 'c' (transform Capacitor (sequence2 exact exact))
	inductor		= code 'l' (transform Inductor  (sequence2 exact exact))
	vsource			= code 'v' (transform Vsource   list)
	isource			= code 'i' (transform Isource   list)
	junction		= code 'j' (transform Junction  (sequence3 exact exact exact))

	code c			= transform (\(n, e) -> (e, n)) . (sequence2 (remove_left (character c) (sequence3 name name name)))
	
list	:: Parser List
list	= repetition1 (enclose (character '(') (glue approx (character ',') approx) (character ')'))

approx	:: Parser Approx
approx	= transform circa exact

exact	:: Parser Exact
exact	= transform (\((sign, int), (frac, fact)) -> sign * (int + frac) * fact) (sequence2 (sequence2 sign integer) (sequence2 fraction factor))
  where
  	sign			= transform make_sign (option (character '-'))
	make_sign ""		=  1 % 1
	make_sign "-"		= -1 % 1
	
	integer			= transform make_integer (parse_while isDigit)
	make_integer s		= read s % 1
	
	fraction		= transform make_fraction (option (remove_left (character' '.') (parse_while isDigit)))
	make_fraction [ ]	= 0 % 1
	make_fraction [s]	= read s % read ('1' : map (const '0') s)

	factor			= transform make_factor (option (parse_if' (`elem` "afpnumkMGTPE")))
	make_factor "a"		= 1 % 1000000000000000000
	make_factor "f"		= 1 % 1000000000000000
	make_factor "p"		= 1 % 1000000000000
	make_factor "n"		= 1 % 1000000000
	make_factor "u"		= 1 % 1000000
	make_factor "m"		= 1 % 1000
	make_factor ""		= 1 % 1
	make_factor "k"		= 1000 % 1
	make_factor "M"		= 1000000 % 1
	make_factor "G"		= 1000000000 % 1
	make_factor "T"		= 1000000000000 % 1
	make_factor "P"		= 1000000000000000 % 1
	make_factor "E"		= 1000000000000000000 % 1

integer :: Parser Integer
integer = transform read (choice [cons (character '-') (parse_while isDigit), parse_while isDigit])

name	:: Parser Name
name	= parse_while isAlphaNum
