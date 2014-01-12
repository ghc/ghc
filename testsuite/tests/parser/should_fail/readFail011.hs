-- !!! Test line numbers in presence of string gaps.

main = print "a\
	     \b\
	     \c"

wibble = = -- this is a parse error on line 7
