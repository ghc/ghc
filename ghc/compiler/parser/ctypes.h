/*-----------------------------------------------------------------------------
  ctypes.h for Haskell

  (c) Simon Marlow 1993
-----------------------------------------------------------------------------*/

#define C_Ident     1
#define C_Symbol    1<<1
#define C_Any       1<<2
#define C_Space     1<<3
#define C_Lower	    1<<4
#define C_Upper	    1<<5
#define C_Digit     1<<6

#define _IsType(c,flags) (char_types[(int)(c)] & flags)

#define IsSpace(c)     (_IsType(c,C_Space))
#define IsIdent(c)     (_IsType(c,C_Ident))
#define IsAny(c)       (_IsType(c,C_Any))
#define IsSymbol(c)    (_IsType(c,C_Symbol))
#define IsLower(c)     (_IsType(c,C_Lower))
#define IsUpper(c)     (_IsType(c,C_Upper))
#define IsDigit(c)     (_IsType(c,C_Digit))

extern const unsigned char char_types[];
