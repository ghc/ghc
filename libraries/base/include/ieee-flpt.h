/* this file is #included into both C (.c and .hc) and Haskell files */

    /* IEEE format floating-point */
#define IEEE_FLOATING_POINT 1

   /* Radix of exponent representation */
#ifndef FLT_RADIX
# define FLT_RADIX 2
#endif

   /* Number of base-FLT_RADIX digits in the significand of a float */
#ifndef FLT_MANT_DIG
# define FLT_MANT_DIG 24
#endif
   /* Minimum int x such that FLT_RADIX**(x-1) is a normalised float */
#ifndef FLT_MIN_EXP
#  define FLT_MIN_EXP (-125)
#endif
   /* Maximum int x such that FLT_RADIX**(x-1) is a representable float */
#ifndef FLT_MAX_EXP
# define FLT_MAX_EXP 128
#endif

   /* Number of base-FLT_RADIX digits in the significand of a double */
#ifndef DBL_MANT_DIG
# define DBL_MANT_DIG 53
#endif
   /* Minimum int x such that FLT_RADIX**(x-1) is a normalised double */
#ifndef DBL_MIN_EXP
#  define DBL_MIN_EXP (-1021)
#endif
   /* Maximum int x such that FLT_RADIX**(x-1) is a representable double */
#ifndef DBL_MAX_EXP
# define DBL_MAX_EXP 1024
#endif
