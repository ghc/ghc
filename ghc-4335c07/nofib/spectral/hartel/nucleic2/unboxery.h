#ifdef USE_UNBOXED_FLOATS
#define FLOAT_TY Float#
#define FL_LIT(x) (x#)
#define BOX_FLOAT(x) (F# (x))
#define _ADD_ `plusFloat#`
#define _SUB_ `minusFloat#`
#define _MUL_ `timesFloat#`
#define _NEG_ negateFloat#
#define _SIN_ sinFloat#
#define _COS_ cosFloat#
#define _SQRT_ sqrtFloat#
#define _ATAN2_ atan2Float#
#define _LE_FLT_ `leFloat#`

#define INT_TY Int#
#define INT_LIT(x) (x#)
#define _EQ_INT_ `eqInt#`

#else /* ! USE_UNBOXED_FLOATS */
#define FLOAT_TY Float
#define FL_LIT(x) (x)
#define BOX_FLOAT(x) (x)
#define _ADD_ +
#define _SUB_ -
#define _MUL_ *
#define _NEG_ -
#define _SIN_ sin
#define _COS_ cos
#define _SQRT_ sqrt
#define _ATAN2_ atan2
#define _LE_FLT_ <=

#define INT_TY Int
#define INT_LIT(x) (x)
#define _EQ_INT_ ==

#endif /* ! USE_UNBOXED_FLOATS */
