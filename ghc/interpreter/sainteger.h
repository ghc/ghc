
#define B_BASE 256
#define B_BASE_FLT (256.0)

/* this really ought to be abstract */
typedef 
   struct {
      int            sign;
      int            size;
      int            used;
      unsigned char  stuff[0];
   }
   B;

/* the ops themselves */
int  do_getsign  ( B* x );
int  do_cmp      ( B* x, B* y );
void do_add      ( B* x, B* y, int sizeRes, B* res );
void do_sub      ( B* x, B* y, int sizeRes, B* res );
void do_mul      ( B* x, B* y, int sizeRes, B* res );
void do_qrm      ( B* x, B* y, int sizeRes, B* qres, B* rres );
void do_neg      ( B* x,       int sizeRes, B* res );

void do_renormalise ( B* x );
int  is_sane ( B* x );

void do_fromInt  ( int            n,   int sizeRes, B*   res );
void do_fromWord ( unsigned int   n,   int sizeRes, B*   res );
void do_fromStr  ( char*        str,   int sizeRes, B*   res );

int          do_toInt    ( B* x );
unsigned int do_toWord   ( B* x );
float        do_toFloat  ( B* x );
double       do_toDouble ( B* x );

/* the number of bytes needed to hold result of an op */
int  size_add      ( B* x, B* y );
int  size_sub      ( B* x, B* y );
int  size_mul      ( B* x, B* y );
int  size_qrm      ( B* x, B* y );
int  size_neg      ( B* x );
int  size_fromInt  ( void );
int  size_fromWord ( void );
int  size_fromStr  ( char* str );
int  size_dblmantissa ( void );
int  size_fltmantissa ( void );

