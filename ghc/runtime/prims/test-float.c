/* compile with something vaguely like...

gcc -o test-float -g -I ../includes prims/test-float.c prims/PrimArith_ap_o.o gmp/libgmp.a

*/
#include "rtsdefs.h"
#include <errno.h>

StgFloat float_val[] = {
  0.0, -1.0, 1.0, 1.2, -1.5, 1.5, -1.5e19, -1.5e-19, 1.5e19, 1.5e-19, 1.5e30, 1.5e-30,
  3.14159265, -3.14159265, 42, -42, 42.2, -42.2
};
#define NF 18

StgDouble double_val[] = {
  0.0, -1.0, 1.0, 1.2, -1.5, 1.5, -1.5e19, -1.5e-19, 1.5e19, 1.5e-19, 1.5e30, 1.5e-30,
  3.14159265, -3.14159265, 42, -42, 42.2, -42.2
};
#define ND 18

P_ Hp_SAVE;
const W_ ArrayOfData_info[4];

void
main ()
{
    MP_INT m;
    I_ e;
    StgFloat f;
    StgDouble d;
    int i;
    char *str;

    mpz_init ( &m );

    __decodeDouble( &m, &e, (StgDouble) 42 );

    str = mpz_get_str( NULL, 10, &m );

    printf("decoded 42: mant=%s, expon=%d (0x%x)\n", str, e,e);

    /* test decoding (doubles) */
    for (i = 0; i < ND; i++) {
	__decodeDouble( &m, &e, double_val[i]);
	str = mpz_get_str( NULL, 10, &m );
	d = __encodeDouble( &m, e);

        printf("decodedD: d=%g, mant=%s, expon=%d (0x%x)\n",
                double_val[i], str, e,e);
        printf("encodedD: d=%g\n\n", d);
    }

    /* test decoding (floats) */
#if 0
    for (i = 0; i < NF; i++) {
        m = floatSignificandZh(float_val[i]);
        e = floatExponentZh(float_val[i]);

	f = encodeFloatZh(m, e);

        printf("decodedF: f=%g, mant=%d (0x%x), expon=%d (0x%x)\n",
                float_val[i], m,m, e,e);
        printf("encodedF: f=%g\n\n", f);
    }
#endif
}
