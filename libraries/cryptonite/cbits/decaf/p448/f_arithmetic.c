/**
 * @cond internal
 * @file f_arithmetic.c
 * @copyright
 *   Copyright (c) 2014 Cryptography Research, Inc.  \n
 *   Released under the MIT License.  See LICENSE.txt for license information.
 * @author Mike Hamburg
 * @brief Field-specific arithmetic.
 */

#include "field.h"

mask_t cryptonite_gf_isr (
    gf a,
    const gf x
) {
    gf L0, L1, L2;
    cryptonite_gf_sqr  (L1,     x );
    cryptonite_gf_mul  (L2,     x,   L1 );
    cryptonite_gf_sqr  (L1,   L2 );
    cryptonite_gf_mul  (L2,     x,   L1 );
    cryptonite_gf_sqrn (L1,   L2,     3 );
    cryptonite_gf_mul  (L0,   L2,   L1 );
    cryptonite_gf_sqrn (L1,   L0,     3 );
    cryptonite_gf_mul  (L0,   L2,   L1 );
    cryptonite_gf_sqrn (L2,   L0,     9 );
    cryptonite_gf_mul  (L1,   L0,   L2 );
    cryptonite_gf_sqr  (L0,   L1 );
    cryptonite_gf_mul  (L2,     x,   L0 );
    cryptonite_gf_sqrn (L0,   L2,    18 );
    cryptonite_gf_mul  (L2,   L1,   L0 );
    cryptonite_gf_sqrn (L0,   L2,    37 );
    cryptonite_gf_mul  (L1,   L2,   L0 );
    cryptonite_gf_sqrn (L0,   L1,    37 );
    cryptonite_gf_mul  (L1,   L2,   L0 );
    cryptonite_gf_sqrn (L0,   L1,   111 );
    cryptonite_gf_mul  (L2,   L1,   L0 );
    cryptonite_gf_sqr  (L0,   L2 );
    cryptonite_gf_mul  (L1,     x,   L0 );
    cryptonite_gf_sqrn (L0,   L1,   223 );
    cryptonite_gf_mul  (L1,   L2,   L0 );
    cryptonite_gf_sqr  (L2, L1);
    cryptonite_gf_mul  (L0, L2, x);
    cryptonite_gf_copy(a,L1);
    return cryptonite_gf_eq(L0,ONE);
}
