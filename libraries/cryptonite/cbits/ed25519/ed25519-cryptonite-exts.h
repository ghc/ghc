/*
    Public domain by Olivier Chéron <olivier.cheron@gmail.com>

    Arithmetic extensions to Ed25519-donna
*/


/*
    Scalar functions
*/

void
ED25519_FN(ed25519_scalar_encode) (unsigned char out[32], const bignum256modm in) {
    contract256_modm(out, in);
}

void
ED25519_FN(ed25519_scalar_decode_long) (bignum256modm out, const unsigned char *in, size_t len) {
    expand256_modm(out, in, len);
}

int
ED25519_FN(ed25519_scalar_eq) (const bignum256modm a, const bignum256modm b) {
    bignum256modm_element_t e = 0;

    for (int i = 0; i < bignum256modm_limb_size; i++) {
        e |= a[i] ^ b[i];
    }

    return (int) (1 & ((e - 1) >> bignum256modm_bits_per_limb));
}

void
ED25519_FN(ed25519_scalar_add) (bignum256modm r, const bignum256modm x, const bignum256modm y) {
    add256_modm(r, x, y);
}

void
ED25519_FN(ed25519_scalar_mul) (bignum256modm r, const bignum256modm x, const bignum256modm y) {
    mul256_modm(r, x, y);
}


/*
    Point functions
*/

void
ED25519_FN(ed25519_point_encode) (unsigned char r[32], const ge25519 *p) {
    ge25519_pack(r, p);
}

int
ED25519_FN(ed25519_point_decode_vartime) (ge25519 *r, const unsigned char p[32]) {
    unsigned char p_neg[32];

    // invert parity bit of X coordinate so the point is negated twice
    // (once here, once in ge25519_unpack_negative_vartime)
    for (int i = 0; i < 31; i++) {
        p_neg[i] = p[i];
    }
    p_neg[31] = p[31] ^ 0x80;

    return ge25519_unpack_negative_vartime(r, p_neg);
}

int
ED25519_FN(ed25519_point_eq) (const ge25519 *p, const ge25519 *q) {
    bignum25519 a, b;
    unsigned char contract_a[32], contract_b[32];
    int eq;

    // pX * qZ = qX * pZ
    curve25519_mul(a, p->x, q->z);
    curve25519_contract(contract_a, a);
    curve25519_mul(b, q->x, p->z);
    curve25519_contract(contract_b, b);
    eq = ed25519_verify(contract_a, contract_b, 32);

    // pY * qZ = qY * pZ
    curve25519_mul(a, p->y, q->z);
    curve25519_contract(contract_a, a);
    curve25519_mul(b, q->y, p->z);
    curve25519_contract(contract_b, b);
    eq &= ed25519_verify(contract_a, contract_b, 32);

    return eq;
}

static int
ED25519_FN(ed25519_point_is_identity) (const ge25519 *p) {
    static const unsigned char zero[32] = {0};
    unsigned char check[32];
    bignum25519 d;
    int eq;

    // pX = 0
    curve25519_contract(check, p->x);
    eq = ed25519_verify(check, zero, 32);

    // pY - pZ = 0
    curve25519_sub_reduce(d, p->y, p->z);
    curve25519_contract(check, d);
    eq &= ed25519_verify(check, zero, 32);

    return eq;
}

void
ED25519_FN(ed25519_point_negate) (ge25519 *r, const ge25519 *p) {
    curve25519_neg(r->x, p->x);
    curve25519_copy(r->y, p->y);
    curve25519_copy(r->z, p->z);
    curve25519_neg(r->t, p->t);
}

void
ED25519_FN(ed25519_point_add) (ge25519 *r, const ge25519 *p, const ge25519 *q) {
    ge25519_add(r, p, q);
}

void
ED25519_FN(ed25519_point_double) (ge25519 *r, const ge25519 *p) {
    ge25519_double(r, p);
}

void
ED25519_FN(ed25519_point_mul_by_cofactor) (ge25519 *r, const ge25519 *p) {
    ge25519_double_partial(r, p);
    ge25519_double_partial(r, r);
    ge25519_double(r, r);
}

void
ED25519_FN(ed25519_point_base_scalarmul) (ge25519 *r, const bignum256modm s) {
    ge25519_scalarmult_base_niels(r, ge25519_niels_base_multiples, s);
}

#if defined(ED25519_64BIT)
typedef uint64_t ed25519_move_cond_word;
#else
typedef uint32_t ed25519_move_cond_word;
#endif

/* out = (flag) ? in : out */
DONNA_INLINE static void
ed25519_move_cond_pniels(ge25519_pniels *out, const ge25519_pniels *in, uint32_t flag) {
    const int word_count = sizeof(ge25519_pniels) / sizeof(ed25519_move_cond_word);
    const ed25519_move_cond_word nb = (ed25519_move_cond_word) flag - 1, b = ~nb;

    ed25519_move_cond_word *outw = (ed25519_move_cond_word *) out;
    const ed25519_move_cond_word *inw  = (const ed25519_move_cond_word *) in;

    // ge25519_pniels has 4 coordinates, so word_count is divisible by 4
    for (int i = 0; i < word_count; i += 4) {
        outw[i + 0] = (outw[i + 0] & nb) | (inw[i + 0] & b);
        outw[i + 1] = (outw[i + 1] & nb) | (inw[i + 1] & b);
        outw[i + 2] = (outw[i + 2] & nb) | (inw[i + 2] & b);
        outw[i + 3] = (outw[i + 3] & nb) | (inw[i + 3] & b);
    }
}

static void
ed25519_point_scalarmul_w_choose_pniels(ge25519_pniels *t, const ge25519_pniels table[15], uint32_t pos) {
    // initialize t to identity, i.e. (1, 1, 1, 0)
    memset(t, 0, sizeof(ge25519_pniels));
    t->ysubx[0] = 1;
    t->xaddy[0] = 1;
    t->z[0] = 1;

    // move one entry from table matching requested position,
    // scanning all table to avoid cache-timing attack
    //
    // when pos == 0, no entry matches and this returns
    // identity as expected
    for (uint32_t i = 1; i < 16; i++) {
        uint32_t flag = ((i ^ pos) - 1) >> 31;
        ed25519_move_cond_pniels(t, table + i - 1, flag);
    }
}

void
ED25519_FN(ed25519_point_scalarmul) (ge25519 *r, const ge25519 *p, const bignum256modm s) {
    ge25519_pniels mult[15];
    ge25519_pniels pn;
    ge25519_p1p1 t;
    unsigned char ss[32];

    // transform scalar as little-endian number
    contract256_modm(ss, s);

    // initialize r to identity, i.e. ge25519 (0, 1, 1, 0)
    memset(r, 0, sizeof(ge25519));
    r->y[0] = 1;
    r->z[0] = 1;

    // precompute multiples of P: 1.P, 2.P, ..., 15.P
    ge25519_full_to_pniels(&mult[0], p);
    for (int i = 1; i < 15; i++) {
        ge25519_pnielsadd(&mult[i], p, &mult[i-1]);
    }

    // 4-bit fixed window, still 256 doublings but 64 additions
    for (int i = 31; i >= 0; i--) {
        // higher bits in ss[i]
        ed25519_point_scalarmul_w_choose_pniels(&pn, mult, ss[i] >> 4);
        ge25519_pnielsadd_p1p1(&t, r, &pn, 0);
        ge25519_p1p1_to_partial(r, &t);

        ge25519_double_partial(r, r);
        ge25519_double_partial(r, r);
        ge25519_double_partial(r, r);
        ge25519_double(r, r);

        // lower bits in ss[i]
        ed25519_point_scalarmul_w_choose_pniels(&pn, mult, ss[i] & 0x0F);
        ge25519_pnielsadd_p1p1(&t, r, &pn, 0);
        if (i > 0) {
            ge25519_p1p1_to_partial(r, &t);

            ge25519_double_partial(r, r);
            ge25519_double_partial(r, r);
            ge25519_double_partial(r, r);
            ge25519_double(r, r);
        } else {
            ge25519_p1p1_to_full(r, &t);
        }
    }
}

void
ED25519_FN(ed25519_base_double_scalarmul_vartime) (ge25519 *r, const bignum256modm s1, const ge25519 *p2, const bignum256modm s2) {
    // computes [s1]basepoint + [s2]p2
    ge25519_double_scalarmult_vartime(r, p2, s2, s1);
}

int
ED25519_FN(ed25519_point_has_prime_order) (const ge25519 *p) {
    static const bignum256modm sc_zero = {0};
    ge25519 q;

    // computes Q = m.P, vartime allowed because m is not secret
    ED25519_FN(ed25519_base_double_scalarmul_vartime) (&q, sc_zero, p, modm_m);

    return ED25519_FN(ed25519_point_is_identity) (&q);
}
