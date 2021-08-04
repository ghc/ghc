/**
 * @file decaf/point_448.h
 * @author Mike Hamburg
 *
 * @copyright
 *   Copyright (c) 2015-2016 Cryptography Research, Inc.  \n
 *   Released under the MIT License.  See LICENSE.txt for license information.
 *
 * @brief A group of prime order p, based on Ed448-Goldilocks.
 *
 * @warning This file was automatically generated in Python.
 * Please do not edit it.
 */

#ifndef __CRYPTONITE_DECAF_POINT_448_H__
#define __CRYPTONITE_DECAF_POINT_448_H__ 1

#include <decaf/common.h>

#ifdef __cplusplus
extern "C" {
#endif

/** @cond internal */
#define CRYPTONITE_DECAF_448_SCALAR_LIMBS ((446-1)/CRYPTONITE_DECAF_WORD_BITS+1)
/** @endcond */

/** The number of bits in a scalar */
#define CRYPTONITE_DECAF_448_SCALAR_BITS 446

/** @cond internal */
#ifndef __CRYPTONITE_DECAF_448_GF_DEFINED__
#define __CRYPTONITE_DECAF_448_GF_DEFINED__ 1
/** @brief Galois field element internal structure */
typedef struct cryptonite_gf_448_s {
    cryptonite_decaf_word_t limb[512/CRYPTONITE_DECAF_WORD_BITS];
} __attribute__((aligned(16))) cryptonite_gf_448_s, cryptonite_gf_448_t[1];
#endif /* __CRYPTONITE_DECAF_448_GF_DEFINED__ */
/** @endcond */

/** Number of bytes in a serialized point. */
#define CRYPTONITE_DECAF_448_SER_BYTES 56

/** Number of bytes in an elligated point.  For now set the same as SER_BYTES
 * but could be different for other curves.
 */
#define CRYPTONITE_DECAF_448_HASH_BYTES 56

/** Number of bytes in a serialized scalar. */
#define CRYPTONITE_DECAF_448_SCALAR_BYTES 56

/** Number of bits in the "which" field of an elligator inverse */
#define CRYPTONITE_DECAF_448_INVERT_ELLIGATOR_WHICH_BITS 3

/** Number of bytes in an x448 public key */
#define CRYPTONITE_DECAF_X448_PUBLIC_BYTES 56

/** Number of bytes in an x448 private key */
#define CRYPTONITE_DECAF_X448_PRIVATE_BYTES 56

/** Twisted Edwards extended homogeneous coordinates */
typedef struct cryptonite_decaf_448_point_s {
    /** @cond internal */
    cryptonite_gf_448_t x,y,z,t;
    /** @endcond */
} cryptonite_decaf_448_point_t[1];

/** Precomputed table based on a point.  Can be trivial implementation. */
struct cryptonite_decaf_448_precomputed_s;

/** Precomputed table based on a point.  Can be trivial implementation. */
typedef struct cryptonite_decaf_448_precomputed_s cryptonite_decaf_448_precomputed_s; 

/** Size and alignment of precomputed point tables. */
extern const size_t cryptonite_decaf_448_sizeof_precomputed_s CRYPTONITE_DECAF_API_VIS, cryptonite_decaf_448_alignof_precomputed_s CRYPTONITE_DECAF_API_VIS;

/** Scalar is stored packed, because we don't need the speed. */
typedef struct cryptonite_decaf_448_scalar_s {
    /** @cond internal */
    cryptonite_decaf_word_t limb[CRYPTONITE_DECAF_448_SCALAR_LIMBS];
    /** @endcond */
} cryptonite_decaf_448_scalar_t[1];

/** A scalar equal to 1. */
extern const cryptonite_decaf_448_scalar_t cryptonite_decaf_448_scalar_one CRYPTONITE_DECAF_API_VIS;

/** A scalar equal to 0. */
extern const cryptonite_decaf_448_scalar_t cryptonite_decaf_448_scalar_zero CRYPTONITE_DECAF_API_VIS;

/** The identity point on the curve. */
extern const cryptonite_decaf_448_point_t cryptonite_decaf_448_point_identity CRYPTONITE_DECAF_API_VIS;

/** An arbitrarily chosen base point on the curve. */
extern const cryptonite_decaf_448_point_t cryptonite_decaf_448_point_base CRYPTONITE_DECAF_API_VIS;

/** Precomputed table for the base point on the curve. */
extern const struct cryptonite_decaf_448_precomputed_s *cryptonite_decaf_448_precomputed_base CRYPTONITE_DECAF_API_VIS;

/**
 * @brief Read a scalar from wire format or from bytes.
 *
 * @param [in] ser Serialized form of a scalar.
 * @param [out] out Deserialized form.
 *
 * @retval CRYPTONITE_DECAF_SUCCESS The scalar was correctly encoded.
 * @retval CRYPTONITE_DECAF_FAILURE The scalar was greater than the modulus,
 * and has been reduced modulo that modulus.
 */
cryptonite_decaf_error_t cryptonite_decaf_448_scalar_decode (
    cryptonite_decaf_448_scalar_t out,
    const unsigned char ser[CRYPTONITE_DECAF_448_SCALAR_BYTES]
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_WARN_UNUSED CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Read a scalar from wire format or from bytes.  Reduces mod
 * scalar prime.
 *
 * @param [in] ser Serialized form of a scalar.
 * @param [in] ser_len Length of serialized form.
 * @param [out] out Deserialized form.
 */
void cryptonite_decaf_448_scalar_decode_long (
    cryptonite_decaf_448_scalar_t out,
    const unsigned char *ser,
    size_t ser_len
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;
    
/**
 * @brief Serialize a scalar to wire format.
 *
 * @param [out] ser Serialized form of a scalar.
 * @param [in] s Deserialized scalar.
 */
void cryptonite_decaf_448_scalar_encode (
    unsigned char ser[CRYPTONITE_DECAF_448_SCALAR_BYTES],
    const cryptonite_decaf_448_scalar_t s
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE CRYPTONITE_DECAF_NOINLINE;
        
/**
 * @brief Add two scalars.  The scalars may use the same memory.
 * @param [in] a One scalar.
 * @param [in] b Another scalar.
 * @param [out] out a+b.
 */
void cryptonite_decaf_448_scalar_add (
    cryptonite_decaf_448_scalar_t out,
    const cryptonite_decaf_448_scalar_t a,
    const cryptonite_decaf_448_scalar_t b
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Compare two scalars.
 * @param [in] a One scalar.
 * @param [in] b Another scalar.
 * @retval CRYPTONITE_DECAF_TRUE The scalars are equal.
 * @retval CRYPTONITE_DECAF_FALSE The scalars are not equal.
 */    
cryptonite_decaf_bool_t cryptonite_decaf_448_scalar_eq (
    const cryptonite_decaf_448_scalar_t a,
    const cryptonite_decaf_448_scalar_t b
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_WARN_UNUSED CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Subtract two scalars.  The scalars may use the same memory.
 * @param [in] a One scalar.
 * @param [in] b Another scalar.
 * @param [out] out a-b.
 */  
void cryptonite_decaf_448_scalar_sub (
    cryptonite_decaf_448_scalar_t out,
    const cryptonite_decaf_448_scalar_t a,
    const cryptonite_decaf_448_scalar_t b
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Multiply two scalars.  The scalars may use the same memory.
 * @param [in] a One scalar.
 * @param [in] b Another scalar.
 * @param [out] out a*b.
 */  
void cryptonite_decaf_448_scalar_mul (
    cryptonite_decaf_448_scalar_t out,
    const cryptonite_decaf_448_scalar_t a,
    const cryptonite_decaf_448_scalar_t b
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;
        
/**
* @brief Halve a scalar.  The scalars may use the same memory.
* @param [in] a A scalar.
* @param [out] out a/2.
*/
void cryptonite_decaf_448_scalar_halve (
   cryptonite_decaf_448_scalar_t out,
   const cryptonite_decaf_448_scalar_t a
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Invert a scalar.  When passed zero, return 0.  The input and output may alias.
 * @param [in] a A scalar.
 * @param [out] out 1/a.
 * @return CRYPTONITE_DECAF_SUCCESS The input is nonzero.
 */  
cryptonite_decaf_error_t cryptonite_decaf_448_scalar_invert (
    cryptonite_decaf_448_scalar_t out,
    const cryptonite_decaf_448_scalar_t a
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_WARN_UNUSED CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Copy a scalar.  The scalars may use the same memory, in which
 * case this function does nothing.
 * @param [in] a A scalar.
 * @param [out] out Will become a copy of a.
 */
static inline void CRYPTONITE_DECAF_NONNULL cryptonite_decaf_448_scalar_copy (
    cryptonite_decaf_448_scalar_t out,
    const cryptonite_decaf_448_scalar_t a
) {
    *out = *a;
}

/**
 * @brief Set a scalar to an unsigned 64-bit integer.
 * @param [in] a An integer.
 * @param [out] out Will become equal to a.
 */  
void cryptonite_decaf_448_scalar_set_unsigned (
    cryptonite_decaf_448_scalar_t out,
    uint64_t a
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL;

/**
 * @brief Encode a point as a sequence of bytes.
 *
 * @param [out] ser The byte representation of the point.
 * @param [in] pt The point to encode.
 */
void cryptonite_decaf_448_point_encode (
    uint8_t ser[CRYPTONITE_DECAF_448_SER_BYTES],
    const cryptonite_decaf_448_point_t pt
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Decode a point from a sequence of bytes.
 *
 * Every point has a unique encoding, so not every
 * sequence of bytes is a valid encoding.  If an invalid
 * encoding is given, the output is undefined.
 *
 * @param [out] pt The decoded point.
 * @param [in] ser The serialized version of the point.
 * @param [in] allow_identity CRYPTONITE_DECAF_TRUE if the identity is a legal input.
 * @retval CRYPTONITE_DECAF_SUCCESS The decoding succeeded.
 * @retval CRYPTONITE_DECAF_FAILURE The decoding didn't succeed, because
 * ser does not represent a point.
 */
cryptonite_decaf_error_t cryptonite_decaf_448_point_decode (
    cryptonite_decaf_448_point_t pt,
    const uint8_t ser[CRYPTONITE_DECAF_448_SER_BYTES],
    cryptonite_decaf_bool_t allow_identity
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_WARN_UNUSED CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Copy a point.  The input and output may alias,
 * in which case this function does nothing.
 *
 * @param [out] a A copy of the point.
 * @param [in] b Any point.
 */
static inline void CRYPTONITE_DECAF_NONNULL cryptonite_decaf_448_point_copy (
    cryptonite_decaf_448_point_t a,
    const cryptonite_decaf_448_point_t b
) {
    *a=*b;
}

/**
 * @brief Test whether two points are equal.  If yes, return
 * CRYPTONITE_DECAF_TRUE, else return CRYPTONITE_DECAF_FALSE.
 *
 * @param [in] a A point.
 * @param [in] b Another point.
 * @retval CRYPTONITE_DECAF_TRUE The points are equal.
 * @retval CRYPTONITE_DECAF_FALSE The points are not equal.
 */
cryptonite_decaf_bool_t cryptonite_decaf_448_point_eq (
    const cryptonite_decaf_448_point_t a,
    const cryptonite_decaf_448_point_t b
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_WARN_UNUSED CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Add two points to produce a third point.  The
 * input points and output point can be pointers to the same
 * memory.
 *
 * @param [out] sum The sum a+b.
 * @param [in] a An addend.
 * @param [in] b An addend.
 */
void cryptonite_decaf_448_point_add (
    cryptonite_decaf_448_point_t sum,
    const cryptonite_decaf_448_point_t a,
    const cryptonite_decaf_448_point_t b
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL;

/**
 * @brief Double a point.  Equivalent to
 * cryptonite_decaf_448_point_add(two_a,a,a), but potentially faster.
 *
 * @param [out] two_a The sum a+a.
 * @param [in] a A point.
 */
void cryptonite_decaf_448_point_double (
    cryptonite_decaf_448_point_t two_a,
    const cryptonite_decaf_448_point_t a
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL;

/**
 * @brief Subtract two points to produce a third point.  The
 * input points and output point can be pointers to the same
 * memory.
 *
 * @param [out] diff The difference a-b.
 * @param [in] a The minuend.
 * @param [in] b The subtrahend.
 */
void cryptonite_decaf_448_point_sub (
    cryptonite_decaf_448_point_t diff,
    const cryptonite_decaf_448_point_t a,
    const cryptonite_decaf_448_point_t b
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL;
    
/**
 * @brief Negate a point to produce another point.  The input
 * and output points can use the same memory.
 *
 * @param [out] nega The negated input point
 * @param [in] a The input point.
 */
void cryptonite_decaf_448_point_negate (
   cryptonite_decaf_448_point_t nega,
   const cryptonite_decaf_448_point_t a
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL;

/**
 * @brief Multiply a base point by a scalar: scaled = scalar*base.
 *
 * @param [out] scaled The scaled point base*scalar
 * @param [in] base The point to be scaled.
 * @param [in] scalar The scalar to multiply by.
 */
void cryptonite_decaf_448_point_scalarmul (
    cryptonite_decaf_448_point_t scaled,
    const cryptonite_decaf_448_point_t base,
    const cryptonite_decaf_448_scalar_t scalar
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Multiply a base point by a scalar: scaled = scalar*base.
 * This function operates directly on serialized forms.
 *
 * @warning This function is experimental.  It may not be supported
 * long-term.
 *
 * @param [out] scaled The scaled point base*scalar
 * @param [in] base The point to be scaled.
 * @param [in] scalar The scalar to multiply by.
 * @param [in] allow_identity Allow the input to be the identity.
 * @param [in] short_circuit Allow a fast return if the input is illegal.
 *
 * @retval CRYPTONITE_DECAF_SUCCESS The scalarmul succeeded.
 * @retval CRYPTONITE_DECAF_FAILURE The scalarmul didn't succeed, because
 * base does not represent a point.
 */
cryptonite_decaf_error_t cryptonite_decaf_448_direct_scalarmul (
    uint8_t scaled[CRYPTONITE_DECAF_448_SER_BYTES],
    const uint8_t base[CRYPTONITE_DECAF_448_SER_BYTES],
    const cryptonite_decaf_448_scalar_t scalar,
    cryptonite_decaf_bool_t allow_identity,
    cryptonite_decaf_bool_t short_circuit
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_WARN_UNUSED CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief RFC 7748 Diffie-Hellman scalarmul.  This function uses a different
 * (non-Decaf) encoding.
 *
 * @param [out] scaled The scaled point base*scalar
 * @param [in] base The point to be scaled.
 * @param [in] scalar The scalar to multiply by.
 *
 * @retval CRYPTONITE_DECAF_SUCCESS The scalarmul succeeded.
 * @retval CRYPTONITE_DECAF_FAILURE The scalarmul didn't succeed, because the base
 * point is in a small subgroup.
 */
cryptonite_decaf_error_t cryptonite_decaf_x448 (
    uint8_t out[CRYPTONITE_DECAF_X448_PUBLIC_BYTES],
    const uint8_t base[CRYPTONITE_DECAF_X448_PUBLIC_BYTES],
    const uint8_t scalar[CRYPTONITE_DECAF_X448_PRIVATE_BYTES]
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_WARN_UNUSED CRYPTONITE_DECAF_NOINLINE;

/** The base point for X448 Diffie-Hellman */
extern const uint8_t cryptonite_decaf_x448_base_point[CRYPTONITE_DECAF_X448_PUBLIC_BYTES] CRYPTONITE_DECAF_API_VIS;

/**
 * @brief RFC 7748 Diffie-Hellman base point scalarmul.  This function uses
 * a different (non-Decaf) encoding.
 *
 * @deprecated Renamed to cryptonite_decaf_x448_derive_public_key.
 * I have no particular timeline for removing this name.
 *
 * @param [out] scaled The scaled point base*scalar
 * @param [in] scalar The scalar to multiply by.
 */
void cryptonite_decaf_x448_generate_key (
    uint8_t out[CRYPTONITE_DECAF_X448_PUBLIC_BYTES],
    const uint8_t scalar[CRYPTONITE_DECAF_X448_PRIVATE_BYTES]
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE CRYPTONITE_DECAF_DEPRECATED("Renamed to cryptonite_decaf_x448_derive_public_key");
    
/**
 * @brief RFC 7748 Diffie-Hellman base point scalarmul.  This function uses
 * a different (non-Decaf) encoding.
 *
 * Does exactly the same thing as cryptonite_decaf_x448_generate_key,
 * but has a better name.
 *
 * @param [out] scaled The scaled point base*scalar
 * @param [in] scalar The scalar to multiply by.
 */
void cryptonite_decaf_x448_derive_public_key (
    uint8_t out[CRYPTONITE_DECAF_X448_PUBLIC_BYTES],
    const uint8_t scalar[CRYPTONITE_DECAF_X448_PRIVATE_BYTES]
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/* FUTURE: uint8_t cryptonite_decaf_448_encode_like_curve448) */

/**
 * @brief Precompute a table for fast scalar multiplication.
 * Some implementations do not include precomputed points; for
 * those implementations, this implementation simply copies the
 * point.
 *
 * @param [out] a A precomputed table of multiples of the point.
 * @param [in] b Any point.
 */
void cryptonite_decaf_448_precompute (
    cryptonite_decaf_448_precomputed_s *a,
    const cryptonite_decaf_448_point_t b
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Multiply a precomputed base point by a scalar:
 * scaled = scalar*base.
 * Some implementations do not include precomputed points; for
 * those implementations, this function is the same as
 * cryptonite_decaf_448_point_scalarmul
 *
 * @param [out] scaled The scaled point base*scalar
 * @param [in] base The point to be scaled.
 * @param [in] scalar The scalar to multiply by.
 */
void cryptonite_decaf_448_precomputed_scalarmul (
    cryptonite_decaf_448_point_t scaled,
    const cryptonite_decaf_448_precomputed_s *base,
    const cryptonite_decaf_448_scalar_t scalar
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Multiply two base points by two scalars:
 * scaled = scalar1*base1 + scalar2*base2.
 *
 * Equivalent to two calls to cryptonite_decaf_448_point_scalarmul, but may be
 * faster.
 *
 * @param [out] combo The linear combination scalar1*base1 + scalar2*base2.
 * @param [in] base1 A first point to be scaled.
 * @param [in] scalar1 A first scalar to multiply by.
 * @param [in] base2 A second point to be scaled.
 * @param [in] scalar2 A second scalar to multiply by.
 */
void cryptonite_decaf_448_point_double_scalarmul (
    cryptonite_decaf_448_point_t combo,
    const cryptonite_decaf_448_point_t base1,
    const cryptonite_decaf_448_scalar_t scalar1,
    const cryptonite_decaf_448_point_t base2,
    const cryptonite_decaf_448_scalar_t scalar2
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;
    
/**
 * Multiply one base point by two scalars:
 *
 * a1 = scalar1 * base
 * a2 = scalar2 * base
 *
 * Equivalent to two calls to cryptonite_decaf_448_point_scalarmul, but may be
 * faster.
 *
 * @param [out] a1 The first multiple.  It may be the same as the input point.
 * @param [out] a2 The second multiple.  It may be the same as the input point.
 * @param [in] base1 A point to be scaled.
 * @param [in] scalar1 A first scalar to multiply by.
 * @param [in] scalar2 A second scalar to multiply by.
 */
void cryptonite_decaf_448_point_dual_scalarmul (
    cryptonite_decaf_448_point_t a1,
    cryptonite_decaf_448_point_t a2,
    const cryptonite_decaf_448_point_t base1,
    const cryptonite_decaf_448_scalar_t scalar1,
    const cryptonite_decaf_448_scalar_t scalar2
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Multiply two base points by two scalars:
 * scaled = scalar1*cryptonite_decaf_448_point_base + scalar2*base2.
 *
 * Otherwise equivalent to cryptonite_decaf_448_point_double_scalarmul, but may be
 * faster at the expense of being variable time.
 *
 * @param [out] combo The linear combination scalar1*base + scalar2*base2.
 * @param [in] scalar1 A first scalar to multiply by.
 * @param [in] base2 A second point to be scaled.
 * @param [in] scalar2 A second scalar to multiply by.
 *
 * @warning: This function takes variable time, and may leak the scalars
 * used.  It is designed for signature verification.
 */
void cryptonite_decaf_448_base_double_scalarmul_non_secret (
    cryptonite_decaf_448_point_t combo,
    const cryptonite_decaf_448_scalar_t scalar1,
    const cryptonite_decaf_448_point_t base2,
    const cryptonite_decaf_448_scalar_t scalar2
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Constant-time decision between two points.  If pick_b
 * is zero, out = a; else out = b.
 *
 * @param [out] out The output.  It may be the same as either input.
 * @param [in] a Any point.
 * @param [in] b Any point.
 * @param [in] pick_b If nonzero, choose point b.
 */
void cryptonite_decaf_448_point_cond_sel (
    cryptonite_decaf_448_point_t out,
    const cryptonite_decaf_448_point_t a,
    const cryptonite_decaf_448_point_t b,
    cryptonite_decaf_word_t pick_b
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Constant-time decision between two scalars.  If pick_b
 * is zero, out = a; else out = b.
 *
 * @param [out] out The output.  It may be the same as either input.
 * @param [in] a Any scalar.
 * @param [in] b Any scalar.
 * @param [in] pick_b If nonzero, choose scalar b.
 */
void cryptonite_decaf_448_scalar_cond_sel (
    cryptonite_decaf_448_scalar_t out,
    const cryptonite_decaf_448_scalar_t a,
    const cryptonite_decaf_448_scalar_t b,
    cryptonite_decaf_word_t pick_b
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Test that a point is valid, for debugging purposes.
 *
 * @param [in] to_test The point to test.
 * @retval CRYPTONITE_DECAF_TRUE The point is valid.
 * @retval CRYPTONITE_DECAF_FALSE The point is invalid.
 */
cryptonite_decaf_bool_t cryptonite_decaf_448_point_valid (
    const cryptonite_decaf_448_point_t to_test
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_WARN_UNUSED CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Torque a point, for debugging purposes.  The output
 * will be equal to the input.
 *
 * @param [out] q The point to torque.
 * @param [in] p The point to torque.
 */
void cryptonite_decaf_448_point_debugging_torque (
    cryptonite_decaf_448_point_t q,
    const cryptonite_decaf_448_point_t p
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Projectively scale a point, for debugging purposes.
 * The output will be equal to the input, and will be valid
 * even if the factor is zero.
 *
 * @param [out] q The point to scale.
 * @param [in] p The point to scale.
 * @param [in] factor Serialized GF factor to scale.
 */
void cryptonite_decaf_448_point_debugging_pscale (
    cryptonite_decaf_448_point_t q,
    const cryptonite_decaf_448_point_t p,
    const unsigned char factor[CRYPTONITE_DECAF_448_SER_BYTES]
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Almost-Elligator-like hash to curve.
 *
 * Call this function with the output of a hash to make a hash to the curve.
 *
 * This function runs Elligator2 on the cryptonite_decaf_448 Jacobi quartic model.  It then
 * uses the isogeny to put the result in twisted Edwards form.  As a result,
 * it is safe (cannot produce points of order 4), and would be compatible with
 * hypothetical other implementations of Decaf using a Montgomery or untwisted
 * Edwards model.
 *
 * Unlike Elligator, this function may be up to 4:1 on [0,(p-1)/2]:
 *   A factor of 2 due to the isogeny.
 *   A factor of 2 because we quotient out the 2-torsion.
 *
 * This makes it about 8:1 overall, or 16:1 overall on curves with cofactor 8.
 *
 * Negating the input (mod q) results in the same point.  Inverting the input
 * (mod q) results in the negative point.  This is the same as Elligator.
 *
 * This function isn't quite indifferentiable from a random oracle.
 * However, it is suitable for many protocols, including SPEKE and SPAKE2 EE. 
 * Furthermore, calling it twice with independent seeds and adding the results
 * is indifferentiable from a random oracle.
 *
 * @param [in] hashed_data Output of some hash function.
 * @param [out] pt The data hashed to the curve.
 */
void
cryptonite_decaf_448_point_from_hash_nonuniform (
    cryptonite_decaf_448_point_t pt,
    const unsigned char hashed_data[CRYPTONITE_DECAF_448_HASH_BYTES]
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Indifferentiable hash function encoding to curve.
 *
 * Equivalent to calling cryptonite_decaf_448_point_from_hash_nonuniform twice and adding.
 *
 * @param [in] hashed_data Output of some hash function.
 * @param [out] pt The data hashed to the curve.
 */ 
void cryptonite_decaf_448_point_from_hash_uniform (
    cryptonite_decaf_448_point_t pt,
    const unsigned char hashed_data[2*CRYPTONITE_DECAF_448_HASH_BYTES]
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE;

/**
 * @brief Inverse of elligator-like hash to curve.
 *
 * This function writes to the buffer, to make it so that
 * cryptonite_decaf_448_point_from_hash_nonuniform(buffer) = pt if
 * possible.  Since there may be multiple preimages, the
 * "which" parameter chooses between them.  To ensure uniform
 * inverse sampling, this function succeeds or fails
 * independently for different "which" values.
 *
 * @param [out] recovered_hash Encoded data.
 * @param [in] pt The point to encode.
 * @param [in] which A value determining which inverse point
 * to return.
 *
 * @retval CRYPTONITE_DECAF_SUCCESS The inverse succeeded.
 * @retval CRYPTONITE_DECAF_FAILURE The inverse failed.
 */
cryptonite_decaf_error_t
cryptonite_decaf_448_invert_elligator_nonuniform (
    unsigned char recovered_hash[CRYPTONITE_DECAF_448_HASH_BYTES],
    const cryptonite_decaf_448_point_t pt,
    uint32_t which
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE CRYPTONITE_DECAF_WARN_UNUSED;

/**
 * @brief Inverse of elligator-like hash to curve.
 *
 * This function writes to the buffer, to make it so that
 * cryptonite_decaf_448_point_from_hash_uniform(buffer) = pt if
 * possible.  Since there may be multiple preimages, the
 * "which" parameter chooses between them.  To ensure uniform
 * inverse sampling, this function succeeds or fails
 * independently for different "which" values.
 *
 * @param [out] recovered_hash Encoded data.
 * @param [in] pt The point to encode.
 * @param [in] which A value determining which inverse point
 * to return.
 *
 * @retval CRYPTONITE_DECAF_SUCCESS The inverse succeeded.
 * @retval CRYPTONITE_DECAF_FAILURE The inverse failed.
 */
cryptonite_decaf_error_t
cryptonite_decaf_448_invert_elligator_uniform (
    unsigned char recovered_hash[2*CRYPTONITE_DECAF_448_HASH_BYTES],
    const cryptonite_decaf_448_point_t pt,
    uint32_t which
) CRYPTONITE_DECAF_API_VIS CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_NOINLINE CRYPTONITE_DECAF_WARN_UNUSED;

/**
 * @brief Overwrite scalar with zeros.
 */
void cryptonite_decaf_448_scalar_destroy (
    cryptonite_decaf_448_scalar_t scalar
) CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_API_VIS;

/**
 * @brief Overwrite point with zeros.
 */
void cryptonite_decaf_448_point_destroy (
    cryptonite_decaf_448_point_t point
) CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_API_VIS;

/**
 * @brief Overwrite precomputed table with zeros.
 */
void cryptonite_decaf_448_precomputed_destroy (
    cryptonite_decaf_448_precomputed_s *pre
) CRYPTONITE_DECAF_NONNULL CRYPTONITE_DECAF_API_VIS;

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __CRYPTONITE_DECAF_POINT_448_H__ */
