/**
 * @file ed448goldilocks/eddsa.c
 * @author Mike Hamburg
 *
 * @copyright
 *   Copyright (c) 2015-2016 Cryptography Research, Inc.  \n
 *   Released under the MIT License.  See LICENSE.txt for license information.
 *
 * @cond internal
 * @brief EdDSA routines.
 *
 * @warning This file was automatically generated in Python.
 * Please do not edit it.
 */
#include "word.h"
#include <decaf/ed448.h>
#include <decaf/shake.h>
#include <decaf/sha512.h>
#include <string.h>

#define API_NAME "cryptonite_decaf_448"
#define API_NS(_id) cryptonite_decaf_448_##_id

#define hash_ctx_t   cryptonite_decaf_shake256_ctx_t
#define hash_init    cryptonite_decaf_shake256_init
#define hash_update  cryptonite_decaf_shake256_update
#define hash_final   cryptonite_decaf_shake256_final
#define hash_destroy cryptonite_decaf_shake256_destroy
#define hash_hash    cryptonite_decaf_shake256_hash

#define NO_CONTEXT CRYPTONITE_DECAF_EDDSA_448_SUPPORTS_CONTEXTLESS_SIGS
#define EDDSA_USE_SIGMA_ISOGENY 0
#define COFACTOR 4

#if NO_CONTEXT
const uint8_t CRYPTONITE_NO_CONTEXT_POINTS_HERE = 0;
const uint8_t * const CRYPTONITE_DECAF_ED448_NO_CONTEXT = &CRYPTONITE_NO_CONTEXT_POINTS_HERE;
#endif

/* EDDSA_BASE_POINT_RATIO = 1 or 2
 * Because EdDSA25519 is not on E_d but on the isogenous E_sigma_d,
 * its base point is twice ours.
 */
#define EDDSA_BASE_POINT_RATIO (1+EDDSA_USE_SIGMA_ISOGENY)

static void clamp (
    uint8_t secret_scalar_ser[CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES]
) {
    /* Blarg */
    secret_scalar_ser[0] &= -COFACTOR;
    uint8_t hibit = (1<<0)>>1;
    if (hibit == 0) {
        secret_scalar_ser[CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES - 1] = 0;
        secret_scalar_ser[CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES - 2] |= 0x80;
    } else {
        secret_scalar_ser[CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES - 1] &= hibit-1;
        secret_scalar_ser[CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES - 1] |= hibit;
    }
}

static void hash_init_with_dom(
    hash_ctx_t hash,
    uint8_t prehashed,
    uint8_t for_prehash,
    const uint8_t *context,
    uint8_t context_len
) {
    hash_init(hash);

#if NO_CONTEXT
    if (context_len == 0 && context == CRYPTONITE_DECAF_ED448_NO_CONTEXT) {
        (void)prehashed;
        (void)for_prehash;
        (void)context;
        (void)context_len;
        return;
    }
#endif
    const char *dom_s = "SigEd448";
    const uint8_t dom[2] = {2+word_is_zero(prehashed)+word_is_zero(for_prehash), context_len};
    hash_update(hash,(const unsigned char *)dom_s, strlen(dom_s));
    hash_update(hash,dom,2);
    hash_update(hash,context,context_len);
}

void cryptonite_decaf_ed448_prehash_init (
    hash_ctx_t hash
) {
    hash_init(hash);
}

/* In this file because it uses the hash */
void cryptonite_decaf_ed448_convert_private_key_to_x448 (
    uint8_t x[CRYPTONITE_DECAF_X448_PRIVATE_BYTES],
    const uint8_t ed[CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES]
) {
    /* pass the private key through hash_hash function */
    /* and keep the first CRYPTONITE_DECAF_X448_PRIVATE_BYTES bytes */
    hash_hash(
        x,
        CRYPTONITE_DECAF_X448_PRIVATE_BYTES,
        ed,
        CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES
    );
}
    
void cryptonite_decaf_ed448_derive_public_key (
    uint8_t pubkey[CRYPTONITE_DECAF_EDDSA_448_PUBLIC_BYTES],
    const uint8_t privkey[CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES]
) {
    /* only this much used for keygen */
    uint8_t secret_scalar_ser[CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES];
    
    hash_hash(
        secret_scalar_ser,
        sizeof(secret_scalar_ser),
        privkey,
        CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES
    );
    clamp(secret_scalar_ser);
        
    API_NS(scalar_t) secret_scalar;
    API_NS(scalar_decode_long)(secret_scalar, secret_scalar_ser, sizeof(secret_scalar_ser));
    
    /* Since we are going to mul_by_cofactor during encoding, divide by it here.
     * However, the EdDSA base point is not the same as the decaf base point if
     * the sigma isogeny is in use: the EdDSA base point is on Etwist_d/(1-d) and
     * the decaf base point is on Etwist_d, and when converted it effectively
     * picks up a factor of 2 from the isogenies.  So we might start at 2 instead of 1. 
     */
    for (unsigned int c = EDDSA_BASE_POINT_RATIO; c < COFACTOR; c <<= 1) {
        API_NS(scalar_halve)(secret_scalar,secret_scalar);
    }
    
    API_NS(point_t) p;
    API_NS(precomputed_scalarmul)(p,API_NS(precomputed_base),secret_scalar);
    
    API_NS(point_mul_by_cofactor_and_encode_like_eddsa)(pubkey, p);
        
    /* Cleanup */
    API_NS(scalar_destroy)(secret_scalar);
    API_NS(point_destroy)(p);
    cryptonite_decaf_bzero(secret_scalar_ser, sizeof(secret_scalar_ser));
}

void cryptonite_decaf_ed448_sign (
    uint8_t signature[CRYPTONITE_DECAF_EDDSA_448_SIGNATURE_BYTES],
    const uint8_t privkey[CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES],
    const uint8_t pubkey[CRYPTONITE_DECAF_EDDSA_448_PUBLIC_BYTES],
    const uint8_t *message,
    size_t message_len,
    uint8_t prehashed,
    const uint8_t *context,
    uint8_t context_len
) {
    API_NS(scalar_t) secret_scalar;
    hash_ctx_t hash;
    {
        /* Schedule the secret key */
        struct {
            uint8_t secret_scalar_ser[CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES];
            uint8_t seed[CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES];
        } __attribute__((packed)) expanded;
        hash_hash(
            (uint8_t *)&expanded,
            sizeof(expanded),
            privkey,
            CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES
        );
        clamp(expanded.secret_scalar_ser);   
        API_NS(scalar_decode_long)(secret_scalar, expanded.secret_scalar_ser, sizeof(expanded.secret_scalar_ser));
    
        /* Hash to create the nonce */
        hash_init_with_dom(hash,prehashed,0,context,context_len);
        hash_update(hash,expanded.seed,sizeof(expanded.seed));
        hash_update(hash,message,message_len);
        cryptonite_decaf_bzero(&expanded, sizeof(expanded));
    }
    
    /* Decode the nonce */
    API_NS(scalar_t) nonce_scalar;
    {
        uint8_t nonce[2*CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES];
        hash_final(hash,nonce,sizeof(nonce));
        API_NS(scalar_decode_long)(nonce_scalar, nonce, sizeof(nonce));
        cryptonite_decaf_bzero(nonce, sizeof(nonce));
    }
    
    uint8_t nonce_point[CRYPTONITE_DECAF_EDDSA_448_PUBLIC_BYTES] = {0};
    {
        /* Scalarmul to create the nonce-point */
        API_NS(scalar_t) nonce_scalar_2;
        API_NS(scalar_halve)(nonce_scalar_2,nonce_scalar);
        for (unsigned int c = 2*EDDSA_BASE_POINT_RATIO; c < COFACTOR; c <<= 1) {
            API_NS(scalar_halve)(nonce_scalar_2,nonce_scalar_2);
        }
        
        API_NS(point_t) p;
        API_NS(precomputed_scalarmul)(p,API_NS(precomputed_base),nonce_scalar_2);
        API_NS(point_mul_by_cofactor_and_encode_like_eddsa)(nonce_point, p);
        API_NS(point_destroy)(p);
        API_NS(scalar_destroy)(nonce_scalar_2);
    }
    
    API_NS(scalar_t) challenge_scalar;
    {
        /* Compute the challenge */
        hash_init_with_dom(hash,prehashed,0,context,context_len);
        hash_update(hash,nonce_point,sizeof(nonce_point));
        hash_update(hash,pubkey,CRYPTONITE_DECAF_EDDSA_448_PUBLIC_BYTES);
        hash_update(hash,message,message_len);
        uint8_t challenge[2*CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES];
        hash_final(hash,challenge,sizeof(challenge));
        hash_destroy(hash);
        API_NS(scalar_decode_long)(challenge_scalar,challenge,sizeof(challenge));
        cryptonite_decaf_bzero(challenge,sizeof(challenge));
    }
    
    API_NS(scalar_mul)(challenge_scalar,challenge_scalar,secret_scalar);
    API_NS(scalar_add)(challenge_scalar,challenge_scalar,nonce_scalar);
    
    cryptonite_decaf_bzero(signature,CRYPTONITE_DECAF_EDDSA_448_SIGNATURE_BYTES);
    memcpy(signature,nonce_point,sizeof(nonce_point));
    API_NS(scalar_encode)(&signature[CRYPTONITE_DECAF_EDDSA_448_PUBLIC_BYTES],challenge_scalar);
    
    API_NS(scalar_destroy)(secret_scalar);
    API_NS(scalar_destroy)(nonce_scalar);
    API_NS(scalar_destroy)(challenge_scalar);
}


void cryptonite_decaf_ed448_sign_prehash (
    uint8_t signature[CRYPTONITE_DECAF_EDDSA_448_SIGNATURE_BYTES],
    const uint8_t privkey[CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES],
    const uint8_t pubkey[CRYPTONITE_DECAF_EDDSA_448_PUBLIC_BYTES],
    const cryptonite_decaf_ed448_prehash_ctx_t hash,
    const uint8_t *context,
    uint8_t context_len
) {
    uint8_t hash_output[64]; /* MAGIC but true for all existing schemes */
    {
        cryptonite_decaf_ed448_prehash_ctx_t hash_too;
        memcpy(hash_too,hash,sizeof(hash_too));
        hash_final(hash_too,hash_output,sizeof(hash_output));
        hash_destroy(hash_too);
    }

    cryptonite_decaf_ed448_sign(signature,privkey,pubkey,hash_output,sizeof(hash_output),1,context,context_len);
    cryptonite_decaf_bzero(hash_output,sizeof(hash_output));
}

cryptonite_decaf_error_t cryptonite_decaf_ed448_verify (
    const uint8_t signature[CRYPTONITE_DECAF_EDDSA_448_SIGNATURE_BYTES],
    const uint8_t pubkey[CRYPTONITE_DECAF_EDDSA_448_PUBLIC_BYTES],
    const uint8_t *message,
    size_t message_len,
    uint8_t prehashed,
    const uint8_t *context,
    uint8_t context_len
) { 
    API_NS(point_t) pk_point, r_point;
    cryptonite_decaf_error_t error = API_NS(point_decode_like_eddsa_and_ignore_cofactor)(pk_point,pubkey);
    if (CRYPTONITE_DECAF_SUCCESS != error) { return error; }
    
    error = API_NS(point_decode_like_eddsa_and_ignore_cofactor)(r_point,signature);
    if (CRYPTONITE_DECAF_SUCCESS != error) { return error; }
    
    API_NS(scalar_t) challenge_scalar;
    {
        /* Compute the challenge */
        hash_ctx_t hash;
        hash_init_with_dom(hash,prehashed,0,context,context_len);
        hash_update(hash,signature,CRYPTONITE_DECAF_EDDSA_448_PUBLIC_BYTES);
        hash_update(hash,pubkey,CRYPTONITE_DECAF_EDDSA_448_PUBLIC_BYTES);
        hash_update(hash,message,message_len);
        uint8_t challenge[2*CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES];
        hash_final(hash,challenge,sizeof(challenge));
        hash_destroy(hash);
        API_NS(scalar_decode_long)(challenge_scalar,challenge,sizeof(challenge));
        cryptonite_decaf_bzero(challenge,sizeof(challenge));
    }
    API_NS(scalar_sub)(challenge_scalar, API_NS(scalar_zero), challenge_scalar);
    
    API_NS(scalar_t) response_scalar;
    API_NS(scalar_decode_long)(
        response_scalar,
        &signature[CRYPTONITE_DECAF_EDDSA_448_PUBLIC_BYTES],
        CRYPTONITE_DECAF_EDDSA_448_PRIVATE_BYTES
    );
#if EDDSA_BASE_POINT_RATIO == 2
    API_NS(scalar_add)(response_scalar,response_scalar,response_scalar);
#endif
    
    
    /* pk_point = -c(x(P)) + (cx + k)G = kG */
    API_NS(base_double_scalarmul_non_secret)(
        pk_point,
        response_scalar,
        pk_point,
        challenge_scalar
    );
    return cryptonite_decaf_succeed_if(API_NS(point_eq(pk_point,r_point)));
}


cryptonite_decaf_error_t cryptonite_decaf_ed448_verify_prehash (
    const uint8_t signature[CRYPTONITE_DECAF_EDDSA_448_SIGNATURE_BYTES],
    const uint8_t pubkey[CRYPTONITE_DECAF_EDDSA_448_PUBLIC_BYTES],
    const cryptonite_decaf_ed448_prehash_ctx_t hash,
    const uint8_t *context,
    uint8_t context_len
) {
    cryptonite_decaf_error_t ret;
    
    uint8_t hash_output[64]; /* MAGIC but true for all existing schemes */
    {
        cryptonite_decaf_ed448_prehash_ctx_t hash_too;
        memcpy(hash_too,hash,sizeof(hash_too));
        hash_final(hash_too,hash_output,sizeof(hash_output));
        hash_destroy(hash_too);
    }
    
    ret = cryptonite_decaf_ed448_verify(signature,pubkey,hash_output,sizeof(hash_output),1,context,context_len);
    
    return ret;
}
