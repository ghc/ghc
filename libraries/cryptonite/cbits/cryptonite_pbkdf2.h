#ifndef CRYPTONITE_PBKDF2_H_
#define CRYPTONITE_PBKDF2_H_

#include <stdlib.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

void cryptonite_fastpbkdf2_hmac_sha1( const uint8_t *pw, size_t npw
                                    , const uint8_t *salt, size_t nsalt
                                    , uint32_t iterations
                                    , uint8_t *out, size_t nout
                                    );
void cryptonite_fastpbkdf2_hmac_sha256( const uint8_t *pw, size_t npw
                                      , const uint8_t *salt, size_t nsalt
                                      , uint32_t iterations
                                      , uint8_t *out, size_t nout
                                      );
void cryptonite_fastpbkdf2_hmac_sha512( const uint8_t *pw, size_t npw
                                      , const uint8_t *salt, size_t nsalt
                                      , uint32_t iterations
                                      , uint8_t *out, size_t nout
                                      );

#ifdef __cplusplus
}
#endif

#endif
