/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1994-2000
 *
 * Hashing functions based on:
 *
 *   "Fast Hashing of Variable Length Text Strings"
 *    Peter K. Pearson, CACM June 1990
 *
 * They return a 32 bit value containing 16 bits of hash value.
 *
 * ---------------------------------------------------------------------------*/

typedef unsigned long hash_t;

hash_t hash_str(char *str);
hash_t hash_fixed(char *data, nat len);
