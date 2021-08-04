#ifndef CRYPTONITE_ALIGN_H
#define CRYPTONITE_ALIGN_H

#include "cryptonite_bitfn.h"

#if (defined(__i386__))
# define UNALIGNED_ACCESS_OK
#elif defined(__x86_64__)
# define UNALIGNED_ACCESS_OK
#else
# define UNALIGNED_ACCESS_FAULT
#endif

/* n need to be power of 2.
 * IS_ALIGNED(p,8) */
#define IS_ALIGNED(p,alignment) (((uintptr_t) (p)) & ((alignment)-1))

#ifdef WITH_ASSERT_ALIGNMENT
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
# define ASSERT_ALIGNMENT(up, alignment) \
	do { if (IS_ALIGNED(up, alignment)) \
	{ printf("ALIGNMENT-ASSERT-FAILURE: %s:%d: ptr=%p alignment=%d\n", __FILE__, __LINE__, (void *) up, (alignment)); \
	  exit(99); \
	}; } while (0)
#else
# define ASSERT_ALIGNMENT(p, n) do {} while (0)
#endif

#ifdef UNALIGNED_ACCESS_OK
#define need_alignment(p,n) (0)
#else
#define need_alignment(p,n) IS_ALIGNED(p,n)
#endif

static inline uint32_t load_le32_aligned(const uint8_t *p)
{
	return le32_to_cpu(*((uint32_t *) p));		
}

static inline void store_le32_aligned(uint8_t *dst, const uint32_t v)
{
	*((uint32_t *) dst) = cpu_to_le32(v);
}

static inline void xor_le32_aligned(uint8_t *dst, const uint32_t v)
{
	*((uint32_t *) dst) ^= cpu_to_le32(v);
}

static inline void store_be32_aligned(uint8_t *dst, const uint32_t v)
{
	*((uint32_t *) dst) = cpu_to_be32(v);
}

static inline void xor_be32_aligned(uint8_t *dst, const uint32_t v)
{
	*((uint32_t *) dst) ^= cpu_to_be32(v);
}

static inline void store_le64_aligned(uint8_t *dst, const uint64_t v)
{
	*((uint64_t *) dst) = cpu_to_le64(v);
}

static inline void store_be64_aligned(uint8_t *dst, const uint64_t v)
{
	*((uint64_t *) dst) = cpu_to_be64(v);
}

static inline void xor_be64_aligned(uint8_t *dst, const uint64_t v)
{
	*((uint64_t *) dst) ^= cpu_to_be64(v);
}

#ifdef UNALIGNED_ACCESS_OK
#define load_le32(a) load_le32_aligned(a)
#else
static inline uint32_t load_le32(const uint8_t *p)
{
	return ((uint32_t)p[0]) | ((uint32_t)p[1] <<  8) | ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24);
}
#endif

#ifdef UNALIGNED_ACCESS_OK
#define store_le32(a, b) store_le32_aligned(a, b)
#define xor_le32(a, b) xor_le32_aligned(a, b)
#else
static inline void store_le32(uint8_t *dst, const uint32_t v)
{
	dst[0] = v; dst[1] = v >> 8; dst[2] = v >> 16; dst[3] = v >> 24;
}
static inline void xor_le32(uint8_t *dst, const uint32_t v)
{
	dst[0] ^= v; dst[1] ^= v >> 8; dst[2] ^= v >> 16; dst[3] ^= v >> 24;
}
#endif

#ifdef UNALIGNED_ACCESS_OK
#define store_be32(a, b) store_be32_aligned(a, b)
#define xor_be32(a, b) xor_be32_aligned(a, b)
#else
static inline void store_be32(uint8_t *dst, const uint32_t v)
{
	dst[3] = v; dst[2] = v >> 8; dst[1] = v >> 16; dst[0] = v >> 24;
}
static inline void xor_be32(uint8_t *dst, const uint32_t v)
{
	dst[3] ^= v; dst[2] ^= v >> 8; dst[1] ^= v >> 16; dst[0] ^= v >> 24;
}
#endif

#ifdef UNALIGNED_ACCESS_OK
#define store_le64(a, b) store_le64_aligned(a, b)
#else
static inline void store_le64(uint8_t *dst, const uint64_t v)
{
	dst[0] = v      ; dst[1] = v >> 8 ; dst[2] = v >> 16; dst[3] = v >> 24;
	dst[4] = v >> 32; dst[5] = v >> 40; dst[6] = v >> 48; dst[7] = v >> 56;
}
#endif

#ifdef UNALIGNED_ACCESS_OK
#define store_be64(a, b) store_be64_aligned(a, b)
#define xor_be64(a, b) xor_be64_aligned(a, b)
#else
static inline void store_be64(uint8_t *dst, const uint64_t v)
{
	dst[7] = v      ; dst[6] = v >> 8 ; dst[5] = v >> 16; dst[4] = v >> 24;
	dst[3] = v >> 32; dst[2] = v >> 40; dst[1] = v >> 48; dst[0] = v >> 56;
}
static inline void xor_be64(uint8_t *dst, const uint64_t v)
{
	dst[7] ^= v      ; dst[6] ^= v >> 8 ; dst[5] ^= v >> 16; dst[4] ^= v >> 24;
	dst[3] ^= v >> 32; dst[2] ^= v >> 40; dst[1] ^= v >> 48; dst[0] ^= v >> 56;
}
#endif

#endif
