/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2012
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#pragma once

/*
 * We use the m32 allocator for symbol extras on Windows and other mmap-using
 * platforms.
 */
#if RTS_LINKER_USE_MMAP
#define NEED_M32 1
#endif

#include "BeginPrivate.h"

#if defined(NEED_M32)
#define M32_NO_RETURN    /* Nothing */
#else
#define M32_NO_RETURN    GNUC3_ATTRIBUTE(__noreturn__)
#endif

struct m32_allocator_t;
typedef struct m32_allocator_t m32_allocator;

m32_allocator *m32_allocator_new(bool executable) M32_NO_RETURN;

void m32_allocator_free(m32_allocator *alloc) M32_NO_RETURN;

void m32_allocator_flush(m32_allocator *alloc) M32_NO_RETURN;

void * m32_alloc(m32_allocator *alloc, size_t size, size_t alignment) M32_NO_RETURN;

#include "EndPrivate.h"
