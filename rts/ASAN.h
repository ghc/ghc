#pragma once

/* Note [AddressSanitizer instrumentation]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * AddressSanitizer (ASAN) is a sanitizer tool for catching out-of-bounds and
 * use-after-free accesses in C/C++. We currently provide minimal
 * instrumentation for pinned arrays, allowing ASAN to catch bad accesses to
 * buffers allocated in Haskell and passed to native libraries via FFI.
 *
 * We do this by poisoning all MBlocks as they are committed and then
 * unpoisoning pinned arrays as they are allocated.
 *
 * We do *not* support building the RTS itself with ASAN.
 */

__attribute__((weak)) extern void __asan_poison_memory_region(const void* addr, size_t sz);
__attribute__((weak)) extern void __asan_unpoison_memory_region(const void* addr, size_t sz);

INLINE_HEADER void poisonMemoryRegion(void *addr, size_t sz) {
    if (__asan_poison_memory_region) {
        __asan_poison_memory_region(addr, sz);
    }
}

INLINE_HEADER void unpoisonMemoryRegion(void *addr, size_t sz) {
    if (__asan_unpoison_memory_region) {
        __asan_unpoison_memory_region(addr, sz);
    }
}
