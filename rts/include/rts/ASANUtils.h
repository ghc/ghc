#pragma once

#if defined(__SANITIZE_ADDRESS__)
#define ASAN_ENABLED
#elif defined(__has_feature)
#if __has_feature(address_sanitizer)
#define ASAN_ENABLED
#endif
#endif

#if defined(ASAN_ENABLED)
#include <sanitizer/asan_interface.h>
#else

#include <stddef.h>

static inline void __asan_poison_memory_region(void const volatile *addr
                                               __attribute__((unused)),
                                               size_t size
                                               __attribute__((unused))) {}

static inline void __asan_unpoison_memory_region(void const volatile *addr
                                                 __attribute__((unused)),
                                                 size_t size
                                                 __attribute__((unused))) {}

#endif
