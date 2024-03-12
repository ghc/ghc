/*
 * Declarations and RTS symbol table entries for the outline atomics
 * symbols provided by some ARMv8 compilers.
 *
 * See Note [ARM outline atomics and the RTS linker] in m4/fp_armv8_outline_atomics.m4.
 *
 * See #22012.
 */

#include <stdint.h>
#include <stdatomic.h>

uint8_t ghc___aarch64_cas1_relax(uint8_t old, uint8_t new, _Atomic uint8_t* p);
uint8_t ghc___aarch64_cas1_relax(uint8_t old, uint8_t new, _Atomic uint8_t* p) {
  atomic_compare_exchange_strong_explicit(p, &old, new, memory_order_relaxed, memory_order_relaxed); return old;
}

uint8_t ghc___aarch64_cas1_acq(uint8_t old, uint8_t new, _Atomic uint8_t* p);
uint8_t ghc___aarch64_cas1_acq(uint8_t old, uint8_t new, _Atomic uint8_t* p) {
  atomic_compare_exchange_strong_explicit(p, &old, new, memory_order_acquire, memory_order_acquire); return old;
}

uint8_t ghc___aarch64_cas1_acq_rel(uint8_t old, uint8_t new, _Atomic uint8_t* p);
uint8_t ghc___aarch64_cas1_acq_rel(uint8_t old, uint8_t new, _Atomic uint8_t* p) {
  atomic_compare_exchange_strong_explicit(p, &old, new, memory_order_acq_rel, memory_order_acquire); return old;
}

uint8_t ghc___aarch64_cas1_sync(uint8_t old, uint8_t new, _Atomic uint8_t* p);
uint8_t ghc___aarch64_cas1_sync(uint8_t old, uint8_t new, _Atomic uint8_t* p) {
  atomic_compare_exchange_strong_explicit(p, &old, new, memory_order_seq_cst, memory_order_seq_cst); return old;
}

uint16_t ghc___aarch64_cas2_relax(uint16_t old, uint16_t new, _Atomic uint16_t* p);
uint16_t ghc___aarch64_cas2_relax(uint16_t old, uint16_t new, _Atomic uint16_t* p) {
  atomic_compare_exchange_strong_explicit(p, &old, new, memory_order_relaxed, memory_order_relaxed); return old;
}

uint16_t ghc___aarch64_cas2_acq(uint16_t old, uint16_t new, _Atomic uint16_t* p);
uint16_t ghc___aarch64_cas2_acq(uint16_t old, uint16_t new, _Atomic uint16_t* p) {
  atomic_compare_exchange_strong_explicit(p, &old, new, memory_order_acquire, memory_order_acquire); return old;
}

uint16_t ghc___aarch64_cas2_acq_rel(uint16_t old, uint16_t new, _Atomic uint16_t* p);
uint16_t ghc___aarch64_cas2_acq_rel(uint16_t old, uint16_t new, _Atomic uint16_t* p) {
  atomic_compare_exchange_strong_explicit(p, &old, new, memory_order_acq_rel, memory_order_acquire); return old;
}

uint16_t ghc___aarch64_cas2_sync(uint16_t old, uint16_t new, _Atomic uint16_t* p);
uint16_t ghc___aarch64_cas2_sync(uint16_t old, uint16_t new, _Atomic uint16_t* p) {
  atomic_compare_exchange_strong_explicit(p, &old, new, memory_order_seq_cst, memory_order_seq_cst); return old;
}

uint32_t ghc___aarch64_cas4_relax(uint32_t old, uint32_t new, _Atomic uint32_t* p);
uint32_t ghc___aarch64_cas4_relax(uint32_t old, uint32_t new, _Atomic uint32_t* p) {
  atomic_compare_exchange_strong_explicit(p, &old, new, memory_order_relaxed, memory_order_relaxed); return old;
}

uint32_t ghc___aarch64_cas4_acq(uint32_t old, uint32_t new, _Atomic uint32_t* p);
uint32_t ghc___aarch64_cas4_acq(uint32_t old, uint32_t new, _Atomic uint32_t* p) {
  atomic_compare_exchange_strong_explicit(p, &old, new, memory_order_acquire, memory_order_acquire); return old;
}

uint32_t ghc___aarch64_cas4_acq_rel(uint32_t old, uint32_t new, _Atomic uint32_t* p);
uint32_t ghc___aarch64_cas4_acq_rel(uint32_t old, uint32_t new, _Atomic uint32_t* p) {
  atomic_compare_exchange_strong_explicit(p, &old, new, memory_order_acq_rel, memory_order_acquire); return old;
}

uint32_t ghc___aarch64_cas4_sync(uint32_t old, uint32_t new, _Atomic uint32_t* p);
uint32_t ghc___aarch64_cas4_sync(uint32_t old, uint32_t new, _Atomic uint32_t* p) {
  atomic_compare_exchange_strong_explicit(p, &old, new, memory_order_seq_cst, memory_order_seq_cst); return old;
}

uint64_t ghc___aarch64_cas8_relax(uint64_t old, uint64_t new, _Atomic uint64_t* p);
uint64_t ghc___aarch64_cas8_relax(uint64_t old, uint64_t new, _Atomic uint64_t* p) {
  atomic_compare_exchange_strong_explicit(p, &old, new, memory_order_relaxed, memory_order_relaxed); return old;
}

uint64_t ghc___aarch64_cas8_acq(uint64_t old, uint64_t new, _Atomic uint64_t* p);
uint64_t ghc___aarch64_cas8_acq(uint64_t old, uint64_t new, _Atomic uint64_t* p) {
  atomic_compare_exchange_strong_explicit(p, &old, new, memory_order_acquire, memory_order_acquire); return old;
}

uint64_t ghc___aarch64_cas8_acq_rel(uint64_t old, uint64_t new, _Atomic uint64_t* p);
uint64_t ghc___aarch64_cas8_acq_rel(uint64_t old, uint64_t new, _Atomic uint64_t* p) {
  atomic_compare_exchange_strong_explicit(p, &old, new, memory_order_acq_rel, memory_order_acquire); return old;
}

uint64_t ghc___aarch64_cas8_sync(uint64_t old, uint64_t new, _Atomic uint64_t* p);
uint64_t ghc___aarch64_cas8_sync(uint64_t old, uint64_t new, _Atomic uint64_t* p) {
  atomic_compare_exchange_strong_explicit(p, &old, new, memory_order_seq_cst, memory_order_seq_cst); return old;
}

uint8_t ghc___aarch64_swp1_relax(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_swp1_relax(uint8_t v, _Atomic uint8_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_relaxed);
}

uint8_t ghc___aarch64_swp1_acq(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_swp1_acq(uint8_t v, _Atomic uint8_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_acquire);
}

uint8_t ghc___aarch64_swp1_rel(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_swp1_rel(uint8_t v, _Atomic uint8_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_release);
}

uint8_t ghc___aarch64_swp1_acq_rel(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_swp1_acq_rel(uint8_t v, _Atomic uint8_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_acq_rel);
}

uint8_t ghc___aarch64_swp1_sync(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_swp1_sync(uint8_t v, _Atomic uint8_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_seq_cst);
}

uint16_t ghc___aarch64_swp2_relax(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_swp2_relax(uint16_t v, _Atomic uint16_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_relaxed);
}

uint16_t ghc___aarch64_swp2_acq(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_swp2_acq(uint16_t v, _Atomic uint16_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_acquire);
}

uint16_t ghc___aarch64_swp2_rel(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_swp2_rel(uint16_t v, _Atomic uint16_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_release);
}

uint16_t ghc___aarch64_swp2_acq_rel(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_swp2_acq_rel(uint16_t v, _Atomic uint16_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_acq_rel);
}

uint16_t ghc___aarch64_swp2_sync(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_swp2_sync(uint16_t v, _Atomic uint16_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_seq_cst);
}

uint32_t ghc___aarch64_swp4_relax(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_swp4_relax(uint32_t v, _Atomic uint32_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_relaxed);
}

uint32_t ghc___aarch64_swp4_acq(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_swp4_acq(uint32_t v, _Atomic uint32_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_acquire);
}

uint32_t ghc___aarch64_swp4_rel(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_swp4_rel(uint32_t v, _Atomic uint32_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_release);
}

uint32_t ghc___aarch64_swp4_acq_rel(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_swp4_acq_rel(uint32_t v, _Atomic uint32_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_acq_rel);
}

uint32_t ghc___aarch64_swp4_sync(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_swp4_sync(uint32_t v, _Atomic uint32_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_seq_cst);
}

uint64_t ghc___aarch64_swp8_relax(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_swp8_relax(uint64_t v, _Atomic uint64_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_relaxed);
}

uint64_t ghc___aarch64_swp8_acq(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_swp8_acq(uint64_t v, _Atomic uint64_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_acquire);
}

uint64_t ghc___aarch64_swp8_rel(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_swp8_rel(uint64_t v, _Atomic uint64_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_release);
}

uint64_t ghc___aarch64_swp8_acq_rel(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_swp8_acq_rel(uint64_t v, _Atomic uint64_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_acq_rel);
}

uint64_t ghc___aarch64_swp8_sync(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_swp8_sync(uint64_t v, _Atomic uint64_t* p) {
  return atomic_exchange_explicit(p, v, memory_order_seq_cst);
}

uint8_t ghc___aarch64_ldadd1_relax(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldadd1_relax(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_relaxed);
}

uint8_t ghc___aarch64_ldadd1_acq(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldadd1_acq(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_acquire);
}

uint8_t ghc___aarch64_ldadd1_rel(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldadd1_rel(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_release);
}

uint8_t ghc___aarch64_ldadd1_acq_rel(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldadd1_acq_rel(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_acq_rel);
}

uint8_t ghc___aarch64_ldadd1_sync(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldadd1_sync(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_seq_cst);
}

uint16_t ghc___aarch64_ldadd2_relax(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldadd2_relax(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_relaxed);
}

uint16_t ghc___aarch64_ldadd2_acq(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldadd2_acq(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_acquire);
}

uint16_t ghc___aarch64_ldadd2_rel(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldadd2_rel(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_release);
}

uint16_t ghc___aarch64_ldadd2_acq_rel(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldadd2_acq_rel(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_acq_rel);
}

uint16_t ghc___aarch64_ldadd2_sync(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldadd2_sync(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_seq_cst);
}

uint32_t ghc___aarch64_ldadd4_relax(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldadd4_relax(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_relaxed);
}

uint32_t ghc___aarch64_ldadd4_acq(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldadd4_acq(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_acquire);
}

uint32_t ghc___aarch64_ldadd4_rel(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldadd4_rel(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_release);
}

uint32_t ghc___aarch64_ldadd4_acq_rel(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldadd4_acq_rel(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_acq_rel);
}

uint32_t ghc___aarch64_ldadd4_sync(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldadd4_sync(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_seq_cst);
}

uint64_t ghc___aarch64_ldadd8_relax(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldadd8_relax(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_relaxed);
}

uint64_t ghc___aarch64_ldadd8_acq(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldadd8_acq(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_acquire);
}

uint64_t ghc___aarch64_ldadd8_rel(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldadd8_rel(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_release);
}

uint64_t ghc___aarch64_ldadd8_acq_rel(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldadd8_acq_rel(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_acq_rel);
}

uint64_t ghc___aarch64_ldadd8_sync(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldadd8_sync(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_add_explicit(p, v, memory_order_seq_cst);
}

uint8_t ghc___aarch64_ldclr1_relax(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldclr1_relax(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_relaxed);
}

uint8_t ghc___aarch64_ldclr1_acq(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldclr1_acq(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_acquire);
}

uint8_t ghc___aarch64_ldclr1_rel(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldclr1_rel(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_release);
}

uint8_t ghc___aarch64_ldclr1_acq_rel(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldclr1_acq_rel(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_acq_rel);
}

uint8_t ghc___aarch64_ldclr1_sync(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldclr1_sync(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_seq_cst);
}

uint16_t ghc___aarch64_ldclr2_relax(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldclr2_relax(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_relaxed);
}

uint16_t ghc___aarch64_ldclr2_acq(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldclr2_acq(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_acquire);
}

uint16_t ghc___aarch64_ldclr2_rel(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldclr2_rel(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_release);
}

uint16_t ghc___aarch64_ldclr2_acq_rel(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldclr2_acq_rel(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_acq_rel);
}

uint16_t ghc___aarch64_ldclr2_sync(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldclr2_sync(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_seq_cst);
}

uint32_t ghc___aarch64_ldclr4_relax(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldclr4_relax(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_relaxed);
}

uint32_t ghc___aarch64_ldclr4_acq(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldclr4_acq(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_acquire);
}

uint32_t ghc___aarch64_ldclr4_rel(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldclr4_rel(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_release);
}

uint32_t ghc___aarch64_ldclr4_acq_rel(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldclr4_acq_rel(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_acq_rel);
}

uint32_t ghc___aarch64_ldclr4_sync(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldclr4_sync(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_seq_cst);
}

uint64_t ghc___aarch64_ldclr8_relax(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldclr8_relax(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_relaxed);
}

uint64_t ghc___aarch64_ldclr8_acq(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldclr8_acq(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_acquire);
}

uint64_t ghc___aarch64_ldclr8_rel(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldclr8_rel(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_release);
}

uint64_t ghc___aarch64_ldclr8_acq_rel(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldclr8_acq_rel(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_acq_rel);
}

uint64_t ghc___aarch64_ldclr8_sync(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldclr8_sync(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_and_explicit(p, v, memory_order_seq_cst);
}

uint8_t ghc___aarch64_ldeor1_relax(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldeor1_relax(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_relaxed);
}

uint8_t ghc___aarch64_ldeor1_acq(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldeor1_acq(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_acquire);
}

uint8_t ghc___aarch64_ldeor1_rel(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldeor1_rel(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_release);
}

uint8_t ghc___aarch64_ldeor1_acq_rel(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldeor1_acq_rel(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_acq_rel);
}

uint8_t ghc___aarch64_ldeor1_sync(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldeor1_sync(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_seq_cst);
}

uint16_t ghc___aarch64_ldeor2_relax(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldeor2_relax(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_relaxed);
}

uint16_t ghc___aarch64_ldeor2_acq(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldeor2_acq(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_acquire);
}

uint16_t ghc___aarch64_ldeor2_rel(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldeor2_rel(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_release);
}

uint16_t ghc___aarch64_ldeor2_acq_rel(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldeor2_acq_rel(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_acq_rel);
}

uint16_t ghc___aarch64_ldeor2_sync(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldeor2_sync(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_seq_cst);
}

uint32_t ghc___aarch64_ldeor4_relax(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldeor4_relax(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_relaxed);
}

uint32_t ghc___aarch64_ldeor4_acq(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldeor4_acq(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_acquire);
}

uint32_t ghc___aarch64_ldeor4_rel(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldeor4_rel(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_release);
}

uint32_t ghc___aarch64_ldeor4_acq_rel(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldeor4_acq_rel(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_acq_rel);
}

uint32_t ghc___aarch64_ldeor4_sync(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldeor4_sync(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_seq_cst);
}

uint64_t ghc___aarch64_ldeor8_relax(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldeor8_relax(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_relaxed);
}

uint64_t ghc___aarch64_ldeor8_acq(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldeor8_acq(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_acquire);
}

uint64_t ghc___aarch64_ldeor8_rel(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldeor8_rel(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_release);
}

uint64_t ghc___aarch64_ldeor8_acq_rel(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldeor8_acq_rel(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_acq_rel);
}

uint64_t ghc___aarch64_ldeor8_sync(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldeor8_sync(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_xor_explicit(p, v, memory_order_seq_cst);
}

uint8_t ghc___aarch64_ldset1_relax(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldset1_relax(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_relaxed);
}

uint8_t ghc___aarch64_ldset1_acq(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldset1_acq(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_acquire);
}

uint8_t ghc___aarch64_ldset1_rel(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldset1_rel(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_release);
}

uint8_t ghc___aarch64_ldset1_acq_rel(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldset1_acq_rel(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_acq_rel);
}

uint8_t ghc___aarch64_ldset1_sync(uint8_t v, _Atomic uint8_t* p);
uint8_t ghc___aarch64_ldset1_sync(uint8_t v, _Atomic uint8_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_seq_cst);
}

uint16_t ghc___aarch64_ldset2_relax(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldset2_relax(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_relaxed);
}

uint16_t ghc___aarch64_ldset2_acq(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldset2_acq(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_acquire);
}

uint16_t ghc___aarch64_ldset2_rel(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldset2_rel(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_release);
}

uint16_t ghc___aarch64_ldset2_acq_rel(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldset2_acq_rel(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_acq_rel);
}

uint16_t ghc___aarch64_ldset2_sync(uint16_t v, _Atomic uint16_t* p);
uint16_t ghc___aarch64_ldset2_sync(uint16_t v, _Atomic uint16_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_seq_cst);
}

uint32_t ghc___aarch64_ldset4_relax(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldset4_relax(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_relaxed);
}

uint32_t ghc___aarch64_ldset4_acq(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldset4_acq(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_acquire);
}

uint32_t ghc___aarch64_ldset4_rel(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldset4_rel(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_release);
}

uint32_t ghc___aarch64_ldset4_acq_rel(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldset4_acq_rel(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_acq_rel);
}

uint32_t ghc___aarch64_ldset4_sync(uint32_t v, _Atomic uint32_t* p);
uint32_t ghc___aarch64_ldset4_sync(uint32_t v, _Atomic uint32_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_seq_cst);
}

uint64_t ghc___aarch64_ldset8_relax(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldset8_relax(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_relaxed);
}

uint64_t ghc___aarch64_ldset8_acq(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldset8_acq(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_acquire);
}

uint64_t ghc___aarch64_ldset8_rel(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldset8_rel(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_release);
}

uint64_t ghc___aarch64_ldset8_acq_rel(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldset8_acq_rel(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_acq_rel);
}

uint64_t ghc___aarch64_ldset8_sync(uint64_t v, _Atomic uint64_t* p);
uint64_t ghc___aarch64_ldset8_sync(uint64_t v, _Atomic uint64_t* p) {
  return atomic_fetch_or_explicit(p, v, memory_order_seq_cst);
}


#define RTS_ARM_OUTLINE_ATOMIC_SYMBOLS \
    SymI_HasProto_redirect(__aarch64_cas1_relax, ghc___aarch64_cas1_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_cas1_acq, ghc___aarch64_cas1_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_cas1_acq_rel, ghc___aarch64_cas1_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_cas1_sync, ghc___aarch64_cas1_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_cas2_relax, ghc___aarch64_cas2_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_cas2_acq, ghc___aarch64_cas2_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_cas2_acq_rel, ghc___aarch64_cas2_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_cas2_sync, ghc___aarch64_cas2_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_cas4_relax, ghc___aarch64_cas4_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_cas4_acq, ghc___aarch64_cas4_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_cas4_acq_rel, ghc___aarch64_cas4_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_cas4_sync, ghc___aarch64_cas4_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_cas8_relax, ghc___aarch64_cas8_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_cas8_acq, ghc___aarch64_cas8_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_cas8_acq_rel, ghc___aarch64_cas8_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_cas8_sync, ghc___aarch64_cas8_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp1_relax, ghc___aarch64_swp1_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp1_acq, ghc___aarch64_swp1_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp1_rel, ghc___aarch64_swp1_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp1_acq_rel, ghc___aarch64_swp1_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp1_sync, ghc___aarch64_swp1_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp2_relax, ghc___aarch64_swp2_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp2_acq, ghc___aarch64_swp2_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp2_rel, ghc___aarch64_swp2_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp2_acq_rel, ghc___aarch64_swp2_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp2_sync, ghc___aarch64_swp2_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp4_relax, ghc___aarch64_swp4_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp4_acq, ghc___aarch64_swp4_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp4_rel, ghc___aarch64_swp4_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp4_acq_rel, ghc___aarch64_swp4_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp4_sync, ghc___aarch64_swp4_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp8_relax, ghc___aarch64_swp8_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp8_acq, ghc___aarch64_swp8_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp8_rel, ghc___aarch64_swp8_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp8_acq_rel, ghc___aarch64_swp8_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_swp8_sync, ghc___aarch64_swp8_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd1_relax, ghc___aarch64_ldadd1_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd1_acq, ghc___aarch64_ldadd1_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd1_rel, ghc___aarch64_ldadd1_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd1_acq_rel, ghc___aarch64_ldadd1_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd1_sync, ghc___aarch64_ldadd1_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd2_relax, ghc___aarch64_ldadd2_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd2_acq, ghc___aarch64_ldadd2_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd2_rel, ghc___aarch64_ldadd2_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd2_acq_rel, ghc___aarch64_ldadd2_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd2_sync, ghc___aarch64_ldadd2_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd4_relax, ghc___aarch64_ldadd4_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd4_acq, ghc___aarch64_ldadd4_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd4_rel, ghc___aarch64_ldadd4_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd4_acq_rel, ghc___aarch64_ldadd4_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd4_sync, ghc___aarch64_ldadd4_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd8_relax, ghc___aarch64_ldadd8_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd8_acq, ghc___aarch64_ldadd8_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd8_rel, ghc___aarch64_ldadd8_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd8_acq_rel, ghc___aarch64_ldadd8_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldadd8_sync, ghc___aarch64_ldadd8_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr1_relax, ghc___aarch64_ldclr1_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr1_acq, ghc___aarch64_ldclr1_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr1_rel, ghc___aarch64_ldclr1_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr1_acq_rel, ghc___aarch64_ldclr1_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr1_sync, ghc___aarch64_ldclr1_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr2_relax, ghc___aarch64_ldclr2_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr2_acq, ghc___aarch64_ldclr2_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr2_rel, ghc___aarch64_ldclr2_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr2_acq_rel, ghc___aarch64_ldclr2_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr2_sync, ghc___aarch64_ldclr2_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr4_relax, ghc___aarch64_ldclr4_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr4_acq, ghc___aarch64_ldclr4_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr4_rel, ghc___aarch64_ldclr4_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr4_acq_rel, ghc___aarch64_ldclr4_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr4_sync, ghc___aarch64_ldclr4_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr8_relax, ghc___aarch64_ldclr8_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr8_acq, ghc___aarch64_ldclr8_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr8_rel, ghc___aarch64_ldclr8_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr8_acq_rel, ghc___aarch64_ldclr8_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldclr8_sync, ghc___aarch64_ldclr8_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor1_relax, ghc___aarch64_ldeor1_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor1_acq, ghc___aarch64_ldeor1_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor1_rel, ghc___aarch64_ldeor1_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor1_acq_rel, ghc___aarch64_ldeor1_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor1_sync, ghc___aarch64_ldeor1_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor2_relax, ghc___aarch64_ldeor2_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor2_acq, ghc___aarch64_ldeor2_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor2_rel, ghc___aarch64_ldeor2_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor2_acq_rel, ghc___aarch64_ldeor2_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor2_sync, ghc___aarch64_ldeor2_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor4_relax, ghc___aarch64_ldeor4_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor4_acq, ghc___aarch64_ldeor4_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor4_rel, ghc___aarch64_ldeor4_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor4_acq_rel, ghc___aarch64_ldeor4_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor4_sync, ghc___aarch64_ldeor4_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor8_relax, ghc___aarch64_ldeor8_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor8_acq, ghc___aarch64_ldeor8_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor8_rel, ghc___aarch64_ldeor8_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor8_acq_rel, ghc___aarch64_ldeor8_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldeor8_sync, ghc___aarch64_ldeor8_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset1_relax, ghc___aarch64_ldset1_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset1_acq, ghc___aarch64_ldset1_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset1_rel, ghc___aarch64_ldset1_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset1_acq_rel, ghc___aarch64_ldset1_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset1_sync, ghc___aarch64_ldset1_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset2_relax, ghc___aarch64_ldset2_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset2_acq, ghc___aarch64_ldset2_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset2_rel, ghc___aarch64_ldset2_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset2_acq_rel, ghc___aarch64_ldset2_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset2_sync, ghc___aarch64_ldset2_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset4_relax, ghc___aarch64_ldset4_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset4_acq, ghc___aarch64_ldset4_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset4_rel, ghc___aarch64_ldset4_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset4_acq_rel, ghc___aarch64_ldset4_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset4_sync, ghc___aarch64_ldset4_sync, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset8_relax, ghc___aarch64_ldset8_relax, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset8_acq, ghc___aarch64_ldset8_acq, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset8_rel, ghc___aarch64_ldset8_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset8_acq_rel, ghc___aarch64_ldset8_acq_rel, STRENGTH_STRONG, SYM_TYPE_CODE) \
    SymI_HasProto_redirect(__aarch64_ldset8_sync, ghc___aarch64_ldset8_sync, STRENGTH_STRONG, SYM_TYPE_CODE)
