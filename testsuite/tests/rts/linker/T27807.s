/* A b or bl branch instruction on aarch64 can reach +/-2^27 bytes.
 * A branch to a target further away requires a range extension veneer.
 *
 * We test that the RTS linker correctly builds a veneer when needed, including
 * any addend.
 *
 * The target is padded on both sides by more than 2^27 bytes, to ensure the use
 * of a veneer, no matter where the linker places the text.far section (above or
 * below the .text section).
 *
 * We create two entry points, at 0 and +24 to exercise the addend logic,
 * testing the return value.
 */

        .text
        .globl  probe
probe:
        b       target+24

        .section .text.far,"ax",@progbits

        .space  (1 << 27) + (1 << 20)

        .globl  target
target:
        mov     w0, #1
        ret
        .space  16
        mov     w0, #2
        ret

        .space  (1 << 27) + (1 << 20)
