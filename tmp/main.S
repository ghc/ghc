        .attribute      4, 16
        .attribute      5, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0_zmmul1p0_zaamo1p0_zalrsc1p0_zca1p0_zcd1p0"
        .file   "example.c"
        .option push
        .option arch, +v, +zve32f, +zve32x, +zve64d, +zve64f, +zve64x, +zvl128b, +zvl32b, +zvl64b
        .text
        .globl  foo                             # -- Begin function foo
        .p2align        1
        .type   foo,@function
        .variant_cc     foo
foo:                                    # @foo
        .cfi_startproc
# %bb.0:
        la a0, base_idx
        vsetivli        zero, 2, e64, m1, ta, ma
        vle16.v v10, (a0)
        vrgatherei16.vv v8, v8, v10
        ret
.Lfunc_end0:
        .size   foo, .Lfunc_end0-foo
        .cfi_endproc
                                        # -- End function
        .option pop
        .option push
        .option arch, +v, +zve32f, +zve32x, +zve64d, +zve64f, +zve64x, +zvl128b, +zvl32b, +zvl64b
        .globl  main                            # -- Begin function main
        .p2align        1
        .type   main,@function
main:                                   # @main
        .cfi_startproc
# %bb.0:
        addi    sp, sp, -16
        .cfi_def_cfa_offset 16
        sd      ra, 8(sp)                       # 8-byte Folded Spill
        .cfi_offset ra, -8
        lui     a0, %hi(base_vec)
        addi    a0, a0, %lo(base_vec)
        vsetivli        zero, 2, e64, m1, ta, ma
        vle64.v v8, (a0)
        call    foo
        vsetivli        zero, 1, e64, m1, ta, ma
        vmv.x.s a0, v8
        sext.w  a0, a0
        ld      ra, 8(sp)                       # 8-byte Folded Reload
        .cfi_restore ra
        addi    sp, sp, 16
        .cfi_def_cfa_offset 0
        ret
.Lfunc_end1:
        .size   main, .Lfunc_end1-main
        .cfi_endproc

.section .rodata
	.balign 8                                     # -- End function
base_idx:
        .half   1                               # 0x1
        .half   1                               # 0x0

.section .rodata
	.balign 8 
base_vec:
        .dword    3                              # 0x0
        .dword   11                              # 0xb

.section .rodata
	.balign 8 
noise_vec:
        .word    7                              # 0x0
        .word    13                             # 0xb
        .word    23                             # 0xb
        .word    31                             # 0xb
