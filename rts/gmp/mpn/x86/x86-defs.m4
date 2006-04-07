divert(-1)

dnl  m4 macros for x86 assembler.


dnl  Copyright (C) 1999, 2000 Free Software Foundation, Inc.
dnl 
dnl  This file is part of the GNU MP Library.
dnl
dnl  The GNU MP Library is free software; you can redistribute it and/or
dnl  modify it under the terms of the GNU Lesser General Public License as
dnl  published by the Free Software Foundation; either version 2.1 of the
dnl  License, or (at your option) any later version.
dnl
dnl  The GNU MP Library is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl  Lesser General Public License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public
dnl  License along with the GNU MP Library; see the file COPYING.LIB.  If
dnl  not, write to the Free Software Foundation, Inc., 59 Temple Place -
dnl  Suite 330, Boston, MA 02111-1307, USA.


dnl  Notes:
dnl
dnl  m4 isn't perfect for processing BSD style x86 assembler code, the main
dnl  problems are,
dnl
dnl  1. Doing define(foo,123) and then using foo in an addressing mode like
dnl     foo(%ebx) expands as a macro rather than a constant.  This is worked
dnl     around by using deflit() from asm-defs.m4, instead of define().
dnl
dnl  2. Immediates in macro definitions need a space or `' to stop the $
dnl     looking like a macro parameter.  For example,
dnl
dnl  	        define(foo, `mov $ 123, %eax')
dnl
dnl     This is only a problem in macro definitions, not in ordinary text,
dnl     nor in macro parameters like text passed to forloop() or ifdef().


deflit(BYTES_PER_MP_LIMB, 4)


dnl  --------------------------------------------------------------------------
dnl  Replacement PROLOGUE/EPILOGUE with more sophisticated error checking.
dnl  Nesting and overlapping not allowed.
dnl


dnl  Usage: PROLOGUE(functionname)
dnl
dnl  Generate a function prologue.  functionname gets GSYM_PREFIX added.
dnl  Examples,
dnl
dnl         PROLOGUE(mpn_add_n)
dnl         PROLOGUE(somefun)

define(`PROLOGUE',
m4_assert_numargs(1)
m4_assert_defined(`PROLOGUE_cpu')
`ifdef(`PROLOGUE_current_function',
`m4_error(`PROLOGUE'(`PROLOGUE_current_function') needs an `EPILOGUE'() before `PROLOGUE'($1)
)')dnl
m4_file_seen()dnl
define(`PROLOGUE_current_function',`$1')dnl
PROLOGUE_cpu(GSYM_PREFIX`'$1)')


dnl  Usage: EPILOGUE()
dnl
dnl  Notice the function name is passed to EPILOGUE_cpu(), letting it use $1
dnl  instead of the long PROLOGUE_current_function symbol.

define(`EPILOGUE',
m4_assert_numargs(0)
m4_assert_defined(`EPILOGUE_cpu')
`ifdef(`PROLOGUE_current_function',,
`m4_error(`EPILOGUE'() with no `PROLOGUE'()
)')dnl
EPILOGUE_cpu(GSYM_PREFIX`'PROLOGUE_current_function)`'dnl
undefine(`PROLOGUE_current_function')')

m4wrap_prepend(
`ifdef(`PROLOGUE_current_function',
`m4_error(`EPILOGUE() for PROLOGUE('PROLOGUE_current_function`) never seen
')')')


dnl  Usage: PROLOGUE_assert_inside()
dnl
dnl  Use this unquoted on a line on its own at the start of a macro
dnl  definition to add some code to check the macro is only used inside a
dnl  PROLOGUE/EPILOGUE pair, and that hence PROLOGUE_current_function is
dnl  defined.

define(PROLOGUE_assert_inside,
m4_assert_numargs(0)
``PROLOGUE_assert_inside_internal'(m4_doublequote($`'0))`dnl '')

define(PROLOGUE_assert_inside_internal,
m4_assert_numargs(1)
`ifdef(`PROLOGUE_current_function',,
`m4_error(`$1 used outside a PROLOGUE / EPILOGUE pair
')')')


dnl  Usage: L(labelname)
dnl         LF(functionname,labelname)
dnl
dnl  Generate a local label in the current or given function.  For LF(),
dnl  functionname gets GSYM_PREFIX added, the same as with PROLOGUE().
dnl
dnl  For example, in a function mpn_add_n (and with MPN_PREFIX __gmpn),
dnl
dnl         L(bar)          => L__gmpn_add_n__bar
dnl         LF(somefun,bar) => Lsomefun__bar
dnl
dnl  The funtion name and label name get two underscores between them rather
dnl  than one to guard against clashing with a separate external symbol that
dnl  happened to be called functionname_labelname.  (Though this would only
dnl  happen if the local label prefix is is empty.)  Underscores are used so
dnl  the whole label will still be a valid C identifier and so can be easily
dnl  used in gdb.

dnl  LSYM_PREFIX can be L$, so defn() is used to prevent L expanding as the
dnl  L macro and making an infinite recursion.
define(LF,
m4_assert_numargs(2)
m4_assert_defined(`LSYM_PREFIX')
`defn(`LSYM_PREFIX')GSYM_PREFIX`'$1`'__$2')

define(`L',
m4_assert_numargs(1)
PROLOGUE_assert_inside()
`LF(PROLOGUE_current_function,`$1')')


dnl  Called: PROLOGUE_cpu(gsym)
dnl          EPILOGUE_cpu(gsym)

define(PROLOGUE_cpu,
m4_assert_numargs(1)
	`GLOBL	$1
	TYPE($1,`function')
$1:')

define(EPILOGUE_cpu,
m4_assert_numargs(1)
`	SIZE($1,.-$1)')



dnl  --------------------------------------------------------------------------
dnl  Various x86 macros.
dnl


dnl  Usage: ALIGN_OFFSET(bytes,offset)
dnl
dnl  Align to `offset' away from a multiple of `bytes'.
dnl
dnl  This is useful for testing, for example align to something very strict
dnl  and see what effect offsets from it have, "ALIGN_OFFSET(256,32)".
dnl
dnl  Generally you wouldn't execute across the padding, but it's done with
dnl  nop's so it'll work.

define(ALIGN_OFFSET,
m4_assert_numargs(2)
`ALIGN($1)
forloop(`i',1,$2,`	nop
')')


dnl  Usage: defframe(name,offset)
dnl
dnl  Make a definition like the following with which to access a parameter
dnl  or variable on the stack.
dnl
dnl         define(name,`FRAME+offset(%esp)')
dnl
dnl  Actually m4_empty_if_zero(FRAME+offset) is used, which will save one
dnl  byte if FRAME+offset is zero, by putting (%esp) rather than 0(%esp).
dnl  Use define(`defframe_empty_if_zero_disabled',1) if for some reason the
dnl  zero offset is wanted.
dnl
dnl  The new macro also gets a check that when it's used FRAME is actually
dnl  defined, and that the final %esp offset isn't negative, which would
dnl  mean an attempt to access something below the current %esp.
dnl
dnl  deflit() is used rather than a plain define(), so the new macro won't
dnl  delete any following parenthesized expression.  name(%edi) will come
dnl  out say as 16(%esp)(%edi).  This isn't valid assembler and should
dnl  provoke an error, which is better than silently giving just 16(%esp).
dnl
dnl  See README.family for more on the suggested way to access the stack
dnl  frame.

define(defframe,
m4_assert_numargs(2)
`deflit(`$1',
m4_assert_defined(`FRAME')
`defframe_check_notbelow(`$1',$2,FRAME)dnl
defframe_empty_if_zero(FRAME+($2))(%esp)')')

dnl  Called: defframe_empty_if_zero(expression)
define(defframe_empty_if_zero,
`ifelse(defframe_empty_if_zero_disabled,1,
`eval($1)',
`m4_empty_if_zero($1)')')

dnl  Called: defframe_check_notbelow(`name',offset,FRAME)
define(defframe_check_notbelow,
m4_assert_numargs(3)
`ifelse(eval(($3)+($2)<0),1,
`m4_error(`$1 at frame offset $2 used when FRAME is only $3 bytes
')')')


dnl  Usage: FRAME_pushl()
dnl         FRAME_popl()
dnl         FRAME_addl_esp(n)
dnl         FRAME_subl_esp(n)
dnl
dnl  Adjust FRAME appropriately for a pushl or popl, or for an addl or subl
dnl  %esp of n bytes.
dnl
dnl  Using these macros is completely optional.  Sometimes it makes more
dnl  sense to put explicit deflit(`FRAME',N) forms, especially when there's
dnl  jumps and different sequences of FRAME values need to be used in
dnl  different places.

define(FRAME_pushl,
m4_assert_numargs(0)
m4_assert_defined(`FRAME')
`deflit(`FRAME',eval(FRAME+4))')

define(FRAME_popl,
m4_assert_numargs(0)
m4_assert_defined(`FRAME')
`deflit(`FRAME',eval(FRAME-4))')

define(FRAME_addl_esp,
m4_assert_numargs(1)
m4_assert_defined(`FRAME')
`deflit(`FRAME',eval(FRAME-($1)))')

define(FRAME_subl_esp,
m4_assert_numargs(1)
m4_assert_defined(`FRAME')
`deflit(`FRAME',eval(FRAME+($1)))')


dnl  Usage: defframe_pushl(name)
dnl
dnl  Do a combination of a FRAME_pushl() and a defframe() to name the stack
dnl  location just pushed.  This should come after a pushl instruction.
dnl  Putting it on the same line works and avoids lengthening the code.  For
dnl  example,
dnl
dnl         pushl   %eax     defframe_pushl(VAR_COUNTER)
dnl
dnl  Notice the defframe() is done with an unquoted -FRAME thus giving its
dnl  current value without tracking future changes.

define(defframe_pushl,
`FRAME_pushl()defframe(`$1',-FRAME)')


dnl  --------------------------------------------------------------------------
dnl  Assembler instruction macros.
dnl


dnl  Usage: emms_or_femms
dnl         femms_available_p
dnl
dnl  femms_available_p expands to 1 or 0 according to whether the AMD 3DNow
dnl  femms instruction is available.  emms_or_femms expands to femms if
dnl  available, or emms if not.
dnl
dnl  emms_or_femms is meant for use in the K6 directory where plain K6
dnl  (without femms) and K6-2 and K6-3 (with a slightly faster femms) are
dnl  supported together.
dnl
dnl  On K7 femms is no longer faster and is just an alias for emms, so plain
dnl  emms may as well be used.

define(femms_available_p,
m4_assert_numargs(-1)
`m4_ifdef_anyof_p(
	`HAVE_TARGET_CPU_k62',
	`HAVE_TARGET_CPU_k63',
	`HAVE_TARGET_CPU_athlon')')

define(emms_or_femms,
m4_assert_numargs(-1)
`ifelse(femms_available_p,1,`femms',`emms')')


dnl  Usage: femms
dnl
dnl  The gas 2.9.1 that comes with FreeBSD 3.4 doesn't support femms, so the
dnl  following is a replacement using .byte.
dnl
dnl  If femms isn't available, an emms is generated instead, for convenience
dnl  when testing on a machine without femms.

define(femms,
m4_assert_numargs(-1)
`ifelse(femms_available_p,1,
`.byte	15,14	C AMD 3DNow femms',
`emms`'dnl
m4_warning(`warning, using emms in place of femms, use for testing only
')')')


dnl  Usage: jadcl0(op)
dnl
dnl  Issue a jnc/incl as a substitute for adcl $0,op.  This isn't an exact
dnl  replacement, since it doesn't set the flags like adcl does.
dnl
dnl  This finds a use in K6 mpn_addmul_1, mpn_submul_1, mpn_mul_basecase and
dnl  mpn_sqr_basecase because on K6 an adcl is slow, the branch
dnl  misprediction penalty is small, and the multiply algorithm used leads
dnl  to a carry bit on average only 1/4 of the time.
dnl
dnl  jadcl0_disabled can be set to 1 to instead issue an ordinary adcl for
dnl  comparison.  For example,
dnl
dnl		define(`jadcl0_disabled',1)
dnl
dnl  When using a register operand, eg. "jadcl0(%edx)", the jnc/incl code is
dnl  the same size as an adcl.  This makes it possible to use the exact same
dnl  computed jump code when testing the relative speed of jnc/incl and adcl
dnl  with jadcl0_disabled.

define(jadcl0,
m4_assert_numargs(1)
`ifelse(jadcl0_disabled,1,
	`adcl	$`'0, $1',
	`jnc	1f
	incl	$1
1:dnl')')


dnl  Usage: cmov_available_p
dnl
dnl  Expand to 1 if cmov is available, 0 if not.

define(cmov_available_p,
`m4_ifdef_anyof_p(
	`HAVE_TARGET_CPU_pentiumpro',
	`HAVE_TARGET_CPU_pentium2',
	`HAVE_TARGET_CPU_pentium3',
	`HAVE_TARGET_CPU_athlon')')


dnl  Usage: x86_lookup(target, key,value, key,value, ...)
dnl         x86_lookup_p(target, key,value, key,value, ...)
dnl
dnl  Look for `target' among the `key' parameters.
dnl
dnl  x86_lookup expands to the corresponding `value', or generates an error
dnl  if `target' isn't found.
dnl
dnl  x86_lookup_p expands to 1 if `target' is found, or 0 if not.

define(x86_lookup,
`ifelse(eval($#<3),1,
`m4_error(`unrecognised part of x86 instruction: $1
')',
`ifelse(`$1',`$2', `$3',
`x86_lookup(`$1',shift(shift(shift($@))))')')')

define(x86_lookup_p,
`ifelse(eval($#<3),1, `0',
`ifelse(`$1',`$2',    `1',
`x86_lookup_p(`$1',shift(shift(shift($@))))')')')


dnl  Usage: x86_opcode_reg32(reg)
dnl         x86_opcode_reg32_p(reg)
dnl
dnl  x86_opcode_reg32 expands to the standard 3 bit encoding for the given
dnl  32-bit register, eg. `%ebp' turns into 5.
dnl
dnl  x86_opcode_reg32_p expands to 1 if reg is a valid 32-bit register, or 0
dnl  if not.

define(x86_opcode_reg32,
m4_assert_numargs(1)
`x86_lookup(`$1',x86_opcode_reg32_list)')

define(x86_opcode_reg32_p,
m4_assert_onearg()
`x86_lookup_p(`$1',x86_opcode_reg32_list)')

define(x86_opcode_reg32_list,
``%eax',0,
`%ecx',1,
`%edx',2,
`%ebx',3,
`%esp',4,
`%ebp',5,
`%esi',6,
`%edi',7')


dnl  Usage: x86_opcode_tttn(cond)
dnl
dnl  Expand to the 4-bit "tttn" field value for the given x86 branch
dnl  condition (like `c', `ae', etc).

define(x86_opcode_tttn,
m4_assert_numargs(1)
`x86_lookup(`$1',x86_opcode_ttn_list)')

define(x86_opcode_tttn_list,
``o',  0,
`no',  1,
`b',   2, `c',  2, `nae',2,
`nb',  3, `nc', 3, `ae', 3,
`e',   4, `z',  4,
`ne',  5, `nz', 5,
`be',  6, `na', 6,
`nbe', 7, `a',  7,
`s',   8,
`ns',  9,
`p',  10, `pe', 10, `npo',10,
`np', 11, `npe',11, `po', 11,
`l',  12, `nge',12,
`nl', 13, `ge', 13,
`le', 14, `ng', 14,
`nle',15, `g',  15')


dnl  Usage: cmovCC(srcreg,dstreg)
dnl
dnl  Generate a cmov instruction if the target supports cmov, or simulate it
dnl  with a conditional jump if not (the latter being meant only for
dnl  testing).  For example,
dnl
dnl         cmovz(  %eax, %ebx)
dnl
dnl  cmov instructions are generated using .byte sequences, since only
dnl  recent versions of gas know cmov.
dnl
dnl  The source operand can only be a plain register.  (m4 code implementing
dnl  full memory addressing modes exists, believe it or not, but isn't
dnl  currently needed and isn't included.)
dnl
dnl  All the standard conditions are defined.  Attempting to use one without
dnl  the macro parentheses, such as just "cmovbe %eax, %ebx", will provoke
dnl  an error.  This ensures the necessary .byte sequences aren't
dnl  accidentally missed.

dnl  Called: define_cmov_many(cond,tttn,cond,tttn,...)
define(define_cmov_many,
`ifelse(m4_length(`$1'),0,,
`define_cmov(`$1',`$2')define_cmov_many(shift(shift($@)))')')

dnl  Called: define_cmov(cond,tttn)
define(define_cmov,
m4_assert_numargs(2)
`define(`cmov$1',
m4_instruction_wrapper()
m4_assert_numargs(2)
`cmov_internal'(m4_doublequote($`'0),``$1',`$2'',dnl
m4_doublequote($`'1),m4_doublequote($`'2)))')

define_cmov_many(x86_opcode_tttn_list)


dnl  Called: cmov_internal(name,cond,tttn,src,dst)
define(cmov_internal,
m4_assert_numargs(5)
`ifelse(cmov_available_p,1,
`cmov_bytes_tttn(`$1',`$3',`$4',`$5')',
`m4_warning(`warning, simulating cmov with jump, use for testing only
')cmov_simulate(`$2',`$4',`$5')')')

dnl  Called: cmov_simulate(cond,src,dst)
dnl  If this is going to be used with memory operands for the source it will
dnl  need to be changed to do a fetch even if the condition is false, so as
dnl  to trigger exceptions the same way a real cmov does.
define(cmov_simulate,
m4_assert_numargs(3)
	`j$1	1f	C cmov$1 $2, $3
	jmp	2f
1:	movl	$2, $3
2:')

dnl  Called: cmov_bytes_tttn(name,tttn,src,dst)
define(cmov_bytes_tttn,
m4_assert_numargs(4)
`.byte	dnl
15, dnl
eval(64+$2), dnl
eval(192+8*x86_opcode_reg32(`$4')+x86_opcode_reg32(`$3')) dnl
	C `$1 $3, $4'')


dnl  Usage: loop_or_decljnz label
dnl
dnl  Generate either a "loop" instruction or a "decl %ecx / jnz", whichever
dnl  is better.  "loop" is better on K6 and probably on 386, on other chips
dnl  separate decl/jnz is better.
dnl
dnl  This macro is just for mpn/x86/divrem_1.asm and mpn/x86/mod_1.asm where
dnl  this loop_or_decljnz variation is enough to let the code be shared by
dnl  all chips.

define(loop_or_decljnz,
`ifelse(loop_is_better_p,1,
	`loop',
	`decl	%ecx
	jnz')')

define(loop_is_better_p,
`m4_ifdef_anyof_p(`HAVE_TARGET_CPU_k6',
                  `HAVE_TARGET_CPU_k62',
                  `HAVE_TARGET_CPU_k63',
                  `HAVE_TARGET_CPU_i386')')


dnl  Usage: Zdisp(inst,op,op,op)
dnl
dnl  Generate explicit .byte sequences if necessary to force a byte-sized
dnl  zero displacement on an instruction.  For example,
dnl
dnl         Zdisp(  movl,   0,(%esi), %eax)
dnl
dnl  expands to
dnl
dnl                 .byte   139,70,0  C movl 0(%esi), %eax
dnl
dnl  If the displacement given isn't 0, then normal assembler code is
dnl  generated.  For example,
dnl
dnl         Zdisp(  movl,   4,(%esi), %eax)
dnl
dnl  expands to
dnl
dnl                 movl    4(%esi), %eax
dnl
dnl  This means a single Zdisp() form can be used with an expression for the
dnl  displacement, and .byte will be used only if necessary.  The
dnl  displacement argument is eval()ed.
dnl
dnl  Because there aren't many places a 0(reg) form is wanted, Zdisp is
dnl  implemented with a table of instructions and encodings.  A new entry is
dnl  needed for any different operation or registers.

define(Zdisp,
`define(`Zdisp_found',0)dnl
Zdisp_match( movl, %eax, 0,(%edi), `137,71,0',    $@)`'dnl
Zdisp_match( movl, %ebx, 0,(%edi), `137,95,0',    $@)`'dnl
Zdisp_match( movl, %esi, 0,(%edi), `137,119,0',   $@)`'dnl
Zdisp_match( movl, 0,(%ebx), %eax, `139,67,0',    $@)`'dnl
Zdisp_match( movl, 0,(%ebx), %esi, `139,115,0',   $@)`'dnl
Zdisp_match( movl, 0,(%esi), %eax, `139,70,0',    $@)`'dnl
Zdisp_match( movl, 0,(%esi,%ecx,4), %eax, `0x8b,0x44,0x8e,0x00',      $@)`'dnl
Zdisp_match( addl, %ebx, 0,(%edi), `1,95,0',      $@)`'dnl
Zdisp_match( addl, %ecx, 0,(%edi), `1,79,0',      $@)`'dnl
Zdisp_match( addl, %esi, 0,(%edi), `1,119,0',     $@)`'dnl
Zdisp_match( subl, %ecx, 0,(%edi), `41,79,0',     $@)`'dnl
Zdisp_match( adcl, 0,(%edx), %esi, `19,114,0',    $@)`'dnl
Zdisp_match( sbbl, 0,(%edx), %esi, `27,114,0',    $@)`'dnl
Zdisp_match( movq, 0,(%eax,%ecx,8), %mm0, `0x0f,0x6f,0x44,0xc8,0x00', $@)`'dnl
Zdisp_match( movq, 0,(%ebx,%eax,4), %mm0, `0x0f,0x6f,0x44,0x83,0x00', $@)`'dnl
Zdisp_match( movq, 0,(%ebx,%eax,4), %mm2, `0x0f,0x6f,0x54,0x83,0x00', $@)`'dnl
Zdisp_match( movq, 0,(%esi),        %mm0, `15,111,70,0',     $@)`'dnl
Zdisp_match( movq, %mm0,        0,(%edi), `15,127,71,0',     $@)`'dnl
Zdisp_match( movq, %mm2, 0,(%ecx,%eax,4), `0x0f,0x7f,0x54,0x81,0x00', $@)`'dnl
Zdisp_match( movq, %mm2, 0,(%edx,%eax,4), `0x0f,0x7f,0x54,0x82,0x00', $@)`'dnl
Zdisp_match( movq, %mm0, 0,(%edx,%ecx,8), `0x0f,0x7f,0x44,0xca,0x00', $@)`'dnl
Zdisp_match( movd, 0,(%eax,%ecx,8), %mm1, `0x0f,0x6e,0x4c,0xc8,0x00', $@)`'dnl
Zdisp_match( movd, 0,(%edx,%ecx,8), %mm0, `0x0f,0x6e,0x44,0xca,0x00', $@)`'dnl
Zdisp_match( movd, %mm0, 0,(%eax,%ecx,4), `0x0f,0x7e,0x44,0x88,0x00', $@)`'dnl
Zdisp_match( movd, %mm0, 0,(%ecx,%eax,4), `0x0f,0x7e,0x44,0x81,0x00', $@)`'dnl
Zdisp_match( movd, %mm2, 0,(%ecx,%eax,4), `0x0f,0x7e,0x54,0x81,0x00', $@)`'dnl
ifelse(Zdisp_found,0,
`m4_error(`unrecognised instruction in Zdisp: $1 $2 $3 $4
')')')

define(Zdisp_match,
`ifelse(eval(m4_stringequal_p(`$1',`$6')
	&& m4_stringequal_p(`$2',0)
	&& m4_stringequal_p(`$3',`$8')
	&& m4_stringequal_p(`$4',`$9')),1,
`define(`Zdisp_found',1)dnl
ifelse(eval(`$7'),0,
`	.byte	$5  C `$1 0$3, $4'',
`	$6	$7$8, $9')',

`ifelse(eval(m4_stringequal_p(`$1',`$6')
	&& m4_stringequal_p(`$2',`$7')
	&& m4_stringequal_p(`$3',0)
	&& m4_stringequal_p(`$4',`$9')),1,
`define(`Zdisp_found',1)dnl
ifelse(eval(`$8'),0,
`	.byte	$5  C `$1 $2, 0$4'',
`	$6	$7, $8$9')')')')


dnl  Usage: shldl(count,src,dst)
dnl         shrdl(count,src,dst)
dnl         shldw(count,src,dst)
dnl         shrdw(count,src,dst)
dnl
dnl  Generate a double-shift instruction, possibly omitting a %cl count
dnl  parameter if that's what the assembler requires, as indicated by
dnl  WANT_SHLDL_CL in config.m4.  For example,
dnl
dnl         shldl(  %cl, %eax, %ebx)
dnl
dnl  turns into either
dnl
dnl         shldl   %cl, %eax, %ebx
dnl  or
dnl         shldl   %eax, %ebx
dnl
dnl  Immediate counts are always passed through unchanged.  For example,
dnl
dnl         shrdl(  $2, %esi, %edi)
dnl  becomes
dnl         shrdl   $2, %esi, %edi
dnl
dnl
dnl  If you forget to use the macro form "shldl( ...)" and instead write
dnl  just a plain "shldl ...", an error results.  This ensures the necessary
dnl  variant treatment of %cl isn't accidentally bypassed.

define(define_shd_instruction,
`define($1,
m4_instruction_wrapper()
m4_assert_numargs(3)
`shd_instruction'(m4_doublequote($`'0),m4_doublequote($`'1),dnl
m4_doublequote($`'2),m4_doublequote($`'3)))')

dnl  Effectively: define(shldl,`shd_instruction(`$0',`$1',`$2',`$3')') etc
define_shd_instruction(shldl)
define_shd_instruction(shrdl)
define_shd_instruction(shldw)
define_shd_instruction(shrdw)

dnl  Called: shd_instruction(op,count,src,dst)
define(shd_instruction,
m4_assert_numargs(4)
m4_assert_defined(`WANT_SHLDL_CL')
`ifelse(eval(m4_stringequal_p(`$2',`%cl') && !WANT_SHLDL_CL),1,
``$1'	`$3', `$4'',
``$1'	`$2', `$3', `$4'')')


dnl  Usage: ASSERT(cond, instructions)
dnl
dnl  If WANT_ASSERT is 1, output the given instructions and expect the given
dnl  flags condition to then be satisfied.  For example,
dnl
dnl         ASSERT(ne, `cmpl %eax, %ebx')
dnl
dnl  The instructions can be omitted to just assert a flags condition with
dnl  no extra calculation.  For example,
dnl
dnl         ASSERT(nc)
dnl
dnl  When `instructions' is not empty, a pushf/popf is added to preserve the
dnl  flags, but the instructions themselves must preserve any registers that
dnl  matter.  FRAME is adjusted for the push and pop, so the instructions
dnl  given can use defframe() stack variables.

define(ASSERT,
m4_assert_numargs_range(1,2)
`ifelse(WANT_ASSERT,1,
	`C ASSERT
ifelse(`$2',,,`	pushf	ifdef(`FRAME',`FRAME_pushl()')')
	$2
	j`$1'	1f
	ud2	C assertion failed
1:
ifelse(`$2',,,`	popf	ifdef(`FRAME',`FRAME_popl()')')
')')


dnl  Usage: movl_text_address(label,register)
dnl
dnl  Get the address of a text segment label, using either a plain movl or a
dnl  position-independent calculation, as necessary.  For example,
dnl
dnl         movl_code_address(L(foo),%eax)
dnl
dnl  This macro is only meant for use in ASSERT()s or when testing, since
dnl  the PIC sequence it generates will want to be done with a ret balancing
dnl  the call on CPUs with return address branch predition.
dnl
dnl  The addl generated here has a backward reference to 1b, and so won't
dnl  suffer from the two forwards references bug in old gas (described in
dnl  mpn/x86/README.family).

define(movl_text_address,
`ifdef(`PIC',
	`call	1f
1:	popl	$2	C %eip
	addl	`$'$1-1b, $2',
	`movl	`$'$1, $2')')


divert`'dnl
