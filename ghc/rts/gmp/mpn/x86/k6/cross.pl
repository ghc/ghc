#! /usr/bin/perl

# Copyright (C) 2000 Free Software Foundation, Inc.
#
# This file is part of the GNU MP Library.
#
# The GNU MP Library is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation; either version 2.1 of the License, or (at
# your option) any later version.
#
# The GNU MP Library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
# MA 02111-1307, USA.


# Usage: cross.pl [filename.o]...
#
# Produce an annotated disassembly of the given object files, indicating
# certain code alignment and addressing mode problems afflicting K6 chips.
# "ZZ" is used on all annotations, so this can be searched for.
#
# With no arguments, all .o files corresponding to .asm files are processed.
# This is good in the mpn object directory of a k6*-*-* build.
#
# As far as fixing problems goes, any cache line crossing problems in loops
# get attention, but as a rule it's too tedious to rearrange code or slip in
# nops to fix every problem in setup or finishup code.
#
# Bugs:
#
# Instructions without mod/rm bytes or which are already vector decoded are
# unaffected by cache line boundary crossing, but not all of these have yet
# been put in as exceptions.  All that occur in practice in GMP are present
# though.
#
# There's no messages for using the vector decoded addressing mode (%esi),
# but that mode is easy to avoid when coding.

use strict;

sub disassemble {
    my ($file) = @_;
    my ($addr,$b1,$b2,$b3, $prefix,$opcode,$modrm);

    open (IN, "objdump -Srfh $file |")
	|| die "Cannot open pipe from objdump\n";
    while (<IN>) {
	print;

	if (/^[ \t]*[0-9]+[ \t]+\.text[ \t]/ && /2\*\*([0-9]+)$/) {
	    if ($1 < 5) {
		print "ZZ need at least 2**5 for predictable cache line crossing\n";
	    }
	}
	
	if (/^[ \t]*([0-9a-f]*):[ \t]*([0-9a-f]+)[ \t]+([0-9a-f]+)[ \t]+([0-9a-f]+)/) {
	    ($addr,$b1,$b2,$b3) = ($1,$2,$3,$4);

	} elsif (/^[ \t]*([0-9a-f]*):[ \t]*([0-9a-f]+)[ \t]+([0-9a-f]+)/) {
	    ($addr,$b1,$b2,$b3) = ($1,$2,$3,'');

	} elsif (/^[ \t]*([0-9a-f]*):[ \t]*([0-9a-f]+)/) {
	    ($addr,$b1,$b2,$b3) = ($1,$2,'','');

	} else {
	    next;
	}

	if ($b1 =~ /0f/) {
	    $prefix = $b1;
	    $opcode = $b2;
	    $modrm = $b3;
	} else {
	    $prefix = '';
	    $opcode = $b1;
	    $modrm = $b2;
	}

	# modrm of the form 00-xxx-100 with an 0F prefix is the problem case
	# for K6 and pre-CXT K6-2
	if ($prefix =~ /0f/
	    && $opcode !~ /^8/         # jcond disp32
	    && $modrm =~ /^[0-3][4c]/) {
	    print "ZZ ($file) >3 bytes to determine instruction length\n";
	}

	# with just an opcode, starting 1f mod 20h
	if ($addr =~ /[13579bdf]f$/
	    && $prefix !~ /0f/
	    && $opcode !~ /1[012345]/ # adc
	    && $opcode !~ /1[89abcd]/ # sbb
	    && $opcode !~ /68/        # push $imm32
	    && $opcode !~ /^7/        # jcond disp8
	    && $opcode !~ /a[89]/     # test+imm
	    && $opcode !~ /a[a-f]/    # stos/lods/scas
	    && $opcode !~ /b8/        # movl $imm32,%eax
	    && $opcode !~ /e[0123]/   # loop/loopz/loopnz/jcxz
	    && $opcode !~ /e[b9]/     # jmp disp8/disp32
	    && $opcode !~ /f[89abcd]/ # clc,stc,cli,sti,cld,std
	    && !($opcode =~ /f[67]/          # grp 1
		 && $modrm =~ /^[2367abef]/) # mul, imul, div, idiv
	    && $modrm !~ /^$/) {
	    print "ZZ ($file) opcode/modrm cross 32-byte boundary\n";
	}

	# with an 0F prefix, anything starting at 1f mod 20h
	if ($addr =~ /[13579bdf][f]$/
	    && $prefix =~ /0f/) {
	    print "ZZ ($file) prefix/opcode cross 32-byte boundary\n";
	}

	# with an 0F prefix, anything with mod/rm starting at 1e mod 20h
	if ($addr =~ /[13579bdf][e]$/
	    && $prefix =~ /0f/
	     && $opcode !~ /^8/        # jcond disp32
	    && $modrm !~ /^$/) {
	    print "ZZ ($file) prefix/opcode/modrm cross 32-byte boundary\n";
	}
    }
    close IN || die "Error from objdump (or objdump not available)\n";
}


my @files;
if ($#ARGV >= 0) {
    @files = @ARGV;
} else {
    @files = glob "*.asm";
    map {s/.asm/.o/} @files;
}

foreach (@files)  {
    disassemble($_);
}
