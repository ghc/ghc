#************************************************************************
#*                                                                      *
#* \section[Driver-obj-splitting]{Splitting into many \tr{.o} files (for libraries)}
#*                                                                      *
#************************************************************************

$TargetPlatform = $TARGETPLATFORM;

($Pgm = $0) =~ s|.*/||;
$ifile      = $ARGV[0];
$Tmp_prefix = $ARGV[1];
$Output     = $ARGV[2];

&split_asm_file($ifile);

open(OUTPUT, '>', $Output) ||  &tidy_up_and_die(1,"$Pgm: failed to open `$Output' (to write)\n");
print OUTPUT "$NoOfSplitFiles\n";
close(OUTPUT);

exit(0);


sub split_asm_file {
    (my $asm_file,) = @_;
    my @pieces = ();

    open(TMPI, '<', $asm_file) || &tidy_up_and_die(1,"$Pgm: failed to open `$asm_file' (to read)\n");


    $octr = 0;  # output file counter

    %LocalConstant = (); # we have to subvert C compiler's commoning-up of constants...

    $s_stuff = &ReadTMPIUpToAMarker( '', $octr );
    # that first stuff is a prologue for all .s outputs
    $prologue_stuff = &process_asm_block ( $s_stuff );
    # $_ already has some of the next stuff in it...

#   &tidy_up_and_die(1,"$Pgm: no split markers in .s file!\n")
#       if $prologue_stuff eq $s_stuff;

    while ( $_ ne '' ) { # not EOF
        $octr++;

        # grab and de-mangle a section of the .s file...
        $s_stuff = &ReadTMPIUpToAMarker ( $_, $octr );
        $pieces[$octr] = &process_asm_block ( $s_stuff );
    }

    # Make sure that we still have some output when the input file is empty
    if ($octr == 0) {
        $octr = 1;
        $pieces[$octr] = '';
    }

    $NoOfSplitFiles = $octr;

    if ($pieces[$NoOfSplitFiles] =~ /(\n[ \t]*\.section[ \t]+\.note\.GNU-stack,[^\n]*\n)/m) {
        $note_gnu_stack = $1;
        for $octr (1..($NoOfSplitFiles - 1)) {
            $pieces[$octr] .= $note_gnu_stack;
        }
    }

    for $octr (1..$NoOfSplitFiles) {
        # output to a file of its own
        # open a new output file...
        $ofname = "${Tmp_prefix}__${octr}.s";
        open(OUTF, '>', $ofname) || die "$Pgm: can't open output file: $ofname\n";

        print OUTF $prologue_stuff;
        print OUTF $pieces[$octr];

        close(OUTF)
          || &tidy_up_and_die(1,"$Pgm:Failed writing ${Tmp_prefix}__${octr}.s\n");
    }

    close(TMPI) || &tidy_up_and_die(1,"Failed reading $asm_file\n");
}

sub ReadTMPIUpToAMarker {
    (my $str, my $count) = @_; # already read bits


    for ( $_ = <TMPI>; $_ ne '' && ! /_?__stg_split_marker/m; $_ = <TMPI> ) {
        $str .= $_;
    }
    # if not EOF, then creep forward until next "real" line
    # (throwing everything away).
    # that first "real" line will stay in $_.

    # This loop is intended to pick up the body of the split_marker function

    while ($_ ne '' && (/_?__stg_split_marker/m
                     || /^L[^C].*:$/m
                     || /\t\.frame/m
                     # || /\t\.end/ NOT!  Let the split_marker regexp catch it
                     # || /\t\.ent/ NOT!  Let the split_marker regexp catch it
                     || /^\s+(save|retl?|restore|nop)/m)) {
        $_ = <TMPI>;
    }

    print STDERR "### BLOCK:$count:\n$str" if $Dump_asm_splitting_info;

    # return str
    $str =~ tr/\r//d if $TargetPlatform =~ /-mingw32$/m; # in case Perl doesn't convert line endings
    $str;
}
=pod

We must (a)~strip the marker off the block, (b)~record any literal C
constants that are defined here, and (c)~inject copies of any C constants
that are used-but-not-defined here.

=cut

sub process_asm_block {
    local($str) = @_;

    return(&process_asm_block_sparc($str)) if $TargetPlatform =~ /^sparc-/m;
    return(&process_asm_block_iX86($str))  if $TargetPlatform =~ /^i[34]86-/m;
    return(&process_asm_block_x86_64($str))  if $TargetPlatform =~ /^x86_64-/m;
    return(&process_asm_block_powerpc_linux($str))
                            if $TargetPlatform =~ /^powerpc-[^-]+-linux/m;

    # otherwise...
    &tidy_up_and_die(1,"$Pgm: no process_asm_block for $TargetPlatform\n");
}

sub process_asm_block_sparc {
    local($str) = @_;

    # strip the marker
    $str =~ s/(\.text\n\t\.align .\n)\t\.global\s+.*_?__stg_split_marker.*\n\t\.proc.*\n/$1/m;
    $str =~ s/(\t\.align .\n)\t\.global\s+.*_?__stg_split_marker.*\n\t\.proc.*\n/$1/m;

    # remove/record any literal constants defined here
    while ( $str =~ /(\t\.align .\n\.?(L?LC\d+):\n(\t\.asci[iz].*\n)+)/m ) {
        local($label) = $2;
        local($body)  = $1;

        &tidy_up_and_die(1,"Local constant label $label already defined!\n")
            if $LocalConstant{$label};

        $LocalConstant{$label} = $body;

        $str =~ s/\t\.align .\n\.?LL?C\d+:\n(\t\.asci[iz].*\n)+//m;
    }

    # inject definitions for any local constants now used herein
    foreach $k (keys %LocalConstant) {
        if ( $str =~ /\b$k\b/m ) {
            $str = $LocalConstant{$k} . $str;
        }
    }

   print STDERR "### STRIPPED BLOCK (sparc):\n$str" if $Dump_asm_splitting_info;

   $str;
}

sub process_asm_block_iX86 {
    (my $str,) = @_;

    # strip the marker

    $str =~ s/(\.text\n\t\.align .(?:,0x90)?\n)\.globl\s+.*_?__stg_split_marker.*\n/$1/m;
    $str =~ s/(\t\.align .(?:,0x90)?\n)\.globl\s+.*_?__stg_split_marker.*\n/$1/m;

    # it seems prudent to stick on one of these:
    $str = "\.text\n\t.align 4\n" . $str;

    # remove/record any literal constants defined here
    # [perl made uglier to work around the perl 5.7/5.8 bug documented at
    # http://bugs6.perl.org/rt2/Ticket/Display.html?id=1760 and illustrated
    # by the seg fault of perl -e '("x\n" x 5000) =~ /(.*\n)+/'
    # -- ccshan 2002-09-05]
    while ( ($str =~ /((?:^|\.)(LC\d+):\n(\t\.(ascii|string).*\n|\s*\.byte.*\n){1,100})/m )) {
        local($label) = $2;
        local($body)  = $1;
        local($prefix, $suffix) = ($`, $');

        &tidy_up_and_die(1,"Local constant label $label already defined!\n")
            if $LocalConstant{$label};

        while ( $suffix =~ /^((\t\.(ascii|string).*\n|\s*\.byte.*\n){1,100})/ ) {
            $body .= $1;
            $suffix = $';
        }
        $LocalConstant{$label} = $body;
        $str = $prefix . $suffix;
    }

    # inject definitions for any local constants now used herein
    foreach $k (keys %LocalConstant) {
        if ( $str =~ /\b$k\b/m ) {
            $str = $LocalConstant{$k} . $str;
        }
    }

   print STDERR "### STRIPPED BLOCK (iX86):\n$str" if $Dump_asm_splitting_info;

   $str;
}

sub process_asm_block_x86_64 {
    local($str) = @_;

    # remove/record any literal constants defined here
    # [perl made uglier to work around the perl 5.7/5.8 bug documented at
    # http://bugs6.perl.org/rt2/Ticket/Display.html?id=1760 and illustrated
    # by the seg fault of perl -e '("x\n" x 5000) =~ /(.*\n)+/'
    # -- ccshan 2002-09-05]
    while ( ($str =~ /((?:^|\.)(LC\d+):\n(\t\.(ascii|string).*\n|\s*\.byte.*\n){1,100})/m )) {
        local($label) = $2;
        local($body)  = $1;
        local($prefix, $suffix) = ($`, $');

        &tidy_up_and_die(1,"Local constant label $label already defined!\n")
            if $LocalConstant{$label};

        while ( $suffix =~ /^((\t\.(ascii|string).*\n|\s*\.byte.*\n){1,100})/ ) {
            $body .= $1;
            $suffix = $';
        }
        $LocalConstant{$label} = $body;
        $str = $prefix . $suffix;
    }

    # inject definitions for any local constants now used herein
    foreach $k (keys %LocalConstant) {
        if ( $str =~ /\b$k\b/m ) {
            $str = $LocalConstant{$k} . $str;
        }
    }

   print STDERR "### STRIPPED BLOCK (x86_64):\n$str" if $Dump_asm_splitting_info;

   $str;
}

sub process_asm_block_powerpc_linux {
    local($str) = @_;

    # strip the marker
    $str =~ s/__stg_split_marker.*\n//m;

    # remove/record any literal constants defined here
    while ( $str =~ s/^(\s+.section\s+\.rodata\n\s+\.align.*\n(\.LC\d+):\n(\s\.(byte|short|long|quad|2byte|4byte|8byte|fill|space|ascii|string).*\n)+)//m ) {
        local($label) = $2;
        local($body)  = $1;

        &tidy_up_and_die(1,"Local constant label $label already defined!\n")
            if $LocalConstant{$label};

        $LocalConstant{$label} = $body;
    }

    # inject definitions for any local constants now used herein
    foreach $k (keys %LocalConstant) {
        if ( $str =~ /[\s,]$k\b/m ) {
            $str = $LocalConstant{$k} . $str;
        }
    }

    print STDERR "### STRIPPED BLOCK (powerpc linux):\n$str" if $Dump_asm_splitting_info;

    $str;
}

sub tidy_up_and_die {
    local($return_val, $msg) = @_;
    print STDERR $msg;
    exit (($return_val == 0) ? 0 : 1);
}
