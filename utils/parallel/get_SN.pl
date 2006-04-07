#!/usr/local/bin/perl
#############################################################################

#do get_SN($ARGV[0]);

#exit 1;

# ---------------------------------------------------------------------------

sub get_SN {
    local ($file) = @_;
    local ($id,$idx,$sn);

    open (FILE,$file) || die "get_SN: Can't open file $file\n";

    $line_no=0;
    while (<FILE>) {
	next unless /END/;
	# PE  0 [3326775]: END 0, SN 0, ST 0, EXP F, BB 194, HA 1464, RT 983079, BT 1449032 (7), FT 0 (0), LS 0, GS 27, MY T 
	
	if (/^PE\s*(\d+) \[(\d+)\]: END ([0-9a-fx]+), SN (\d+)/) {
	    $line_no++;
	    $idx = $3;
	    $id = hex($idx);
	    $sn = $4;
	    #print STDERR "Id: $id ($idx) --> $sn\n";
	    $id2sn{$id} = $sn;
	}
    }

    # print STDERR "get_SN: $line_no lines processed\n";
    close (FILE);

    # print STDERR "Summary: " . "="x15 . "\n";
    # foreach $key (keys %id2sn) {
    #   print STDERR ">  $key --> $id2sn{$key}\n";
    #}
}

1;
