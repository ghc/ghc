while(<>) {
    chop;
    ($PE, $pe, $time, $act, $tid, $rest) = split;
    next if $act eq 'REPLY';
    chop($tid) if $act eq 'END';
    $from = $queue{$tid};
    $extra = "";
    if ($act eq 'START') {
	$from = '*';
	$to = 'G';
    } elsif ($act eq 'START(Q)') {
	$from = '*';
	$to = 'A';
    } elsif ($act eq 'STEALING') {
	$to = 'C';
    } elsif ($act eq 'STOLEN') {
	$to = 'G';
    } elsif ($act eq 'STOLEN(Q)') {
	$to = 'A';
    } elsif ($act eq 'FETCH') {
	$to = 'Y';
    } elsif ($act eq 'BLOCK') {
	$to = 'R';
    } elsif ($act eq 'RESUME') {
	$to = 'G';
	$extra = " 0 0x0";
    } elsif ($act eq 'RESUME(Q)') {
	$to = 'A';
	$extra = " 0 0x0";
    } elsif ($act eq 'END') {
	$to = '*';
    } elsif ($act eq 'SCHEDULE') {
	$to = 'G';
    } elsif ($act eq 'DESCHEDULE') {
	$to = 'A';
    }
    $queue{$tid} = $to;

    if ($to ne $from) {
        print substr($time,1,length($time)-3), " ", 
	  $from, $to, " 0 0x", $tid, $extra, "\n";
    }
    delete $queue{$tid} if $to eq '*';
    
}    
