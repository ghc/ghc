#
# This command wraps round a command execution, adding some hpc tests.
#

while($ARGV[0] =~ /^--/) {
  $arg = shift @ARGV;
  if ($arg =~ /--hpc=(.*)/) {
    $HPC = $1;
  } 
  elsif ($arg =~ /--clear/) {
    $CLEAR = 1;       
  }
  elsif ($arg =~ /--exeext=(.*)/) {
    $exeext = $1;
  }
  else {
    die "Bad arg: $arg"
  }
}

die "no option --hpc=* provided\n" if (!defined($HPC));
        
$binary = $ARGV[0] . $exeext;

# get the basename: needed for the test function/subdir/tough2
$binary =~ s/^.*\/([^\/]*)$/$1/;

unlink "$binary.tix" if (defined($CLEAR));

system @ARGV;
print "\n\n";
system ($HPC, "report", "$binary.tix");
print "\n\n";
system ($HPC, "report", "$binary.tix", "--per-module");
print "\n\n";
open(MARKUP, "-|", $HPC, "markup", "$binary.tix");
while(<MARKUP>) {
  my $line = $_;
  print $line;
  if (/Writing: (\S+.html)/) {
     system("cat $1");        
  }
}
print "\n\n";
