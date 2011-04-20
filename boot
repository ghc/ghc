#!/usr/bin/perl -w

use strict;

use Cwd;

my %required_tag;
my $validate;

$required_tag{"-"} = 1;
$validate = 0;

while ($#ARGV ne -1) {
    my $arg = shift @ARGV;

    if ($arg =~ /^--required-tag=(.*)/) {
        $required_tag{$1} = 1;
    }
    elsif ($arg =~ /^--validate$/) {
        $validate = 1;
    }
    else {
        die "Bad arg: $arg";
    }
}

{
    local $/ = undef;
    open FILE, "packages" or die "Couldn't open file: $!";
    binmode FILE;
    my $string = <FILE>;
    close FILE;

    if ($string =~ /\r/) {
        print STDERR <<EOF;
Found ^M in packages.
Perhaps you need to run
    git config --global core.autocrlf false
and re-check out the tree?
EOF
        exit 1;
    }
}

# Create libraries/*/{ghc.mk,GNUmakefile}
system("/usr/bin/perl", "-w", "boot-pkgs") == 0
    or die "Running boot-pkgs failed: $?";

my $tag;
my $dir;
my $curdir;

$curdir = &cwd()
    or die "Can't find current directory: $!";

# Check that we have all boot packages.
open PACKAGES, "< packages";
while (<PACKAGES>) {
    if (/^#/) {
        # Comment; do nothing
    }
    elsif (/^([a-zA-Z0-9\/.-]+) +([^ ]+) +[^ ]+ +[^ ]+ +[^ ]+$/) {
        $dir = $1;
        $tag = $2;
        
        # If $tag is not "-" then it is an optional repository, so its
        # absence isn't an error.
        if (defined($required_tag{$tag})) {
            # We would like to just check for a .git directory here,
            # but in an lndir tree we avoid making .git directories,
            # so it doesn't exist. We therefore require that every repo
            # has a LICENSE file instead.
            if (! -f "$dir/LICENSE") {
                print STDERR "Error: $dir/LICENSE doesn't exist.\n";
                die "Maybe you haven't done './sync-all get'?";
            }
        }
    }
    else {
        die "Bad line in packages file: $_";
    }
}
close PACKAGES;

# autoreconf everything that needs it.
foreach $dir (".", glob("libraries/*/")) {
    if (-f "$dir/configure.ac") {
        print "Booting $dir\n";
        chdir $dir or die "can't change to $dir: $!";
        system("autoreconf") == 0
            or die "Running autoreconf failed with exitcode $?";
        chdir $curdir or die "can't change to $curdir: $!";
    }
}

if ($validate eq 0 && ! -f "mk/build.mk") {
    print <<EOF;

WARNING: You don't have a mk/build.mk file.

By default a standard GHC build will be done, which uses optimisation
and builds the profiling libraries. This will take a long time, so may
not be what you want if you are developing GHC or the libraries, rather
than simply building it to use it.

For information on creating a mk/build.mk file, please see:
    http://hackage.haskell.org/trac/ghc/wiki/Building/Using#Buildconfiguration

EOF
}

