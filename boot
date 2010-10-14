#!/usr/bin/perl -w

use strict;

use Cwd;

my %required_tag;

$required_tag{"-"} = 1;

while ($#ARGV ne -1) {
    my $arg = shift @ARGV;

    if ($arg =~ /^--required-tag=(.*)/) {
        $required_tag{$1} = 1;
    }
    else {
        die "Bad arg: $arg";
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
            # We would like to just check for an _darcs directory here,
            # but in an lndir tree we avoid making _darcs directories,
            # so it doesn't exist. We therefore require that every repo
            # has a LICENSE file instead.
            if (! -f "$dir/LICENSE") {
                print STDERR "Error: $dir/LICENSE doesn't exist.\n";
                die "Maybe you haven't done './darcs-all get'?";
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

# Alas, darcs doesn't handle file permissions, so fix a few of them.
for my $file ("boot", "darcs-all", "validate") {
    chmod 0755, $file if -f $file
        or die "Can't chmod 0755 $file: $!";
}
