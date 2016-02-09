#!/usr/bin/env perl

use warnings;
use strict;

use Cwd;
use File::Path 'rmtree';
use File::Basename;

my %required_tag;
my $validate;
my $curdir;

# See Trac #11530
$ENV{GREP_OPTIONS} = '';

$required_tag{"-"} = 1;
$validate = 0;

$curdir = &cwd()
    or die "Can't find current directory: $!";

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

sub sanity_check_line_endings {
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

sub sanity_check_tree {
    my $tag;
    my $dir;

    if (-d ".git"  &&
        system("git config remote.origin.url | grep github.com > /dev/null") == 0 &&
        system("git config --get-regexp '^url.*github.com/.*/packages-.insteadOf' > /dev/null") != 0) {
        # If we cloned from github, make sure the url rewrites are set.
        # Otherwise 'git submodule update --init' prints confusing errors.
        die <<EOF;
It seems you cloned this repository from GitHub. But your git config files
don't contain the url rewrites that are needed to make this work (GitHub
doesn't support '/' in repository names, so we use a different naming scheme
for the submodule repositories there).

Please run the following commands first:

  git config --global url."git://github.com/ghc/packages-".insteadOf     git://github.com/ghc/packages/
  git config --global url."http://github.com/ghc/packages-".insteadOf    http://github.com/ghc/packages/
  git config --global url."https://github.com/ghc/packages-".insteadOf   https://github.com/ghc/packages/
  git config --global url."ssh://git\@github.com/ghc/packages-".insteadOf ssh://git\@github.com/ghc/packages/
  git config --global url."git\@github.com:/ghc/packages-".insteadOf      git\@github.com:/ghc/packages/

And then:

  git submodule update --init
  ./boot

Or start over, and clone the GHC repository from the haskell server:

  git clone --recursive git://git.haskell.org/ghc.git

For more information, see:
  * https://ghc.haskell.org/trac/ghc/wiki/Newcomers or
  * https://ghc.haskell.org/trac/ghc/wiki/Building/GettingTheSources#CloningfromGitHub
EOF
    }

    # Check that we have all boot packages.
    open PACKAGES, "< packages";
    while (<PACKAGES>) {
        if (/^#/) {
            # Comment; do nothing
        }
        elsif (/^([a-zA-Z0-9\/.-]+) +([^ ]+) +[^ ]+ +[^ ]+$/) {
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
                    die "Maybe you haven't done 'git submodule update --init'?";
                }
            }
        }
        else {
            die "Bad line in packages file: $_";
        }
    }
    close PACKAGES;
}

# Create libraries/*/{ghc.mk,GNUmakefile}
sub boot_pkgs {
    my @library_dirs = ();

    my $package;

    for $package (glob "libraries/*/") {
        $package =~ s/\/$//;
        my $pkgs = "$package/ghc-packages";
        if (-f $pkgs) {
            open PKGS, "< $pkgs"
                or die "Failed to open $pkgs: $!";
            while (<PKGS>) {
                chomp;
                s/\r//g;
                if (/.+/) {
                    push @library_dirs, "$package/$_";
                }
            }
        }
        else {
            push @library_dirs, $package;
        }
    }

    for $package (@library_dirs) {
        my $dir = &basename($package);
        my @cabals = glob("$package/*.cabal.in");
        if ($#cabals < 0) {
            @cabals = glob("$package/*.cabal");
        }
        if ($#cabals > 0) {
            die "Too many .cabal file in $package\n";
        }
        if ($#cabals eq 0) {
            my $cabal = $cabals[0];
            my $pkg;
            my $top;
            if (-f $cabal) {
                $pkg = $cabal;
                $pkg =~ s#.*/##;
                $pkg =~ s/\.cabal.in$//;
                $pkg =~ s/\.cabal$//;
                $top = $package;
                $top =~ s#[^/]+#..#g;
                $dir = $package;
                $dir =~ s#^libraries/##g;

                print "Creating $package/ghc.mk\n";
                open GHCMK, "> $package/ghc.mk"
                    or die "Opening $package/ghc.mk failed: $!";
                print GHCMK "${package}_PACKAGE = ${pkg}\n";
                print GHCMK "${package}_dist-install_GROUP = libraries\n";
                print GHCMK "\$(if \$(filter ${dir},\$(PACKAGES_STAGE0)),\$(eval \$(call build-package,${package},dist-boot,0)))\n";
                print GHCMK "\$(if \$(filter ${dir},\$(PACKAGES_STAGE1)),\$(eval \$(call build-package,${package},dist-install,1)))\n";
                print GHCMK "\$(if \$(filter ${dir},\$(PACKAGES_STAGE2)),\$(eval \$(call build-package,${package},dist-install,2)))\n";
                close GHCMK
                    or die "Closing $package/ghc.mk failed: $!";

                print "Creating $package/GNUmakefile\n";
                open GNUMAKEFILE, "> $package/GNUmakefile"
                    or die "Opening $package/GNUmakefile failed: $!";
                print GNUMAKEFILE "dir = ${package}\n";
                print GNUMAKEFILE "TOP = ${top}\n";
                print GNUMAKEFILE "include \$(TOP)/mk/sub-makefile.mk\n";
                print GNUMAKEFILE "FAST_MAKE_OPTS += stage=0\n";
                close GNUMAKEFILE
                    or die "Closing $package/GNUmakefile failed: $!";
            }
        }
    }
}

# autoreconf everything that needs it.
sub autoreconf {
    my $dir;
    my $fail;

    foreach $dir (".", glob("libraries/*/")) {
        if (-f "$dir/configure.ac") {
            next if (my $pid = fork);
            die "fork failed: $!" if (! defined $pid);
            print "Booting $dir\n";
            chdir $dir or die "can't change to $dir: $!";
            exec("autoreconf");
            exit 1;
        }
    }

    # Wait for all child processes to finish.
    while (wait() != -1) {
        $fail = 1 if $?;
    }

    die "Running autoreconf failed" if $fail;
}

sub checkBuildMk {
    if ($validate eq 0 && ! -f "mk/build.mk") {
        print <<EOF;

WARNING: You don't have a mk/build.mk file.

By default a standard GHC build will be done, which uses optimisation
and builds the profiling libraries. This will take a long time, so may
not be what you want if you are developing GHC or the libraries, rather
than simply building it to use it.

For information on creating a mk/build.mk file, please see:
    http://ghc.haskell.org/trac/ghc/wiki/Building/Using#Buildconfiguration

EOF
    }
}

&sanity_check_line_endings();
&sanity_check_tree();
&boot_pkgs();
&autoreconf();
&checkBuildMk();

