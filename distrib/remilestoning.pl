#!/usr/bin/perl

use strict;

use DBI;

# ===== Config:

my $dbfile = "trac.db";
my $milestone = "7.4.1";
my $test = 0;

# ===== Code:

my $dbh = DBI->connect("dbi:SQLite:dbname=$dbfile","","", {});

my %emailof;
my %ticketsfor;

sub getUserAddress {
    my $sth = $dbh->prepare("SELECT sid, value FROM session_attribute WHERE name = 'email'");
    $sth->execute();
    while (my $result = $sth->fetchrow_hashref("NAME_lc")) {
        my $username = $result->{sid};
        my $email    = $result->{value};
        if (defined($emailof{$username})) {
            die "Two e-mail addresses found for $username";
        }
        if ($email =~ /@/) {
            $emailof{$username} = $email;
        }
        else {
            # warn "The e-mail address $email for $username contains no @";
        }
    }
    $sth->finish;
}

sub doTickets {
    my $sth = $dbh->prepare("SELECT id, summary, reporter, cc FROM ticket WHERE milestone = ? AND status = 'new'");
    $sth->execute($milestone);
    while (my $result = $sth->fetchrow_hashref("NAME_lc")) {
        my $ticket   = $result->{id};
        my $title    = $result->{summary};
        my $reporter = $result->{reporter};
        my $cc       = $result->{cc};
        my %addresses;
        my $address_added;
        for my $who ($reporter, split /[ ,]+/, $cc) {
            $address_added = 0;
            if ($who =~ /@/) {
                $addresses{$who} = 1;
                $address_added = 1;
            }
            if (defined($emailof{$who})) {
                $addresses{$emailof{$who}} = 1;
                $address_added = 1;
            }
            if ($who ne "nobody" && $address_added eq 0) {
                # warn "No address found for $who";
            }
        }
        for my $address (keys(%addresses)) {
            $ticketsfor{$address}{$ticket}{"title"} = $title;
        }
    }
    $sth->finish;
}

sub doEmails {
    for my $email (sort (keys %ticketsfor)) {
        if ($test ne 0) {
            open FH, ">&STDOUT";
        }
        else {
            open(FH, '|-', 'mail', '-s', 'GHC bugs', '-a', 'From: glasgow-haskell-bugs@haskell.org', $email) or die "Running mail failed: $!";
        }
        print FH <<'EOF';

Hello,

You are receiving this mail because you are the reporter, or on the CC
list, for one or more GHC tickets that are automatically having their
priority reduced due to our post-release ticket handling policy:
    http://ghc.haskell.org/trac/ghc/wiki/WorkingConventions/BugTracker#Remilestoningticketsafterarelease

The list of tickets for which you are the reporter or on the CC list is
given below. If any of these are causing problems for you, please let us
know on glasgow-haskell-bugs@haskell.org and we'll look at raising the
priority.

Better still, if you are able to make any progress on any of the tickets
yourself (whether that be actually fixing the bug, or just making it
easier for someone else to - for example, by making a small,
self-contained test-case), then that would be a great help. We at GHC HQ
have limited resources, so if anything is waiting for us to make
progress then it can be waiting a long time!
EOF
        for my $ticket (sort {$a <=> $b} (keys %{$ticketsfor{$email}})) {
            my $title = $ticketsfor{$email}{$ticket}{"title"};
            print FH "\n";
            print FH "#$ticket $title:\n";
            print FH "    http://ghc.haskell.org/trac/ghc/ticket/$ticket\n";
        }
        print FH <<'EOF';

-- 
The GHC Team
http://www.haskell.org/ghc/
EOF
        close FH or die "Close failed: $!";
    }
}

&getUserAddress();
&doTickets();
&doEmails();

