#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Std;

my %opts = (n=>2);
getopts("n:j", \%opts);

my $state = 0;
my @a;
while (<>) {
	chomp;
	my $line = $_;
	if (/^\|\s*Label/) {
		$state = 1;
	} elsif ($state && /^\|[^:]/) {
		my @t = split(/\|/, $line);
		my @b = ($t[1]);
		for my $i (5 .. 8) {
			push(@b, $t[$i] =~ /(\d+\.\d+)/? $1 + 0.0 : 0.0);
		}
		if (defined $opts{j}) {
			next if ($t[1] =~ /js:(k8|deno)/);
		}
		if ($opts{n} == 2) {
			if ($b[1] > 0.0 && $b[2] > 0.0) {
				push(@a, \@b);
			}
		} elsif ($opts{n} == 3) {
			if ($b[1] > 0.0 && $b[2] > 0.0 && $b[3] > 0.0) {
				push(@a, \@b);
			}
		} elsif ($opts{n} >= 4) {
			if ($b[1] > 0.0 && $b[2] > 0.0 && $b[3] > 0.0 && $b[4]) {
				push(@a, \@b);
			}
		}
	}
}

if ($opts{n} == 2) {
	for my $x (sort { ($a->[1] + $a->[2]) <=> ($b->[1] + $b->[2]) } @a) {
		print(join("\t", @{$x}), "\n");
	}
} elsif ($opts{n} == 3) {
	for my $x (sort { ($a->[1] + $a->[2] + $a->[3]) <=> ($b->[1] + $b->[2] + $b->[3]) } @a) {
		print(join("\t", @{$x}), "\n");
	}
} elsif ($opts{n} >= 4) {
	for my $x (sort { ($a->[1] + $a->[2] + $a->[3] + $a->[4]) <=> ($b->[1] + $b->[2] + $b->[3] + $b->[4]) } @a) {
		print(join("\t", @{$x}), "\n");
	}
}
