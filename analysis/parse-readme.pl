#!/usr/bin/env perl

use strict;
use warnings;

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
		if ($b[1] > 0.0 && $b[2] > 0.0) {
			push(@a, \@b);
		}
	}
}
for my $x (sort { ($a->[1] + $a->[2]) <=> ($b->[1] + $b->[2]) } @a) {
	print(join("\t", @{$x}), "\n");
}
