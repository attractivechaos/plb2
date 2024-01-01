#!/usr/bin/perl -w

use strict;
use warnings;

&main;

sub main {
	my $n = $ARGV[0] || 1000;
	$n = int($n/2) * 2;
	my (@a, @b, @x);
	&matgen($n, \@a);
	&matgen($n, \@b);
	&matmul(\@a, \@b, \@x);
	print $x[$n/2][$n/2], "\n";
}

sub matgen {
	my ($n, $a) = @_;
	@$a = ();
	my $tmp = 1. / $n / $n;
	for my $i (0 .. $n - 1) {
		for my $j (0 .. $n - 1) {
			$a->[$i][$j] = $tmp * ($i - $j) * ($i + $j);
		}
	}
}

sub matmul {
	my ($a, $b, $x) = @_;
	my $m = @$a;
	my $n = @{$a->[0]};
	my $p = @{$b->[0]};
	my @c;
	for my $i (0 .. $m - 1) {
		@{$x->[$i]} = (0) x $n;
		my $xi = \@{$x->[$i]};
		for my $k (0 .. $n - 1) {
			my $aik = $a->[$i][$k];
			my $bk = \@{$b->[$k]};
			for my $j (0 .. $p - 1) {
				$xi->[$j] += $aik * $bk->[$j];
			}
		}
	}
}
