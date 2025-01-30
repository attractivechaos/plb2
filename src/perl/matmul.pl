#!/usr/bin/perl -w

use strict;
use warnings;

my $n = $ARGV[0] || 1500;
$n = int($n/2) * 2;
my $a = matgen($n);
my $b = matgen($n);
my $x = matmul($a, $b);
print $x->[$n/2][$n/2], "\n";

exit;

sub matgen {
	my ($n) = @_;
	my $a;
	my $tmp = 1 / $n / $n;
	for my $i (0 .. $n - 1) {
		for my $j (0 .. $n - 1) {
			$a->[$i][$j] = $tmp * ($i - $j) * ($i + $j);
		}
	}
	return $a;
}

sub matmul {
	my ($a, $b) = @_;
	my $x;
	my $n = @{$a->[0]};
	my $p = @{$b->[0]};

	for my $i (0 .. @$a - 1) {
		my $xi = $x->[$i] = [ () x $n ];
		for my $k (0 .. $n - 1) {
			my $aik = $a->[$i][$k];
			my $bk  = \@{$b->[$k]};
			for my $j (0 .. $p - 1) {
				$xi->[$j] += $aik * $bk->[$j];
			}
		}
	}
	return $x;
}
