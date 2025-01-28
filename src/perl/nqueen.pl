#!/usr/bin/env perl

use strict;
use warnings;

sub nq_solve() {
	my $n = shift;
	my @a = (-1) x $n;
	my @l = (0) x $n;
	my @c = (0) x $n;
	my @r = (0) x $n;
	my $y0 = (1<<$n) - 1;
	my ($k, $m) = 0;
	while ($k >= 0) {
		my $y = ($l[$k] | $c[$k] | $r[$k]) & $y0;
		if (($y ^ $y0) >> ($a[$k] + 1)) {
			my $i = $a[$k] + 1;
			while ($i < $n && ($y & 1<<$i)) {
				$i++;
			}
			if ($k < $n - 1) {
				my $z = 1<<$i;
				$a[$k++] = $i;
				$l[$k] = ($l[$k-1] | $z) << 1;
				$c[$k] =  $c[$k-1] | $z;
				$r[$k] = ($r[$k-1] | $z) >> 1;
			} else {
				$m++;
				$k--;
			}
		} else {
			$a[$k--] = -1;
		}
	}
	return $m;
}

my $n = 15;
$n = int($ARGV[0]) if @ARGV > 0;
print(&nq_solve($n), "\n");
