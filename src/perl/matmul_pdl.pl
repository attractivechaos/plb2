#!/usr/bin/env perl

use warnings;
use strict;
use PDL;

my $n = $ARGV[0] || 1500;
$n = int($n/2) * 2;

my $a = generate_matrix($n);
my $b = generate_matrix($n);

my $x = $a x $b;
print $x->at($n/2, $n/2), "\n";

sub generate_matrix {
    my $n = shift;
    my $c = 1 / $n / $n;
    my $i = xvals($n, $n);
    my $j = yvals($n, $n);
    return $c * ($i - $j) * ($i + $j);
}
