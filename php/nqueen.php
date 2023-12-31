<?php
# explicit JIT: php83 -dopcache.enable_cli=1 -dopcache.jit_buffer_size=100M nqueen.php
# though it seems that php83 enables JIT by default

function nq_solve(int $n): int {
	$a = array_fill(0, $n, -1);
	$l = array_fill(0, $n, 0);
	$c = array_fill(0, $n, 0);
	$r = array_fill(0, $n, 0);
	$y0 = (1<<$n) - 1;
	$m = 0;
	$k = 0;
	while ($k >= 0) {
		$y = ($l[$k] | $c[$k] | $r[$k]) & $y0;
		if (($y ^ $y0) >> ($a[$k] + 1)) {
			$i = $a[$k] + 1;
			while ($i < $n && ($y & 1<<$i)) {
				++$i;
			}
			if ($k < $n - 1) {
				$z = 1<<$i;
				$a[$k++] = $i;
				$l[$k] = ($l[$k-1] | $z) << 1;
				$c[$k] =  $c[$k-1] | $z;
				$r[$k] = ($r[$k-1] | $z) >> 1;
			} else {
				++$m;
				--$k;
			}
		} else {
			$a[$k--] = -1;
		}
	}
	return $m;
}

$n = 15;
$m = nq_solve($n);
echo $m, "\n";
?>
