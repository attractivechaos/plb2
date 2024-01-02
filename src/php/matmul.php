<?php
function matgen(int $n) {
	$a = array_fill(0, $n, array_fill(0, $n, 0));
	$tmp = 1.0 / $n / $n;
	for ($i = 0; $i < $n; ++$i)
		for ($j = 0; $j < $n; ++$j)
			$a[$i][$j] = $tmp * ($i - $j) * ($i + $j);
	return $a;
}
function matmul(int $n, $a, $b) {
	$c = array_fill(0, $n, array_fill(0, $n, 0));
	for ($i = 0; $i < $n; ++$i) {
		for ($k = 0; $k < $n; ++$k) {
			$aik = $a[$i][$k];
			$bk = $b[$k];
			for ($j = 0; $j < $n; ++$j)
				$c[$i][$j] += $aik * $bk[$j];
		}
	}
	return $c;
}
$n = 1500;
$a = matgen($n);
$b = matgen($n);
$c = matmul($n, $a, $b);
echo $c[$n>>1][$n>>1], "\n";
?>
