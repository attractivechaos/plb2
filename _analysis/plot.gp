#set t pdfcairo transparent enh font "Helvetica,16"
set t po eps co so enh "Helvetica,18"

set style line 1 lt 1 lc rgb "#fbb4ae" lw 1;
set style line 2 lt 1 lc rgb "#b3cde3" lw 1;
set style line 3 lt 1 lc rgb "#ccebc5" lw 1;
set style line 4 lt 1 lc rgb "#decbe4" lw 1;

set size 1,1

set style histogram rowstacked
set xtics rotate by 40 right nomirror font "Helvetica,16"
set boxwidth 0.8 relative
set style data histograms
set style fill solid 1.0 border lt -1
#set style fill pattern 7 border lt -1
set ylab "Elapsed time (sec)" off +0.0,0
set bmargin 5
set lmargin 8
set key top left

set title "On arm64-darwin (sorted by nqueens + matmul)"

set out "time-all.eps"
plot \
	"<k8 parse-readme.js ../README.md | grep -v k8 | grep -v deno" u ($2):xtic(1) t 'nqueens' ls 1, \
	"" u ($3) t 'matmul' ls 2, \
	"" u ($4) t 'sudoku' ls 3, \
	"" u ($5) t 'bedcov' ls 4

set out "time-zoom.eps"
set yran [0:40]
plot \
	"<k8 parse-readme.js ../README.md | grep -v k8 | grep -v deno" u ($2):xtic(1) t 'nqueens' ls 1, \
	"" u ($3) t 'matmul' ls 2, \
	"" u ($4) t 'sudoku' ls 3, \
	"" u ($5) t 'bedcov' ls 4
