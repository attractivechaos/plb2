.SUFFIXES:.eps .pdf
.PHONY:all clean

all: time-full2.pdf time-zoom2.pdf time-zoom3.pdf time-zoom4.pdf

time-full2.eps time-zoom2.eps time-zoom3.eps time-zoom4.eps: plot.gp ../README.md
	gnuplot plot.gp

.eps.pdf:
	epstopdf $< $o
