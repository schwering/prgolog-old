set yrange [-10.0 : 10.0]
set terminal postscript
set nokey
set xlabel "time [s]"
set xtics 3
set ylabel "lateral position [m]"
set ytics 2.5

set datafile missing 'NaN'

left_line(x) = 5
middle_line(x) = 0
right_line(x) = -5

plot \
"traces/0000.dat" using 1:5 with lines linetype 1 linewidth 2,\
"traces/0000.dat" using 1:5:7:9 with yerrorbars pointtype 0 linetype 1,\
"traces/0000.dat" using 1:3 with points pointtype 6,\
middle_line(x) with lines linetype 3 linewidth 2,\
left_line(x) with lines linetype 1 linewidth 2,\
right_line(x) with lines linetype 1 linewidth 2

