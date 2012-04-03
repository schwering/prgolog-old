set terminal svg

set xlabel "Room width and height"
set ylabel "Runtime in seconds"
set grid

set key left top

# Mercury binary tabling+naive, naive, standalone:
set style line 2 lc rgb '#0060ad' lt 1 lw 2 pt 12 ps 1.6
set style line 3 lc rgb '#0060ad' lt 1 lw 2 pt 6 ps 1.4
set style line 4 lc rgb '#0060ad' lt 1 lw 2 pt 7 ps 1.1

# Mercury Java naive, standalone:
set style line 5 lc rgb '#ff6633' lt 1 lw 2 pt 6 ps 1.4
set style line 6 lc rgb '#ff6633' lt 1 lw 2 pt 7 ps 1.1

# ECLiPSe-CLP naive, standalone:
set style line 7 lc rgb '#008800' lt 1 lw 2 pt 6 ps 1.4
set style line 8 lc rgb '#008800' lt 1 lw 2 pt 7 ps 1.1

set ytics 10
set out "plot_naive.svg"
plot "x" u 1:3 title 'Mercury/binary, naive' w linespoints ls 3,\
     "x" u 1:5 title 'Mercury/Java, naive' w linespoints ls 5,\
     "x" u 1:7 title 'ECLiPSe-CLP, naive' w linespoints ls 7

set ytics 2
set out "plot_stndaln.svg"
plot "x" u 1:4 title 'Mercury/binary, standalone' w linespoints ls 4,\
     "x" u 1:6 title 'Mercury/Java, standalone' w linespoints ls 6,\
     "x" u 1:8 title 'ECLiPSe-CLP, standalone' w linespoints ls 8

set ytics 10
set out "plot_binary.svg"
plot "x" u 1:2 title 'Mercury/binary, naive with tabling' w linespoints ls 2,\
     "x" u 1:3 title 'Mercury/binary, naive' w linespoints ls 3,\
     "x" u 1:4 title 'Mercury/binary, standalone' w linespoints ls 4

set ytics 2
set out "plot_best.svg"
plot "x" u 1:2 title 'Mercury/binary, naive with tabling' w linespoints ls 2,\
     "x" u 1:4 title 'Mercury/binary, standalone' w linespoints ls 4,\
     "x" u 1:6 title 'Mercury/Java, standalone' w linespoints ls 6,\
     "x" u 1:8 title 'ECLiPSe-CLP, standalone' w linespoints ls 8

set ytics 10
set out "plot_all.svg"
plot "x" u 1:2 title 'Mercury/binary, naive with tabling' w linespoints ls 2,\
     "x" u 1:3 title 'Mercury/binary, naive' w linespoints ls 3,\
     "x" u 1:4 title 'Mercury/binary, standalone' w linespoints ls 4,\
     "x" u 1:5 title 'Mercury/Java, naive' w linespoints ls 5,\
     "x" u 1:6 title 'Mercury/Java, standalone' w linespoints ls 6,\
     "x" u 1:7 title 'ECLiPSe-CLP, naive' w linespoints ls 7,\
     "x" u 1:8 title 'ECLiPSe-CLP, standalone' w linespoints ls 8

