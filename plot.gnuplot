set terminal svg

set xlabel "Room size"
set ylabel "Runtime in seconds"
set grid

set ytics 50
set out "plot_naive.svg"
plot "x" u 1:3 title 'Mercury/binary naive' w linespoints,\
     "x" u 1:5 title 'Mercury/Java naive' w linespoints,\
     "x" u 1:7 title 'ECLiPSe-CLP naive' w linespoints

set ytics 10
set out "plot_stndaln.svg"
plot "x" u 1:4 title 'Mercury/binary standalone' w linespoints,\
     "x" u 1:6 title 'Mercury/Java standalone' w linespoints,\
     "x" u 1:8 title 'ECLiPSe-CLP standalone' w linespoints

set ytics 50
set out "plot_binary.svg"
plot "x" u 1:2 title 'Mercury/binary naive with tabling' w linespoints,\
     "x" u 1:3 title 'Mercury/binary naive' w linespoints,\
     "x" u 1:4 title 'Mercury/binary standalone' w linespoints

set ytics 10
set out "plot_best.svg"
plot "x" u 1:2 title 'Mercury/binary naive with tabling' w linespoints,\
     "x" u 1:4 title 'Mercury/binary standalone' w linespoints,\
     "x" u 1:6 title 'Mercury/Java standalone' w linespoints,\
     "x" u 1:8 title 'ECLiPSe-CLP standalone' w linespoints

