export LD_LIBRARY_PATH=/home/chs/Documents/Prolog/mercury/lib/mercury/lib/hlc.par.gc/ && mk clean && rm -f mercury_interface.* && mk && LD_LIBRARY_PATH=/home/chs/Documents/Prolog/mercury/lib/mercury/lib/hlc.par.gc/
make && cat torcs-overtake-7.log | /usr/bin/time -f "%U + %S = %E / %Mkb / %P" ./cars
