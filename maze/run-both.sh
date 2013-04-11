#!/bin/bash

#/usr/bin/time -f "time=%e cpu=%P mem=%Mk swaps=%W ctxinvol=%c ctxvol=%w" ./maze >/dev/null

MERCURY_INST=$(dirname $(which mmc))/..

MERCURY_ENABLE_MEMOIZATION="sed --in-place -e 's/^.\+pragma memo(pos\/1/:- pragma memo(pos\/1/' maze.m"
MERCURY_DISABLE_MEMOIZATION="sed --in-place -e 's/^.\+pragma memo(pos\/1/%:- pragma memo(pos\/1/' maze.m"

NAIVE_UNVISITED="sed --in-place -e 's/^unvisited(P, S1) :- .\+_unvisited(P, S1)\./unvisited(P, S1) :- naive_unvisited(P, S1)\./g' "
STANDALONE_UNVISITED="sed --in-place -e 's/^unvisited(P, S1) :- .\+_unvisited(P, S1)\./unvisited(P, S1) :- standalone_unvisited(P, S1)\./g' "

MERCURY_NAIVE_UNVISITED="${NAIVE_UNVISITED} maze.m"
MERCURY_STANDALONE_UNVISITED="${STANDALONE_UNVISITED} maze.m"

ECLIPSE_NAIVE_UNVISITED="${NAIVE_UNVISITED} maze.ecl"
ECLIPSE_STANDALONE_UNVISITED="${STANDALONE_UNVISITED} maze.ecl"

# --rebuild         rebuild all files, no matter whether or not they changed (otherwise use --make)
# --O6              maximal optimization (very slight impact; choose first, because otherwise it overrides previous flags)
# -H                compile to *high-level* C (faster for single-threaded C because it uses loops; doesn't work with and-parallelism)
# --infer-all       infers modes and types if not set explicitly (but they're set everywhere explicitly in my code)
# --intermod-opt    optimize inter-module calls
# I found these at: http://adventuresinmercury.blogspot.com/search?updated-max=2011-04-04T20:06:00-07:00&max-results=7
# Great blog.
MERCURY_COMPILE="mmc --rebuild -O6 -H --infer-all --intermod-opt maze >/dev/null 2>/dev/null || (echo 'Compile error.' && exit)"
MERCURY_COMPILE_JAVA="mmc --rebuild --java -O6 -H --infer-all --intermod-opt maze >/dev/null 2>/dev/null || (echo 'Compile error.' && exit)"
MERCURY_RUN_JAVA="java -cp Mercury/classs/:${MERCURY_INST}/lib/mercury/lib/java/mer_rt.jar:${MERCURY_INST}/lib/mercury/lib/java/mer_std.jar jmercury.maze"

for i in 3 10 15 20 30 40 50 60 70 80
do
        sed --in-place -e "s/^room_size = .\\+\\./room_size = $i\\./" maze.m
        sed --in-place -e "s/^room_size(.\\+\\./room_size($i)\\./" maze.ecl

        echo -n "$i "

        # Mercury with memoization with naive_unvisited.
        # The memoization of pos/1 compensates the *naive*_unvisited by far.
        # And boom goes the dynamite :-)
        eval ${MERCURY_ENABLE_MEMOIZATION}
        eval ${MERCURY_NAIVE_UNVISITED}
        eval ${MERCURY_COMPILE}
        /usr/bin/time -f "%es" ./maze 2>&1 >/dev/null | tr -d '\n'
        echo -n " "

        # Mercury without memoization with naive_unvisited.
        # Now we see how badly the *naive*_unvisited is without memoization due to the many evaluations of the pos/1 fluent.
        eval ${MERCURY_DISABLE_MEMOIZATION}
        eval ${MERCURY_NAIVE_UNVISITED}
        eval ${MERCURY_COMPILE}
        /usr/bin/time -f "%es" ./maze 2>&1 >/dev/null | tr -d '\n'
        echo -n " "

        # Mercury without memoization with standalone_unvisited.
        # The standalone_unvisited does not call pos/1 that often and is thus faster, but still slower than naive_unvisited with memoized pos/1.
        eval ${MERCURY_DISABLE_MEMOIZATION}
        eval ${MERCURY_STANDALONE_UNVISITED}
        eval ${MERCURY_COMPILE}
        /usr/bin/time -f "%es" ./maze 2>&1 >/dev/null | tr -d '\n'
        echo -n " "

        # Mercury on Java without memoization with naive_unvisited.
        # There's no memoization for Mercury on Java.
        eval ${MERCURY_DISABLE_MEMOIZATION}
        eval ${MERCURY_NAIVE_UNVISITED}
        eval ${MERCURY_COMPILE_JAVA}
        /usr/bin/time -f "%es" ${MERCURY_RUN_JAVA} 2>&1 >/dev/null | tr -d '\n'
        echo -n " "

        # Mercury on Java without memoization with standalone_unvisited.
        # There's no memoization for Mercury on Java.
        eval ${MERCURY_DISABLE_MEMOIZATION}
        eval ${MERCURY_STANDALONE_UNVISITED}
        eval ${MERCURY_COMPILE_JAVA}
        /usr/bin/time -f "%es" ${MERCURY_RUN_JAVA} 2>&1 >/dev/null | tr -d '\n'
        echo -n " "

        # ECLiPSe-CLP without memoization with naive_unvisited.
        # There's no memoization for ECLiPSe-CLP.
        eval ${ECLIPSE_NAIVE_UNVISITED}
        eclipse -b maze.ecl -e 'profile(maze:main)' | grep "Total user time" | sed -e 's/.\+: //' | tr -d '\n'
        echo -n " "

        # ECLiPSe-CLP without memoization with standalone_unvisited.
        # There's no memoization for ECLiPSe-CLP.
        eval ${ECLIPSE_STANDALONE_UNVISITED}
        eclipse -b maze.ecl -e 'profile(maze:main)' | grep "Total user time" | sed -e 's/.\+: //' | tr -d '\n'
        echo ""
done

