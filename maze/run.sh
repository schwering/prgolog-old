#!/bin/bash

for i in 3 10 15 20 30 40 50 60 70 80
do
        sed --in-place -e "s/^room_size = .\\+\\./room_size = $i\\./" maze.m
        mmc --make maze >/dev/null 2>/dev/null || (echo "Compile error." && exit)
        echo -n "$i: "
        /usr/bin/time -f "time=%e cpu=%P mem=%Mk swaps=%W ctxinvol=%c ctxvol=%w" ./maze >/dev/null
done

