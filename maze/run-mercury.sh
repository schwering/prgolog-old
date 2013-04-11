#!/bin/bash

for i in 3 10 15 20 30 40 50 60 70 80
do
        sed --in-place -e "s/^room_size = .\\+\\./room_size = $i\\./" maze.m
        echo -n "$i: "

        sed --in-place -e 's/^%:- pragma memo(pos\/1/:- pragma memo(pos\/1/' maze.m
        mmc --make maze >/dev/null 2>/dev/null || (echo "Compile error." && exit)
        /usr/bin/time -f "%es" ./maze 2>&1 >/dev/null | sed -e 's/\n//' | tr -d '\n'
        echo -n " "

        mmc --make maze >/dev/null 2>/dev/null || (echo "Compile error." && exit)
        sed --in-place -e 's/^:- pragma memo(pos\/1/%:- pragma memo(pos\/1/' maze.m
        /usr/bin/time -f "%es" ./maze 2>&1 >/dev/null
        echo ""
done

