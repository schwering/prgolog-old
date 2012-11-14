#!/bin/bash

for i in 3 10 15 20 30 40 50 60 70 80
do
        sed --in-place -e "s/^room_size(.\\+\\./room_size($i)\\./" maze.ecl
        echo -n "$i: "
        eclipse -b maze.ecl -e 'profile(maze:main)' | grep "Total user time" | sed -e 's/.\+: //'
done

