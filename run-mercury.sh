for i in 3 10 15 20 30 40 50 60 70 80
do
        sed --in-place -e "s/^room_size = .\\+\\./room_size = $i\\./" maze.m
        sed --in-place -e "s/^room_size(.\\+\\./room_size($i)\\./" maze.ecl

        echo -n "$i: "

        sed --in-place -e 's/^%:- pragma memo(pos\/1/:- pragma memo(pos\/1/' maze.m
        mmc --make maze >/dev/null 2>/dev/null || (echo "Compile error." && exit)
        /usr/bin/time -f "%es" ./maze 2>&1 >/dev/null | tr -d '\n'
        echo -n " "

        mmc --make maze >/dev/null 2>/dev/null || (echo "Compile error." && exit)
        sed --in-place -e 's/^:- pragma memo(pos\/1/%:- pragma memo(pos\/1/' maze.m
        /usr/bin/time -f "%es" ./maze 2>&1 >/dev/null | tr -d '\n'
        echo -n " "

        eclipse -b maze.ecl -e 'profile(maze:main)' | grep "Total user time" | sed -e 's/.\+: //' | tr -d '\n'
        echo ""
done

