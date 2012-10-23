prGolog
=======

This is a situation calculus- and Golog-based system written in
[Mercury][Mercury].
See this [paper][Paper] or [these slides][Slides] for more information.

Contact: [Christoph Schwering][Schwering] (schwering at kbsg.rwth-aachen.de).


Directories
-----------

The following shortly describes what's in the different directories.

### `prgolog`
Contains the raw prGolog interpreter without support for continuous
which we move to the basic aciton theories since not all domains
need it. Domains need to implement the `bat` typeclass.

### `domain`
Contains interfaces (typeclasses `obs_bat`, `pr_bat`, `obs_source`)
that need to be implemented by modules that either implement a basic
action theory for plan recognition or provide access to observations,
respectively.
It also contains the implementation of the car domain with
continuous change and time (implements `obs_bat` and `pr_bat`) and
provides a consumer-producer-way of storing observations.

### `planrecog`
Using the aforementioned typeclasses `bat`, `obs_bat`, `pr_bat` and
`obs_source`, the plan recognition system incrementally executes a
hypothesis program and merges incoming observations into the program
to provide an online way of plan recognition.

### `lp-server`
Provides a simple stand-alone server for solving linear programs.
Communication (the objective, constraints, and solutions) is done
either via TCP or UNIX sockets. The solver is COIN Osi Clp.

### `osi`
Provides facilities to solve linear systems of equations. The
underlying solver is Coin Osi Clp. It is either linked dynamically
or access via the lp-server.

### `cars-server`
Listens to a TCP socket, enqueues incoming observations and runs
plan recognition on these observations.

### `cars-main`
TODO

### `util`
Contains some general helper predicates and functions.

### `maze`
A simple maze scenario in which an agent starts at some cell and
searches for a goal destination. This has actually nothing to do
with plan recognition but serves as a comparison of [Mercury][Mercury]
and [ECLiPSe-CLP][ECLiPSe] implementation.



Installation
------------

First install the [Mercury compiler][Mercury] and then compile:

    $ make depend
    $ make

Now you should have some libaries in `lib`:

    $ ls lib/mercury/lib/hlc.par.gc/*.so
    lib/mercury/lib/hlc.par.gc/libdomain.so
    lib/mercury/lib/hlc.par.gc/libplanrecog.so
    lib/mercury/lib/hlc.par.gc/libutil.so
    lib/mercury/lib/hlc.par.gc/libosi.so
    lib/mercury/lib/hlc.par.gc/libprgolog.so
    lib/mercury/lib/hlc.par.gc/libvisual.so

And three binaries:

    $ ls cars-main/cars cars-server/cars_server lp-server/lp_server 
    cars-main/cars
    cars-server/cars_server
    lp-server/lp_server

The cars binary can be fed with observations from stdin.

The cars_server binary is a bit harder to use.
Given a configured system, you just have to run `./cars-server.sh`
which then starts an `lp_server` process and the `cars-server/cars_server`.
The `cars-server/cars_server` process connects to the `lp-server/lp_server`.
You could then run a specially configured [TORCS][TORCS] which sends
observations to the `cars-server/cars_server` process.
If you set up an SSH tunnel with `cars-server/sshtunnel.sh`, you can
run `cars-server.sh` on a remote machine and your locally running TORCS
game will use the remotely running plan recognition.

I'm sorry the system is currently more or less unusable without TORCS;
I hope to fix that soon.


Todo
----

* a new BAT for traffic
* make things usable without TORCS and all the configuration stuff
* find a unit testing framework
* write tests



[Paper]: http://www.aaai.org/ocs/index.php/WS/AAAIW12/paper/view/5281
[Slides]: http://www-kbsg.informatik.rwth-aachen.de/~schwering/CogRob-2012/slides.html
[Schwering]: http://www.kbsg.rwth-aachen.de/~schwering/
[Mercury]: http://www.mercury.csse.unimelb.edu.au/search.html
[ECLiPSe]: http://www.eclipseclp.org/
[TORCS]: http://torcs.sourceforge.net/

