prGolog
=======

This is a [situation calculus][SitCalc]- and [Golog][Golog]-based system
written in [Mercury][Mercury].
See this [paper][Paper] or [these slides][Slides] for more information.

Contact: [Christoph Schwering][Schwering] (schwering at kbsg.rwth-aachen.de).


Directories
-----------

The following shortly describes what's in the different directories.

### `prgolog`
Contains the raw prGolog interpreter.
The interpreter closely follows the formal semantics in many ways.
It implements the [paper][Paper]'s transition semantics which allows for
incremental program execution with the following programming constructs:
sequence, "nondeterministic" branch, loop and pick, concurrency by
interleaving, atomic complex actions (they cannot be interleaved with
concurrent programs), constant primitive actions, situation-dependent
primitive actions (allows for functional fluents in actions) and
procedure calls.
Nondeterminism is resolved decision-theoretically by opting for the
alternative that leads to the highest rewarded situation.
Two other features are not implemented: continuous time and stochastic
actions. The former can be easily and much more flexibly implemented in
the basic action theory (BAT). The latter can be implemented in the BAT
by sampling.
Domains need to implement the `bat` typeclass to use the prGolog
interpreter.

### `domain`
Contains interfaces (typeclasses `obs_bat`, `pr_bat`, `obs_source`)
that need to be implemented by modules that either implement a basic
action theory (BAT) for plan recognition or provide access to
observations, respectively.
Furthermore there are two implementations of `obs_bat` and `pr_bat` for
the automotive domain: The first one uses a simple but unrealistic
global-coordinate model and a linear constraint solver. The second is
based on a relative spatio-temporal calculus and simple searches to
solve the constraints.
Finally two implementations of `obs_source` are available: The first one
reads observations from `stdin`. The second one listens for observations
sent by our [TORCS][TORCS] instance.

### `planrecog`
Using the aforementioned typeclasses `bat`, `obs_bat`, `pr_bat` and
`obs_source`, the plan recognition system incrementally executes a
hypothesis program and merges incoming observations into the program
to provide an online way of plan recognition.

### `osi`
Provides facilities to solve linear systems of equations. The
underlying solver is Coin Osi Clp. It is either linked dynamically
or access via the `lp-server` (recommended).

### `lp-server`
Provides a simple stand-alone server for solving linear programs.
Communication (the objective, constraints, and solutions) is done either
via TCP or UNIX sockets. The solver back-end is COIN Osi Clp.

### `cars-server`
Listens to a TCP socket, enqueues incoming observations and runs plan
recognition on these observations.
For the global-coordinate BAT, there's a nice visualization using
NCurses.

### `cars-main`
Reads observations stdin and feeds them to the plan recognition system.
The results are simply printed to stdout.
Primarily for simple testing purposes when `cars-server` doesn't work
for some reason.

### `util`
Contains a bunch general helper predicates and functions.

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

And four binaries:

    $ ls cars-main/cars cars-server/cars_server lp-server/lp_server 
    cars-main/cars
    cars-server/cars_server
    cars-server/replay
    lp-server/lp_server

The cars binary can be fed with observations from `stdin`.
You can simply pipe the observations from a text-file.

The `cars_server` binary is a bit harder to use, but the script `server.sh`
helps alot.
It starts an `lp_server` in the background (which is only needed if you
use the global-coordinate-based BAT).
Then it starts a `cars_server` process which connects to the `lp_server`
and listens for incoming connections from observation sources.

We have extended a the racing game [TORCS][TORCS] (via a so-called robot)
to function as driving simulator and observation source.
The game sends all observations to the plan recognition system.
As these observations are logged, they can be replayed using the binary
`cars-server/replay` which imitates a TORCS instance and sends the logged
observations to the plan recognition system.

I'm sorry the system is currently more or less unusable without TORCS;
I hope to fix that soon or at least publish our TORCS robot and the
appropriate configuration.


Todo
----

* more plans, experiments and great results
* another simulator


[SitCalc]: http://en.wikipedia.org/wiki/Situation_calculus
[Golog]: http://www.cs.toronto.edu/cogrobo/main/
[Paper]: http://www.aaai.org/ocs/index.php/WS/AAAIW12/paper/view/5281
[Slides]: http://www-kbsg.informatik.rwth-aachen.de/~schwering/CogRob-2012/slides.html
[Schwering]: http://www.kbsg.rwth-aachen.de/~schwering/
[Mercury]: http://www.mercury.csse.unimelb.edu.au/
[ECLiPSe]: http://www.eclipseclp.org/
[TORCS]: http://torcs.sourceforge.net/

