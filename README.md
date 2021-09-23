Elevators Demo
==============

Elevator control system demonstrating Erlang/OTP upgrades.

Recent updates
--------------

This is the **classic** branch of the demo repo. It includes all the
recent updates, and in addition it also includes a copy of the `gs`
modules from the `OTP-19.3.6.13` release of Erlang/OTP -- the last
stable release with `gs`.

The sole purpose of providing this branch is so that those wishing to
modernize the graphics interface have a working version to work
from. However, there is no guarantee that it will work on all
platforms!

Details about the addition of `gs` source code can be found in
`apps/gs/README.md`.

History
-------

Originally written by Håkan Huss in the late 90s at Erlang Systems.

It was part of the 5-day Erlang Literacy course aimed at test and
support staff of the GPRS project. It was used as a lab exercise
in several parts of the course to practise different topics such
as behaviours, supervisors, release handling, etc. The first exercise
using the elevator example was the mentioned "bug" that the elevator
always stopped at every floor (it is actually not a bug, but an
elevator scheduler which is trivially guaranteed to fulfill the
requirements on elevator scheduling used in the example). Several
refinements of the scheduling were made in the course lab exercises.

Since the purpose of the exercise was to use many different aspects
of Erlang and OTP, the code has been written to use many behaviours
rather than to be the "best" solution for the problem. The resulting
architecture should still be pretty good, though.

It has also been used to demonstrate how Quick Check can test
elevator systems, and featured in a few academic papers.

Thanks go to Francesco Cesarini and Håkan Huss for the historical
informations.

In 2013 Loïc Hoguin made the software avialable in a public
[repository](https://github.com/extend/elevators/), and made the
[announcement](http://erlang.org/pipermail/erlang-questions/2013-January/071433.html)
on the Erlang mailing list. The repo received some updates during 2013
and 2014. This repo is the continuation of the earlier efforts.

Quick start
-----------

 *  Make sure you have `make` and `rebar3` installed and in your `$PATH`
 *  Type `make` to build everything
 *  Start the release with

> `./_build/default/rel/elevators/bin/elevators console`

 *  Make elevators move and notice they stop at each floor
 *  (Exercise: Find bugs, fix them and build a release upgrade)
 *  Place fixes in `upgrade/scheduler.erl`
 *  Run `./v1to2.sh`

Note that `v1to2.sh` will build the releases in `_build/` and deploy
them in `_tmp/elevators/`. The `_build` and `_tmp/elevators/`
directories are deleted and recreated each time the script is run!

You can also use `rebar3 shell` to start the simulator and experiment
with it.

### Working with releases

* Run `./v1to2.sh`
* `cd _tmp/elevators`
* `./bin/elevators daemon_attach` to connect to the server
* have a play ...
* leave the console with `ctrl-D`
  * using `ctrl-C` or `ctrl-G` to exit will also stop the simulator!
* live switch between the two versions with:
  * `./bin/elevators downgrade 1.0`
  * `./bin/elevators upgrade 2.0`
  * You can do this while the elevators are moving and see the change
    take effect immediately :)

Troubleshooting
---------------

The current compilation process prepares the demo entirely, but doesn't
double check things. If something fails during the compilation or the
release generation, you might want to reset the working directory.
