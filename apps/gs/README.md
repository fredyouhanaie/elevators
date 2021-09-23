# The `gs` code for the `elevators` demo

The files in this directory have been copied from the `OTP-19.3.6.13`
release of Erlang/OTP on github.

Only the minimal subset of `lib/gs` have been taken, enough to enable
us run the original graphics part of the demo. These include:

* The `LICENSE.TXT` from the top directory
* The entire `lib/gs/priv` directory
* The entire `lib/gs/src` directory, except `Makefile`

The `Makefile` in the `lib/gs/src` directory has been replaced with a
simpler one that serves our needs. The new file is there so that the
`gstk_generic.hrl` file can be generated for the `rebar3` run.

The make command is initiated through rebar3 pre-hook entries.

While this works on a linux laptop, there is no guarantee that it will
also work elsewhere! Free free to provide feedback via the issue
tracker.
