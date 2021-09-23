
# generate the minimal files so that rebar3 can build the gs
# application

gs: gstk_generic.hrl

gstk_generic.hrl: gs_make.beam gs.beam
	erl -s gs_make -s erlang halt -noshell

gs_make.beam: gs_make.erl
	erlc gs_make.erl

gs.beam: gs.erl
	erlc gs.erl

clean:
	rm -f *.beam gstk_generic.hrl
