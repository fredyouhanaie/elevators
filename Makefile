PROJECT = elevators
REBAR = rebar3

REL_DIR = _build/default/rel/$(PROJECT)

V1 = 1.0
V2 = 2.0

#------------------------------------------------------------------

all: rel1

app:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

#------------------------------------------------------------------

rel1: clean-release app
	@$(REBAR) release --relname $(PROJECT) --relvsn $(V1) \
	&& $(REBAR) tar   --relname $(PROJECT) --relvsn $(V1)

#------------------------------------------------------------------

rel2: clean app
	@$(REBAR) release --relname $(PROJECT) --relvsn $(V2) \
	&& $(REBAR) relup --relname $(PROJECT) --relvsn $(V2) --upfrom $(V1) \
	&& $(REBAR) tar   --relname $(PROJECT) --relvsn $(V2)

#------------------------------------------------------------------

clean-release:
	rm -rf $(REL_DIR)

#------------------------------------------------------------------
