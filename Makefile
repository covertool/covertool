#!/usr/bin/make
REBAR=rebar

.PHONY : all compile test clean
all: compile test
compile:
	@$(REBAR) compile escriptize
test:
	-@$(REBAR) eunit
clean:
	@$(REBAR) clean
