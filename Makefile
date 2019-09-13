#!/usr/bin/make

.PHONY : all deps compile test clean
all: deps compile test
REBAR?=rebar
deps:
	@$(REBAR) get-deps
compile:
	@$(REBAR) compile escriptize
test:
	-@$(REBAR) skip_deps=true eunit
clean:
	@$(REBAR) clean
