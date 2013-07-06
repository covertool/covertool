#!/usr/bin/make
REBAR?=rebar

.PHONY : all deps compile test clean
all: deps compile test
deps:
	@$(REBAR) get-deps update-deps
	$(MAKE) -C deps/rebar
compile:
	@$(REBAR) compile escriptize
test:
	-@$(REBAR) skip_deps=true eunit
clean:
	@$(REBAR) clean
