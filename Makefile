#!/usr/bin/make

.PHONY : all deps compile test clean
all: deps compile test
REBAR?=rebar
REBAR3?=rebar3
deps:
	@$(REBAR3) get-deps
compile:
	@$(REBAR3) compile
	@$(REBAR3) escriptize
	@cp -f _build/default/bin/covertool .
test:
	-@$(REBAR3) eunit
clean:
	@$(REBAR3) clean
	@rm -f covertool
