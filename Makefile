#!/usr/bin/make

ifdef REBAR
deps:
	$(info REBAR is $(REBAR))
else
REBAR=rebar
deps:
	@$(REBAR) get-deps update-deps
	$(MAKE) -C deps/rebar
endif

.PHONY : all deps compile test clean
all: deps compile test
compile:
	@$(REBAR) compile escriptize
test:
	-@$(REBAR) skip_deps=true eunit
clean:
	@$(REBAR) clean
	@rm cover.xml
