compile:
	./rebar3 compile

console:
	./rebar3 shell

test:
	./rebar3 eunit
	./rebar3 ct

dialyzer:
	./rebar3 dialyzer

build: dialyzer compile test

.PHONY: all compile console test
