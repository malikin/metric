compile:
	./rebar3 compile

console:
	./rebar3 shell

test:
	./rebar3 ct

dialyzer:
	./rebar3 dialyzer

.PHONY: all compile console test
