REBAR=./rebar3

all: xref compile

compile:
	@${REBAR} compile

clean:
	@${REBAR} clean

deps:
	@${REBAR} get-deps

xref:
	@${REBAR} xref

dialyzer:
	@${REBAR} dialyzer

ct:
	@${REBAR} ct

.PHONY: deps
