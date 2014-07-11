all: deps compile escriptize

deps:
	rebar get-deps

compile:
	rebar compile

escriptize:
	rebar escriptize
