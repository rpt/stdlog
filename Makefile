.PHONY: all clean compile compile-all console deps test

all: deps compile-all

clean:
	@ rebar skip_deps=true clean
	@- rm -df ebin
	@- rm -f erl_crash.dump

compile:
	@ rebar skip_deps=true compile

compile-all:
	@ rebar compile

console: compile
	@ erl -pa ebin deps/*/ebin

deps:
	@ rebar get-deps

test:
	@ rebar skip_deps=true eunit
