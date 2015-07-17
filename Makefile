.PHONY: all clean compile console test

all: compile

clean:
	@ rebar skip_deps=true clean
	@- rm -df ebin
	@- rm -f erl_crash.dump

compile:
	@ rebar skip_deps=true compile

console: compile
	@ erl -pa ebin

test:
	@ rebar skip_deps=true eunit
