.PHONY: all clean compile console test

all: compile

clean:
	@ rebar skip_deps=true clean
	@- rmdir ebin

compile:
	@ rebar skip_deps=true compile

console: compile
	@ erl -pa ebin

test:
	@ rebar skip_deps=true eunit
