run: compile
	erl -pa _build/default/lib/asm/ebin -s asm_app

compile:
	rebar3 compile
