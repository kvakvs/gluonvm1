asm: compile
	erl -pa _build/default/lib/*/ebin -s asm_app

emu: compile
	erl -pa _build/default/lib/*/ebin -s emu_app


prototype: compile
	erl -pa _build/default/lib/*/ebin -s prototype_app

compile:
	rebar3 compile
