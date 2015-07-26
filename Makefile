.PHONY: codegen test_erl asm compile
compile:
	rebar3 compile

#asm: codegen test_erl compile
#asm: test_erl
#	erl -pa _build/default/lib/*/ebin -s asm_app

#emu: codegen compile
#	erl -pa _build/default/lib/*/ebin -s emu_app

# Compile test scripts in test/ dir
test_erl:
	cd test && ./test.sh && cd ..

# Run codegen phase
codegen:
	cd codegen && make && cd ..

#prototype: compile
#	erl -pa _build/default/lib/*/ebin -s prototype_app
