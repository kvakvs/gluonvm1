.PHONY: codegen debug testerlfiles compile vg

debug: testerlfiles compile
	gdb _build/gluon

testerlfiles:
	cd ../test/ && make && cd ../emulator

compile: codegen
	cd _build && make -j4 && cd ..

vg: compile
	valgrind --leak-check=yes --log-file="valgrind.txt" _build/gluon

codegen:
	cd ../codegen && make && cd ../emulator
