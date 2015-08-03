.PHONY: debug testerlfiles compile vg

debug: testerlfiles compile
	gdb _build/gluon

testerlfiles:
	cd ../test/ && make && cd ../emulator

compile:
	cd _build && make && cd ..

vg: compile
	valgrind --leak-check=yes --log-file="valgrind.txt" _build/gluon
