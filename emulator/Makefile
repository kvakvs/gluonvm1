.PHONY: debug testerlfiles

debug: testerlfiles
	gdb _build/gluon

testerlfiles:
	cd ../test/ && make && cd ../emulator
