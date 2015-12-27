.PHONY: debug compile

debug: compile
	gdb _build/llvm-gluon

compile:
	cd _build && make -j7 && cd ..
