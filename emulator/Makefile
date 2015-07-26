.PHONY: debug make_g testerlfiles

debug: make_g _build/gluon testerlfiles
	gdb _build/gluon

testerlfiles:
	cd ../test/ && make && cd ../emulator

make_g:
	cd _build && make && cd ..