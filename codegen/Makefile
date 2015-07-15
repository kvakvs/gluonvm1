.PHONY: all
all: asm_genop g_genop

.PHONY: asm_genop g_genop
asm_genop:
	./genop.py > asm_genop.erl
g_genop:
	./genop_emu.py > g_genop.h
