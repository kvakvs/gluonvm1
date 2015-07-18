.PHONY: all
all: asm_genop genop vm_loop vm_copypaste_impl
#vm_jump_table 

.PHONY: asm_genop g_genop
asm_genop:
	./genop.py > ../apps/asm/src/asm_genop.erl
genop:
	./genop_emu.py > ../emulator/src/g_genop.h
vm_loop:
	./vm_loop.py > ../emulator/src/g_vm_loop.cpp
#vm_jump_table:
#	./vm_jump_table.py > ../emulator/src/g_vm_jump_table.inl
vm_copypaste_impl:
	./vm_copypaste_impl.py > vm_copypaste_impl.txt
#vm_opcode_impl:
#	./vm_opcode_impl.py > ../emulator/src/g_vm_opcode_impl.inl
