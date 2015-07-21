.PHONY: all
all: asm_genop genop vm_loop vm_copypaste_impl vm_predef_atoms
#vm_jump_table 

SRCDIR="../emulator/src"
INCDIR="../emulator/include"

.PHONY: asm_genop g_genop
asm_genop:
	./genop.py > ../apps/asm/src/asm_genop.erl
genop:
	./vm_genop_cpp.py > ${SRCDIR}/g_genop.cpp && \
	./vm_genop_h.py > ${INCDIR}/g_genop.h
vm_loop:
	./vm_loop.py > ${SRCDIR}/g_vm_loop.cpp
#vm_jump_table:
#	./vm_jump_table.py > ../emulator/src/g_vm_jump_table.inl
vm_copypaste_impl:
	./vm_copypaste_impl.py > vm_copypaste_impl.txt
#vm_opcode_impl:
#	./vm_opcode_impl.py > ../emulator/src/g_vm_opcode_impl.inl
vm_predef_atoms:
	./vm_predef_atoms_h.py > ${INCDIR}/g_predef_atoms.h && \
	./vm_predef_atoms_cpp.py > ${SRCDIR}/g_predef_atoms.cpp
