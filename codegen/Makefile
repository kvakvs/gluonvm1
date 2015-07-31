.PHONY: all
all: asm_genop genop vm_loop vm_predef_atoms vm_bif_tab
#vm_jump_table 

SRCDIR="../emulator/src"
INCDIR="../emulator/include"

.PHONY: asm_genop
asm_genop:
	./genop.py > ../apps/asm/src/asm_genop.erl

.PHONY: genop
genop:
	./vm_genop_cpp.py > ${SRCDIR}/g_genop.cpp && \
	./vm_genop_h.py > ${INCDIR}/g_genop.h

.PHONY: vm_loop
vm_loop:
	./vm_loop.py > ${SRCDIR}/g_vm_loop.cpp

.PHONY: vm_copypaste_impl
vm_copypaste_impl:
	./vm_copypaste_impl.py > vm_copypaste_impl.txt

.PHONY: vm_predef_atoms
vm_predef_atoms:
	./vm_predef_atoms_h.py > ${INCDIR}/g_predef_atoms.h && \
	./vm_predef_atoms_cpp.py > ${SRCDIR}/g_predef_atoms.cpp

.PHONY: vm_bif_tab
vm_bif_tab:
	./vm_bif_tab_h.py > ${SRCDIR}/g_vm_bif_tab.h && \
	./vm_bif_tab_cpp.py > ${SRCDIR}/g_vm_bif_tab.cpp
