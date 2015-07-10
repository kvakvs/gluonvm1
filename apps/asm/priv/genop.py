#!/usr/bin/python
# takes: genop.tab from erlang/otp
# produces: genop.erl module which maps erlang asm opcodes to numbers

ops = []
for ln in file("genop.tab"):
    ln = ln.strip()
    if not ln: continue
    if ln.startswith("#"): continue

    p1 = ln.split(" ")
    if len(p1) != 2: continue

    opcode = p1[0].strip(":")
    (opname, oparity) = p1[1].split("/")
    opname = opname.strip("-")
    ops.append({'name': opname, 'arity': int(oparity), 'opcode': int(opcode)})

print("-module(asm_genop).")
print("-export([arity/1, name/1, opcode/1]).")
print

for op in ops:
    print "arity('%s') -> %d;" % (op['name'], op['arity'])
print("arity(X) -> erlang:error({bad_arity, X}).")
print

for op in ops:
    print "name(%d) -> '%s';" % (op['opcode'], op['name'])
print("name(X) -> erlang:error({bad_name, X}).")
print

for op in ops:
    print "opcode('%s') -> %d;" % (op['name'], op['opcode'])
print("opcode(X) -> erlang:error({bad_opcode, X}).")
print
