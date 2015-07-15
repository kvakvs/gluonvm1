#!/usr/bin/python
# takes: genop.tab from erlang/otp
# returns list of dicts{name:str(), arity:int(), opcode:int()}

MIN_OPCODE = 1
MAX_OPCODE = 158

def load():
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

    return ops
