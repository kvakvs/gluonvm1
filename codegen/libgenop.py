#!/usr/bin/python
# takes: genop.tab from erlang/otp
# returns list of dicts{name:str(), arity:int(), opcode:int()}

import string

MIN_OPCODE = 1
MAX_OPCODE = 158

def load_opcodes():
    global ops
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

    # make op map by opcode
    global ops_by_code
    ops_by_code = {}
    for op in ops:
        ops_by_code[op['opcode']] = op

def filter_comments(lst):
    # skip lines starting with # and empty lines
    return [i for i in lst
            if not i.strip().startswith("#") and len(i.strip()) > 0]

implemented_ops = filter_comments(file("implemented_ops.tab").read().split("\n"))
atom_tab = []
bif_tab = []

def is_printable(s):
    printable = string.ascii_letters + string.digits + "_"
    for c in s:
        if c not in printable: 
            return False
    return true

def load_bifs():
    global bif_tab, atom_tab
    atoms = filter_comments(file("atoms.tab").read().split("\n"))
    for a in atoms:
        atom_tab.append({'atom': a})

    bifs = filter_comments(file("bif.tab").read().split("\n"))
    for b in bifs:
        b = b.split()
        bif_tab.append({'atom': b[0], 'arity': int(b[1]), 'cname': b[2]})
        if is_printable(b[0]): atom_tab.append(b[0])
        else: atom_tab.append({'atom': b[0], 'cname': b[2]})

def load():
    load_opcodes()
    load_bifs()

ops = []
ops_by_code = {}
