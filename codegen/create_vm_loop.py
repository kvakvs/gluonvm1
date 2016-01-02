#!/usr/bin/python
# takes: genop.tab from erlang/otp
# produces: g_vm_loop includable file with label copypaste template for vm 
# loop function
import libgenop

libgenop.load()

print("""// Generated by codegen/vm_loop.py
""")

OPS_WHICH_MAY_YIELD = ['return', 'call', 'call_only', 'call_last',
    'call_ext', 'call_fun', 'call_ext_only', 'call_ext_last',
    'bif0', 'bif1', 'bif2', 'gc_bif1', 'gc_bif2', 'send', 
    'apply', 'apply_last',
    'apply_mfargs_', 'normal_exit_', 'error_exit_']
OPS_WHICH_ALWAYS_YIELD = ['wait']

for opcode in range(libgenop.MIN_OPCODE, libgenop.MAX_OPCODE+1):
    op = libgenop.ops_by_code[opcode]
    # unconditional code end
    print("OP_%s: // opcode: %d" % (op['name'], opcode))
    if op['name'] == "int_code_end":
        print("  goto vm_end;\n")
        continue

    # call handler or print TODO error
    if op['name'] in libgenop.implemented_ops:
        print('  if (debug_mode) { libc::fmt(tGreen("%s") "/%d args="); }' % (op['name'], op['arity']))
        print('  ctx.print_args(%d);' % (op['arity']))

        # unconditional scheduling
        if op['name'] in OPS_WHICH_ALWAYS_YIELD:
            print("  impl::opcode_%s(proc, ctx);" % (op['name']))
            print("  goto schedule;")
            continue
        # conditional scheduling - false means we yield
        elif op['name'] in OPS_WHICH_MAY_YIELD:
            # special instruction which can interrupt loop
            print("  opcode_result = impl::opcode_%s(proc, ctx);" % (op['name']))
            print("  if (G_UNLIKELY(opcode_result != impl::WantSchedule::NextProcess)) {")
            print("    goto schedule;")
            print("  }")
        else:
            print("  impl::opcode_%s(proc, ctx);" % (op['name']))
        print("  goto next_instr;")
    else:
        print("  throw err::TODO(\"notimpl %s\");" % (op['name']))
        print("  G_IF_NODEBUG(goto next_instr;)")
    print

#
# Init table with labels and export it to global scope
#
print("""vm_jump_table_init: {
  // Init table with labels and export it to global scope
  static const void *l_opcode_labels[] = { nullptr,""")

for opcode in range(libgenop.MIN_OPCODE, libgenop.MAX_OPCODE+1):
    op = libgenop.ops_by_code[opcode]
    print("      &&OP_%s, // opcode %d" % (op['name'], opcode))

print("""    };
    g_opcode_labels = l_opcode_labels;
  } // end init
""")
