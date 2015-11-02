#include "g_beam.h"
#include "g_functional.h"
#include "g_predef_atoms.h"

// Generated opcode arity table
#include "g_genop.h"

namespace gluon {

using genop::Opcode;

// Scans raw code in bytes:sz, and builds jump table with processed args
void BeamLoader::beam_prepare_code(Module* m, ArrayView<const Uint8> data) {
  Vector<Word> output;
  // rough estimate of what code size would be, vector will grow if needed
  output.reserve(data.size() * 2);

  //
  // First pass here
  //
  // save references to labels in code and resolve them in second pass
  Vector<Word> postponed_labels = read_code(m, data, output);

  //
  // Second pass
  //
  if (feature_code_ranges) {
    // mark end of code by adding last open fun to code index
    beam_op_func_info(output, the_non_value, 0);
    m->set_fun_ranges(coderanges_.fun_map);
  }

  // TODO: just scan code and resolve in place maybe? don't have to accum labels
  resolve_labels(postponed_labels, output);

  m->set_code(output);  // give ownership

  output_exports(m);
  output_lambdas(m);
  output_selectlists(m);

  m->set_labels(labels_);  // transfer data

  // TODO: merge code and literal heap together maybe?

  if (feature_line_numbers) {
    m->set_line_numbers(linenums_.line_refs, linenums_.filenames);
  }
}

Vector<Word> BeamLoader::read_code(Module* m,
                                   ArrayView<const Uint8> data,
                                   Vector<Word>& output) {
  tool::Reader r(data);
  static_assert(sizeof(void*) == sizeof(Word),
                "oops word size must be same as void*");

  Word arity;
  Word op_ptr;
  Vector<Word> postponed_labels;

  while (!r.is_end()) {
    // Get opcode info
    Opcode opcode = (Opcode)r.read_byte();
    arity = genop::arity_map[(Word)opcode];

    if (opcode < (Opcode)1 || opcode > (Opcode)genop::max_opcode) {
      throw err::BeamLoad("bad opcode");
    }
    if (G_UNLIKELY(opcode == Opcode::Int_code_end)) {
      break;
    }

    // Convert opcode into jump address
    op_ptr = reinterpret_cast<Word>(vm_.g_opcode_labels[(Word)opcode]);
    output.push_back(op_ptr);

    // debug_print_opcode(opcode, arity, r);

    Word* args = &output.back() + 1;
    if (rewrite_opcode(opcode, output, r)) {
      output.resize(output.size() - 1);
      continue;
    }

    for (Word a = 0; a < arity; ++a) {
      Term arg = parse_term(r);

      // Use runtime value 'Catch' to mark label references
      if (arg.is_catch()) {
        postponed_labels.push_back(output.size());
      }

      output.push_back(arg.as_word());
    }

    post_rewrite_opcode(opcode, args, m, output);
  }  // end for all code

  return postponed_labels;
}

void BeamLoader::debug_print_opcode(genop::Opcode opcode,
                                    Word arity,
                                    tool::Reader& r) {
  Std::fmt("op %s(", genop::opcode_name_map[(Word)opcode]);
  tool::Reader r2(r);
  for (Word p = 0; p < arity; ++p) {
    Term arg = parse_term(r2);
    arg.print(vm_);
    if (p + 1 < arity) {
      Std::fmt("; ");
    }
  }
  Std::fmt(")\n");
}

// Here we get chance to preview opcode before its been placed to output,
// parse and place it ourself. Returning true means we did it, and outer
// loop will skip own arg parse implementation.
bool BeamLoader::rewrite_opcode(genop::Opcode opcode,
                                Vector<Word>& output,
                                tool::Reader& r) {
  // line/1 opcode
  if (opcode == Opcode::Line) {
    if (feature_line_numbers) {
      beam_op_line(output, parse_term(r));
    }
    return true;  // processed and we want to skip writing it
  }

  // label/1 opcode - save offset to labels table
  if (opcode == Opcode::Label) {
    Term label = current_label_ = parse_term(r);
    G_ASSERT(label.is_small());

    Word l_id = label.small_word();
    G_ASSERT(l_id < code_label_count_);
    labels_[l_id] = &output.back();

    return true;  // processed and we want to skip writing it
  }

  if (opcode == Opcode::Func_info) {
    tool::Reader r1(r);  // clone reader to peek opcode args ahead
    parse_term(r1);      // mod
    auto fun = parse_term(r1);
    auto arity = parse_term(r1);
    auto w_arity = arity.small_word();

    if (feature_line_numbers || feature_code_ranges) {
      beam_op_func_info(output, fun, w_arity);
    }
    // Remember that function begins here
    label_to_fun_[current_label_] = FunArity(fun, w_arity);

    // FALL THROUGH AND EMIT THE OPCODE
  }

  return false;  // did not rewrite
}

// Here we get chance to review what's been output by code loader (note that
// opcode is emitted as label address, and then follow args)
void BeamLoader::post_rewrite_opcode(genop::Opcode opcode,
                                     Word* opcode_args,
                                     Module* m,
                                     Vector<Word>& output) {
  /*if (opcode == genop::Opcode::Call_ext_only) {
    Term arity(opcode_args[0]);
    Term label(opcode_args[1]);

    // If the command was call_ext_only to erlang:apply - overwrite with apply
    FunArity *fa = label_to_fun_.find_ptr(label);
    G_ASSERT(fa);

    if (mod_name_ == atom::ERLANG && fa->fun == atom::APPLY) {
      output.resize(output.size() - 3); // erase 2 args and opcode

      Word w_arity = arity.small_word();
      emit_opcode(output, Opcode::Move, mod_name_, Term::make_regx(w_arity));
      emit_opcode(output, Opcode::Move, fa->fun, Term::make_regx(w_arity+1));
      emit_opcode(output, Opcode::Apply, arity);

      return; // stop rewriting right here
    }
  }*/

  // Things to resolve from imports:
  if (opcode == Opcode::Bif0) {
    // bif0 import_index Dst - cannot fail, no fail label
    replace_imp_index_with_ptr(opcode_args, m);
  } else if (opcode == Opcode::Bif1 || opcode == Opcode::Bif2 ||
             opcode == Opcode::Call_ext || opcode == Opcode::Call_ext_last ||
             opcode == Opcode::Call_ext_only) {
    // bif1|2 Fail import_index ...Args Dst
    replace_imp_index_with_ptr(opcode_args + 1, m);
  } else if (opcode == Opcode::Gc_bif1 || opcode == Opcode::Gc_bif2 ||
             opcode == Opcode::Gc_bif3) {
    // gc_bif1|2|3 Fail Live import_index ...Args Dst
    replace_imp_index_with_ptr(opcode_args + 2, m);
  } else if (opcode == Opcode::Make_fun2) {
    // make_fun2 LambdaTableIndex
    replace_lambda_index_with_ptr(opcode_args, m);
  }
}

void BeamLoader::beam_op_line(Vector<Word>& code, Term arg) {
  if (feature_line_numbers) {
    Word index = arg.small_word();
    G_ASSERT(index < linenums_.line_refs.size());

    line_instr_t li;
    li.code_pos = code.data();
    li.pos = linenums_.line_refs[index];
    linenums_.line_instr.push_back(li);
  }
}

void BeamLoader::output_exports(Module* m) {
  // Move exports from m_exports to this table, resolving labels to code offsets
  Module::Exports exports;
  auto exps = exp_indexes_.all();
  // we iterate over map_view which is pairs of <fun_arity_t key, Word value>
  for_each(exps, [&](FaLabelindexMap::Iterator fa_lindex) {
    // find out what is label value
    auto lresult = labels_.find_ptr(fa_lindex->second.value());
    G_ASSERT(lresult);  // assume it must exist
    exports[fa_lindex->first] =
        Export(*lresult, MFArity(mod_name_, fa_lindex->first));
  });
  m->set_exports(exports);
}

void BeamLoader::output_lambdas(Module* m) {
  for (auto& la : lambdas_) {
    auto ptr = labels_[la.uniq[0]];
    la.code = ptr;
  }
  m->set_lambdas(lambdas_);
}

void BeamLoader::output_selectlists(Module* m) {
  // Fix select lists (replace label numbers with code pointers)
  for (Term t : resolve_selectlists_) {
    Word t_arity = t.tuple_get_arity() / 2;
    for (Word i = 0; i < t_arity; ++i) {
      Word l_index = t.tuple_get_element(i * 2 + 1).small_word();
      G_ASSERT(labels_.contains(l_index));
      Word* ptr = labels_[l_index];
      t.tuple_set_element(i * 2 + 1, Term::make_boxed_cp(ptr));
    }
  }
}

void BeamLoader::beam_op_func_info(Vector<Word>& code, Term f, Word arity) {
  Word* last_ptr = nullptr;

  if (feature_code_ranges) {
    last_ptr = (&code.back()) + 1;

    // Finish previous function if it already started
    if (current_fun_.fun.is_value()) {
      code::Range range(coderanges_.fun_begin, last_ptr);
      G_ASSERT(current_fun_.fun.is_atom());
      coderanges_.fun_map.add(range, current_fun_);
    }
    if (f.is_non_value()) {
      // we mark end of code by calling this with a non-value
      return;
    }
  }

  current_fun_ = FunArity(f, arity);

  if (feature_code_ranges) {
    coderanges_.fun_begin = last_ptr;
  }

  if (feature_line_numbers) {
    // save fun start address
    linenums_.fun_code_map.push_back(code.data() + code.size());
  }
}

void BeamLoader::replace_imp_index_with_ptr(Word* p, Module* m) {
  Term i(*p);
  Term j = Term::make_boxed(m->get_import_entry(i.small_word()));
  *p = j.as_word();
}

void BeamLoader::replace_lambda_index_with_ptr(Word* p, Module* m) {
  Term i(*p);
  Term j = Term::make_boxed(&lambdas_[i.small_word()]);
  *p = j.as_word();
}

}  // ns gluon
