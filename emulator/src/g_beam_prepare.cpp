#include "g_beam.h"
#include "g_functional.h"

// Generated opcode arity table
#include "g_genop.h"

namespace gluon {

// Scans raw code in bytes:sz, and builds jump table with processed args
void LoaderState::beam_prepare_code(Module *m, array_view<const u8_t> data)
{
  // TODO: use some other heap?
  //Heap *heap = VM::get_heap(VM::HEAP_LOADER_TMP);

  tool::Reader r(data);

  Vector<word_t> output;
  // rough estimate of what code size would be, vector will grow if needed
  output.reserve(data.size() * 2);

  G_ASSERT(sizeof(void*) == sizeof(word_t));

  // save references to labels in code and resolve them in second pass
  Vector<word_t> postponed_labels;

  word_t arity;
  word_t op_ptr;
  while (!r.is_end()) {
    // Get opcode info
    word_t opcode = (word_t)r.read_byte();
//    G_LOG("[" FMT_0xHEX "]: opcode=" FMT_0xHEX " %s; ", code.size(), opcode,
//          genop::opcode_name_map[opcode]);

    if (opcode < 1 || opcode > genop::MAX_OPCODE) {
      throw err::beam_load_error("bad opcode");
    }


    if (G_UNLIKELY(opcode == genop::OPCODE_INT_CODE_END)) {
      break;
    }

    // TODO: can optimize code here if we generalize args reading
    // and then handle different special opcode cases
    if (rewrite_opcode(opcode, output, r)) {
      continue;
    }

    // Convert opcode into jump address
    op_ptr = reinterpret_cast<word_t>(vm_.g_opcode_labels[opcode]);
    output.push_back(op_ptr);

    arity = genop::arity_map[opcode];

    word_t *first_arg = &output.back() + 1;

    for (word_t a = 0; a < arity; ++a) {
      Term arg = parse_term(r);

      // Use runtime value 'Catch' to mark label references
      if (term_tag::Catch::check(arg.as_word())) {
        postponed_labels.push_back(output.size());
      }

      output.push_back(arg.as_word());
    }

//    // If the command was call_ext to a bif - rewrite with bif call
//    if (opcode == genop::OPCODE_CALL_EXT) {
//      Term a(first_arg[0]);
//      // So, an ext call of arity 1, to something that we have bif for
//      if (a.small_get_unsigned() == 1 && VM::resolve_bif1(mfa) != nullptr) {
//        // replace with bif1 op and rewrite args
//      }
//    }

    // Things to resolve from imports:
    if (opcode == genop::OPCODE_BIF0)
    {
      // bif0 import_index Dst - cannot fail, no fail label
      replace_imp_index_with_ptr(first_arg, m);
    } else if (opcode == genop::OPCODE_BIF1
               || opcode == genop::OPCODE_BIF2
               || opcode == genop::OPCODE_CALL_EXT
               || opcode == genop::OPCODE_CALL_EXT_LAST
               || opcode == genop::OPCODE_CALL_EXT_ONLY)
    {
      // bif1|2 Fail import_index ...Args Dst
      replace_imp_index_with_ptr(first_arg+1, m);
    } else if (opcode == genop::OPCODE_GC_BIF1
               || opcode == genop::OPCODE_GC_BIF2
               || opcode == genop::OPCODE_GC_BIF3)
    {
      // gc_bif1|2|3 Fail Live import_index ...Args Dst
      replace_imp_index_with_ptr(first_arg+2, m);
    } else if (opcode == genop::OPCODE_MAKE_FUN2) {
      // make_fun2 LambdaTableIndex
      replace_lambda_index_with_ptr(first_arg, m);
    }
  } // end for all code

#if FEATURE_CODE_RANGES
  // mark end of code by adding last open fun to code index
  beam_op_func_info(output, the_non_value, the_non_value, the_non_value);
  m->set_fun_ranges(coderanges_.fun_map);
#endif

  // TODO: just scan code and resolve in place maybe? don't have to accum labels
  resolve_labels(postponed_labels, output);

  m->set_code(output); // give ownership

  // Move exports from m_exports to this table, resolving labels to code offsets
  Module::exports_t exports;
  auto exps = exp_indexes_.all();
  // we iterate over map_view which is pairs of <fun_arity_t key, word_t value>
  for_each(exps, [&](fa_lindex_map_t::iterator fa_lindex) {
                    // find out what is label value
                    auto lresult = labels_.find_ptr(fa_lindex->second.value);
                    G_ASSERT(lresult); // assume it must exist
                    exports[fa_lindex->first] = export_t(
                          *lresult, mfarity_t(mod_name_, fa_lindex->first)
                          );
                  });
  m->set_exports(exports);

  for (auto &la: lambdas_) {
    auto ptr = labels_[la.uniq[0]];
    la.code = ptr;
  }
  m->set_lambdas(lambdas_);

  // Fix select lists (replace label numbers with code pointers)
  for (Term t: resolve_selectlists_) {
    word_t t_arity = t.tuple_get_arity()/2;
    for (word_t i = 0; i < t_arity; ++i) {
      word_t l_index = t.tuple_get_element(i*2+1).small_get_unsigned();
      G_ASSERT(labels_.contains(l_index));
      word_t *ptr = labels_[l_index];
      t.tuple_set_element(i*2+1, Term::make_boxed_cp(ptr));
    }
  }
  m->set_labels(labels_); // transfer data

  // TODO: merge code and literal heap together maybe?

  if (feature_line_numbers) {
    m->set_line_numbers(linenums_.line_refs, linenums_.filenames);
  }
}

bool LoaderState::rewrite_opcode(word_t opcode,
                                 Vector<word_t> &output,
                                 tool::Reader &r) {
  // line/1 opcode
  if (opcode == genop::OPCODE_LINE) {
    if (feature_line_numbers) {
      beam_op_line(output, parse_term(r));
    }
    return true; // rewritten
  }

  // label/1 opcode - save offset to labels table
  if (opcode == genop::OPCODE_LABEL) {
    Term label = parse_term(r);
    G_ASSERT(label.is_small());

    word_t l_id = label.small_get_unsigned();
    G_ASSERT(l_id < code_label_count_);
    labels_[l_id] = (&output.back())+1;

    return true; // rewritten
  }

  if (opcode == genop::OPCODE_FUNC_INFO) {
    tool::Reader r1 = r.clone();
    auto a = parse_term(r1);
    auto b = parse_term(r1);
    auto c = parse_term(r1);
    if (feature_line_numbers || feature_code_ranges) {
      beam_op_func_info(output, a, b, c);
    }
    // FALL THROUGH AND EMIT THE OPCODE
  }

  return false; // did not rewrite
}

void LoaderState::beam_op_line(Vector<word_t> &code, Term arg)
{
  if (feature_line_numbers) {
    word_t index = arg.small_get_unsigned();
    G_ASSERT(index < linenums_.line_refs.size());

    line_instr_t li;
    li.code_pos = code.data();
    li.pos = linenums_.line_refs[index];
    linenums_.line_instr.push_back(li);
  }
}

void LoaderState::beam_op_func_info(Vector<word_t> &code, Term, Term f, Term a)
{
  word_t *last_ptr = nullptr;

  if (feature_code_ranges) {
  last_ptr = (&code.back()) + 1;

    // Finish previous function if it already started
    if (current_fun_.fun.is_value()) {
      code::Range range(coderanges_.fun_begin, last_ptr);
      coderanges_.fun_map.add(range, current_fun_);
    }
    if (f.is_non_value()) {
      // we mark end of code by calling this with a non-value
      return;
    }
  }

  current_fun_.fun = f;
  current_fun_.arity = a.small_get_unsigned();

  if (feature_code_ranges) {
    coderanges_.fun_begin = last_ptr;
  }

  if (feature_line_numbers) {
    // save fun start address
    linenums_.fun_code_map.push_back(code.data() + code.size());
  }
}

void LoaderState::replace_imp_index_with_ptr(word_t *p, Module *m) {
  Term i(*p);
  Term j = Term::make_boxed(m->get_import_entry(i.small_get_unsigned()));
  *p = j.as_word();
}

void LoaderState::replace_lambda_index_with_ptr(word_t *p, Module *m) {
  Term i(*p);
  Term j = Term::make_boxed(&lambdas_[i.small_get_unsigned()]);
  *p = j.as_word();
}

} // ns gluon
