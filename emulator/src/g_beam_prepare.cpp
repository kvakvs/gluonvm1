#include "g_beam.h"
#include "g_functional.h"

// Generated opcode arity table
#include "g_genop.h"

namespace gluon {

// Scans raw code in bytes:sz, and builds jump table with processed args
void BeamLoader::beam_prepare_code(Module *m, ArrayView<const Uint8> data)
{

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
    beam_op_func_info(output, the_non_value, the_non_value, the_non_value);
    m->set_fun_ranges(coderanges_.fun_map);
  }

  // TODO: just scan code and resolve in place maybe? don't have to accum labels
  resolve_labels(postponed_labels, output);

  m->set_code(output); // give ownership

  output_exports(m);
  output_lambdas(m);
  output_selectlists(m);

  m->set_labels(labels_); // transfer data

  // TODO: merge code and literal heap together maybe?

  if (feature_line_numbers) {
    m->set_line_numbers(linenums_.line_refs, linenums_.filenames);
  }
}

Vector<Word> BeamLoader::read_code(Module *m,
                            ArrayView<const Uint8> data,
                            Vector<Word> &output)
{
  tool::Reader r(data);
  static_assert(sizeof(void*) == sizeof(Word),
                "oops word size must be same as void*");

  Word arity;
  Word op_ptr;
  Vector<Word> postponed_labels;

  using genop::Opcode;

  while (!r.is_end()) {
    // Get opcode info
    Opcode opcode = (Opcode)r.read_byte();

    if (opcode < (Opcode)1 || opcode > (Opcode)genop::max_opcode) {
      throw err::BeamLoad("bad opcode");
    }
    if (G_UNLIKELY(opcode == Opcode::Int_code_end)) {
      break;
    }

    // TODO: can optimize code here if we generalize args reading
    // and then handle different special opcode cases
    if (rewrite_opcode(opcode, output, r)) {
      continue;
    }

    // Convert opcode into jump address
    op_ptr = reinterpret_cast<Word>(vm_.g_opcode_labels[(Word)opcode]);
    output.push_back(op_ptr);

    arity = genop::arity_map[(Word)opcode];

    for (Word a = 0; a < arity; ++a) {
      Term arg = parse_term(r);

      // Use runtime value 'Catch' to mark label references
      if (term_tag::Catch::check(arg.as_word())) {
        postponed_labels.push_back(output.size());
      }

      output.push_back(arg.as_word());
    }

    post_rewrite_opcode(opcode, m, output);
  } // end for all code

  return postponed_labels;
}

bool BeamLoader::rewrite_opcode(genop::Opcode opcode,
                                 Vector<Word> &output,
                                 tool::Reader &r) {
  using genop::Opcode;

  // line/1 opcode
  if (opcode == Opcode::Line) {
    if (feature_line_numbers) {
      beam_op_line(output, parse_term(r));
    }
    return true; // rewritten
  }

  // label/1 opcode - save offset to labels table
  if (opcode == Opcode::Label) {
    Term label = parse_term(r);
    G_ASSERT(label.is_small());

    Word l_id = label.small_get_unsigned();
    G_ASSERT(l_id < code_label_count_);
    labels_[l_id] = (&output.back())+1;

    return true; // rewritten
  }

  if (opcode == Opcode::Func_info) {
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

void BeamLoader::post_rewrite_opcode(genop::Opcode opcode,
                                      Module *m,
                                      Vector<Word> &output)
{
  using genop::Opcode;
  Word *first_arg = &output.back() + 1;

  // If the command was call_ext to a bif - rewrite with bif call
//  if (opcode == genop::Opcode::Call_ext) {
//    Term a(first_arg[0]);
//    // So, an ext call of arity 1, to something that we have bif for
//    if (a.small_get_unsigned() == 1 && vm_.resolve_bif1(mfa) != nullptr) {
//      // replace with bif1 op and rewrite args
//    }
//  }

  // Things to resolve from imports:
  if (opcode == Opcode::Bif0)
  {
    // bif0 import_index Dst - cannot fail, no fail label
    replace_imp_index_with_ptr(first_arg, m);
  } else if (   opcode == Opcode::Bif1
             || opcode == Opcode::Bif2
             || opcode == Opcode::Call_ext
             || opcode == Opcode::Call_ext_last
             || opcode == Opcode::Call_ext_only)
  {
    // bif1|2 Fail import_index ...Args Dst
    replace_imp_index_with_ptr(first_arg+1, m);
  } else if (   opcode == Opcode::Gc_bif1
             || opcode == Opcode::Gc_bif2
             || opcode == Opcode::Gc_bif3)
  {
    // gc_bif1|2|3 Fail Live import_index ...Args Dst
    replace_imp_index_with_ptr(first_arg+2, m);
  } else if (opcode == Opcode::Make_fun2) {
    // make_fun2 LambdaTableIndex
    replace_lambda_index_with_ptr(first_arg, m);
  }
}

void BeamLoader::beam_op_line(Vector<Word> &code, Term arg)
{
  if (feature_line_numbers) {
    Word index = arg.small_get_unsigned();
    G_ASSERT(index < linenums_.line_refs.size());

    line_instr_t li;
    li.code_pos = code.data();
    li.pos = linenums_.line_refs[index];
    linenums_.line_instr.push_back(li);
  }
}

void BeamLoader::output_exports(Module *m)
{
  // Move exports from m_exports to this table, resolving labels to code offsets
  Module::Exports exports;
  auto exps = exp_indexes_.all();
  // we iterate over map_view which is pairs of <fun_arity_t key, Word value>
  for_each(exps, [&](fa_lindex_map_t::Iterator fa_lindex) {
                    // find out what is label value
                    auto lresult = labels_.find_ptr(fa_lindex->second.value());
                    G_ASSERT(lresult); // assume it must exist
                    exports[fa_lindex->first] = Export(
                          *lresult, MFArity(mod_name_, fa_lindex->first)
                          );
                  });
  m->set_exports(exports);
}

void BeamLoader::output_lambdas(Module *m)
{
  for (auto &la: lambdas_) {
    auto ptr = labels_[la.uniq[0]];
    la.code = ptr;
  }
  m->set_lambdas(lambdas_);
}

void BeamLoader::output_selectlists(Module *m)
{
  // Fix select lists (replace label numbers with code pointers)
  for (Term t: resolve_selectlists_) {
    Word t_arity = t.tuple_get_arity()/2;
    for (Word i = 0; i < t_arity; ++i) {
      Word l_index = t.tuple_get_element(i*2+1).small_get_unsigned();
      G_ASSERT(labels_.contains(l_index));
      Word *ptr = labels_[l_index];
      t.tuple_set_element(i*2+1, Term::make_boxed_cp(ptr));
    }
  }
}

void BeamLoader::beam_op_func_info(Vector<Word> &code, Term, Term f, Term a)
{
  Word *last_ptr = nullptr;

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

void BeamLoader::replace_imp_index_with_ptr(Word *p, Module *m) {
  Term i(*p);
  Term j = Term::make_boxed(m->get_import_entry(i.small_get_unsigned()));
  *p = j.as_word();
}

void BeamLoader::replace_lambda_index_with_ptr(Word *p, Module *m) {
  Term i(*p);
  Term j = Term::make_boxed(&lambdas_[i.small_get_unsigned()]);
  *p = j.as_word();
}


} // ns gluon
