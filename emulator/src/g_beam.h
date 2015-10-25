#pragma once

#include "g_vm.h"
#include "g_module.h"

#include "struct/g_array.h"

namespace gluon {

#if FEATURE_LINE_NUMBERS
typedef struct {
  word_t pos;
  word_t *code_pos;
} line_instr_t;
#endif

namespace genop {
  enum class Opcode;
} // ns genop

class BeamLoader {
private:
  VM        &vm_;

  proc::Heap     *heap_= nullptr;
  Vector<Str>     atoms_;
  array_view<const u8_t> code_;
//  const u8_t     *code_ = nullptr; // not owned data
//  word_t          code_sz;
  Term            mod_name_; // an atom in the -module(X) header

  // Data coming from code chunk
  word_t          code_ver_;
  word_t          code_opcode_max;
  word_t          code_label_count_;
  word_t          code_fun_count_;

  Vector<Term>    literals_;
  Module::labels_t  labels_;
  using fa_lindex_map_t = Dict<fun_arity_t, label_index_t>;
  fa_lindex_map_t exp_indexes_;  // list of {f/arity} sequentially
  Module::imports_t imports_;
  Module::lambdas_t lambdas_;
  // postponed select list with label numbers. Resolve to code pointers after
  // loading finished
  Vector<Term>    resolve_selectlists_;

#if FEATURE_LINE_NUMBERS
  // Grouped in struct by feature
  struct {
    word_t        num_line_refs;
    word_t        num_filenames;
    Module::line_refs_t   line_refs;
    Module::file_names_t  filenames;

    Vector<line_instr_t>  line_instr;
    // Mapping fun# to code start for it
    Vector<word_t *>      fun_code_map;
    //word_t                m_current_li = 0;
    word_t      fun_id = 0;
  } linenums_;
#endif

#if FEATURE_LINE_NUMBERS || FEATURE_CODE_RANGES
  fun_arity_t current_fun_;
#endif

#if FEATURE_CODE_RANGES
  // Grouped in struct by feature
  struct {
    word_t      *fun_begin;
    // Maps f/arity to code ranges
    code::Index<fun_arity_t> fun_map;
  } coderanges_;
#endif

public:
  BeamLoader(VM &vm, proc::Heap *h): vm_(vm), heap_(h) {
  }

  const Str &mod_name() const { return atoms_[0]; }

  void load_atom_table(tool::Reader &r, Term expected_name);
  void load_str_table(tool::Reader &r);
  void load_lambda_table(tool::Reader &r);
  void load_export_table(tool::Reader &r);
  void load_import_table(tool::Reader &r);
  void load_code(tool::Reader &r);
  void load_literal_table(tool::Reader &r);
  void load_labels(tool::Reader &r);
  void load_line_table(tool::Reader &r);

  inline const Str &atom_tab_index_to_str(word_t i) const {
    G_ASSERT(i <= atoms_.size());
    return atoms_[i-1];
  }

  // Load finished, create a Module object and inform code server
  Module *finalize(Term modname);
  // Parse raw code creating jump table with decoded args
  void beam_prepare_code(Module *m, array_view<const u8_t> data);

private:
  enum class Tag {
    // The following operand types for generic instructions
    // occur in beam files.
    Literal       = 0,    // TAG_u
    Integer       = 1,    // integer
    Atom          = 2,    // TAG_a
    XRegister     = 3,
    YRegister     = 4,
    Label         = 5,    // TAG_f
    Character     = 6,    // TAG_h
    Extended      = 7,    // TAG_z extended (list, float etc)
    Extended_Base = 8,
    Extended_Float         = Extended_Base + 0,
    Extended_List          = Extended_Base + 1, // Select list for 'case X of'
    Extended_FloatRegister = Extended_Base + 2,
    Extended_AllocList     = Extended_Base + 3,
    Extended_Literal       = Extended_Base + 4,

//    // The following operand types are only used in the loader.
//    NIL       = 8,
//    NoLabel   = 9,
//    Register  = 10,  // TAG_r
//    V         = 11,
//    L         = 12,
//    LiteralRef  = 13, // TAG_q
//    Overflow  = 14,   // overflow/bigint
  };

  void resolve_labels(const Vector<word_t> &postponed_labels,
                      Vector<word_t> &code);

  void replace_imp_index_with_ptr(word_t *p, Module *m);
  void replace_lambda_index_with_ptr(word_t *p, Module *m);

  // on FEATURE_LINE_NUMBERS
  void beam_op_line(Vector<word_t> &code, Term arg);

  // on FEATURE_LINE_NUMBERS || FEATURE_CODE_RANGES
  void beam_op_func_info(Vector<word_t> &code, Term, Term, Term);

  Term parse_term(tool::Reader &r);

  //
  // Code parsing functions - run a loop for each opcode, and then output
  // results to target module
  //
  Vector<word_t> read_code(Module *m, array_view<const u8_t>,
                           Vector<word_t> &output);
  bool rewrite_opcode(genop::Opcode opcode,
                      Vector<word_t> &output,
                      tool::Reader &r);
  void post_rewrite_opcode(genop::Opcode opcode,
                           Module *m,
                           Vector<word_t> &output);
  void output_exports(Module *m);
  void output_lambdas(Module *m);
  void output_selectlists(Module *m);
  // end code parsing

  static Tag parse_tag(tool::Reader &r, u8_t value, int tag=-1);
  static Term parse_int_term(tool::Reader &r, u8_t first);
  inline static bool is_base_tag(Tag t) { return t < Tag::Extended_Base; }
  static Term create_int_term(tool::Reader &r, u8_t first);
  static Term parse_bigint(tool::Reader & /*r*/, word_t /*byte_count*/);
  static Term parse_float(tool::Reader & /*r*/);
  static Term parse_alloclist(tool::Reader &r);
  static Pair<sword_t, bool> parse_small_int(tool::Reader &r, u8_t first);
  static Pair<sword_t, bool> parse_create_small_int(tool::Reader &r, u8_t tag);

  // Returns result + overflow flag, overflow means we want to read bigint
  static Pair<sword_t, bool> read_signed_word(tool::Reader &r, word_t count);
};

} // ns gluon
