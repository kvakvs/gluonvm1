#pragma once

#include "g_vm.h"
#include "g_module.h"

#include "struct/g_array.h"

namespace gluon {

#if FEATURE_LINE_NUMBERS
typedef struct {
  Word pos;
  Word *code_pos;
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
  ArrayView<const Uint8> code_;
//  const u8_t     *code_ = nullptr; // not owned data
//  word_t          code_sz;
  Term            mod_name_; // an atom in the -module(X) header

  // Data coming from code chunk
  Word  code_ver_;
  Word  code_opcode_max;
  Word  code_label_count_;
  Word  code_fun_count_;

  Vector<Term>    literals_;
  Module::Labels  labels_;
  Term            current_label_ = the_non_value; // last encountered label

  using FaLabelindexMap = Dict<FunArity, LabelIndex>;
  FaLabelindexMap exp_indexes_;  // list of exported {f/arity} sequentially

  using LabelindexFaMap = Dict<Term, FunArity>;
  LabelindexFaMap label_to_fun_;  // maps label(as term) to funarity

  Module::Imports imports_;
  Module::Lambdas lambdas_;
  // postponed select list with label numbers. Resolve to code pointers after
  // loading finished
  Vector<Term>    resolve_selectlists_;

#if FEATURE_LINE_NUMBERS
  // Grouped in struct by feature
  struct {
    Word        num_line_refs;
    Word        num_filenames;
    Module::LineRefs   line_refs;
    Module::FileNames  filenames;

    Vector<line_instr_t>  line_instr;
    // Mapping fun# to code start for it
    Vector<Word *>      fun_code_map;
    //word_t                m_current_li = 0;
    Word      fun_id = 0;
  } linenums_;
#endif

#if FEATURE_LINE_NUMBERS || FEATURE_CODE_RANGES
  FunArity current_fun_;
#endif

#if FEATURE_CODE_RANGES
  // Grouped in struct by feature
  struct {
    Word      *fun_begin;
    // Maps f/arity to code ranges
    code::Index<FunArity> fun_map;
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

  inline const Str &atom_tab_index_to_str(Word i) const {
    G_ASSERT(i <= atoms_.size());
    return atoms_[i-1];
  }

  // Load finished, create a Module object and inform code server
  Module *finalize(Term modname);
  // Parse raw code creating jump table with decoded args
  void beam_prepare_code(Module *m, ArrayView<const Uint8> data);

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

  void resolve_labels(const Vector<Word> &postponed_labels,
                      Vector<Word> &code);

  void replace_imp_index_with_ptr(Word *p, Module *m);
  void replace_lambda_index_with_ptr(Word *p, Module *m);

  // on FEATURE_LINE_NUMBERS
  void beam_op_line(Vector<Word> &code, Term arg);

  // on FEATURE_LINE_NUMBERS || FEATURE_CODE_RANGES
  void beam_op_func_info(Vector<Word> &code, Term, Term, Term);

  Term parse_term(tool::Reader &r);

  //
  // Code parsing functions - run a loop for each opcode, and then output
  // results to target module
  //
  Vector<Word> read_code(Module *m, ArrayView<const Uint8>,
                           Vector<Word> &output);
  bool rewrite_opcode(genop::Opcode opcode,
                      Vector<Word> &output,
                      tool::Reader &r);
  void post_rewrite_opcode(genop::Opcode opcode,
                           Word *first_arg,
                           Module *m,
                           Vector<Word> &output);
  void output_exports(Module *m);
  void output_lambdas(Module *m);
  void output_selectlists(Module *m);

  void emit_opcode(Vector<Word> &output, genop::Opcode op) {
    output.push_back((Word)op);
  }

  void emit_opcode(Vector<Word> &output, genop::Opcode op, Term arg1) {
    output.push_back((Word)op);
    output.push_back(arg1.as_word());
  }

  void emit_opcode(Vector<Word> &output, genop::Opcode op, Term arg1, Term arg2) {
    output.push_back((Word)op);
    output.push_back(arg1.as_word());
    output.push_back(arg2.as_word());
  }
  // end code parsing
  //

  static Tag parse_tag(tool::Reader &r, Uint8 value, int tag /*=-1*/);
  //static Uint8 parse_tag_u8(tool::Reader &r, Uint8 value, int tag);

  static Term parse_int_term(tool::Reader &r, Uint8 first);
  inline static bool is_base_tag(Tag t) { return t < Tag::Extended_Base; }
  static Term create_int_term(tool::Reader &r, Uint8 first);
  static Term parse_bigint(tool::Reader & /*r*/, Word /*byte_count*/);
  static Term parse_float(tool::Reader & /*r*/);
  static Term parse_alloclist(tool::Reader &r);
  static Pair<SWord, bool> parse_small_int(tool::Reader &r, Uint8 first);
  static Pair<SWord, bool> parse_create_small_int(tool::Reader &r, Uint8 tag);

  // Returns result + overflow flag, overflow means we want to read bigint
  static Pair<SWord, bool> read_signed_word(tool::Reader &r, Word count);
  void debug_print_opcode(genop::Opcode opcode, Word arity, tool::Reader &r);
};

} // ns gluon
