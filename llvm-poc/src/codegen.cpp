#include "jit.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/Passes.h"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IRPrintingPasses.h"
//#include "llvm/IR/PassManager.h"

#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"

#include <cctype>
#include <cstdio>
#include <map>
#include <string>
#include <vector>

//using namespace llvm;
using namespace llvm::orc;

class Erllvm {
public:
  llvm::IRBuilder<> builder_;
  //std::unique_ptr<llvm::legacy::FunctionPassManager> fpm_;
  std::unique_ptr<llvm::legacy::PassManager> pm_;
  //std::unique_>ptr<KaleidoscopeJIT> jit_;
  std::unique_ptr<llvm::Module> module_;

public:
  Erllvm(): builder_(llvm::getGlobalContext()) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    //jit_ = llvm::make_unique<KaleidoscopeJIT>();
  }

  void init_module() {
    // Open a new module.
    module_ = llvm::make_unique<llvm::Module>("erllvm_test",
                                              llvm::getGlobalContext());
    //module_->setDataLayout(jit_->getTargetMachine().createDataLayout());
  }

  void init_pass_mgr() {
    // Create a new pass manager attached to it.
    //fpm_ = llvm::make_unique<llvm::legacy::FunctionPassManager>(module_.get());
    pm_ = llvm::make_unique<llvm::legacy::PassManager>();

    // Do simple "peephole" optimizations and bit-twiddling optzns.
//    fpm_->add(llvm::createInstructionCombiningPass());
//    // Reassociate expressions.
//    fpm_->add(llvm::createReassociatePass());
//    // Eliminate Common SubExpressions.
//    fpm_->add(llvm::createGVNPass());
//    // Simplify the control flow graph (deleting unreachable blocks, etc).
//    fpm_->add(llvm::createCFGSimplificationPass());
    pm_->add(llvm::createPrintModulePass(llvm::outs()));

    //fpm_->doInitialization();
  }

  void try_generate() {
    init_module();
    init_pass_mgr();

    auto fun_ = make_fun();

    llvm::verifyFunction(*fun_);
    llvm::verifyModule(*module_);

    pm_->run(*module_);
  }

  llvm::Function *make_fun() {
    auto& c = llvm::getGlobalContext();
//    auto ft = llvm::FunctionType::get(
//          llvm::Type::getInt64Ty(c), false
//          );
    llvm::Constant* co = module_->getOrInsertFunction(
          "main",
          llvm::IntegerType::get(c, 64),
          llvm::IntegerType::get(c, 64),
          llvm::IntegerType::get(c, 64),
          llvm::IntegerType::get(c, 64),
          nullptr);
    llvm::Function* f = llvm::cast<llvm::Function>(co);
    f->setCallingConv(llvm::CallingConv::C);

    // Give names to arguments and save pointers to them in variables
    llvm::Function::arg_iterator args = f->arg_begin();
    llvm::Argument& x = *(args++);
    x.setName("x");
    llvm::Argument& y = *(args++);
    y.setName("y");
    llvm::Argument& z = *(args++);
    z.setName("z");

    llvm::BasicBlock* block = llvm::BasicBlock::Create(c, "entry", f);
    llvm::IRBuilder<> builder(block);

    llvm::Value* tmp = builder.CreateBinOp(llvm::Instruction::Mul,
                                           &x, &y, "tmp");
    llvm::Value* tmp2 = builder.CreateBinOp(llvm::Instruction::Add,
                                            tmp, &z, "tmp2");

    builder.CreateRet(tmp2);

    return f;
  }
};

int main() {
  Erllvm e;

  e.try_generate();

  return 0;
}
