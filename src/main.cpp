#include <optional>
#include <memory>
#include <iostream>
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/MemoryBufferRef.h"
#include "llvm/Support/Error.h"
#include "llvm/ADT/StringMap.h"

// #include "Lexer.h"
#include "Token.h"
#include "Parser.h"
// #include "Logger.h"
// #include "Ast.h"

#include "Support.h"
#include "VisualizeAst.h"

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringMap.h"

#include "ErrorMessage.h"

#include "TypeCheck.h"

#include <variant>

#include "llvm/Support/Error.h"

int main(int argc, char **argv) {
  // tig::ArenaScopeGuard ag; 
  // const char *src = R"(
  //   let 
  //   in 
  //     ""
  //   end
  // )";
  // tig::Parser parser(src, ag.getAlloc());
  // llvm::Expected<tig::ast::ExprPtr> ast = parser.parse();
  // if (!ast) {
  //   std::cerr << llvm::toString(ast.takeError()) << "\n";
  //   return 1;
  // }

  // auto tc = tig::TypeChecker::create();
  // llvm::Error err = tc->check(*(ast.get()));
  // std::cout << llvm::toString(std::move(err)) << "\n";


  std::cout << llvm::toString(tig::makeError(tig::ErrC::ParserError, tig::MsgNo::EmptyToken)) << std::endl;

  return EXIT_SUCCESS;
}
