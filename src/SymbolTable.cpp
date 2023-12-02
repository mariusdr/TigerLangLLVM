#include "SymbolTable.h"
#include "ErrorMessage.h"

namespace tig {

SymbolInfo ScopedSymbolTable::lookup(llvm::StringRef key) const {
  auto v = symbolTable.lookup(key);
  if (v.kind == SymbolKind::Unknown && parent) {
    return parent->lookup(key);
  }
  return v;
}

bool ScopedSymbolTable::contains(llvm::StringRef key) const {
  return lookup(key).kind != SymbolKind::Unknown;
}

bool ScopedSymbolTable::containsAs(llvm::StringRef key, SymbolKind kind) const {
  return lookup(key).kind == kind;
}

std::pair<bool, SymbolTableIter> ScopedSymbolTable::findImpl(llvm::StringRef key) {
  auto it = symbolTable.find(key);
  if (it != symbolTable.end()) {
    return std::make_pair(true, it);
  } else if (parent) {
    auto res = parent->findImpl(key);
    if (res.first) {
      return res;
    }
  }
  return std::make_pair(false, it);
}

std::pair<bool, ConstSymbolTableIter> ScopedSymbolTable::findImpl(llvm::StringRef key) const{
  auto it = symbolTable.find(key);
  if (it != symbolTable.end()) {
    return std::make_pair(true, it);
  } else if (parent) {
    auto res = parent->findImpl(key);
    if (res.first) {
      return res;
    }
  }
  return std::make_pair(false, it);
}

SymbolTableIter ScopedSymbolTable::find(llvm::StringRef key) {
  bool found = false;
  SymbolTableIter it;
  std::tie(found, it) = findImpl(key);
  return found ? it : symbolTable.end();
}

ConstSymbolTableIter ScopedSymbolTable::find(llvm::StringRef key) const {
  bool found = false;
  ConstSymbolTableIter it;
  std::tie(found, it) = findImpl(key);
  return found ? it : symbolTable.end();
}

std::pair<SymbolTableIter, bool> ScopedSymbolTable::insert(std::pair<llvm::StringRef, SymbolInfo> kv) {
  return symbolTable.insert(kv);
}

llvm::Expected<SymbolTableIter> ScopedSymbolTable::declareSymbol(llvm::StringRef symbol, SymbolInfo info) {
  SymbolTableIter it;
  bool success = false;
  std::tie(it, success) = symbolTable.insert(std::make_pair(symbol, info));
  if (!success) {
    auto ec = std::make_error_code(std::errc::operation_not_permitted);
    auto fmt = llvm::formatv(message(MsgNo::SymbAlreadyKnown), symbol);
    return llvm::createStringError(ec, fmt.str());
  }
  return it;
}

llvm::Expected<SymbolTableIter> ScopedSymbolTable::declareSymbol(const ast::VarDecl &varDecl) {
  return declareSymbol(varDecl.varName(), SymbolKind::Variable);
}

llvm::Expected<SymbolTableIter> ScopedSymbolTable::declareSymbol(const ast::TypeDecl &typeDecl) {
  return declareSymbol(typeDecl.typeName(), SymbolKind::Type);
}

llvm::Expected<SymbolTableIter> ScopedSymbolTable::declareSymbol(const ast::FunctionDecl &funcDecl) {
  return declareSymbol(funcDecl.functionName(), SymbolKind::Function);
}

llvm::Expected<SymbolTableIter> ScopedSymbolTable::declareSymbol(const ast::ParamDecl &paramDecl) {
  return declareSymbol(paramDecl.paramName(), SymbolKind::Function);
}

} // namespace
