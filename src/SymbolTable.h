#ifndef TIGER_LANG_SYMBOL_TABLE_H
#define TIGER_LANG_SYMBOL_TABLE_H

#include <memory>
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/Error.h"

#include "Ast.h"

namespace tig {

enum class SymbolKind: uint8_t {
  Unknown = 0,
  Type = 1,
  Variable = 2,
  Function = 3
};

using TypeIndex = ssize_t;

struct SymbolInfo {
  SymbolInfo():
    kind(SymbolKind::Unknown)
  {}
  SymbolInfo(SymbolKind kind):
    kind(kind)
  {}
  SymbolInfo(SymbolKind kind, TypeIndex typeIdx):
    kind(kind), typeIdx(typeIdx)
  {}

  SymbolKind kind;
  TypeIndex typeIdx;
};

using SymbolTable = llvm::StringMap<SymbolInfo>;
using SymbolTableIter = llvm::StringMap<SymbolInfo>::iterator;
using ConstSymbolTableIter = llvm::StringMap<SymbolInfo>::const_iterator;

class ScopedSymbolTable;
using ScopedSymbolTablePtr = std::unique_ptr<ScopedSymbolTable>;

class ScopedSymbolTable {
public:
  explicit ScopedSymbolTable(ScopedSymbolTablePtr &&parent):
    parent(std::move(parent))
  {}

  /// @brief Look for a symbol in the highest scope first, going down from there.
  /// Note that llvm::StringMap::lookup returns the default
  /// constructed value of the value type when the key is not 
  /// found. These semantics are repeated here. An info with kind 
  /// SymbolKind::Unknown is the default constructed value.
  /// @param key symbol
  /// @return symbol info structure
  [[nodiscard]] SymbolInfo lookup(llvm::StringRef key) const;

  SymbolTable &getCurrentScope() { 
    return symbolTable; 
  }

  [[nodiscard]] bool contains(llvm::StringRef key) const;
  [[nodiscard]] bool containsAs(llvm::StringRef key, SymbolKind kind) const;

  /// @brief Check if key exists in some scope. If so, return an iterator. 
  ///        Otherwise return an iterator to the end of the top level scope 
  ///        symbol table.
  /// @param key symbol
  /// @return iterator to symbol or end of symbol table
  [[nodiscard]] SymbolTableIter find(llvm::StringRef key);
  
  /// @brief Check if key exists in some scope. If so, return an iterator. 
  ///        Otherwise return an iterator to the end of the top level scope 
  ///        symbol table.
  /// @param key symbol
  /// @return iterator to symbol or end of symbol table
  [[nodiscard]] ConstSymbolTableIter find(llvm::StringRef key) const;

  [[nodiscard]] SymbolTableIter begin() { return symbolTable.begin(); }
  [[nodiscard]] ConstSymbolTableIter begin() const { return symbolTable.begin(); }
  [[nodiscard]] SymbolTableIter end() { return symbolTable.end(); }
  [[nodiscard]] ConstSymbolTableIter end() const { return symbolTable.end(); }

  ScopedSymbolTablePtr takeParent() {
    symbolTable.clear();
    ScopedSymbolTablePtr p = std::move(parent);
    return p;
  }

  /// @brief Insert entry into top-level symbol table.
  /// @param kv entry as std::pair
  /// @return the returned bool is true iff the insertion took place, the iterator 
  ///         points to the inserted entry.
  std::pair<SymbolTableIter, bool> insert(std::pair<llvm::StringRef, SymbolInfo> kv);

  /// @brief Declare a symbol. If the symbol was already declared in this scope 
  ///        raise an error.
  /// @param symbol identifier 
  /// @param info metadata 
  /// @return iterator to symbol on success, error if symbol was already declared in scope
  llvm::Expected<SymbolTableIter> declareSymbol(llvm::StringRef symbol, SymbolInfo info);

  llvm::Expected<SymbolTableIter> declareSymbol(const ast::VarDecl &varDecl);
  llvm::Expected<SymbolTableIter> declareSymbol(const ast::TypeDecl &typeDecl);
  llvm::Expected<SymbolTableIter> declareSymbol(const ast::FunctionDecl &funcDecl);
  llvm::Expected<SymbolTableIter> declareSymbol(const ast::ParamDecl &paramDecl);

private:
  ScopedSymbolTablePtr parent;
  SymbolTable symbolTable;

  std::pair<bool, SymbolTableIter> findImpl(llvm::StringRef key);
  std::pair<bool, ConstSymbolTableIter> findImpl(llvm::StringRef key) const;
};

inline ScopedSymbolTablePtr makeScopedSymbolTable(ScopedSymbolTablePtr parent) {
  return std::make_unique<ScopedSymbolTable>(std::move(parent));
}

/// @brief Create root symbol table and populate it with built-in types.
inline ScopedSymbolTablePtr makeTopLevelScope() {
  ScopedSymbolTablePtr st = makeScopedSymbolTable(nullptr);
  st->getCurrentScope()["int"] = SymbolKind::Type;
  st->getCurrentScope()["string"] = SymbolKind::Type;
  st->getCurrentScope()["nil"] = SymbolKind::Type;
  return st;
}

} // namespace
#endif
