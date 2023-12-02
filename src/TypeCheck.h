#ifndef TIGER_LANG_TYPE_CHECK_H
#define TIGER_LANG_TYPE_CHECK_H
#include "Support.h"
#include "Ast.h"
#include "SymbolTable.h"

#include "variant"

namespace tig {

using TypeIndex = ssize_t;

/// For variable and function declarations the type and the 
/// return type may be omitted. In that case they have this 
/// constant value in the type table.
constexpr TypeIndex InfereType = -1;

struct TTPrimitiveType {
  enum class Kind {
    Int,
    String,
    Nil
  } kind;
};

struct TTScalarType {
  TypeIndex originalType;
};

struct TTArray {
  TypeIndex baseType;
};

struct TTRecordField {
  TypeIndex baseType;
};

struct TTRecordFieldsEnd {
};


struct TTFunctionParam {
  TypeIndex baseType;
};

/// doubles as the "guard" for the function params list
struct TTFunctionReturnType {
  TypeIndex baseType;
};

using TTEntry = std::variant< 
  TTPrimitiveType,
  TTScalarType,
  TTArray,
  TTRecordField,
  TTRecordFieldsEnd,
  TTFunctionParam,
  TTFunctionReturnType
>;

/// Keep this in sync with TTEntry.
enum class TTEntryKind {
  PrimitiveType = 0,
  ScalarType = 1,
  Array = 2,
  RecordField = 3,
  RecordFieldsEnd = 4,
  FunctionParam = 5,
  FunctionReturnType = 6
};

using TypeTable = std::vector<TTEntry>;
using TypeTableIter = TypeTable::iterator;
using ConstTypeTableIter = TypeTable::const_iterator;

constexpr bool endOfRecord(TypeTableIter it) {
  return std::holds_alternative<TTRecordFieldsEnd>(*it);
}

constexpr bool endOfRecord(ConstTypeTableIter it) {
  return std::holds_alternative<TTRecordFieldsEnd>(*it);
}

constexpr bool endOfFunc(TypeTableIter it) {
  return std::holds_alternative<TTFunctionReturnType>(*it);
}

constexpr bool endOfFunc(ConstTypeTableIter it) {
  return std::holds_alternative<TTFunctionReturnType>(*it);
}

class TypeChecker : public std::enable_shared_from_this<TypeChecker> {
public:

  /// Visitors hold shared_ptrs on this type checker instance, constructed
  /// with shared_from_this() in the check method. The program will dump with
  /// a bad_weak_ptr exception when a shared_from_this() pointer is used from
  /// a class instance that is not already wrapped in a shared_ptr. So we have to
  /// statically ensure that only heap allocated instances wrapped in shared_ptr 
  /// can be constructed.
  static std::shared_ptr<TypeChecker> create() {
    TypeChecker *ptr = new TypeChecker;
    return std::shared_ptr<TypeChecker>(ptr);
  }

  /// @brief Runs type check on the given expression. 
  /// The caller takes ownership of the error. Ignoring 
  /// it is most likely an error.
  /// If the type check is successful an llvm::Error::success() 
  /// instance is returned.
  /// @param expr expression
  /// @return type check error or llvm::Error::success()
  [[nodiscard]] llvm::Error check(const ast::Expr &expr);

  [[nodiscard]] llvm::Expected<TypeIndex> addRenamedTypeDeclaration(const ast::RenamedTypeDecl &decl);
  [[nodiscard]] llvm::Expected<TypeIndex> addArrayTypeDeclaration(const ast::ArrayTypeDecl &decl);
  [[nodiscard]] llvm::Expected<TypeIndex> addRecordTypeDeclaration(const ast::RecordTypeDecl &decl);
  [[nodiscard]] llvm::Expected<TypeIndex> addRecordFieldTypeDeclaration(const ast::ParamDecl &decl);

  /// Add the given type declaration to the type and symbol tables. 
  [[nodiscard]] llvm::Error addTypeDeclaration(const ast::TypeDecl &decl);

  /// Functions may not be a first class citizen in the type system but for call expressions 
  /// the parameter and return types must be checked nevertheless.
  /// @{

  [[nodiscard]] llvm::Expected<TypeIndex> addFunctionParamDeclaration(const ast::ParamDecl &decl);

  /// Add function declaration to type and symbol table. 
  [[nodiscard]] llvm::Error addFunctionDeclaration(const ast::FunctionDecl &decl);

  /// Variables do not declare types but they are symbols that should be added to 
  /// the symbol table.
  [[nodiscard]] llvm::Error addVariableDeclaration(const ast::VarDecl &decl);

  /// @}

  /// @brief Check if one type resolves linearily through renaming to another type.
  /// @param type renamed type
  /// @param baseType base type
  /// @return true if type resolves to baseType through renaming, false otherwise 
  bool resolvesTo(TypeIndex type, TypeIndex baseType) const;

  bool equivalent(TypeIndex first, TypeIndex second) const {
    return resolvesTo(first, second) | resolvesTo(second, first);
  } 

  /// @brief Can the given type be converted into a truth value.
  /// @param type type
  /// @return true if it can, false otherwise.
  bool isTruthy(TypeIndex type) const;

  void pushScope() {
    ScopedSymbolTablePtr newScope = makeScopedSymbolTable(std::move(symbolTable));
    symbolTable = std::move(newScope);
  }

  void popScope() {
    ScopedSymbolTablePtr oldScope = symbolTable->takeParent();
    symbolTable = std::move(oldScope);
  }

  ScopedSymbolTable& getSymbolTable() {
    return *symbolTable;
  }

  TypeTable &getTypeTable() {
    return typeTable;
  }

  void addError(llvm::Error &&error) {
    this->error = std::move(error);
  }

  bool hasError() {
    return bool(error);
  }

  llvm::Error takeError() {
    llvm::Error err = std::move(error);
    return err;
  }

  TypeIndex getInt() const { 
    return intTypeIndex;
  }

  TypeIndex getString() const {
    return stringTypeIndex;
  }

  TypeIndex getNil() const {
    return nilTypeIndex;
  }

  std::shared_ptr<TypeChecker> getSharedPtr() {
    return shared_from_this();
  }

private:
  explicit TypeChecker();
  
  ScopedSymbolTablePtr symbolTable;
  TypeTable typeTable;
  llvm::Error error;

  /// "shortcuts" to the primitive types @{ 

  TypeIndex intTypeIndex;
  TypeIndex stringTypeIndex;
  TypeIndex nilTypeIndex;

  /// @}
};


} // namespace
#endif
