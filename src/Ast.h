#ifndef TIGER_LANG_AST_H
#define TIGER_LANG_AST_H

#include <utility>
#include <memory>
#include <variant>
#include <vector>
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Error.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Allocator.h"

#include "Token.h"
#include "Support.h"

namespace tig {
namespace ast {

enum class ExprKind : uint8_t {
  Nil = 0,
  Variable = 1,
  IntLit = 2,
  StrLit = 3,
  Call = 4, 
  Operator = 5, 
  Record = 6,
  Sequence = 7,
  Assign = 8,
  If = 9,
  While = 10, 
  For = 11, 
  Break = 12,
  Let = 13,
  Array = 14
};

class Expr;
using ExprPtr = AllocPtr<Expr, llvm::BumpPtrAllocator>;

using String = AllocString<llvm::BumpPtrAllocator>;

class NilExpr {
public:
};

/// Integer constant.
class IntLitExpr {
public:
  explicit IntLitExpr(uint64_t value) noexcept;
  IntLitExpr(IntLitExpr &&rhs) noexcept;
  IntLitExpr &operator=(IntLitExpr &&rhs) noexcept;
  IntLitExpr(const IntLitExpr &rhs) = delete;
  IntLitExpr &operator=(const IntLitExpr &rhs) = delete;

  uint64_t getValue() const;

  static llvm::Expected<IntLitExpr> fromStr(llvm::StringRef sref);
private:
  uint64_t value;
};

class StrLitExpr {
public:
  explicit StrLitExpr(llvm::StringRef sref, llvm::BumpPtrAllocator &arena);
  StrLitExpr(StrLitExpr&& rhs);
  StrLitExpr &operator=(StrLitExpr &&rhs);
  StrLitExpr(const StrLitExpr&) = delete;
  StrLitExpr &operator=(const StrLitExpr &) = delete;

  llvm::StringRef getValue() const;
private:
  String value;
};

enum class OperatorKind : uint8_t {
  Plus,
  Minus,
  Times, 
  Divide,
  Or,
  And,
  Equals,
  Unequal,
  LessThan, 
  GreaterThan,
  LessEqual,
  GreaterEqual
};

class OperatorExpr {
public:
  OperatorExpr(ExprPtr &&lhs, ExprPtr &&rhs, OperatorKind kind) noexcept;
  OperatorExpr(OperatorExpr &&other) noexcept;
  OperatorExpr &operator=(OperatorExpr &&other);
  OperatorExpr(const OperatorExpr &) = delete;
  OperatorExpr &operator=(const OperatorExpr &) = delete;

  OperatorKind opKind() const;
  const Expr &leftOperand() const;
  const Expr &rightOperand() const;
  Expr &leftOperand();
  Expr &rightOperand();
private:
  ExprPtr lhs;
  ExprPtr rhs;
  OperatorKind kind;
};

struct Field {
  String symbol;
  ExprPtr expr;
  Field(String &&symbol, ExprPtr &&expr);
};

using FieldList = AllocVector<Field, llvm::BumpPtrAllocator>;
using FieldIter = AllocVector<Field, llvm::BumpPtrAllocator>::iterator;
using ConstFieldIter = AllocVector<Field, llvm::BumpPtrAllocator>::const_iterator;

class RecordExpr {
public:
  RecordExpr(FieldList &&fields);
  RecordExpr(RecordExpr &&other);
  RecordExpr &operator=(RecordExpr &&other);
  RecordExpr(const RecordExpr &) = delete;
  RecordExpr &operator=(const RecordExpr &) = delete;

  FieldIter begin();
  FieldIter end();
  ConstFieldIter cbegin() const;
  ConstFieldIter cend() const;
  
  size_t size() const {
    return fields.size();
  }
private:
  FieldList fields;
};

using ExprList = AllocVector<ExprPtr, llvm::BumpPtrAllocator>;
using ExprListIter = ExprList::iterator;
using ConstExprListIter = ExprList::const_iterator;

class CallExpr {
public:
  CallExpr(String &&symbol, ExprList &&exprs);
  CallExpr(CallExpr &&other);
  CallExpr &operator=(CallExpr &&other);
  CallExpr(const CallExpr &) = delete;
  CallExpr &operator=(const CallExpr &) = delete;

  llvm::StringRef functionName() const;
  ExprListIter begin();
  ExprListIter end();
  ConstExprListIter cbegin() const;
  ConstExprListIter cend() const;

  const Expr &at(size_t i) const {
    return *exprs.at(i);
  }
  
  Expr &at(size_t i) {
    return *exprs.at(i);
  }

  size_t size() const {
    return exprs.size();
  }
private:
  String symbol;
  ExprList exprs;
};

class SequenceExpr {
public:
  SequenceExpr(ExprList &&exprs);
  SequenceExpr(SequenceExpr &&other);
  SequenceExpr &operator=(SequenceExpr &&other);
  SequenceExpr(const SequenceExpr &) = delete; 
  SequenceExpr &operator=(const SequenceExpr &other) = delete;
  
  ExprListIter begin();
  ExprListIter end();
  ConstExprListIter cbegin() const;
  ConstExprListIter cend() const;
  
  const Expr &at(size_t i) const {
    return *exprs.at(i);
  }
  
  Expr &at(size_t i) {
    return *exprs.at(i);
  }

  size_t size() const {
    return exprs.size();
  }
private:
  ExprList exprs;
};


enum class VariableKind : uint8_t {
  Simple = 0, 
  Field = 1,
  Subscript = 2
};

class VariableExpr;
using VariableExprPtr = AllocPtr<VariableExpr, llvm::BumpPtrAllocator>;

struct SimpleVariable {
  String id;
  SimpleVariable(String &&id);
  llvm::StringRef getId() const { return id.data(); }
};

/// Expression of the form 'LVALUE.ID' 
struct FieldVariable {
  VariableExprPtr lvalue;
  String id;
  FieldVariable(VariableExprPtr &&var, String &&id);
  const VariableExpr &getVar() const { return *lvalue; }
  llvm::StringRef getId() const { return id.data(); }
};

/// Expression of the form 'LVALUE[EXPR]'
struct SubscriptVariable {
  VariableExprPtr lvalue; 
  ExprPtr expr;
  SubscriptVariable(VariableExprPtr &&var, ExprPtr &&expr);
  const VariableExpr &getVar() const { return *lvalue; }
  const Expr &getExpr() const { return *expr; }
};

class VariableExprVisitor {
public:
  virtual void operator()(SimpleVariable &) {}
  virtual void operator()(FieldVariable &) {}
  virtual void operator()(SubscriptVariable &) {}
};

class ConstVariableExprVisitor {
public:
  virtual void operator()(const SimpleVariable &) {}
  virtual void operator()(const FieldVariable &) {}
  virtual void operator()(const SubscriptVariable &) {}
};

/// LVALUE := ID | LVALUE.ID | LVALUE[EXPR]
class VariableExpr {
public:
  explicit VariableExpr(String &&id);
  VariableExpr(VariableExprPtr &&lvalue, String &&id);
  VariableExpr(VariableExprPtr &&lvalue, ExprPtr &&expr);
  VariableExpr(VariableExpr &&other);
  VariableExpr &operator=(VariableExpr &&other);
  
  VariableExpr(const VariableExpr &) = delete;
  VariableExpr &operator=(const VariableExpr &) = delete;

  VariableKind getKind() const {
    const size_t idx = data.index();
    assert(idx != std::variant_npos);
    return static_cast<VariableKind>(idx);
  }

  llvm::Expected<const SimpleVariable &> toSimpleVariable() const;
  llvm::Expected<const FieldVariable &> toFieldVariable() const;
  llvm::Expected<const SubscriptVariable &> toSubscriptVariable() const;
  
  llvm::Expected<SimpleVariable &> toSimpleVariable();
  llvm::Expected<FieldVariable &> toFieldVariable();
  llvm::Expected<SubscriptVariable &> toSubscriptVariable();

  /// @brief Utility method to trace the whole path of this variable.
  /// For example, variables that refer to structure fields return a name 
  /// like "StructName.FieldName" and variables that refer to arrays have 
  /// brackets appended.
  /// @param buf buffer to hold concatenation of names.
  /// @return StringRef to the buffer
  llvm::StringRef getName(llvm::SmallString<64>& buf) const;

  void visit(VariableExprVisitor &visitor) {
    std::visit(visitor, data);
  }
  
  void visit(ConstVariableExprVisitor &visitor) const {
    std::visit(visitor, data);
  }

  static VariableExprPtr create(VariableExpr &&var, llvm::BumpPtrAllocator &arena);
  static VariableExprPtr create(String &&id, llvm::BumpPtrAllocator &arena);
  static VariableExprPtr createSV(std::string_view id, llvm::BumpPtrAllocator &arena);
  static VariableExprPtr create(VariableExprPtr &&lvalue, String &&id, llvm::BumpPtrAllocator &arena);
  static VariableExprPtr createSV(VariableExprPtr &&lvalue, std::string_view id, llvm::BumpPtrAllocator &arena);
  static VariableExprPtr create(VariableExprPtr &&lvalue, ExprPtr &&expr, llvm::BumpPtrAllocator &arena);
private:
  using Variant = std::variant<
    SimpleVariable,
    FieldVariable, 
    SubscriptVariable>;
  Variant data; 
};

class AssignExpr {
public:
  AssignExpr(ExprPtr &&variable, ExprPtr &&expr);
  AssignExpr(AssignExpr &&other);
  AssignExpr &operator=(AssignExpr &&other);
  AssignExpr(const AssignExpr &other) = delete;
  AssignExpr &operator=(const AssignExpr &other) = delete;

  Expr &variable() const {
    return *var;
  };
  
  Expr &variable() {
    return *var;
  };

  Expr &expr() const {
    return *rhs;
  }
  
  Expr &expr() {
    return *rhs;
  }
private:
  ExprPtr var;
  ExprPtr rhs;
};

class IfExpr {
public:
  IfExpr(ExprPtr &&cond, ExprPtr &&then);
  IfExpr(ExprPtr &&cond, ExprPtr &&then, ExprPtr &&otherwise);
  IfExpr(IfExpr &&other);
  IfExpr &operator=(IfExpr &&other);
  IfExpr(const IfExpr &) = delete;
  IfExpr &operator=(const IfExpr &) = delete;

  Expr &condition() const;
  Expr &thenBranch() const;
  llvm::Expected<const Expr &> elseBranch() const;
  bool hasElseBranch() const;
private:
  ExprPtr cond;
  ExprPtr then;
  ExprPtr otherwise;
};

class WhileExpr {
public:
  WhileExpr(ExprPtr &&cond, ExprPtr &&body);
  WhileExpr(WhileExpr &&other);
  WhileExpr &operator=(WhileExpr &&other);
  WhileExpr(const WhileExpr &other) = delete;
  WhileExpr &operator=(const WhileExpr &other) = delete;

  Expr &condition() const;
  Expr &bodyExpr() const;
private:
  ExprPtr cond;
  ExprPtr body;
};

class ForExpr {
public:
  ForExpr(ExprPtr &&variable, ExprPtr &&lower, ExprPtr &&upper, ExprPtr &&body);
  ForExpr(ForExpr &&other);
  ForExpr &operator=(ForExpr &&other);
  ForExpr(const ForExpr &other) = delete;
  ForExpr &operator=(const ForExpr &other) = delete;

  Expr &loopVar() const;
  Expr &lowerBound() const;
  Expr &upperBound() const;
  Expr &bodyExpr() const;
private:
  ExprPtr var;
  ExprPtr lo;
  ExprPtr hi; 
  ExprPtr body;
};

class BreakExpr {
public:
};

class VarDecl {
public:
  VarDecl(String &&name, String &&type, ExprPtr &&initalizer);
  VarDecl(VarDecl &&other);
  VarDecl& operator=(VarDecl &&other);
  VarDecl(const VarDecl &other) = delete;
  VarDecl& operator=(const VarDecl &other) = delete;

  llvm::StringRef varName() const;
  llvm::StringRef varType() const; 
  Expr &getInitalizer();
  Expr &getInitalizer() const;
private:
  String name;
  String type;
  ExprPtr initializer;
};

struct ParamDecl {
  String name;
  String type;
  ParamDecl(String &&name, String &&type);
  llvm::StringRef paramName() const { return name.data(); }
  llvm::StringRef paramType() const { return type.data(); } 
};

using ParamList = AllocVector<ParamDecl, llvm::BumpPtrAllocator>;
using ParamListIter = ParamList::iterator;
using ConstParamListIter = ParamList::const_iterator;

class FunctionDecl {
public:
  FunctionDecl(String &&name, ParamList &&params, String &&returnType, ExprPtr &&body);
  FunctionDecl(FunctionDecl &&other);
  FunctionDecl& operator=(FunctionDecl &&other);
  FunctionDecl(const FunctionDecl &other) = delete;
  FunctionDecl& operator=(const FunctionDecl &other) = delete;

  llvm::StringRef functionName() const;
  llvm::StringRef functionReturnType() const;
  Expr &functionBody();
  Expr &functionBody() const;
  ParamListIter begin();
  ParamListIter end(); 
  ConstParamListIter cbegin() const;
  ConstParamListIter cend() const; 

  size_t size() const {
    return parameters.size();
  }

  bool hasReturnType() const {
    return returnType.size() > 0;
  }
private:
  String name;
  ParamList parameters;
  String returnType;
  ExprPtr body;
};

enum class TypeDeclKind : uint8_t {
  Rename,
  Record,
  Array
};

struct RenamedTypeDecl {
  String originalTypeName;
  RenamedTypeDecl(String &&originalTypeName);
  llvm::StringRef getOriginalType() const { return originalTypeName.data(); }
};

struct RecordTypeDecl {
  ParamList fields;
  RecordTypeDecl(ParamList &&params);
  ParamListIter begin() { return fields.begin(); }
  ParamListIter end() { return fields.end(); }
  ConstParamListIter cbegin() const { return fields.cbegin(); }
  ConstParamListIter cend() const { return fields.cend(); }
  size_t size() const {
    return fields.size();
  }
  /// @brief Find record field declaration with the given name.
  /// @param name field name
  /// @return iterator to field declaration if found, 
  ///         iterator to end of field list otherwise
  ConstParamListIter findField(llvm::StringRef name) const;
};

struct ArrayTypeDecl {
  String elementTypeName;
  ArrayTypeDecl(String &&elementTypeName);
  llvm::StringRef getElementType() const { return elementTypeName.data(); }
};

class TypeDeclVisitor {
public:
  virtual void operator()(RenamedTypeDecl &) {}
  virtual void operator()(RecordTypeDecl &) {}
  virtual void operator()(ArrayTypeDecl &) {}
};

class ConstTypeDeclVisitor {
public:
  virtual void operator()(const RenamedTypeDecl &) {}
  virtual void operator()(const RecordTypeDecl &) {}
  virtual void operator()(const ArrayTypeDecl &) {}
};

class TypeDecl {
public:
  explicit TypeDecl(String &&name, RenamedTypeDecl &&decl);
  explicit TypeDecl(String &&name, RecordTypeDecl &&decl);
  explicit TypeDecl(String &&name, ArrayTypeDecl &&decl);
  TypeDecl(TypeDecl &&other);
  TypeDecl &operator=(TypeDecl &&other);
  TypeDecl(const TypeDecl &) = delete;
  TypeDecl &operator=(const TypeDecl &) = delete;

  llvm::StringRef typeName() const;
  TypeDeclKind getKind() const;
  llvm::Expected<const RenamedTypeDecl &> toRenameDecl() const;
  llvm::Expected<const ArrayTypeDecl &> toArrayDecl() const;
  llvm::Expected<const RecordTypeDecl &> toRecordDecl() const;

  void visit(TypeDeclVisitor &visitor) {
    std::visit(visitor, data);
  }

  void visit(ConstTypeDeclVisitor &visitor) const {
    std::visit(visitor, data);
  }
private:
  using Variant = std::variant<
    RenamedTypeDecl,
    RecordTypeDecl,
    ArrayTypeDecl>;
  
  String name;
  Variant data;
};

enum class DeclKind : uint8_t {
  Variable = 0,
  Function = 1,
  Type = 2
};

class DeclVisitor {
public:
  virtual void operator()(VarDecl &) {}
  virtual void operator()(FunctionDecl &) {}
  virtual void operator()(TypeDecl &) {}
};

class ConstDeclVisitor {
public:
  virtual void operator()(const VarDecl &) {}
  virtual void operator()(const FunctionDecl &) {}
  virtual void operator()(const TypeDecl &) {}
};

class Decl {
public:
  explicit Decl(VarDecl &&decl);
  explicit Decl(FunctionDecl &&decl);
  explicit Decl(TypeDecl &&delc);
  Decl(Decl &&other);
  Decl &operator=(Decl &&other);
  Decl(const Decl &other) = delete;
  Decl &operator=(const Decl &other) = delete;

  DeclKind getKind() const;
  llvm::Expected<const VarDecl &> toVarDecl() const;
  llvm::Expected<const FunctionDecl &> toFunctionDecl() const;
  llvm::Expected<const TypeDecl &> toTypeDecl() const;

  void visit(DeclVisitor &visitor) {
    std::visit(visitor, decl);
  }
  
  void visit(ConstDeclVisitor &visitor) const {
    std::visit(visitor, decl);
  }
private:
  std::variant<
    VarDecl,
    FunctionDecl,
    TypeDecl> decl;
};

inline const VarDecl &unwrapVarDecl(const Decl &decl) {
  return llvm::cantFail(decl.toVarDecl());
}

inline const FunctionDecl &unwrapFunctionDecl(const Decl &decl) {
  return llvm::cantFail(decl.toFunctionDecl());
}

inline const TypeDecl &unwrapTypeDecl(const Decl &decl) {
  return llvm::cantFail(decl.toTypeDecl());
}

inline const RenamedTypeDecl &unwrapRenamedTypeDecl(const Decl &decl) {
  return llvm::cantFail(unwrapTypeDecl(decl).toRenameDecl());
}

inline const RecordTypeDecl &unwrapRecordTypeDecl(const Decl &decl) {
  return llvm::cantFail(unwrapTypeDecl(decl).toRecordDecl());
}

inline const ArrayTypeDecl &unwrapArrayTypeDecl(const Decl &decl) {
  return llvm::cantFail(unwrapTypeDecl(decl).toArrayDecl());
}

using DeclList = AllocVector<Decl, llvm::BumpPtrAllocator>;
using DeclListIter = DeclList::iterator;
using ConstDeclListIter = DeclList::const_iterator;

class LetExpr {
public:
  LetExpr(DeclList &&decls, ExprPtr &&body);
  LetExpr(LetExpr &&other);
  LetExpr &operator=(LetExpr &&other);
  LetExpr(const LetExpr &other) = delete;
  LetExpr &operator=(const LetExpr &other) = delete;

  Expr &innerExprs();
  Expr &innerExprs() const;
  DeclListIter begin();
  DeclListIter end();
  ConstDeclListIter cbegin() const;
  ConstDeclListIter cend() const;
  size_t size() const {
    return declarations.size();
  }

  /// @brief Lookup declaration by name.
  /// If not fitting declaration can be found in this let 
  /// expression an iterator to the end of the declaration list
  /// is returned.
  /// @param name variable, type or function name
  /// @return iterator to declaration if found
  ConstDeclListIter findDecl(llvm::StringRef name) const;

  /// @brief Lookup variable declaration by name.
  /// If no variable with the given name was declared in this let
  /// expression an iterator to the end of the declaration 
  /// list is returned.
  /// @param name variable name
  /// @return iterator to variable declaration if found
  ConstDeclListIter findVarDecl(llvm::StringRef name) const;

  /// @brief Lookup type declaration by name.
  /// If no type with the given name was declared in this let
  /// expression an iterator to the end of the declaration 
  /// list is returned.
  /// @param name variable name
  /// @return iterator to variable declaration if found
  ConstDeclListIter findTypeDecl(llvm::StringRef name) const;

  /// @brief Lookup function declaration by name.
  /// If no function with the given name was declared in this let
  /// expression an iterator to the end of the declaration 
  /// list is returned.
  /// @param name variable name
  /// @return iterator to variable declaration if found
  ConstDeclListIter findFunctionDecl(llvm::StringRef name) const;
private:
  DeclList declarations;
  ExprPtr body;
};

class ArrayExpr {
public:
  ArrayExpr(String &&type, ExprPtr &&size, ExprPtr &&init);
  ArrayExpr(ArrayExpr &&other);
  ArrayExpr &operator=(ArrayExpr &&other);
  ArrayExpr(const ArrayExpr &other) = delete;
  ArrayExpr &operator=(const ArrayExpr &other) = delete;

  llvm::StringRef arrayType() const;
  Expr &arraySize();
  Expr &arraySize() const;
  Expr &initialValue();
  Expr &initialValue() const;
private:
  String type;
  ExprPtr size;
  ExprPtr initializer;
};

class ExprVisitor {
public:
  virtual void operator()(NilExpr &) {}
  virtual void operator()(VariableExpr &) {}
  virtual void operator()(IntLitExpr &) {}
  virtual void operator()(StrLitExpr &) {}
  virtual void operator()(CallExpr &) {}
  virtual void operator()(OperatorExpr &) {}
  virtual void operator()(RecordExpr &) {}
  virtual void operator()(SequenceExpr &) {}
  virtual void operator()(AssignExpr &) {}
  virtual void operator()(IfExpr &) {}
  virtual void operator()(WhileExpr &) {}
  virtual void operator()(ForExpr &) {}
  virtual void operator()(BreakExpr &) {}
  virtual void operator()(LetExpr &) {}
  virtual void operator()(ArrayExpr &) {}
};

class ConstExprVisitor {
public:
  virtual void operator()(const NilExpr &) {}
  virtual void operator()(const VariableExpr &) {}
  virtual void operator()(const IntLitExpr &) {}
  virtual void operator()(const StrLitExpr &) {}
  virtual void operator()(const CallExpr &) {}
  virtual void operator()(const OperatorExpr &) {}
  virtual void operator()(const RecordExpr &) {}
  virtual void operator()(const SequenceExpr &) {}
  virtual void operator()(const AssignExpr &) {}
  virtual void operator()(const IfExpr &) {}
  virtual void operator()(const WhileExpr &) {}
  virtual void operator()(const ForExpr &) {}
  virtual void operator()(const BreakExpr &) {}
  virtual void operator()(const LetExpr &) {}
  virtual void operator()(const ArrayExpr &) {}
};

class Expr {
public:
  template <class ExprT>
  Expr(ExprT &&expr):
    data(std::move(expr))
  {}

  template <class ExprT>
  static ExprPtr create(ExprT &&expr, llvm::BumpPtrAllocator &arena) {
    return allocateUnique<Expr, llvm::BumpPtrAllocator>(arena, std::move(expr));
  }

  [[nodiscard]] llvm::Expected<NilExpr &> toNil() {
    return toExprT<NilExpr>();
  }

  [[nodiscard]] llvm::Expected<IntLitExpr &> toIntLit() {
    return toExprT<IntLitExpr>();
  }

  [[nodiscard]] llvm::Expected<StrLitExpr &> toStrLit() {
    return toExprT<StrLitExpr>();
  }

  [[nodiscard]] llvm::Expected<OperatorExpr &> toOperator() {
    return toExprT<OperatorExpr>();
  }

  [[nodiscard]] llvm::Expected<RecordExpr &> toRecord() {
    return toExprT<RecordExpr>();
  }

  [[nodiscard]] llvm::Expected<CallExpr &> toCall() {
    return toExprT<CallExpr>();
  }

  [[nodiscard]] llvm::Expected<VariableExpr &> toVariable() {
    return toExprT<VariableExpr>();
  }

  [[nodiscard]] llvm::Expected<AssignExpr &> toAssign() {
    return toExprT<AssignExpr>();
  }

  [[nodiscard]] llvm::Expected<IfExpr &> toIf() {
    return toExprT<IfExpr>();
  }

  [[nodiscard]] llvm::Expected<WhileExpr &> toWhile() {
    return toExprT<WhileExpr>();
  }

  [[nodiscard]] llvm::Expected<ForExpr &> toFor() {
    return toExprT<ForExpr>();
  }

  [[nodiscard]] llvm::Expected<BreakExpr &> toBreak() {
    return toExprT<BreakExpr>();
  }

  [[nodiscard]] llvm::Expected<LetExpr &> toLet() {
    return toExprT<LetExpr>();
  }

  [[nodiscard]] llvm::Expected<ArrayExpr &> toArray() {
    return toExprT<ArrayExpr>();
  }

  [[nodiscard]] llvm::Expected<SequenceExpr &> toSequence() {
    return toExprT<SequenceExpr>();
  }

  [[nodiscard]] llvm::Expected<const NilExpr &> toNil() const {
    return toExprT<NilExpr>();
  }

  [[nodiscard]] llvm::Expected<const IntLitExpr &> toIntLit() const {
    return toExprT<IntLitExpr>();
  }

  [[nodiscard]] llvm::Expected<const StrLitExpr &> toStrLit() const {
    return toExprT<StrLitExpr>();
  }

  [[nodiscard]] llvm::Expected<const OperatorExpr &> toOperator() const {
    return toExprT<OperatorExpr>();
  }

  [[nodiscard]] llvm::Expected<const RecordExpr &> toRecord() const {
    return toExprT<RecordExpr>();
  }

  [[nodiscard]] llvm::Expected<const CallExpr &> toCall() const {
    return toExprT<CallExpr>();
  }

  [[nodiscard]] llvm::Expected<const VariableExpr &> toVariable() const {
    return toExprT<VariableExpr>();
  }

  [[nodiscard]] llvm::Expected<const AssignExpr &> toAssign() const {
    return toExprT<AssignExpr>();
  }

  [[nodiscard]] llvm::Expected<const IfExpr &> toIf() const {
    return toExprT<IfExpr>();
  }

  [[nodiscard]] llvm::Expected<const WhileExpr &> toWhile() const {
    return toExprT<WhileExpr>();
  }

  [[nodiscard]] llvm::Expected<const ForExpr &> toFor() const {
    return toExprT<ForExpr>();
  }
  
  [[nodiscard]] llvm::Expected<const BreakExpr &> toBreak() const {
    return toExprT<BreakExpr>();
  }
  
  [[nodiscard]] llvm::Expected<const LetExpr &> toLet() const {
    return toExprT<LetExpr>();
  }

  [[nodiscard]] llvm::Expected<const ArrayExpr &> toArray() const {
    return toExprT<ArrayExpr>();
  }
  
  [[nodiscard]] llvm::Expected<const SequenceExpr &> toSequence() const {
    return toExprT<SequenceExpr>();
  }

  [[nodiscard]] llvm::Expected<NilExpr> takeNil() {
    return takeExprT<NilExpr>();
  }

  [[nodiscard]] llvm::Expected<IntLitExpr> takeIntLit() {
    return takeExprT<IntLitExpr>();
  }

  [[nodiscard]] llvm::Expected<OperatorExpr> takeOperator() {
    return takeExprT<OperatorExpr>();
  }

  [[nodiscard]] llvm::Expected<RecordExpr> takeRecord() {
    return takeExprT<RecordExpr>();
  }

  [[nodiscard]] llvm::Expected<CallExpr> takeCall() {
    return takeExprT<CallExpr>();
  }

  [[nodiscard]] llvm::Expected<VariableExpr> takeVariable() {
    return takeExprT<VariableExpr>();
  }

  [[nodiscard]] llvm::Expected<AssignExpr> takeAssign() {
    return takeExprT<AssignExpr>();
  }

  [[nodiscard]] llvm::Expected<IfExpr> takeIf() {
    return takeExprT<IfExpr>();
  }

  [[nodiscard]] llvm::Expected<WhileExpr> takeWhile() {
    return takeExprT<WhileExpr>();
  }

  [[nodiscard]] llvm::Expected<ForExpr> takeFor() {
    return takeExprT<ForExpr>();
  }

  [[nodiscard]] llvm::Expected<BreakExpr> takeBreak() {
    return takeExprT<BreakExpr>();
  }

  [[nodiscard]] llvm::Expected<LetExpr> takeLet() {
    return takeExprT<LetExpr>();
  }

  [[nodiscard]] llvm::Expected<ArrayExpr> takeArray() {
    return takeExprT<ArrayExpr>();
  }
  
  [[nodiscard]] llvm::Expected<SequenceExpr> takeSequence() {
    return takeExprT<SequenceExpr>();
  }

  ExprKind getKind() const {
    return static_cast<ExprKind>(data.index());
  }

  bool contains(ExprKind kind) const {
    return getKind() == kind;
  } 

  void visit(ExprVisitor &visitor) {
    std::visit(visitor, data);
  }

  void visit(ConstExprVisitor &visitor) const {
    std::visit(visitor, data);
  }
private:
  std::variant< 
    NilExpr,
    VariableExpr,
    IntLitExpr,
    StrLitExpr,
    CallExpr,
    OperatorExpr,
    RecordExpr,
    SequenceExpr,
    AssignExpr,
    IfExpr,
    WhileExpr,
    ForExpr,
    BreakExpr,
    LetExpr,
    ArrayExpr
  > data;

  template <class ExprT> 
  llvm::Expected<ExprT &> toExprT() {
    try {
      return std::get<ExprT>(data);
    } catch (std::bad_variant_access ex) {
      auto ec = std::make_error_code(std::errc::operation_not_permitted);
      return llvm::createStringError(ec, ex.what());
    }
  }
  
  template <class ExprT> 
  llvm::Expected<const ExprT &> toExprT() const {
    try {
      return std::get<ExprT>(data);
    } catch (std::bad_variant_access ex) {
      auto ec = std::make_error_code(std::errc::operation_not_permitted);
      return llvm::createStringError(ec, ex.what());
    }
  }

  template <class ExprT> 
  llvm::Expected<ExprT> takeExprT() {
    try {
      ExprT ret = std::move(std::get<ExprT>(data));
      return ret;
    } catch (std::bad_variant_access ex) {
      auto ec = std::make_error_code(std::errc::operation_not_permitted);
      return llvm::createStringError(ec, ex.what());
    }
  }
};

/// @brief Utility function, mostly for testing. Check if the contained
///        xxpression is of the given kind.
/// @param expr expression
/// @param kind expression kind
/// @return true if the inner expression is of the given kind, false otherwise
inline bool hasKind(llvm::Expected<ExprPtr> &expr, ExprKind kind) {
  if (!expr) {
    return false;
  }
  return expr.get()->getKind() == kind;
}

} // namespace ast
} // namespace tig

namespace llvm {

template <>
struct format_provider<tig::ast::ExprKind> {
  static void format(const tig::ast::ExprKind &v, raw_ostream &stream, StringRef style);
};

}
#endif
