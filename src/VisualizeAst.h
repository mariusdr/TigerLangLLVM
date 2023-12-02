#ifndef TIGER_LANG_VISUALIZE_AST_H
#define TIGER_LANG_VISUALIZE_AST_H

#include "Support.h"
#include "Ast.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/FormatProviders.h"
#include <cstring>

namespace tig {

class AstNodeInfo {
private:
  static constexpr size_t TypeNameLength = 32;
  static constexpr size_t ParamNameLength = 32;
  static constexpr size_t ParamValueLength = 128;
public:
  AstNodeInfo() {
    memset(typeName, 0, TypeNameLength);
    memset(param1, 0, ParamNameLength);
    memset(value1, 0, ParamValueLength);
    memset(param2, 0, ParamNameLength);
    memset(value2, 0, ParamValueLength);
    memset(param3, 0, ParamNameLength);
    memset(value3, 0, ParamValueLength);
  }
  explicit AstNodeInfo(const ast::NilExpr &expr) noexcept;
  explicit AstNodeInfo(const ast::IntLitExpr &expr) noexcept;
  explicit AstNodeInfo(const ast::StrLitExpr &expr) noexcept;
  explicit AstNodeInfo(const ast::OperatorExpr &expr) noexcept;
  explicit AstNodeInfo(const ast::Field &field) noexcept;
  explicit AstNodeInfo(const ast::RecordExpr &expr) noexcept;
  explicit AstNodeInfo(const ast::CallExpr &expr) noexcept;
  explicit AstNodeInfo(const ast::SequenceExpr &expr) noexcept;
  explicit AstNodeInfo(const ast::SimpleVariable &var) noexcept;
  explicit AstNodeInfo(const ast::FieldVariable &var) noexcept;
  explicit AstNodeInfo(const ast::SubscriptVariable &var) noexcept;
  explicit AstNodeInfo(const ast::VariableExpr &expr) noexcept;
  explicit AstNodeInfo(const ast::AssignExpr &expr) noexcept;
  explicit AstNodeInfo(const ast::IfExpr &expr) noexcept;
  explicit AstNodeInfo(const ast::WhileExpr &expr) noexcept;
  explicit AstNodeInfo(const ast::ForExpr &expr) noexcept;
  explicit AstNodeInfo(const ast::BreakExpr &expr) noexcept;
  explicit AstNodeInfo(const ast::VarDecl &decl) noexcept;
  explicit AstNodeInfo(const ast::ParamDecl &decl) noexcept;
  explicit AstNodeInfo(const ast::FunctionDecl &decl) noexcept;
  explicit AstNodeInfo(const ast::RenamedTypeDecl &decl) noexcept;
  explicit AstNodeInfo(const ast::RecordTypeDecl &decl) noexcept;
  explicit AstNodeInfo(const ast::ArrayTypeDecl &decl) noexcept;
  explicit AstNodeInfo(const ast::TypeDecl &decl) noexcept;
  explicit AstNodeInfo(const ast::Decl &decl) noexcept;
  explicit AstNodeInfo(const ast::LetExpr &expr) noexcept;
  explicit AstNodeInfo(const ast::ArrayExpr &expr) noexcept;

  llvm::StringRef nodeType() const {
    return typeName;
  }

  llvm::StringRef firstParamName() const {
    return { param1, strlen(param1) };
  }

  llvm::StringRef firstParamValue() const {
    return { value1, strlen(value1) };
  }

  llvm::StringRef secondParamName() const {
    return { param2, strlen(param2) };
  }

  llvm::StringRef secondParamValue() const {
    return { value2, strlen(value2) };
  }

  llvm::StringRef thirdParamName() const {
    return { param3, strlen(param3) };
  }

  llvm::StringRef thirdParamValue() const {
    return { value3, strlen(value3) };
  }
private:
  char typeName[TypeNameLength];
  char param1[ParamNameLength];
  char param2[ParamNameLength];
  char param3[ParamNameLength];
  char value1[ParamValueLength];
  char value2[ParamValueLength];
  char value3[ParamValueLength];
};

class TypeDeclVisualizer : public ast::ConstTypeDeclVisitor {
public:
  explicit TypeDeclVisualizer(): indent(0) {}
  explicit TypeDeclVisualizer(int indent): indent(indent) {}

  virtual void operator()(const ast::RenamedTypeDecl &) override;
  virtual void operator()(const ast::RecordTypeDecl &) override;
  virtual void operator()(const ast::ArrayTypeDecl &) override;
  
  std::vector<AstNodeInfo> moveNodes() { return nodes; }
  std::vector<int> moveIndents() { return indents; }
private:
  std::vector<AstNodeInfo> nodes;
  std::vector<int> indents;
  int indent;
};

class ExprVisualizer;

class DeclVisualizer : public ast::ConstDeclVisitor {
public:
  explicit DeclVisualizer(int indent): indent(indent) {}
  explicit DeclVisualizer(): indent(0) {}

  virtual void operator()(const ast::VarDecl &) override;
  virtual void operator()(const ast::FunctionDecl &) override;
  virtual void operator()(const ast::TypeDecl &) override;
  
  std::vector<AstNodeInfo> moveNodes() { return nodes; }
  std::vector<int> moveIndents() { return indents; }
private:
  std::vector<AstNodeInfo> nodes;
  std::vector<int> indents;
  int indent;

  void append(TypeDeclVisualizer &&vis);
  void append(ExprVisualizer &&vis);
};

class VariableExprVisualizer;

class ExprVisualizer : public ast::ConstExprVisitor {
public:
  explicit ExprVisualizer(): indent(0) {}
  explicit ExprVisualizer(int indent): indent(indent) {}

  virtual void operator()(const ast::NilExpr &) override;
  virtual void operator()(const ast::VariableExpr &) override;
  virtual void operator()(const ast::IntLitExpr &) override;
  virtual void operator()(const ast::StrLitExpr &) override;
  virtual void operator()(const ast::CallExpr &) override;
  virtual void operator()(const ast::OperatorExpr &) override;
  virtual void operator()(const ast::RecordExpr &) override;
  virtual void operator()(const ast::SequenceExpr &) override;
  virtual void operator()(const ast::AssignExpr &) override;
  virtual void operator()(const ast::IfExpr &) override;
  virtual void operator()(const ast::WhileExpr &) override;
  virtual void operator()(const ast::ForExpr &) override;
  virtual void operator()(const ast::BreakExpr &) override;
  virtual void operator()(const ast::LetExpr &) override;
  virtual void operator()(const ast::ArrayExpr &) override;

  std::vector<AstNodeInfo> moveNodes() { return nodes; }
  std::vector<int> moveIndents() { return indents; }
  size_t size() const {
    return nodes.size();
  }
  std::pair<int, AstNodeInfo> at(size_t idx) const {
    return std::make_pair(indents.at(idx), nodes.at(idx));
  }
  void print(llvm::raw_ostream &os) const;
private:
  std::vector<AstNodeInfo> nodes;
  std::vector<int> indents;
  int indent;

  void append(VariableExprVisualizer &&vis);
  void append(ExprVisualizer &&vis);
  void append(DeclVisualizer &&vis);
};

class VariableExprVisualizer : public ast::ConstVariableExprVisitor {
public:
  explicit VariableExprVisualizer(int indent): indent(indent) {}
  explicit VariableExprVisualizer(): indent(0) {}

  virtual void operator()(const ast::SimpleVariable &var) override;
  virtual void operator()(const ast::FieldVariable &var) override;
  virtual void operator()(const ast::SubscriptVariable &var) override;

  std::vector<AstNodeInfo> moveNodes() { return nodes; }
  std::vector<int> moveIndents() { return indents; }
private:
  std::vector<AstNodeInfo> nodes;
  std::vector<int> indents;
  int indent;

  void append(VariableExprVisualizer &&vis);
  void append(ExprVisualizer &&vis);
};


} // namespace

namespace llvm {

raw_ostream &operator<<(raw_ostream &os, const tig::ast::Expr &expr);

template <>
struct format_provider<tig::AstNodeInfo> {
  static void format(const tig::AstNodeInfo &v, raw_ostream &stream, StringRef style);
};

}
#endif
