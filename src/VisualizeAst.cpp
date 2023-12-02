#include "VisualizeAst.h"
#include "llvm/Support/raw_ostream.h"

namespace tig {

AstNodeInfo::AstNodeInfo(const ast::NilExpr &) noexcept: AstNodeInfo() {
  strcpy(typeName, "NilExpr");
}

AstNodeInfo::AstNodeInfo(const ast::IntLitExpr &expr) noexcept: AstNodeInfo() {
  strcpy(typeName, "IntLitExpr");
  strcpy(param1, "value");
  snprintf(value1, ParamValueLength, "%llu", expr.getValue());
}

AstNodeInfo::AstNodeInfo(const ast::StrLitExpr &expr) noexcept: AstNodeInfo() {
  strcpy(typeName, "StrLitExpr");
  strcpy(param1, "value");
  strncpy(value1, expr.getValue().data(), ParamValueLength);
}

AstNodeInfo::AstNodeInfo(const ast::OperatorExpr &expr) noexcept: AstNodeInfo() {
  strcpy(typeName, "OperatorExpr");
  strcpy(param1, "operator kind");
  const char *opstr;
  switch (expr.opKind()) {
  case ast::OperatorKind::Plus:
    opstr = "Plus";
    break;
  case ast::OperatorKind::Minus:
    opstr = "Minus";
    break;
  case ast::OperatorKind::Times:
    opstr = "Times";
    break;
  case ast::OperatorKind::Divide:
    opstr = "Divide";
    break;
  case ast::OperatorKind::Equals:
    opstr = "Equals";
    break;
  case ast::OperatorKind::Unequal:
    opstr = "Unequal";
    break;
  case ast::OperatorKind::LessThan:
    opstr = "LessThan";
    break;
  case ast::OperatorKind::GreaterThan:
    opstr = "GreaterThan";
    break;
  case ast::OperatorKind::LessEqual:
    opstr = "LessEqual";
    break;
  case ast::OperatorKind::GreaterEqual:
    opstr = "GreaterEqual";
    break;
  case ast::OperatorKind::Or:
    opstr = "Or";
    break;
  case ast::OperatorKind::And:
    opstr = "And";
    break;
  }
  strcpy(value1, opstr); 
}

AstNodeInfo::AstNodeInfo(const ast::Field &field) noexcept: AstNodeInfo() {
  strcpy(typeName, "(Record) Field");
  strcpy(param1, "identifier");
  strncpy(value1, field.symbol.data(), ParamValueLength);
}

AstNodeInfo::AstNodeInfo(const ast::RecordExpr &expr) noexcept: AstNodeInfo() {
  strcpy(typeName, "RecordExpr");
  strcpy(param1, "number of fields");
  snprintf(value1, ParamValueLength, "%zu", expr.size());
}

AstNodeInfo::AstNodeInfo(const ast::CallExpr &expr) noexcept: AstNodeInfo() {
  strcpy(typeName, "CallExpr");
  strcpy(param1, "function name");
  strncpy(value1, expr.functionName().data(), ParamValueLength);
  strcpy(param2, "number of function params");
  snprintf(value2, ParamValueLength, "%zu", expr.size());
}

AstNodeInfo::AstNodeInfo(const ast::SequenceExpr &expr) noexcept: AstNodeInfo() {
  strcpy(typeName, "SequenceExpr");
  strcpy(param1, "number of inner expressions");
  snprintf(value1, ParamValueLength, "%zu", expr.size());
}

AstNodeInfo::AstNodeInfo(const ast::SimpleVariable &var) noexcept: AstNodeInfo() {
  strcpy(typeName, "Simple Variable");
  strcpy(param1, "identifier");
  strncpy(value1, var.getId().data(), ParamValueLength);
}

AstNodeInfo::AstNodeInfo(const ast::FieldVariable &var) noexcept: AstNodeInfo() {
  strcpy(typeName, "Record Field Variable");
  strcpy(param1, "identifier");
  strncpy(value1, var.getId().data(), ParamValueLength);
}

AstNodeInfo::AstNodeInfo(const ast::SubscriptVariable &) noexcept: AstNodeInfo() {
  strcpy(typeName, "Subscript Variable");
}

AstNodeInfo::AstNodeInfo(const ast::VariableExpr &expr) noexcept: AstNodeInfo() {
  strcpy(typeName, "VariableExpr");
  strcpy(param1, "variable type");
  const char *kstr;
  switch (expr.getKind()) {
  case ast::VariableKind::Field:
    kstr = "Record Field Variable";
    break;
  case ast::VariableKind::Subscript:
    kstr = "Subscript Variable";
    break;
  case ast::VariableKind::Simple:
    kstr = "Simple Variable";
    break;
  }
  strcpy(value1, kstr);
}

AstNodeInfo::AstNodeInfo(const ast::AssignExpr &) noexcept: AstNodeInfo() {
  strcpy(typeName, "AssignExpr");
}

AstNodeInfo::AstNodeInfo(const ast::IfExpr &expr) noexcept: AstNodeInfo() {
  strcpy(typeName, "IfExpr");
  strcpy(param1, "has else branch");
  if (expr.hasElseBranch()) {
    strcpy(value1, "true");
  } else {
    strcpy(value1, "false");
  }
}

AstNodeInfo::AstNodeInfo(const ast::WhileExpr &) noexcept: AstNodeInfo() {
  strcpy(typeName, "WhileExpr");
}

AstNodeInfo::AstNodeInfo(const ast::ForExpr &) noexcept: AstNodeInfo() {
  strcpy(typeName, "ForExpr");
}

AstNodeInfo::AstNodeInfo(const ast::BreakExpr &) noexcept: AstNodeInfo() {
  strcpy(typeName, "BreakExpr");
}

AstNodeInfo::AstNodeInfo(const ast::VarDecl &decl) noexcept: AstNodeInfo() {
  strcpy(typeName, "Variable Declaration");
  strcpy(param1, "identifier");
  strncpy(value1, decl.varName().data(), ParamValueLength);
  strcpy(param2, "type");
  strncpy(value2, decl.varType().data(), ParamValueLength);
}

AstNodeInfo::AstNodeInfo(const ast::ParamDecl &decl) noexcept: AstNodeInfo() {
  strcpy(typeName, "Parameter Declaration");
  strcpy(param1, "identifier");
  strncpy(value1, decl.paramName().data(), ParamValueLength);
  strcpy(param2, "type");
  strncpy(value2, decl.paramType().data(), ParamValueLength);
}

AstNodeInfo::AstNodeInfo(const ast::FunctionDecl &decl) noexcept: AstNodeInfo() {
  strcpy(typeName, "Function Declaration");
  strcpy(param1, "function name");
  strncpy(value1, decl.functionName().data(), ParamValueLength);
  strcpy(param2, "number of parameters");
  snprintf(value2, ParamValueLength, "%zu", decl.size());
  if (decl.hasReturnType()) {
    strcpy(param3, "return type");
    strncpy(value3, decl.functionReturnType().data(), ParamValueLength);
  }
}

AstNodeInfo::AstNodeInfo(const ast::RenamedTypeDecl &decl) noexcept: AstNodeInfo() {
  strcpy(typeName, "Renaming Type Declaration");
  strcpy(param1, "original type name");
  strncpy(value1, decl.getOriginalType().data(), ParamValueLength);
}

AstNodeInfo::AstNodeInfo(const ast::RecordTypeDecl &decl) noexcept: AstNodeInfo() {
  strcpy(typeName, "Record Type Declaration");
  strcpy(param1, "number of fields");
  snprintf(value1, ParamValueLength, "%zu", decl.size());
}

AstNodeInfo::AstNodeInfo(const ast::ArrayTypeDecl &decl) noexcept: AstNodeInfo() {
  strcpy(typeName, "Array Type Declaration");
  strcpy(param1, "element type name");
  strncpy(value1, decl.getElementType().data(), ParamValueLength);
} 

AstNodeInfo::AstNodeInfo(const ast::TypeDecl &decl) noexcept: AstNodeInfo() {
  strcpy(typeName, "Type Declaration");
  strcpy(param1, "type name");
  strncpy(value1, decl.typeName().data(), ParamValueLength);
  strcpy(param2, "type declaration type");
  const char *kstr;
  switch (decl.getKind()) {
  case ast::TypeDeclKind::Array:
    kstr = "Array Type Declaration";
    break;
  case ast::TypeDeclKind::Record:
    kstr = "Record Type Declaration";
    break;
  case ast::TypeDeclKind::Rename:
    kstr = "Renaming Type Declaration";
    break;
  }
  strcpy(value2, kstr);
}

AstNodeInfo::AstNodeInfo(const ast::Decl &decl) noexcept: AstNodeInfo() {
  strcpy(typeName, "Declaration");
  strcpy(param2, "declaration type");
  const char *kstr;
  switch (decl.getKind()) {
  case ast::DeclKind::Function:
    kstr = "Function Declaration";
    break;
  case ast::DeclKind::Type:
    kstr = "Type Declaration";
    break;
  case ast::DeclKind::Variable:
    kstr = "Variable Declaration";
    break;
  }
  strcpy(value2, kstr);
}

AstNodeInfo::AstNodeInfo(const ast::LetExpr &expr) noexcept: AstNodeInfo() {
  strcpy(typeName, "LetExpr");
  strcpy(param1, "number of declarations");
  snprintf(value1, ParamValueLength, "%zu", expr.size());
}

AstNodeInfo::AstNodeInfo(const ast::ArrayExpr &expr) noexcept: AstNodeInfo() {
  strcpy(typeName, "ArrayExpr");
  strcpy(param1, "element type");
  strncpy(value1, expr.arrayType().data(), ParamValueLength);
}

void TypeDeclVisualizer::operator()(const ast::RenamedTypeDecl &decl) {
  nodes.push_back(AstNodeInfo(decl));
  indents.push_back(indent);
}

void TypeDeclVisualizer::operator()(const ast::RecordTypeDecl &decl) {
  nodes.push_back(AstNodeInfo(decl));
  for (auto it = decl.cbegin(); it != decl.cend(); ++it) {
    nodes.push_back(AstNodeInfo(*it));
    indents.push_back(indent + 1);
  }
}

void TypeDeclVisualizer::operator()(const ast::ArrayTypeDecl &decl) {
  nodes.push_back(AstNodeInfo(decl));
  indents.push_back(indent);
}

void VariableExprVisualizer::append(VariableExprVisualizer &&vis) {
  if (nodes.empty()) {
    nodes = std::move(vis.nodes);
  } else {
    std::move(vis.nodes.begin(), vis.nodes.end(), std::back_inserter(nodes));
    vis.nodes.clear();
  }
  if (indents.empty()) {
    indents = std::move(vis.indents);
  } else {
    std::move(vis.indents.begin(), vis.indents.end(), std::back_inserter(indents));
    vis.indents.clear();
  }
}

void VariableExprVisualizer::append(ExprVisualizer &&vis) {
  std::vector<AstNodeInfo> rNodes(vis.moveNodes());
  std::vector<int> rIndents(vis.moveIndents());
  if (nodes.empty()) {
    nodes = std::move(rNodes);
  } else {
    std::move(rNodes.begin(), rNodes.end(), std::back_inserter(nodes));
  }
  if (indents.empty()) {
    indents = std::move(rIndents);
  } else {
    std::move(rIndents.begin(), rIndents.end(), std::back_inserter(indents));
  }
}

void VariableExprVisualizer::operator()(const ast::SimpleVariable &var) {
  nodes.push_back(AstNodeInfo(var));
  indents.push_back(indent);
}

void VariableExprVisualizer::operator()(const ast::FieldVariable &var) {
  nodes.push_back(AstNodeInfo(var));
  indents.push_back(indent);
  VariableExprVisualizer vis(indent + 1);
  var.getVar().visit(vis);
  append(std::move(vis));
}

void VariableExprVisualizer::operator()(const ast::SubscriptVariable &var) {
  nodes.push_back(AstNodeInfo(var));
  indents.push_back(indent);
  VariableExprVisualizer vis(indent + 1);
  var.lvalue->visit(vis);
  append(std::move(vis));
  ExprVisualizer evis(indent + 1);
  var.expr->visit(evis);
  append(std::move(evis));
}

void ExprVisualizer::append(VariableExprVisualizer &&vis) {
  std::vector<AstNodeInfo> rNodes(vis.moveNodes());
  std::vector<int> rIndents(vis.moveIndents());
  if (nodes.empty()) {
    nodes = std::move(rNodes);
  } else {
    std::move(rNodes.begin(), rNodes.end(), std::back_inserter(nodes));
  }
  if (indents.empty()) {
    indents = std::move(rIndents);
  } else {
    std::move(rIndents.begin(), rIndents.end(), std::back_inserter(indents));
  }
}

void ExprVisualizer::append(ExprVisualizer &&vis) {
  std::vector<AstNodeInfo> rNodes(vis.moveNodes());
  std::vector<int> rIndents(vis.moveIndents());
  if (nodes.empty()) {
    nodes = std::move(rNodes);
  } else {
    std::move(rNodes.begin(), rNodes.end(), std::back_inserter(nodes));
  }
  if (indents.empty()) {
    indents = std::move(rIndents);
  } else {
    std::move(rIndents.begin(), rIndents.end(), std::back_inserter(indents));
  }
}

void ExprVisualizer::append(DeclVisualizer &&vis) {
  std::vector<AstNodeInfo> rNodes(vis.moveNodes());
  std::vector<int> rIndents(vis.moveIndents());
  if (nodes.empty()) {
    nodes = std::move(rNodes);
  } else {
    std::move(rNodes.begin(), rNodes.end(), std::back_inserter(nodes));
  }
  if (indents.empty()) {
    indents = std::move(rIndents);
  } else {
    std::move(rIndents.begin(), rIndents.end(), std::back_inserter(indents));
  }
}

void ExprVisualizer::operator()(const ast::NilExpr &expr) {
  nodes.push_back(AstNodeInfo(expr));
  indents.push_back(indent);
}

void ExprVisualizer::operator()(const ast::VariableExpr &expr) {
  nodes.push_back(AstNodeInfo(expr));
  indents.push_back(indent);
  VariableExprVisualizer vis(indent + 1);
  expr.visit(vis);
  append(std::move(vis));
}

void ExprVisualizer::operator()(const ast::IntLitExpr &expr) {
  nodes.push_back(AstNodeInfo(expr));
  indents.push_back(indent);
}

void ExprVisualizer::operator()(const ast::StrLitExpr &expr) {
  nodes.push_back(AstNodeInfo(expr));
  indents.push_back(indent);
}

void ExprVisualizer::operator()(const ast::CallExpr &expr) {
  nodes.push_back(AstNodeInfo(expr));
  indents.push_back(indent);
  ExprVisualizer vis(indent + 1);
  for (auto it = expr.cbegin(); it != expr.cend(); ++it) {
    (*it)->visit(vis);
  } 
  append(std::move(vis));
}

void ExprVisualizer::operator()(const ast::OperatorExpr &expr) {
  nodes.push_back(AstNodeInfo(expr));
  indents.push_back(indent);
  ExprVisualizer vis(indent + 1);
  expr.leftOperand().visit(vis);
  expr.rightOperand().visit(vis);
  append(std::move(vis));
}

void ExprVisualizer::operator()(const ast::RecordExpr &expr) {
  nodes.push_back(AstNodeInfo(expr));
  indents.push_back(indent);
  ExprVisualizer vis(indent + 1);
  for (auto it = expr.cbegin(); it != expr.cend(); ++it) {
    vis.nodes.push_back(AstNodeInfo(*it));
    vis.indents.push_back(vis.indent);
    it->expr->visit(vis);
  }
  append(std::move(vis));
}

void ExprVisualizer::operator()(const ast::SequenceExpr &expr) {
  nodes.push_back(AstNodeInfo(expr));
  indents.push_back(indent);
  ExprVisualizer vis(indent + 1);
  for (auto it = expr.cbegin(); it != expr.cend(); ++it) {
    (*it)->visit(vis);
  }
  append(std::move(vis));
}

void ExprVisualizer::operator()(const ast::AssignExpr &expr) {
  nodes.push_back(AstNodeInfo(expr));
  indents.push_back(indent);
  ExprVisualizer vis(indent + 1);
  expr.variable().visit(vis);
  expr.expr().visit(vis);
  append(std::move(vis));
}

void ExprVisualizer::operator()(const ast::IfExpr &expr) {
  nodes.push_back(AstNodeInfo(expr));
  indents.push_back(indent);
  ExprVisualizer vis(indent + 1);
  expr.condition().visit(vis);
  expr.thenBranch().visit(vis);
  if (expr.hasElseBranch()) {
    expr.elseBranch()->visit(vis);
  }
  append(std::move(vis));
}

void ExprVisualizer::operator()(const ast::WhileExpr &expr) {
  nodes.push_back(AstNodeInfo(expr));
  indents.push_back(indent);
  ExprVisualizer vis(indent + 1);
  expr.condition().visit(vis);
  expr.bodyExpr().visit(vis);
  append(std::move(vis));
}

void ExprVisualizer::operator()(const ast::ForExpr &expr) {
  nodes.push_back(AstNodeInfo(expr));
  indents.push_back(indent);
  ExprVisualizer vis(indent + 1);
  expr.loopVar().visit(vis);
  expr.lowerBound().visit(vis);
  expr.upperBound().visit(vis);
  expr.bodyExpr().visit(vis);
  append(std::move(vis));
}

void ExprVisualizer::operator()(const ast::BreakExpr &expr) {
  nodes.push_back(AstNodeInfo(expr));
  indents.push_back(indent);
}

void ExprVisualizer::operator()(const ast::ArrayExpr &expr) {
  nodes.push_back(AstNodeInfo(expr));
  indents.push_back(indent);
  ExprVisualizer vis(indent + 1);
  expr.arraySize().visit(vis);
  expr.initialValue().visit(vis);
  append(std::move(vis));
}

void DeclVisualizer::append(TypeDeclVisualizer &&vis) {
  std::vector<AstNodeInfo> rNodes(vis.moveNodes());
  std::vector<int> rIndents(vis.moveIndents());
  if (nodes.empty()) {
    nodes = std::move(rNodes);
  } else {
    std::move(rNodes.begin(), rNodes.end(), std::back_inserter(nodes));
  }
  if (indents.empty()) {
    indents = std::move(rIndents);
  } else {
    std::move(rIndents.begin(), rIndents.end(), std::back_inserter(indents));
  }
}

void DeclVisualizer::append(ExprVisualizer &&vis) {
  std::vector<AstNodeInfo> rNodes(vis.moveNodes());
  std::vector<int> rIndents(vis.moveIndents());
  if (nodes.empty()) {
    nodes = std::move(rNodes);
  } else {
    std::move(rNodes.begin(), rNodes.end(), std::back_inserter(nodes));
  }
  if (indents.empty()) {
    indents = std::move(rIndents);
  } else {
    std::move(rIndents.begin(), rIndents.end(), std::back_inserter(indents));
  }
}

void DeclVisualizer::operator()(const ast::VarDecl &decl) {
  nodes.push_back(AstNodeInfo(decl));
  indents.push_back(indent);
  ExprVisualizer vis(indent + 1);
  decl.getInitalizer().visit(vis);
  append(std::move(vis));
}

void DeclVisualizer::operator()(const ast::FunctionDecl &decl) {
  nodes.push_back(AstNodeInfo(decl));
  indents.push_back(indent);
  for (auto it = decl.cbegin(); it != decl.cend(); ++it) {
    nodes.push_back(AstNodeInfo(*it));
    indents.push_back(indent + 1);
  }
  ExprVisualizer vis(indent + 1);
  decl.functionBody().visit(vis);
  append(std::move(vis));
}

void DeclVisualizer::operator()(const ast::TypeDecl &decl) {
  nodes.push_back(AstNodeInfo(decl));
  indents.push_back(indent);
  TypeDeclVisualizer vis(indent + 1);
  decl.visit(vis);
  append(std::move(vis));
}

void ExprVisualizer::operator()(const ast::LetExpr &expr) {
  nodes.push_back(AstNodeInfo(expr));
  indents.push_back(indent);
  DeclVisualizer dvis(indent + 1);
  for (auto it = expr.cbegin(); it != expr.cend(); ++it) {
    it->visit(dvis);
  }
  append(std::move(dvis));
  ExprVisualizer vis(indent + 1);
  expr.innerExprs().visit(vis);
  append(std::move(vis));
}

void ExprVisualizer::print(llvm::raw_ostream &os) const {
  for (size_t i = 0; i < size(); ++i) {
    for (int ind = 0; ind < indents.at(i); ++ind) {
      os << '.';
    }
    os << llvm::formatv("{0}\n", nodes.at(i));
  }
}

} // namespace

llvm::raw_ostream &llvm::operator<<(
  llvm::raw_ostream &os, 
  const tig::ast::Expr &expr) 
{
  tig::ExprVisualizer vis;
  expr.visit(vis);
  vis.print(os);
  return os;
}

void llvm::format_provider<tig::AstNodeInfo>::format(
  const tig::AstNodeInfo &info, 
  llvm::raw_ostream &out, 
  llvm::StringRef)
{
  out << llvm::formatv("{0} -- ", info.nodeType());
  if (info.firstParamValue().size() > 0) {
    out << llvm::formatv("{0}: {1}; ", 
      info.firstParamName(), 
      info.firstParamValue());
  }
  if (info.secondParamValue().size() > 0) {
    out << llvm::formatv("{0}: {1}; ", 
      info.secondParamName(), 
      info.secondParamValue());
  }
  if (info.thirdParamValue().size() > 0) {
    out << llvm::formatv("{0}: {1}; ", 
      info.thirdParamName(), 
      info.thirdParamValue());
  }
}
