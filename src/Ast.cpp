#include "Ast.h"

namespace tig::ast {

IntLitExpr::IntLitExpr(uint64_t value) noexcept: 
  value(value) 
{}

IntLitExpr::IntLitExpr(IntLitExpr &&rhs) noexcept: 
  value(rhs.value) 
{}

IntLitExpr &IntLitExpr::operator=(IntLitExpr &&rhs) noexcept {
  if (this == &rhs) {
    return *this;
  }
  value = rhs.value;
  return *this;
}

llvm::Expected<IntLitExpr> IntLitExpr::fromStr(llvm::StringRef sref) {
  uint64_t value;
  if (!sref.getAsInteger(0, value)) {
    auto ec = std::make_error_code(std::errc::invalid_argument);
    return llvm::createStringError(ec, "Could not parse integer.");
  }
  return IntLitExpr(value);
}

uint64_t IntLitExpr::getValue() const {
  return value;
}
StrLitExpr::StrLitExpr(llvm::StringRef sref, llvm::BumpPtrAllocator &arena): 
  value(sref.data(), sref.size(), arena) 
{}

StrLitExpr::StrLitExpr(StrLitExpr&& rhs): 
  value(std::move(rhs.value)) 
{}

StrLitExpr &StrLitExpr::operator=(StrLitExpr &&rhs) {
  if (this == &rhs) {
    return *this;
  }
  value = std::move(rhs.value);
  return *this;
}

llvm::StringRef StrLitExpr::getValue() const {
  return llvm::StringRef(value.data());
}

OperatorExpr::OperatorExpr(ExprPtr &&lhs, ExprPtr &&rhs, OperatorKind kind) noexcept:
  lhs(std::move(lhs)), rhs(std::move(rhs)), kind(kind)
{}

OperatorExpr::OperatorExpr(OperatorExpr &&other) noexcept:
  lhs(std::move(other.lhs)), rhs(std::move(other.rhs)), kind(other.kind)
{}

OperatorExpr &OperatorExpr::operator=(OperatorExpr &&other) {
  if (this == &other) {
    return *this;
  }
  lhs = std::move(other.lhs);
  rhs = std::move(other.rhs);
  kind = other.kind;
  return *this;
}

OperatorKind OperatorExpr::opKind() const {
  return kind;
}

const Expr &OperatorExpr::leftOperand() const {
  return *lhs;
}

const Expr &OperatorExpr::rightOperand() const {
  return *rhs;
}

Expr &OperatorExpr::leftOperand() {
  return *lhs;
}

Expr &OperatorExpr::rightOperand() {
  return *rhs;
}

Field::Field(String &&symbol, ExprPtr &&expr):
  symbol(std::move(symbol)),
  expr(std::move(expr))
{}

RecordExpr::RecordExpr(FieldList &&fields): 
  fields(std::move(fields)) 
{}

RecordExpr::RecordExpr(RecordExpr &&other): 
  fields(std::move(other.fields)) 
{}

RecordExpr &RecordExpr::operator=(RecordExpr &&other) {
  if (this == &other) {
    return *this;
  }
  fields = std::move(other.fields);
  return *this;
}

FieldIter RecordExpr::begin() { 
  return fields.begin(); 
}

FieldIter RecordExpr::end() { 
  return fields.end(); 
}

ConstFieldIter RecordExpr::cbegin() const { 
  return fields.cbegin(); 
}

ConstFieldIter RecordExpr::cend() const { 
  return fields.cend(); 
}

CallExpr::CallExpr(String &&symbol, ExprList &&exprs):
  symbol(std::move(symbol)),
  exprs(std::move(exprs))
{}

CallExpr::CallExpr(CallExpr &&other):
  symbol(std::move(other.symbol)),
  exprs(std::move(other.exprs))
{}

CallExpr &CallExpr::operator=(CallExpr &&other) {
  if (this == &other) {
    return *this;
  }
  exprs = std::move(other.exprs);
  return *this;
}

llvm::StringRef CallExpr::functionName() const {
  return llvm::StringRef(symbol.data());
}

ExprListIter CallExpr::begin() { 
  return exprs.begin(); 
}

ExprListIter CallExpr::end() { 
  return exprs.end(); 
}

ConstExprListIter CallExpr::cbegin() const { 
  return exprs.cbegin(); 
}

ConstExprListIter CallExpr::cend() const { 
  return exprs.cend(); 
}

SequenceExpr::SequenceExpr(ExprList &&exprs):
  exprs(std::move(exprs))
{}

SequenceExpr::SequenceExpr(SequenceExpr &&other):
  exprs(std::move(other.exprs))
{}

SequenceExpr &SequenceExpr::operator=(SequenceExpr &&other) {
  if (this == &other) {
    return *this;
  }
  exprs = std::move(other.exprs);
  return *this;
}

ExprListIter SequenceExpr::begin() { 
  return exprs.begin(); 
}

ExprListIter SequenceExpr::end() { 
  return exprs.end(); 
}

ConstExprListIter SequenceExpr::cbegin() const { 
  return exprs.cbegin(); 
}

ConstExprListIter SequenceExpr::cend() const { 
  return exprs.cend(); 
}

SimpleVariable::SimpleVariable(String &&id):
  id(std::move(id))
{}

FieldVariable::FieldVariable(VariableExprPtr &&var, String &&id):
  lvalue(std::move(var)),
  id(std::move(id))
{}

SubscriptVariable::SubscriptVariable(VariableExprPtr &&var, ExprPtr &&expr):
  lvalue(std::move(var)),
  expr(std::move(expr))
{}

VariableExpr::VariableExpr(String &&id):
  data(std::move(id))
{}

VariableExpr::VariableExpr(VariableExprPtr &&lvalue, String &&id):
  data(FieldVariable {std::move(lvalue), std::move(id)})
{}

VariableExpr::VariableExpr(VariableExprPtr &&lvalue, ExprPtr &&expr):
  data(SubscriptVariable {std::move(lvalue), std::move(expr)})
{}

VariableExpr::VariableExpr(VariableExpr &&other) :
  data(std::move(other.data))
{}

VariableExpr &VariableExpr::operator=(VariableExpr &&other) {
  if (this == &other) {
    return *this;
  }
  data = std::move(other.data);
  return *this;
}

VariableExprPtr VariableExpr::create(VariableExpr &&var, llvm::BumpPtrAllocator &arena) {
  return allocateUnique<VariableExpr, llvm::BumpPtrAllocator>(arena, std::move(var));
}

VariableExprPtr VariableExpr::create(String &&id, llvm::BumpPtrAllocator &arena) {
  return create(VariableExpr(std::move(id)), arena);
}

VariableExprPtr VariableExpr::createSV(std::string_view id, llvm::BumpPtrAllocator &arena) {
  String idStr(id.data(), arena);
  return create(VariableExpr(std::move(idStr)), arena);
}

VariableExprPtr VariableExpr::create(VariableExprPtr &&lvalue, String &&id, llvm::BumpPtrAllocator &arena) {
  return create(VariableExpr(std::move(lvalue), std::move(id)), arena);
}

VariableExprPtr VariableExpr::createSV(VariableExprPtr &&lvalue, std::string_view id, llvm::BumpPtrAllocator &arena) {
  String idStr(id.data(), arena);
  return create(VariableExpr(std::move(lvalue), std::move(idStr)), arena);
}

VariableExprPtr VariableExpr::create(VariableExprPtr &&lvalue, ExprPtr &&expr, llvm::BumpPtrAllocator &arena) {
  return create(VariableExpr(std::move(lvalue), std::move(expr)), arena);
}

llvm::Expected<const SimpleVariable &> VariableExpr::toSimpleVariable() const {
  try {
    return std::get<SimpleVariable>(data);
  } catch (std::bad_variant_access ex) {
    auto ec = std::make_error_code(std::errc::operation_not_permitted);
    return llvm::createStringError(ec, ex.what());
  }
}

llvm::Expected<SimpleVariable &> VariableExpr::toSimpleVariable() {
  try {
    return std::get<SimpleVariable>(data);
  } catch (std::bad_variant_access ex) {
    auto ec = std::make_error_code(std::errc::operation_not_permitted);
    return llvm::createStringError(ec, ex.what());
  }
}

llvm::Expected<const FieldVariable &> VariableExpr::toFieldVariable() const {
  try {
    return std::get<FieldVariable>(data);
  } catch (std::bad_variant_access ex) {
    auto ec = std::make_error_code(std::errc::operation_not_permitted);
    return llvm::createStringError(ec, ex.what());
  }
}

llvm::Expected<FieldVariable &> VariableExpr::toFieldVariable() {
  try {
    return std::get<FieldVariable>(data);
  } catch (std::bad_variant_access ex) {
    auto ec = std::make_error_code(std::errc::operation_not_permitted);
    return llvm::createStringError(ec, ex.what());
  }
}

llvm::Expected<const SubscriptVariable &> VariableExpr::toSubscriptVariable() const {
  try {
    return std::get<SubscriptVariable>(data);
  } catch (std::bad_variant_access ex) {
    auto ec = std::make_error_code(std::errc::operation_not_permitted);
    return llvm::createStringError(ec, ex.what());
  }
}

llvm::Expected<SubscriptVariable &> VariableExpr::toSubscriptVariable() {
  try {
    return std::get<SubscriptVariable>(data);
  } catch (std::bad_variant_access ex) {
    auto ec = std::make_error_code(std::errc::operation_not_permitted);
    return llvm::createStringError(ec, ex.what());
  }
}

llvm::StringRef VariableExpr::getName(llvm::SmallString<64>& buf) const {
  switch (getKind()) {
    case VariableKind::Simple: {
      llvm::StringRef id = llvm::cantFail(toSimpleVariable()).getId();
      buf.append(id);
      break;
    }
    case VariableKind::Field: {
      llvm::cantFail(toFieldVariable()).getVar().getName(buf);
      llvm::StringRef id = llvm::cantFail(toFieldVariable()).getId();
      buf.append(".");
      buf.append(id);
      break;
    }
    case VariableKind::Subscript: {
      llvm::cantFail(toSubscriptVariable()).getVar().getName(buf);
      buf.append("[]");
      break;
    }
  }
  return buf;
}

AssignExpr::AssignExpr(ExprPtr &&variable, ExprPtr &&expr):
  var(std::move(variable)),
  rhs(std::move(expr)) 
{}

AssignExpr::AssignExpr(AssignExpr &&other):
  var(std::move(other.var)),
  rhs(std::move(other.rhs))
{}

AssignExpr &AssignExpr::operator=(AssignExpr &&other) {
  if (this == &other) {
    return *this;
  }
  var = std::move(other.var);
  rhs = std::move(other.rhs);
  return *this;
}

IfExpr::IfExpr(ExprPtr &&cond, ExprPtr &&then):
  cond(std::move(cond)),
  then(std::move(then)),
  otherwise(nullptr)
{}

IfExpr::IfExpr(ExprPtr &&cond, ExprPtr &&then, ExprPtr &&otherwise):
  cond(std::move(cond)),
  then(std::move(then)),
  otherwise(std::move(otherwise))
{}

IfExpr::IfExpr(IfExpr &&other):
  cond(std::move(other.cond)),
  then(std::move(other.then)),
  otherwise(std::move(other.otherwise))
{}

IfExpr &IfExpr::operator=(IfExpr &&other) {
  if (this == &other) {
    return *this;
  }
  cond = std::move(other.cond);
  then = std::move(other.then);
  otherwise = std::move(other.otherwise);
  return *this;
}

llvm::Expected<const Expr &> IfExpr::elseBranch() const {
  if (otherwise) {
    return *otherwise;
  }
  auto ec = std::make_error_code(std::errc::bad_address);
  return llvm::createStringError(ec, "no else branch set");
}

Expr &IfExpr::condition() const {
  return *cond;
}

Expr &IfExpr::thenBranch() const {
  return *then;
}

bool IfExpr::hasElseBranch() const {
  return otherwise != nullptr;
}

WhileExpr::WhileExpr(ExprPtr &&cond, ExprPtr &&body):
  cond(std::move(cond)),
  body(std::move(body))
{}

WhileExpr::WhileExpr(WhileExpr &&other):
  cond(std::move(other.cond)),
  body(std::move(other.body))
{}

WhileExpr &WhileExpr::operator=(WhileExpr &&other) {
  if (this == &other) {
    return *this;
  }
  cond = std::move(other.cond);
  body = std::move(other.body);
  return *this;
}

Expr &WhileExpr::condition() const {
  return *cond;
}

Expr &WhileExpr::bodyExpr() const {
  return *body;
}

ForExpr::ForExpr(ExprPtr &&variable, ExprPtr &&lower, ExprPtr &&upper, ExprPtr &&body):
  var(std::move(variable)),
  lo(std::move(lower)),
  hi(std::move(upper)),
  body(std::move(body))
{}

ForExpr::ForExpr(ForExpr &&other):
  var(std::move(other.var)),
  lo(std::move(other.lo)),
  hi(std::move(other.hi)),
  body(std::move(other.body))
{}

ForExpr &ForExpr::operator=(ForExpr &&other) {
  if (this == &other) {
    return *this;
  }
  var = std::move(other.var);
  lo = std::move(other.lo);
  hi = std::move(other.hi);
  body = std::move(other.body);
  return *this;
}

Expr &ForExpr::loopVar() const {
  return *var;
}

Expr &ForExpr::lowerBound() const {
  return *lo;
}

Expr &ForExpr::upperBound() const {
  return *hi;
}

Expr &ForExpr::bodyExpr() const {
  return *body;
}

VarDecl::VarDecl(String &&name, String &&type, ExprPtr &&init):
  name(std::move(name)),
  type(std::move(type)),
  initializer(std::move(init))
{}

VarDecl::VarDecl(VarDecl &&other):
  name(std::move(other.name)),
  type(std::move(other.type)),
  initializer(std::move(other.initializer))
{}

VarDecl& VarDecl::operator=(VarDecl &&other) {
  if (this == &other) {
    return *this;
  }
  name = std::move(other.name);
  type = std::move(other.type);
  initializer = std::move(other.initializer);
  return *this;
}

llvm::StringRef VarDecl::varName() const {
  return name.data();
}

llvm::StringRef VarDecl::varType() const {
  return type.data();
}

Expr &VarDecl::getInitalizer() {
  return *initializer;
}

Expr &VarDecl::getInitalizer() const {
  return *initializer;
}

ParamDecl::ParamDecl(String &&name, String &&type):
  name(std::move(name)),
  type(std::move(type))
{}

FunctionDecl::FunctionDecl(String &&name, ParamList &&params, String &&returnType, ExprPtr &&body):
  name(std::move(name)),
  parameters(std::move(params)),
  returnType(std::move(returnType)),
  body(std::move(body))
{}

FunctionDecl::FunctionDecl(FunctionDecl &&other):
  name(std::move(other.name)),
  parameters(std::move(other.parameters)),
  returnType(std::move(other.returnType)),
  body(std::move(other.body))
{}

FunctionDecl& FunctionDecl::operator=(FunctionDecl &&other) {
  if (this == &other) {
    return *this;
  }
  name = std::move(other.name);
  parameters = std::move(other.parameters);
  returnType = std::move(other.returnType);
  body = std::move(other.body);
  return *this;
}
  
llvm::StringRef FunctionDecl::functionName() const {
  return name.data();
}

llvm::StringRef FunctionDecl::functionReturnType() const {
  return returnType.data();
}

Expr &FunctionDecl::functionBody() const {
  return *body;
}

Expr &FunctionDecl::functionBody() {
  return *body;
}

ParamListIter FunctionDecl::begin() { 
  return parameters.begin(); 
}

ParamListIter FunctionDecl::end() { 
  return parameters.end(); 
}

ConstParamListIter FunctionDecl::cbegin() const { 
  return parameters.cbegin(); 
}

ConstParamListIter FunctionDecl::cend() const { 
  return parameters.cend(); 
}

RenamedTypeDecl::RenamedTypeDecl(String &&originalTypeName):
  originalTypeName(std::move(originalTypeName))
{}

RecordTypeDecl::RecordTypeDecl(ParamList &&fields):
  fields(std::move(fields))
{}

ConstParamListIter RecordTypeDecl::findField(llvm::StringRef name) const {
  for (auto it = cbegin(), E = cend(); it != E; ++it) {
    if (it->paramName() == name) {
      return it;
    }
  }
  return cend();
}

ArrayTypeDecl::ArrayTypeDecl(String &&elementTypeName):
  elementTypeName(std::move(elementTypeName))
{}

TypeDecl::TypeDecl(String &&name, RenamedTypeDecl &&decl):
  name(std::move(name)),
  data(std::move(decl))
{}

TypeDecl::TypeDecl(String &&name, RecordTypeDecl &&decl):
  name(std::move(name)),
  data(std::move(decl))
{}

TypeDecl::TypeDecl(String &&name, ArrayTypeDecl &&decl):
  name(std::move(name)),
  data(std::move(decl))
{}

TypeDecl::TypeDecl(TypeDecl &&other):
  name(std::move(other.name)),
  data(std::move(other.data))
{}

TypeDecl &TypeDecl::operator=(TypeDecl &&other) {
  if (this == &other) {
    return *this;
  }
  name = std::move(other.name);
  data = std::move(other.data);
  return *this;
}

llvm::StringRef TypeDecl::typeName() const {
  return name.data();
}

TypeDeclKind TypeDecl::getKind() const {
  return static_cast<TypeDeclKind>(data.index());
}

llvm::Expected<const RenamedTypeDecl &> TypeDecl::toRenameDecl() const {
  try {
    return std::get<RenamedTypeDecl>(data);
  } catch (std::bad_variant_access ex) {
    auto ec = std::make_error_code(std::errc::operation_not_permitted);
    return llvm::createStringError(ec, ex.what());
  }
}

llvm::Expected<const ArrayTypeDecl &> TypeDecl::toArrayDecl() const {
  try {
    return std::get<ArrayTypeDecl>(data);
  } catch (std::bad_variant_access ex) {
    auto ec = std::make_error_code(std::errc::operation_not_permitted);
    return llvm::createStringError(ec, ex.what());
  }
}

llvm::Expected<const RecordTypeDecl &> TypeDecl::toRecordDecl() const {
  try {
    return std::get<RecordTypeDecl>(data);
  } catch (std::bad_variant_access ex) {
    auto ec = std::make_error_code(std::errc::operation_not_permitted);
    return llvm::createStringError(ec, ex.what());
  }
}

Decl::Decl(VarDecl &&decl):
  decl(std::move(decl))
{}

Decl::Decl(FunctionDecl &&decl):
  decl(std::move(decl))
{}

Decl::Decl(TypeDecl &&decl):
  decl(std::move(decl))
{}

Decl::Decl(Decl &&other):
  decl(std::move(other.decl))
{}

Decl &Decl::operator=(Decl &&other) {
  if (this == &other) {
    return *this;
  }
  decl = std::move(other.decl);
  return *this;
}

DeclKind Decl::getKind() const {
  return static_cast<DeclKind>(decl.index());
}

llvm::Expected<const VarDecl &> Decl::toVarDecl() const {
  try {
    return std::get<VarDecl>(decl);
  } catch (std::bad_variant_access ex) {
    auto ec = std::make_error_code(std::errc::operation_not_permitted);
    return llvm::createStringError(ec, ex.what());
  }
}

llvm::Expected<const FunctionDecl &> Decl::toFunctionDecl() const {
  try {
    return std::get<FunctionDecl>(decl);
  } catch (std::bad_variant_access ex) {
    auto ec = std::make_error_code(std::errc::operation_not_permitted);
    return llvm::createStringError(ec, ex.what());
  }
}

llvm::Expected<const TypeDecl &> Decl::toTypeDecl() const {
  try {
    return std::get<TypeDecl>(decl);
  } catch (std::bad_variant_access ex) {
    auto ec = std::make_error_code(std::errc::operation_not_permitted);
    return llvm::createStringError(ec, ex.what());
  }
}

LetExpr::LetExpr(DeclList &&decls, ExprPtr &&body):
  declarations(std::move(decls)),
  body(std::move(body))
{}

LetExpr::LetExpr(LetExpr &&other):
  declarations(std::move(other.declarations)),
  body(std::move(other.body))
{}

LetExpr &LetExpr::operator=(LetExpr &&other) {
  if (this == &other) {
    return *this;
  }
  declarations = std::move(other.declarations);
  body = std::move(other.body);
  return *this;
}

Expr &LetExpr::innerExprs() {
  return *body;
}

Expr &LetExpr::innerExprs() const {
  return *body;
}

DeclListIter LetExpr::begin() {
  return declarations.begin();
}

DeclListIter LetExpr::end() {
  return declarations.end();
}

ConstDeclListIter LetExpr::cbegin() const {
  return declarations.cbegin();
}

ConstDeclListIter LetExpr::cend() const {
  return declarations.cend();
}

ConstDeclListIter LetExpr::findDecl(llvm::StringRef name) const {
  for (auto it = cbegin(), E = cend(); it != E; ++it) {
    switch (it->getKind()) {
      case DeclKind::Variable: {
        if (it->toVarDecl()->varName() == name) {
          return it;
        }
        break;
      }
      case DeclKind::Function: {
        if (it->toFunctionDecl()->functionName() == name) {
          return it;
        }
        break;
      }
      case DeclKind::Type: {
        if (it->toTypeDecl()->typeName() == name) {
          return it;
        }
        break;
      }
    }
  }
  return cend();
}

ConstDeclListIter LetExpr::findVarDecl(llvm::StringRef name) const {
  for (auto it = cbegin(), E = cend(); it != E; ++it) {
    if (it->getKind() != DeclKind::Variable) {
      continue;
    }
    if (it->toVarDecl()->varName() == name) {
      return it;
    }
  }
  return cend();
}

ConstDeclListIter LetExpr::findTypeDecl(llvm::StringRef name) const {
  for (auto it = cbegin(), E = cend(); it != E; ++it) {
    if (it->getKind() != DeclKind::Type) {
      continue;
    }
    if (it->toTypeDecl()->typeName() == name) {
      return it;
    }
  }
  return cend();
}

ConstDeclListIter LetExpr::findFunctionDecl(llvm::StringRef name) const {
  for (auto it = cbegin(), E = cend(); it != E; ++it) {
    if (it->getKind() != DeclKind::Function) {
      continue;
    }
    if (it->toFunctionDecl()->functionName() == name) {
      return it;
    }
  }
  return cend();
}

ArrayExpr::ArrayExpr(String &&type, ExprPtr &&size, ExprPtr &&init):
  type(std::move(type)),
  size(std::move(size)),
  initializer(std::move(init))
{}

ArrayExpr::ArrayExpr(ArrayExpr &&other):
  type(std::move(other.type)),
  size(std::move(other.size)),
  initializer(std::move(other.initializer))
{}

ArrayExpr &ArrayExpr::operator=(ArrayExpr &&other) {
  if (this == &other) {
    return *this;
  }
  type = std::move(other.type);
  size = std::move(other.size);
  initializer = std::move(other.initializer);
  return *this;
}

llvm::StringRef ArrayExpr::arrayType() const {
  return type.data();
}

Expr &ArrayExpr::arraySize() {
  return *size;
}

Expr &ArrayExpr::arraySize() const {
  return *size;
}

Expr &ArrayExpr::initialValue() {
  return *initializer;
}

Expr &ArrayExpr::initialValue() const {
  return *initializer;
}

} // namespace

void llvm::format_provider<tig::ast::ExprKind>::format(
  const tig::ast::ExprKind &kind, 
  llvm::raw_ostream &out, 
  llvm::StringRef style)
{
  using namespace tig::ast;
  const char *txt;
  switch (kind) {
    case ExprKind::Nil: 
      txt = "Nil";
      break;
    case ExprKind::Variable: 
      txt = "Variable";
      break;
    case ExprKind::IntLit: 
      txt = "IntLit";
      break;
    case ExprKind::StrLit:
      txt = "StrLit";
      break;
    case ExprKind::Call:
      txt = "Call";
      break;
    case ExprKind::Operator:
      txt = "Operator";
      break;
    case ExprKind::Record:
      txt = "Record";
      break;
    case ExprKind::Sequence:
      txt = "Sequence";
      break;
    case ExprKind::Assign:
      txt = "Assign";
      break;
    case ExprKind::If:
      txt = "If";
      break;
    case ExprKind::While:
      txt = "While";
      break;
    case ExprKind::For:
      txt = "For";
      break;
    case ExprKind::Break:
      txt = "Break";
      break;
    case ExprKind::Let:
      txt = "Let";
      break;
    case ExprKind::Array:
      txt = "Array";
      break;
    default:
      llvm_unreachable("non-exhaustive switch statement");
  }
  out << txt;
}
